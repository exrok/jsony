use std::alloc::{Layout, alloc, dealloc, handle_alloc_error, realloc};
use std::borrow::Cow;
use std::fmt;
use std::marker::PhantomData;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ptr::{self, NonNull};

/// An optimized output buffer with dynamic backing storage.
///
/// Similar to `dyn std::io::Write`, `BytesWriter` can be backed by various types including:
/// - `Vec<u8>`
/// - `&mut [MaybeUninit<u8>]`
/// - `dyn std::io::Write`
///
/// The implementation minimizes dynamic dispatch by restricting it to out-of-capacity situations
/// where the underlying buffer needs to be resized or flushed.  ///
/// `BytesWriter` guarantees to maintain a buffer, and when backing storage is already viable,
/// that backing will be used directly. Operations like `push` are guaranteed not to flush to
/// the backing store, meaning pushed bytes remain available and can be popped and discarded.
/// Note that `push_bytes` does not provide this same guarantee.
///
/// See [`crate::TextWriter`] for a UTF-8-specific version that guarantees valid UTF-8 content.
#[repr(C)]
pub struct BytesWriter<'a> {
    data: *mut u8,
    len: usize,
    capacity: usize,
    backing: Backing<'a>,
}

impl Default for BytesWriter<'_> {
    /// A empty BytesWriter backed by an owned buffer.
    fn default() -> Self {
        Self::new()
    }
}

impl<'a> From<&'a mut [MaybeUninit<u8>]> for BytesWriter<'a> {
    fn from(value: &'a mut [MaybeUninit<u8>]) -> Self {
        BytesWriter {
            capacity: value.len(),
            data: value.as_mut_ptr().cast(),
            len: 0,
            backing: Backing::Borrowed {
                marker: PhantomData,
            },
        }
    }
}

impl<'a> From<&'a mut Vec<u8>> for BytesWriter<'a> {
    fn from(value: &'a mut Vec<u8>) -> Self {
        BytesWriter {
            data: value.as_mut_ptr(),
            len: value.len(),
            capacity: value.capacity(),
            backing: Backing::Vec {
                bytes: NonNull::from(value),
                offset: 0,
                marker: PhantomData,
            },
        }
    }
}

impl Drop for BytesWriter<'_> {
    fn drop(&mut self) {
        match self.backing {
            Backing::Owned | Backing::Write { .. } => {
                // SAFETY: for these backing variants, a non-zero `capacity`
                // means `data` was allocated by this type with the global
                // allocator and alignment 1. A zero-capacity writer uses a
                // dangling pointer and has no allocation to free.
                unsafe {
                    if self.capacity != 0 {
                        let layout = Layout::from_size_align_unchecked(self.capacity, 1);
                        dealloc(self.data, layout);
                    }
                }
            }
            Backing::Vec {
                mut bytes, offset, ..
            } => {
                // SAFETY: this writer was created from an exclusive `&mut Vec<u8>`
                // and keeps that borrow for `'a`. The Vec owns the allocation,
                // and `offset..offset + len` is initialized by the writer.
                unsafe {
                    let vec = bytes.as_mut();
                    vec.set_len(offset + self.len);
                }
            }
            Backing::Borrowed { .. } => (),
        }
    }
}

pub(crate) enum Backing<'a> {
    Owned,
    Vec {
        bytes: NonNull<Vec<u8>>,
        offset: usize,
        // Life time as Vec<u8>... we store it as pointer
        // to avoid issues aliasing issues. Although it should be fine.
        marker: PhantomData<&'a ()>,
    },
    Borrowed {
        marker: PhantomData<&'a ()>,
    },
    Write {
        written: usize,
        error: Option<std::io::Error>,
        writer: &'a mut (dyn std::io::Write + Send),
    },
}

impl<'a> BytesWriter<'a> {
    /// Creates a new `BytesWriter` backed by the provided writer.
    pub fn new_writer(writer: &'a mut (dyn std::io::Write + Send)) -> BytesWriter<'a> {
        BytesWriter {
            data: safe_alloc(4096),
            len: 0,
            capacity: 4096,
            backing: Backing::Write {
                written: 0,
                error: None,
                writer,
            },
        }
    }

    pub(crate) fn from_vec_suffix(value: &'a mut Vec<u8>) -> Self {
        let offset = value.len();
        BytesWriter {
            // SAFETY: `offset == len <= capacity`, so this is either in-bounds
            // or one-past the Vec allocation. A zero-capacity Vec uses a
            // non-null dangling pointer and `add(0)`.
            data: unsafe { value.as_mut_ptr().add(offset) },
            len: 0,
            capacity: value.capacity() - offset,
            backing: Backing::Vec {
                bytes: NonNull::from(value),
                offset,
                marker: PhantomData,
            },
        }
    }

    /// Returns a mutable reference to the last two bytes, if available.
    pub fn last_2(&mut self) -> Option<&mut [u8; 2]> {
        if self.len < 2 {
            return None;
        }
        // SAFETY: `len >= 2`, `0..len` is initialized by the writer invariant,
        // and `&mut self` gives exclusive access to these bytes.
        Some(unsafe { &mut *self.data.add(self.len - 2).cast() })
    }
    /// Returns a mutable reference to the last byte, if available.
    pub fn last(&mut self) -> Option<&mut u8> {
        if self.len == 0 {
            return None;
        }
        // SAFETY: `len > 0`, `0..len` is initialized by the writer invariant,
        // and `&mut self` gives exclusive access to this byte.
        Some(unsafe { &mut *self.data.add(self.len - 1) })
    }
    /// Clears the buffer, setting its length to zero.
    pub fn clear(&mut self) {
        self.len = 0;
    }
    /// Returns true if the buffer is empty.
    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    /// Sets the length of the buffer.
    ///
    /// # Safety
    /// - `new_len` must not exceed the buffer's capacity
    /// - All elements in the range `old_len..new_len` must be initialized
    pub unsafe fn set_len(&mut self, new_len: usize) {
        self.len = new_len;
    }

    /// Decrements the length by 1, or does nothing if the buffer is empty.
    pub fn saturting_pop(&mut self) {
        self.len = self.len.saturating_sub(1);
    }

    /// Creates a new empty `BytesWriter`, that will be backed by a owned buffer.
    #[inline]
    pub fn new() -> BytesWriter<'a> {
        BytesWriter {
            data: NonNull::<u8>::dangling().as_ptr(),
            len: 0,
            capacity: 0,
            backing: Backing::Owned,
        }
    }

    /// Creates a new `BytesWriter` with the specified capacity.
    ///
    /// The writer will be backed by an owned buffer with at least the specified capacity.
    pub fn with_capacity(capacity: usize) -> BytesWriter<'a> {
        let data = safe_alloc(capacity);
        BytesWriter {
            data,
            len: 0,
            capacity,
            backing: Backing::Owned,
        }
    }

    /// Returns a reference to the current contents as a byte slice.
    ///
    /// Note that any write operation except `push` may flush this buffer to the backing store.
    pub fn buffer_slice(&self) -> &[u8] {
        // SAFETY: the writer invariant maintains `len <= capacity` and
        // initializes all bytes in `0..len`. For `len == 0`, `data` may be a
        // dangling but non-null aligned pointer, which is allowed for slices.
        unsafe { std::slice::from_raw_parts(self.data, self.len) }
    }

    /// Converts the writer into a `Vec<u8>`, consuming the writer.
    pub fn into_vec(self) -> Vec<u8> {
        let this = ManuallyDrop::new(self);
        match this.backing {
            Backing::Vec {
                mut bytes, offset, ..
            } => {
                // SAFETY: this writer has exclusive access to the borrowed Vec.
                // The initialized logical buffer ends at `offset + this.len`,
                // so restoring that length preserves its allocation invariants.
                unsafe {
                    let vec = bytes.as_mut();
                    vec.set_len(offset + this.len);
                    std::mem::take(vec)
                }
            }
            Backing::Borrowed { .. } => this.buffer_slice().into(),
            Backing::Write { .. } | Backing::Owned => {
                // SAFETY: owned/write buffers are allocated by this type with the
                // global allocator and alignment 1; for capacity 0 the dangling
                // pointer is valid for an empty Vec. `ManuallyDrop` prevents the
                // writer from freeing the allocation after transferring it.
                unsafe { Vec::from_raw_parts(this.data, this.len, this.capacity) }
            }
        }
    }

    /// Consumes the writer and returns the owned buffer as a `Vec<u8>`.
    ///
    /// ### Errors
    /// Panics if the writer is not backed by an owned buffer (e.g., when using a borrowed slice or writer).
    pub fn owned_into_vec(self) -> Vec<u8> {
        let mut this = ManuallyDrop::new(self);
        if let Backing::Owned = this.backing {
            // SAFETY: an owned buffer is allocated by this type with the global
            // allocator and alignment 1; for capacity 0 the dangling pointer is
            // valid for an empty Vec. `ManuallyDrop` prevents a double free.
            unsafe { Vec::from_raw_parts(this.data, this.len, this.capacity) }
        } else {
            // SAFETY: `this` has not been dropped yet, and this branch does not
            // move out any raw allocation owned by it.
            unsafe { ManuallyDrop::drop(&mut this) };
            panic!("Expected write buffer to backed by an owned allocation");
        }
    }

    /// Consumes the writer and returns a reference to the backing slice with
    /// content added since the creation of ByteWriter.
    ///
    /// ### Errors
    /// Panics if the writer is not backed by a `Vec<u8>`.
    pub fn into_backed_with_extended_slice(self) -> &'a [u8] {
        let mut this = ManuallyDrop::new(self);
        let len = this.len;
        if let Backing::Vec { bytes, offset, .. } = &mut this.backing {
            // SAFETY: the writer has exclusive access to the backing Vec. The
            // logical initialized bytes end at `offset + len`, so restoring the
            // Vec length is valid. The previous Vec length is the creation-time
            // suffix boundary.
            let (data, start, suffix_len) = unsafe {
                let bytes = bytes.as_mut();
                let oldlen = bytes.len();
                let end = *offset + len;
                bytes.set_len(end);
                let start = oldlen.min(end);
                (bytes.as_mut_ptr(), start, end - start)
            };
            // SAFETY: `start <= end`, and the backing Vec now owns these
            // initialized bytes for lifetime `'a`. A zero-length suffix may use
            // a one-past or dangling aligned pointer.
            return unsafe { std::slice::from_raw_parts(data.add(start), suffix_len) };
        }
        // SAFETY: `this` has not been dropped yet, and no allocation was moved
        // out on this error path.
        unsafe { ManuallyDrop::drop(&mut this) };
        panic!("Expected write buffer to backed by a Vec<u8>");
    }

    /// Converts the writer into a `Cow<str>`.
    ///
    /// # Safety
    /// The buffer must contain valid UTF-8 data
    pub unsafe fn into_cow_utf8_unchecked(self) -> Cow<'a, str> {
        let mut this = ManuallyDrop::new(self);
        let data = this.data;
        let len = this.len;
        let capacity = this.capacity;
        match &this.backing {
            // SAFETY: the caller guarantees the initialized bytes are UTF-8.
            // The Vec raw parts belong to this writer and are transferred under
            // `ManuallyDrop`.
            Backing::Owned => Cow::Owned(unsafe {
                String::from_utf8_unchecked(Vec::from_raw_parts(data, len, capacity))
            }),
            // SAFETY: the caller guarantees the initialized bytes are UTF-8,
            // and borrowed backing storage outlives `'a`.
            Backing::Borrowed { .. } => Cow::Borrowed(unsafe {
                std::str::from_utf8_unchecked(std::slice::from_raw_parts(data, len))
            }),
            _ => {
                // SAFETY: `this` has not been dropped yet, and no allocation was
                // moved out on this error path.
                unsafe { ManuallyDrop::drop(&mut this) };
                panic!("Expected Borrowed or owneded Instance");
            }
        }
    }
    /// Converts the writer into a `Cow<[u8]>`.
    ///
    /// # Panics
    /// Panics if the writer is not backed by an owned buffer (e.g., when using a borrowed slice or writer).
    pub fn into_cow(self) -> Cow<'a, [u8]> {
        let mut this = ManuallyDrop::new(self);
        let data = this.data;
        let len = this.len;
        let capacity = this.capacity;
        match &this.backing {
            // SAFETY: the raw parts belong to this writer and are transferred
            // under `ManuallyDrop`; `0..len` is initialized.
            Backing::Owned => Cow::Owned(unsafe { Vec::from_raw_parts(data, len, capacity) }),
            Backing::Borrowed { .. } => {
                // SAFETY: borrowed backing storage outlives `'a`, and `0..len`
                // is initialized by the writer invariant.
                Cow::Borrowed(unsafe { std::slice::from_raw_parts(data, len) })
            }
            _ => {
                // SAFETY: `this` has not been dropped yet, and no allocation was
                // moved out on this error path.
                unsafe { ManuallyDrop::drop(&mut this) };
                panic!("Expected Borrowed or owneded Instance");
            }
        }
    }

    /// Completes the write operation and returns the total number of bytes written.
    ///
    /// ### Errors
    /// Returns an error if writing to the underlying writer fails.
    pub fn into_write_finish(mut self) -> Result<usize, std::io::Error> {
        self.flush();
        match &mut self.backing {
            Backing::Write { written, error, .. } => {
                if let Some(error) = error.take() {
                    return Err(error);
                }
                Ok(*written)
            }
            _ => {
                panic!("Expected Write Instance");
            }
        }
    }

    /// Returns a mutable pointer to the buffer's internal data.
    #[inline]
    pub fn as_mut_ptr(&self) -> *mut u8 {
        self.data
    }

    /// Returns the length of this buffer in bytes
    #[inline]
    pub fn len(&self) -> usize {
        self.len
    }

    /// Returns this buffer's capacity in bytes
    #[inline]
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// Increase the length of buffer by `additional` bytes
    ///
    /// # Safety
    ///
    /// - `additional` must be less than or equal to `capacity() - len()`
    /// - The elements at `old_len..old_len + additional` must be initialized
    #[inline]
    pub unsafe fn advance(&mut self, additional: usize) {
        self.len += additional;
    }

    //  /// Same as String::reserve
    //  ///
    //  /// # Panics
    //  ///
    //  /// This method panics if `size` overflows `isize::MAX`.
    //  #[inline]
    //  pub fn reserve(&mut self, size: usize) {
    //      if size <= self.capacity - self.len {
    //          return;
    //      }
    //      self.reserve_internal(size);
    //  }

    /// Same as String::reserve except that undefined behaviour can result if `size`
    /// overflows `isize::MAX`.
    #[inline]
    pub(crate) unsafe fn reserve_small(&mut self, size: usize) {
        debug_assert!(size <= isize::MAX as usize);
        if self.len + size <= self.capacity {
            return;
        }
        self.reserve_internal(size, true);
    }

    /// Appends the raw byte view of T.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `data` is plain old data for this binary
    /// representation: every byte in `T`, including padding, must be
    /// initialized and valid to copy as output.
    pub unsafe fn push_as_bytes<T>(&mut self, data: &T) {
        // SAFETY: the caller guarantees that every byte in `data` is
        // initialized and may be observed as raw output bytes. The reference is
        // valid for `size_of::<T>()` bytes by construction.
        self.push_bytes(unsafe {
            ::std::slice::from_raw_parts(data as *const T as *const u8, ::std::mem::size_of::<T>())
        });
    }

    /// Appends the given bytes to the end of this buffer.
    ///
    /// Note: may flush the buffer to the underlying IO. If flushing results in
    /// error it will be stored in the writer and returned by `into_write_finish`.
    #[inline]
    pub fn push_bytes(&mut self, data: &[u8]) {
        let size = data.len();

        // SAFETY: `reserve_small(size)` ensures `len + size <= capacity`.
        // Slices cannot exceed the maximum object size, satisfying
        // `reserve_small`'s size precondition. The source slice cannot overlap
        // this writer's mutable destination through safe code because `&mut self`
        // is held for the duration of the call.
        unsafe {
            self.reserve_small(size);

            let p = self.data.add(self.len);
            std::ptr::copy_nonoverlapping(data.as_ptr(), p, size);
            self.len += size;
        }
        debug_assert!(self.len <= self.capacity);
    }

    /// Appends the given `byte` to the end of this buffer, guaranteed not
    /// to flush the buffer to the underlying IO.
    #[inline]
    pub fn push(&mut self, byte: u8) {
        // Inform codegen that the length does not change across reserve_internal with grou false.
        let len = self.len;

        // This will panic or abort if we would allocate > isize::MAX bytes
        // or if the length increment would overflow for zero-sized types.
        if len == self.capacity {
            self.reserve_internal(1, false);
        }

        // SAFETY: after the reserve above, `len < capacity`. The destination
        // byte is within the allocation and `&mut self` provides exclusive
        // access to it.
        unsafe {
            *self.data.add(len) = byte;
            self.len = len + 1;
        }
    }
    /// Appends the given `char` to the end of this buffer
    #[inline]
    pub fn push_char(&mut self, data: char) {
        // SAFETY: `reserve_small(4)` is valid because a char encodes to at most
        // four bytes and then guarantees enough spare capacity for the copy.
        unsafe {
            self.reserve_small(4);
            let mut buffer = [0u8; 4];
            let result = data.encode_utf8(&mut buffer);
            let result = result.as_bytes();
            std::ptr::copy_nonoverlapping(result.as_ptr(), self.data.add(self.len), result.len());
            self.len += result.len();
        }
    }

    /// If the backing store is writer, clear the internal buffer writing contents
    /// to backing writer.
    ///
    /// If not backed by writer this `flush` is a no-op.
    fn flush(&mut self) {
        if let Backing::Write {
            written,
            error,
            writer,
        } = &mut self.backing
        {
            use std::io::Write;
            // SAFETY: `0..len` is initialized by the writer invariant.
            let buffered = unsafe { std::slice::from_raw_parts(self.data, self.len) };
            if let Err(err) = writer.write_all(buffered) {
                *error = Some(err);
            }
            *written += self.len;
            self.len = 0;
        }
    }

    #[cold]
    fn reserve_internal(&mut self, size: usize, can_write: bool) {
        debug_assert!(size <= isize::MAX as usize);
        if let Backing::Write {
            written,
            error,
            writer,
        } = &mut self.backing
        {
            if can_write {
                use std::io::Write;
                // SAFETY: `0..len` is initialized by the writer invariant.
                let buffered = unsafe { std::slice::from_raw_parts(self.data, self.len) };
                if let Err(err) = writer.write_all(buffered) {
                    *error = Some(err);
                    return;
                }
                *written += self.len;
                self.len = 0;
                if self.capacity >= size {
                    return;
                }
            }
        }

        let new_capacity = std::cmp::max(self.capacity * 2, self.capacity + size);
        debug_assert!(new_capacity > self.capacity);
        if let Backing::Borrowed { .. } = &self.backing {
            let new_data = safe_alloc(new_capacity);
            // SAFETY: `new_data` points to a fresh allocation of at least
            // `new_capacity`, `self.data` has at least `self.len` initialized
            // bytes, and fresh allocations cannot overlap borrowed storage.
            unsafe {
                ptr::copy_nonoverlapping(self.data, new_data, self.len);
            }
            self.backing = Backing::Owned;
            self.data = new_data;
        } else if let Backing::Vec { bytes, offset, .. } = &mut self.backing {
            let old_capacity = *offset + self.capacity;
            let new_total_capacity = *offset + new_capacity;
            // SAFETY: the writer has exclusive access to the Vec. Its pointer
            // and capacity are the allocation raw parts, and `old_capacity` is
            // the total allocation capacity represented by this suffix view.
            let data = unsafe {
                let vec = bytes.as_mut();
                let original_len = vec.len();
                let data = safe_realloc(vec.as_mut_ptr(), old_capacity, new_total_capacity);
                bytes.write(Vec::from_raw_parts(data, original_len, new_total_capacity));
                data
            };
            // SAFETY: `offset <= new_total_capacity`, so this is in-bounds or
            // one-past the reallocated Vec allocation.
            self.data = unsafe { data.add(*offset) };
            self.capacity = new_capacity;
            return;
        } else {
            // SAFETY: for non-borrowed backing, `data`/`capacity` either
            // represent no allocation (`capacity == 0`) or an allocation made
            // with the global allocator and alignment 1.
            self.data = unsafe { safe_realloc(self.data, self.capacity, new_capacity) };
        }
        self.capacity = new_capacity;

        debug_assert!(!self.data.is_null());
        debug_assert!(self.len <= self.capacity);
    }
}

#[inline(never)]
fn safe_alloc(capacity: usize) -> *mut u8 {
    assert!(capacity > 0);
    assert!(capacity <= isize::MAX as usize, "capacity is too large");

    // SAFETY: capacity is non-zero, and always multiple of alignment (1).
    unsafe {
        let layout = Layout::from_size_align_unchecked(capacity, 1);
        let data = alloc(layout);
        if data.is_null() {
            handle_alloc_error(layout);
        }

        data
    }
}

/// # Safety
///
/// - if `capacity > 0`, `capacity` is the same value that was used to allocate the block
///   of memory pointed by `ptr`.
#[cold]
#[inline(never)]
unsafe fn safe_realloc(ptr: *mut u8, capacity: usize, new_capacity: usize) -> *mut u8 {
    assert!(new_capacity > 0);
    assert!(new_capacity <= isize::MAX as usize, "capacity is too large");

    // SAFETY: both layouts use alignment 1 and non-zero sizes bounded by
    // `isize::MAX`; when `capacity > 0`, the function safety contract says
    // `ptr` was allocated with exactly that old layout.
    let data = if capacity == 0 {
        // SAFETY: `new_capacity` is non-zero and bounded above; alignment 1 is
        // valid for byte storage.
        unsafe {
            let new_layout = Layout::from_size_align_unchecked(new_capacity, 1);
            alloc(new_layout)
        }
    } else {
        // SAFETY: the function contract says `ptr` was allocated with
        // `capacity` bytes and alignment 1; `new_capacity` is non-zero and
        // bounded above.
        unsafe {
            let old_layout = Layout::from_size_align_unchecked(capacity, 1);
            realloc(ptr, old_layout, new_capacity)
        }
    };

    if data.is_null() {
        // SAFETY: `new_capacity > 0`, alignment 1 is valid, and the size was
        // checked against `isize::MAX` above.
        unsafe {
            handle_alloc_error(Layout::from_size_align_unchecked(new_capacity, 1));
        }
    }

    data
}

impl fmt::Write for BytesWriter<'_> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        BytesWriter::push_bytes(self, s.as_bytes());
        Ok(())
    }
}

// SAFETY: moving a writer to another thread preserves the unique borrow carried
// by its lifetime. All mutation of the raw buffer requires `&mut BytesWriter`,
// and shared methods only expose initialized bytes or raw pointers.
unsafe impl Send for BytesWriter<'_> {}
// SAFETY: shared access does not mutate the buffer or backing writer. Methods
// that can change `data`, `len`, `capacity`, or backing state require `&mut self`
// or ownership of the writer.
unsafe impl Sync for BytesWriter<'_> {}

/// Conversion into a `ByteWriter` with content extraction.
///
/// This is primaryily used in [crate::to_binary_into].
pub trait IntoByteWriter<'a> {
    type Output;

    /// Convert Self into TextWriter, preserving the contents.
    fn into_byte_writer(self) -> BytesWriter<'a>;

    /// Should return Output corresponding to added context since the
    /// creation from `into_text_writer()`.
    ///
    /// If `finish_writing` is called on an instance of TextWriter
    /// not create via `IntoTextWriter::into_text_writer` of the same type then this
    /// method may panic.
    fn finish_writing(buffer: BytesWriter<'a>) -> Self::Output;
}

impl<'a> IntoByteWriter<'a> for &'a mut Vec<u8> {
    type Output = &'a [u8];
    fn into_byte_writer(self) -> BytesWriter<'a> {
        BytesWriter::from(self)
    }
    fn finish_writing(buffer: BytesWriter<'a>) -> &'a [u8] {
        buffer.into_backed_with_extended_slice()
    }
}

pub type DynWrite<'a> = &'a mut (dyn std::io::Write + Send);
impl<'a> IntoByteWriter<'a> for DynWrite<'a> {
    type Output = Result<usize, std::io::Error>;
    fn into_byte_writer(self) -> BytesWriter<'a> {
        BytesWriter::new_writer(self)
    }
    fn finish_writing(buffer: BytesWriter<'a>) -> Result<usize, std::io::Error> {
        buffer.into_write_finish()
    }
}

impl<'a> IntoByteWriter<'a> for &'a mut [MaybeUninit<u8>] {
    type Output = Cow<'a, [u8]>;
    fn into_byte_writer(self) -> BytesWriter<'a> {
        BytesWriter::from(self)
    }
    fn finish_writing(buffer: BytesWriter<'a>) -> Cow<'a, [u8]> {
        buffer.into_cow()
    }
}
