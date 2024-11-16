use std::alloc::{alloc, dealloc, handle_alloc_error, realloc, Layout};
use std::borrow::Cow;
use std::fmt;
use std::marker::PhantomData;
use std::mem::{ManuallyDrop, MaybeUninit};
use std::ptr::{self, NonNull};

/// Output buffer which is dynamically generic over:
///     - Writing to an owned buffer
///     - Writing to `std::io::Write`
///     - Writing to a shared slice
/// Similar to a `std::io::BufWriter<&dyn std::io::Write>`, but optimized
///  for when writing in memory.
#[repr(C)]
pub struct BytesWriter<'a> {
    data: *mut u8,
    len: usize,
    capacity: usize,
    backing: Backing<'a>,
}
impl<'a> Default for BytesWriter<'a> {
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
        let mut _0 = ManuallyDrop::new(std::mem::take(value));
        BytesWriter {
            data: _0.as_mut_ptr(),
            len: _0.len(),
            capacity: _0.capacity(),
            backing: Backing::Vec {
                bytes: value,
                offset: _0.len(),
            },
        }
    }
}

impl<'a> Drop for BytesWriter<'a> {
    fn drop(&mut self) {
        if matches!(self.backing, Backing::Borrowed { .. }) {
            return;
        }
        unsafe {
            if self.capacity != 0 {
                // SAFETY: when `self.capacity > 0`, `self.capacity` is the same value
                // used for allocate the block of memory pointed by `self.data`.
                let layout = Layout::from_size_align_unchecked(self.capacity, 1);
                dealloc(self.data, layout);
            }
        }
    }
}

pub enum Backing<'a> {
    Owned,
    Vec {
        offset: usize,
        bytes: &'a mut Vec<u8>,
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
    pub fn last_2(&mut self) -> Option<&mut [u8; 2]> {
        if self.len < 2 {
            return None;
        }
        Some(unsafe { &mut *self.data.add(self.len - 2).cast() })
    }
    pub fn last(&mut self) -> Option<&mut u8> {
        if self.len == 0 {
            return None;
        }
        Some(unsafe { &mut *self.data.add(self.len - 1) })
    }
    pub fn clear(&mut self) {
        self.len = 0;
    }
    pub unsafe fn set_len(&mut self, len: usize) {
        self.len = len;
    }
    pub fn saturting_pop(&mut self) {
        self.len = self.len.saturating_sub(1);
    }
    pub fn new() -> BytesWriter<'a> {
        BytesWriter {
            data: NonNull::<u8>::dangling().as_ptr(),
            len: 0,
            capacity: 0,
            backing: Backing::Owned,
        }
    }
    pub fn with_capacity(capacity: usize) -> BytesWriter<'a> {
        let data = safe_alloc(capacity);
        BytesWriter {
            data,
            len: 0,
            capacity,
            backing: Backing::Owned,
        }
    }
    pub fn buffer_slice(&self) -> &[u8] {
        unsafe { std::slice::from_raw_parts(self.data, self.len) }
    }
    pub fn into_vec(self) -> Vec<u8> {
        let this = ManuallyDrop::new(self);
        if let Backing::Borrowed { .. } = this.backing {
            return this.buffer_slice().into();
        }
        // todo document safety
        return unsafe { Vec::from_raw_parts(this.data, this.len, this.capacity) };
    }
    pub fn owned_into_vec(self) -> Vec<u8> {
        let mut this = ManuallyDrop::new(self);
        if let Backing::Owned = this.backing {
            return unsafe { Vec::from_raw_parts(this.data, this.len, this.capacity) };
        } else {
            unsafe { ManuallyDrop::drop(&mut this) };
            panic!("Expected write buffer to backed by an owned allocation");
        }
    }
    pub fn into_backed_with_extended_slice(self) -> &'a [u8] {
        let mut this = ManuallyDrop::new(self);
        let data = this.data;
        let len = this.len;
        let capacity = this.capacity;
        if let Backing::Vec { offset, bytes } = &mut this.backing {
            **bytes = unsafe { Vec::from_raw_parts(data, len, capacity) };
            return unsafe {
                std::slice::from_raw_parts(bytes.as_ptr().add(*offset), len - *offset)
            };
        }
        unsafe { ManuallyDrop::drop(&mut this) };
        panic!("Expected write buffer to backed by a Vec<u8>");
    }
    pub unsafe fn into_cow_utf8_unchecked(self) -> Cow<'a, str> {
        let mut this = ManuallyDrop::new(self);
        let data = this.data;
        let len = this.len;
        let capacity = this.capacity;
        match this.backing {
            Backing::Owned => {
                return Cow::Owned(unsafe {
                    String::from_utf8_unchecked(Vec::from_raw_parts(data, len, capacity))
                });
            }
            Backing::Borrowed { .. } => {
                return Cow::Borrowed(unsafe {
                    std::str::from_utf8_unchecked(std::slice::from_raw_parts(data, len))
                });
            }
            _ => {
                unsafe { ManuallyDrop::drop(&mut this) };
                panic!("Expected Borrowed or owneded Instance");
            }
        }
    }
    pub fn into_cow(self) -> Cow<'a, [u8]> {
        let mut this = ManuallyDrop::new(self);
        let data = this.data;
        let len = this.len;
        let capacity = this.capacity;
        match this.backing {
            Backing::Owned => {
                return Cow::Owned(unsafe { Vec::from_raw_parts(data, len, capacity) });
            }
            Backing::Borrowed { .. } => {
                return Cow::Borrowed(unsafe { std::slice::from_raw_parts(data, len) });
            }
            _ => {
                unsafe { ManuallyDrop::drop(&mut this) };
                panic!("Expected Borrowed or owneded Instance");
            }
        }
    }
    pub fn into_write_finish(mut self) -> Result<usize, std::io::Error> {
        self.flush();
        match &mut self.backing {
            Backing::Write { written, error, .. } => {
                if let Some(error) = error.take() {
                    return Err(error);
                }
                return Ok(*written);
            }
            _ => {
                panic!("Expected Write Instance");
            }
        }
    }

    /// Returns an unsafe mutable pointer to the inner data
    #[inline]
    pub fn as_mut_ptr(&self) -> *mut u8 {
        self.data
    }

    #[inline]
    pub fn head_ptr(&self) -> *mut u8 {
        unsafe { self.data.add(self.len) }
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
        debug_assert!(size <= std::isize::MAX as usize);
        if self.len + size <= self.capacity {
            return;
        }
        self.reserve_internal(size, true);
    }

    #[inline]
    pub fn push_bytes(&mut self, data: &[u8]) {
        let size = data.len();

        unsafe {
            // SAFETY: this operation won't overflow because slice cannot exceeds
            // isize::MAX bytes.
            // https://doc.rust-lang.org/reference/behavior-considered-undefined.html
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

        unsafe {
            *self.data.add(len) = byte;
            self.len = len + 1;
        }
    }
    /// Appends the given `char` to the end of this buffer
    #[inline]
    pub fn push_char(&mut self, data: char) {
        unsafe {
            self.reserve_small(4);
            let mut buffer = [0u8; 4];
            let result = data.encode_utf8(&mut buffer);
            let result = result.as_bytes();
            std::ptr::copy_nonoverlapping(result.as_ptr(), self.data.add(self.len), result.len());
            self.len += result.len();
        }
    }
    fn flush(&mut self) {
        if let Backing::Write {
            written,
            error,
            writer,
        } = &mut self.backing
        {
            use std::io::Write;
            if let Err(err) =
                writer.write_all(unsafe { std::slice::from_raw_parts(self.data, self.len) })
            {
                *error = Some(err);
            }
            *written += self.len;
            self.len = 0;
        }
    }

    #[cold]
    fn reserve_internal(&mut self, size: usize, can_write: bool) {
        debug_assert!(size <= std::isize::MAX as usize);
        if let Backing::Write {
            written,
            error,
            writer,
        } = &mut self.backing
        {
            if can_write {
                use std::io::Write;
                if let Err(err) =
                    writer.write_all(unsafe { std::slice::from_raw_parts(self.data, self.len) })
                {
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
            unsafe {
                ptr::copy_nonoverlapping(self.data, new_data, self.len);
            }
            self.backing = Backing::Owned;
            self.data = new_data;
        } else {
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
    assert!(
        capacity <= std::isize::MAX as usize,
        "capacity is too large"
    );

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
/// of memory pointed by `ptr`.
#[cold]
#[inline(never)]
unsafe fn safe_realloc(ptr: *mut u8, capacity: usize, new_capacity: usize) -> *mut u8 {
    assert!(new_capacity > 0);
    assert!(
        new_capacity <= std::isize::MAX as usize,
        "capacity is too large"
    );

    let data = if capacity == 0 {
        let new_layout = Layout::from_size_align_unchecked(new_capacity, 1);
        alloc(new_layout)
    } else {
        let old_layout = Layout::from_size_align_unchecked(capacity, 1);
        realloc(ptr, old_layout, new_capacity)
    };

    if data.is_null() {
        handle_alloc_error(Layout::from_size_align_unchecked(new_capacity, 1));
    }

    data
}

impl<'a> fmt::Write for BytesWriter<'a> {
    #[inline]
    fn write_str(&mut self, s: &str) -> fmt::Result {
        BytesWriter::push_bytes(self, s.as_bytes());
        Ok(())
    }
}

unsafe impl<'a> Send for BytesWriter<'a> {}
unsafe impl<'a> Sync for BytesWriter<'a> {}
