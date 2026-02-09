use crate::CapacityTag;
use crate::Value;
use std::alloc::Layout;
use std::ptr::NonNull;

/// A growable array of JSON values.
///
/// `ValueList` is the backing storage for JSON arrays. It supports efficient
/// push operations and can be converted to a [`Vec<Value>`].
///
/// # Examples
///
/// ```
/// use jsony_value::{Value, ValueList};
///
/// let mut list = ValueList::new();
/// list.push(1i64.into());
/// list.push("hello".into());
/// list.push(true.into());
///
/// assert_eq!(list.as_slice().len(), 3);
/// ```
///
/// [`Vec<Value>`]: std::vec::Vec
#[repr(C)]
pub struct ValueList<'a> {
    pub(super) tag: CapacityTag,
    pub(super) len: u32,
    pub(super) ptr: NonNull<Value<'a>>,
}
unsafe impl<'a> Send for ValueList<'a> {}

impl<'a, 'b> IntoIterator for &'b ValueList<'a> {
    type Item = &'b Value<'a>;
    type IntoIter = std::slice::Iter<'b, Value<'a>>;

    fn into_iter(self) -> Self::IntoIter {
        self.as_slice().iter()
    }
}

impl<'a, V: Into<Value<'a>>> FromIterator<V> for ValueList<'a> {
    fn from_iter<T: IntoIterator<Item = V>>(iter: T) -> Self {
        let mut iter = iter.into_iter();
        let (min_size, _) = Iterator::size_hint(&iter);
        let xx = min_size.min(i32::MAX as usize) as u32;
        let mut dest = ValueList::with_capacity(xx);
        for value in iter.by_ref().take(xx as usize) {
            unsafe {
                dest.ptr.add(dest.len as usize).write(value.into());
                dest.len += 1;
            }
        }
        for value in iter {
            dest.push(value.into());
        }

        dest
    }
}

impl<'a> Drop for ValueList<'a> {
    fn drop(&mut self) {
        if self.tag.is_copy() {
            return;
        }
        unsafe {
            std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(
                self.ptr.as_ptr(),
                self.len as usize,
            ));
            std::alloc::dealloc(
                self.ptr.as_ptr() as *mut u8,
                std::alloc::Layout::array::<Value>(self.tag.capacity() as usize).unwrap(),
            );
        }
    }
}

const MIN_CAPACITY: u32 = 1 << 3;

// Note we do not implement deref to keep the index operations predictable where like
// Value, ValueList null coalesces index operations instead of panicking
impl<'a> ValueList<'a> {
    pub(crate) fn to_owned_in_place(&mut self) {
        for entry in self.as_mut_slice() {
            entry.to_owned_in_place();
        }
    }

    /// Converts borrowed data to owned, returning a `'static` lifetime list.
    ///
    /// All borrowed strings within the list are copied to the heap.
    pub fn to_owned(mut self) -> ValueList<'static> {
        self.to_owned_in_place();
        unsafe { std::mem::transmute::<ValueList, ValueList<'static>>(self) }
    }

    /// Creates an empty list.
    pub fn new() -> ValueList<'a> {
        ValueList {
            tag: CapacityTag::new_list(0),
            len: 0,
            ptr: NonNull::dangling(),
        }
    }

    /// Creates an empty list with the specified capacity.
    ///
    /// # Panics
    ///
    /// Panics if memory allocation fails.
    pub fn with_capacity(capacity: u32) -> ValueList<'a> {
        let mut list = ValueList::new();
        if !list.try_reserve(capacity) {
            crate::oom();
        }
        list
    }

    /// Converts the list into a [`Vec<Value>`].
    ///
    /// This is a zero-cost conversion that transfers ownership of the
    /// underlying allocation.
    ///
    /// [`Vec<Value>`]: std::vec::Vec
    pub fn to_vec(self) -> Vec<Value<'a>> {
        let this = std::mem::ManuallyDrop::new(self);
        unsafe {
            Vec::from_raw_parts(
                this.ptr.as_ptr(),
                this.len as usize,
                this.tag.capacity() as usize,
            )
        }
    }

    /// Returns a mutable slice of the list's elements.
    pub fn as_mut_slice(&mut self) -> &mut [Value<'a>] {
        if self.len == 0 {
            &mut []
        } else {
            unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len as usize) }
        }
    }

    /// Returns a slice of the list's elements.
    pub fn as_slice(&self) -> &[Value<'a>] {
        if self.len == 0 {
            &[]
        } else {
            unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len as usize) }
        }
    }

    /// Appends a value to the end of the list.
    ///
    /// # Panics
    ///
    /// Panics if memory allocation fails.
    pub fn push(&mut self, value: Value<'a>) {
        let len = self.len;
        if len == self.tag.capacity() {
            self.grow_one();
        }
        unsafe {
            println!("{} {}", len, self.tag.capacity());
            let end = self.ptr.as_ptr().add(len as usize);
            std::ptr::write(end, value);
            self.len = len + 1;
        }
    }

    /// Grows the list by one element's worth of capacity.
    #[cold]
    pub fn grow_one(&mut self) {
        if !self.try_reserve(1) {
            crate::oom()
        }
    }

    /// Attempts to reserve capacity for at least `size` more elements.
    ///
    /// Returns `true` if successful, `false` if allocation fails or would overflow.
    pub fn try_reserve(&mut self, size: u32) -> bool {
        let needed = self.len as u64 + size as u64;
        let capacity = self.tag.capacity();
        if needed < capacity as u64 {
            return true;
        }

        let target_size = if capacity == 0 {
            MIN_CAPACITY as u64
        } else {
            capacity as u64 * 2
        };

        let new_capacity = ((target_size.max(needed) + 0b111) & (!0b111u64)) | 0b1_000;
        if new_capacity > (u32::MAX as u64).min(isize::MAX as u64) {
            return false;
        }
        let ptr = unsafe {
            if capacity == 0 {
                let layout = std::alloc::Layout::array::<Value>(new_capacity as usize).unwrap();
                std::alloc::alloc(layout)
            } else {
                let layout = std::alloc::Layout::array::<Value>(capacity as usize).unwrap();
                std::alloc::realloc(
                    self.ptr.as_ptr() as *mut u8,
                    layout,
                    new_capacity as usize * std::mem::size_of::<Value>(),
                )
            }
        };

        let Some(ptr) = NonNull::new(ptr as *mut Value<'a>) else {
            return false;
        };

        self.ptr = ptr;
        // due to the check above this will not truncate
        self.tag = CapacityTag::new_list(new_capacity as u32);

        true
    }
}

impl<'a> Clone for ValueList<'a> {
    fn clone(&self) -> ValueList<'a> {
        if self.len == 0 {
            return ValueList::new();
        }

        let new_list = ValueList {
            tag: self.tag,
            len: self.len,
            ptr: unsafe {
                // Allocate new memory for the clone
                let layout = Layout::array::<Value>(self.tag.capacity() as usize).unwrap();
                let ptr = std::alloc::alloc(layout);
                let Some(ptr) = NonNull::new(ptr as *mut Value<'a>) else {
                    crate::oom();
                };
                ptr
            },
        };

        // Clone each Value since they might contain reference counted or other heap data
        unsafe {
            for (i, value) in self.as_slice().iter().enumerate() {
                let value_ptr = new_list.ptr.as_ptr().add(i);
                std::ptr::write(value_ptr, value.clone());
            }
        }

        new_list
    }
}

impl std::fmt::Debug for ValueList<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_list().entries(self.as_slice()).finish()
    }
}

impl PartialEq for ValueList<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.as_slice() == other.as_slice()
    }
}
