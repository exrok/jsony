mod raw;
use crate::{Value, ValueString};

use super::oom;
use crate::Kind;
use raw::RawTable;
use std::alloc::Layout;
use std::hash::BuildHasher;
use std::marker::PhantomData;
use std::ptr::NonNull;

pub(super) type ObjectEntry<'a> = (ValueString<'a>, Value<'a>);

const MIN_CAPACITY: u32 = 1 << 3;

pub(crate) enum InnerMatchIter<'a, 'm, 'k> {
    Slice {
        iter: std::slice::Iter<'m, ObjectEntry<'a>>,
        key: &'k str,
    },
    Map {
        iter: raw::RawIterHash,
        key: &'k str,
        map: &'m ValueMap<'a>,
    },
}

/// An iterator over all values matching a key in a [`ValueMap`].
///
/// Returned by [`ValueMap::get_all`]. Since `ValueMap` allows duplicate keys,
/// this iterator yields all values associated with the given key.
pub struct MatchIter<'a, 'm, 'k> {
    inner: InnerMatchIter<'a, 'm, 'k>,
}
impl<'a, 'm, 'k> Iterator for MatchIter<'a, 'm, 'k> {
    type Item = &'m Value<'a>;
    #[inline]
    fn next(&mut self) -> Option<Self::Item> {
        match &mut self.inner {
            InnerMatchIter::Slice { iter, key } => {
                while let Some((ekey, eval)) = iter.next() {
                    if ekey.as_bytes() == key.as_bytes() {
                        return Some(eval);
                    }
                }
                None
            }
            InnerMatchIter::Map { iter, key, map } => {
                while let Some(i) = iter.next() {
                    unsafe {
                        let (ekey, eval) = map.ptr.add(*i.as_ref() as usize).as_ref();
                        if ekey.as_bytes() == key.as_bytes() {
                            return Some(eval);
                        }
                    }
                }
                None
            }
        }
    }
    fn fold<B, F>(mut self, mut acc: B, mut f: F) -> B
    where
        Self: Sized,
        F: FnMut(B, Self::Item) -> B,
    {
        match &mut self.inner {
            InnerMatchIter::Slice { iter, key } => {
                while let Some((ekey, eval)) = iter.next() {
                    if ekey.as_bytes() == key.as_bytes() {
                        acc = f(acc, eval);
                    }
                }
            }
            InnerMatchIter::Map { iter, key, map } => {
                while let Some(i) = iter.next() {
                    unsafe {
                        let (ekey, eval) = map.ptr.add(*i.as_ref() as usize).as_ref();
                        if ekey.as_bytes() == key.as_bytes() {
                            acc = f(acc, eval);
                        }
                    }
                }
            }
        }
        acc
    }
}

#[repr(C, align(8))]
#[derive(Clone)]
pub(super) struct ObjectKeyIndex {
    table: RawTable,
    hasher: foldhash::quality::RandomState,
}
unsafe impl<'a> Send for ValueMap<'a> {}
unsafe impl<'a> Sync for ValueMap<'a> {}

const _: () = const { assert!(align_of::<ObjectKeyIndex>() >= align_of::<ObjectEntry>()) };

use super::CapacityTag;

/// A JSON object mapping string keys to values.
///
/// `ValueMap` preserves insertion order and allows duplicate keys. For small
/// maps (8 entries or fewer), lookups use linear scan. Larger maps build a
/// hash index for O(1) lookup.
///
/// Use [`get`] for single lookups or [`get_all`] to iterate over all values
/// with duplicate keys.
///
/// # Examples
///
/// ```
/// use jsony_value::{ValueMap, ValueString, Value, ValueRef};
///
/// let mut map = ValueMap::new();
/// map.insert(ValueString::from_borrowed("name"), "Alice".into());
/// map.insert(ValueString::from_borrowed("age"), 30i64.into());
///
/// if let ValueRef::String(s) = map.get("name").unwrap().as_ref() {
///     assert_eq!(s.as_str(), "Alice");
/// }
/// ```
///
/// [`get`]: ValueMap::get
/// [`get_all`]: ValueMap::get_all
#[repr(C)]
pub struct ValueMap<'a> {
    tag: CapacityTag,
    len: u32,
    ptr: NonNull<ObjectEntry<'a>>,
}
use crate::ValueData;
impl<'a> From<ValueMap<'a>> for Value<'a> {
    fn from(value: ValueMap<'a>) -> Self {
        let value = std::mem::ManuallyDrop::new(value);
        Value {
            tag: value.tag,
            meta: value.len,
            data: ValueData {
                map_entries: value.ptr,
            },
            marker: PhantomData,
        }
    }
}

/// A builder for constructing [`ValueMap`] instances efficiently.
///
/// Use this when building a map from a known sequence of entries. The builder
/// defers hash index construction until [`build`] is called, which is more
/// efficient than inserting entries one-by-one into a `ValueMap`.
///
/// # Examples
///
/// ```
/// use jsony_value::{ValueMapBuilder, ValueString};
///
/// let mut builder = ValueMapBuilder::new();
/// builder.insert(ValueString::from_borrowed("a"), 1i64.into());
/// builder.insert(ValueString::from_borrowed("b"), 2i64.into());
/// let map = builder.build();
///
/// assert_eq!(map.get("a").unwrap().as_i64(), Some(1));
/// ```
///
/// [`build`]: ValueMapBuilder::build
#[repr(C)]
pub struct ValueMapBuilder<'a> {
    entry_capacity: u32,
    len: u32,
    ptr: NonNull<ObjectEntry<'a>>,
}

impl<'a> ValueMapBuilder<'a> {
    /// Creates an empty builder.
    pub const fn new() -> ValueMapBuilder<'a> {
        ValueMapBuilder {
            entry_capacity: 0,
            len: 0,
            ptr: NonNull::dangling(),
        }
    }

    fn with_capacity(size: u32) -> ValueMapBuilder<'a> {
        let mut r = ValueMapBuilder {
            entry_capacity: 0,
            len: 0,
            ptr: NonNull::dangling(),
        };
        if !r.try_reserve(size) {
            crate::oom()
        }
        r
    }
    fn try_reserve(&mut self, size: u32) -> bool {
        let needed = self.len as u64 + size as u64;

        if needed < self.entry_capacity as u64 {
            return true;
        }
        let capacity = self.entry_capacity + INDEX_ENTRY_COUNT as u32;

        let mut target_capacity = if capacity == INDEX_ENTRY_COUNT as u32 {
            MIN_CAPACITY
        } else {
            let Some(ok) = capacity.checked_mul(2) else {
                return false;
            };
            ok | 0b1_000
        } as u64;
        if target_capacity <= needed {
            // note since size as u32, this can't overflow
            target_capacity = ((needed + 0b111) & (!0b111)) | 0b1_000
        }
        unsafe { self.try_reserve_inner(target_capacity) }
    }
    unsafe fn try_reserve_inner(&mut self, target_capacity: u64) -> bool {
        if target_capacity > (i32::MAX as u64) {
            return false;
        }
        let layout = Layout::array::<ObjectEntry<'static>>(target_capacity as usize).unwrap();
        let layout = layout.align_to(align_of::<ObjectKeyIndex>()).unwrap();
        let ptr = unsafe { std::alloc::alloc(layout) as *mut ObjectEntry<'static> };
        let Some(sap) = NonNull::new(ptr) else {
            return false;
        };

        let ptr = unsafe { sap.add(INDEX_ENTRY_COUNT) };
        if self.len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(self.ptr.as_ptr(), ptr.as_ptr(), self.len as usize);
            }
        }

        if self.entry_capacity != 0 {
            let layout =
                Layout::array::<ObjectEntry<'static>>(self.entry_capacity as usize).unwrap();
            let layout = layout.align_to(align_of::<ObjectKeyIndex>()).unwrap();
            unsafe {
                std::alloc::dealloc(self.ptr.sub(INDEX_ENTRY_COUNT).as_ptr() as *mut u8, layout);
            }
        }

        self.entry_capacity = target_capacity as u32 - INDEX_ENTRY_COUNT as u32;
        self.ptr = ptr;

        true
    }
    /// Inserts a key-value pair into the builder.
    ///
    /// # Panics
    ///
    /// Panics if memory allocation fails.
    pub fn insert(&mut self, key: ValueString<'a>, value: Value<'a>) {
        let len = self.len;
        self.reserve(1);
        let ptr = self.ptr;

        let ret = unsafe { ptr.add(len as usize) };
        unsafe {
            ret.write((key, value));
        }
        self.len += 1;
    }

    fn reserve(&mut self, size: u32) {
        if !self.try_reserve(size) {
            oom();
        }
    }

    /// Consumes the builder and returns the constructed [`ValueMap`].
    ///
    /// For maps with more than 8 entries, this builds a hash index for O(1) lookups.
    pub fn build(self) -> ValueMap<'a> {
        let this = std::mem::ManuallyDrop::new(self);
        let map = ValueMap {
            tag: CapacityTag::new_map(this.entry_capacity + INDEX_ENTRY_COUNT as u32),
            len: this.len,
            ptr: this.ptr,
        };

        if map.tag.map_has_index() {
            let mut index = ObjectKeyIndex {
                table: RawTable::with_capacity(map.len as u32),
                hasher: foldhash::quality::RandomState::default(),
            };
            for (i, (key, _)) in map.entries().iter().enumerate() {
                unsafe {
                    index
                        .table
                        .insert_no_grow(index.hasher.hash_one(key.as_bytes()), i as u32);
                }
            }
            unsafe {
                map.ptr
                    .sub(INDEX_ENTRY_COUNT)
                    .cast::<ObjectKeyIndex>()
                    .write(index);
            }
        }
        map
    }
}

impl<'a> Drop for ValueMap<'a> {
    fn drop(&mut self) {
        let capacity = self.tag.capacity();
        if capacity != 0 {
            let layout = Layout::array::<ObjectEntry<'static>>(capacity as usize).unwrap();
            let layout = layout.align_to(align_of::<ObjectKeyIndex>()).unwrap();
            unsafe {
                std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(
                    self.ptr.as_ptr(),
                    self.len as usize,
                ));
                if self.tag.map_has_index() {
                    std::ptr::drop_in_place(
                        self.ptr
                            .sub(INDEX_ENTRY_COUNT)
                            .cast::<ObjectKeyIndex>()
                            .as_ptr(),
                    )
                }
                std::alloc::dealloc(self.ptr.sub(INDEX_ENTRY_COUNT).as_ptr() as *mut u8, layout);
            }
        }
    }
}
impl<'a> Drop for ValueMapBuilder<'a> {
    fn drop(&mut self) {
        if self.entry_capacity != 0 {
            let layout = Layout::array::<ObjectEntry<'static>>(
                (self.entry_capacity) as usize + INDEX_ENTRY_COUNT,
            )
            .unwrap();
            let layout = layout.align_to(align_of::<ObjectKeyIndex>()).unwrap();
            unsafe {
                std::ptr::drop_in_place(std::ptr::slice_from_raw_parts_mut(
                    self.ptr.as_ptr(),
                    self.len as usize,
                ));
                std::alloc::dealloc(self.ptr.sub(INDEX_ENTRY_COUNT).as_ptr() as *mut u8, layout);
            }
        }
    }
}

impl<'a, K: Into<ValueString<'a>>, V: Into<Value<'a>>> FromIterator<(K, V)> for ValueMap<'a> {
    fn from_iter<T: IntoIterator<Item = (K, V)>>(iter: T) -> Self {
        let iter = iter.into_iter();
        let (min_size, _) = Iterator::size_hint(&iter);
        let mut dest: ValueMapBuilder<'a> = ValueMapBuilder::with_capacity(min_size as u32);
        for (k, v) in iter {
            dest.insert(k.into(), v.into());
        }
        dest.build()
    }
}

pub(super) const INDEX_ENTRY_COUNT: usize =
    size_of::<ObjectKeyIndex>().div_ceil(size_of::<ObjectEntry>());

impl<'a> Clone for ValueMap<'a> {
    fn clone(&self) -> ValueMap<'a> {
        if self.len == 0 {
            return ValueMap::new();
        }
        let mut new = ValueMap::new();
        unsafe {
            new.try_reserve_inner(self.tag.capacity() as u64, false);
        }
        if let Some(index) = self.key_index() {
            unsafe {
                self.ptr
                    .sub(INDEX_ENTRY_COUNT)
                    .cast::<ObjectKeyIndex>()
                    .write(index.clone());
            }
        }
        for (i, (key, value)) in self.entries().iter().enumerate() {
            unsafe {
                new.ptr.add(i).write((key.clone(), value.clone()));
            }
        }
        new.len = self.len;
        new
    }
}

impl<'a> ValueMap<'a> {
    pub(crate) fn to_owned_in_place(&mut self) {
        for (key, value) in self.inner_entries_mut() {
            key.to_owned_in_place();
            value.to_owned_in_place();
        }
    }

    /// Converts borrowed data to owned, returning a `'static` lifetime map.
    ///
    /// All borrowed strings within the map are copied to the heap.
    pub fn to_owned(mut self) -> ValueMap<'static> {
        self.to_owned_in_place();
        unsafe { std::mem::transmute::<ValueMap, ValueMap<'static>>(self) }
    }

    /// Creates an empty map.
    pub const fn new() -> ValueMap<'a> {
        ValueMap {
            tag: CapacityTag::new_map(0),
            len: 0,
            ptr: NonNull::dangling(),
        }
    }

    fn inner_entries_mut(&mut self) -> &mut [ObjectEntry<'a>] {
        unsafe { std::slice::from_raw_parts_mut(self.ptr.as_ptr(), self.len as usize) }
    }

    /// Returns a slice of all key-value pairs in insertion order.
    pub fn entries(&self) -> &[ObjectEntry<'a>] {
        unsafe { std::slice::from_raw_parts(self.ptr.as_ptr(), self.len as usize) }
    }
    fn key_index(&self) -> Option<&ObjectKeyIndex> {
        if !self.tag.map_has_index() {
            return None;
        }
        unsafe {
            Some(
                &*self
                    .ptr
                    .sub(INDEX_ENTRY_COUNT as usize)
                    .cast::<ObjectKeyIndex>()
                    .as_ptr(),
            )
        }
    }

    fn try_reserve(&mut self, size: u32, init_index: bool) -> bool {
        let needed = self.len as u64 + size as u64 + INDEX_ENTRY_COUNT as u64;

        if needed < self.tag.capacity() as u64 {
            return true;
        }
        let capacity = self.tag.map_entry_capacity();

        let mut target_capacity = if capacity < 0 {
            MIN_CAPACITY
        } else {
            let Some(ok) = self.tag.capacity().checked_mul(2) else {
                return false;
            };
            ok | 0b1_000
        } as u64;
        if target_capacity <= needed {
            // note since size as u32, this can't overflow
            target_capacity = ((needed + 0b111) & (!0b111)) | 0b1_000
        }
        unsafe { self.try_reserve_inner(target_capacity, init_index) }
    }
    unsafe fn try_reserve_inner(&mut self, target_capacity: u64, init_index: bool) -> bool {
        if target_capacity > (i32::MAX as u64) {
            return false;
        }
        let layout = Layout::array::<ObjectEntry<'static>>(target_capacity as usize).unwrap();
        let layout = layout.align_to(align_of::<ObjectKeyIndex>()).unwrap();
        let ptr = unsafe { std::alloc::alloc(layout) as *mut ObjectEntry<'static> };
        let Some(sap) = NonNull::new(ptr) else {
            return false;
        };

        unsafe {
            // opt: want to be able to resuse this code without triggering index creation
            if init_index && target_capacity > MIN_CAPACITY as u64 {
                let map = if !self.tag.map_has_index() {
                    let mut index = ObjectKeyIndex {
                        table: RawTable::with_capacity(target_capacity as u32),
                        hasher: foldhash::quality::RandomState::default(),
                    };

                    for (i, (key, _)) in self.entries().iter().enumerate() {
                        index
                            .table
                            .insert_no_grow(index.hasher.hash_one(key.as_bytes()), i as u32);
                    }
                    index
                } else {
                    self.ptr
                        .sub(INDEX_ENTRY_COUNT)
                        .cast::<ObjectKeyIndex>()
                        .read()
                };
                sap.cast::<ObjectKeyIndex>().write(map);
            }
        }
        let ptr = unsafe { sap.add(INDEX_ENTRY_COUNT) };
        if self.len > 0 {
            unsafe {
                std::ptr::copy_nonoverlapping(self.ptr.as_ptr(), ptr.as_ptr(), self.len as usize);
            }
        }

        if self.tag.raw.get() > Kind::Map as u32 {
            let layout =
                Layout::array::<ObjectEntry<'static>>(self.tag.capacity() as usize).unwrap();
            let layout = layout.align_to(align_of::<ObjectKeyIndex>()).unwrap();
            unsafe {
                std::alloc::dealloc(self.ptr.sub(INDEX_ENTRY_COUNT).as_ptr() as *mut u8, layout);
            }
        }

        self.tag = CapacityTag::new_map(target_capacity as u32);
        self.ptr = ptr;

        true
    }
    /// Inserts a key-value pair into the map.
    ///
    /// If the key already exists, this adds a duplicate entry. Use [`entry`]
    /// for upsert behavior.
    ///
    /// # Panics
    ///
    /// Panics if memory allocation fails.
    ///
    /// [`entry`]: ValueMap::entry
    pub fn insert(&mut self, key: ValueString<'a>, value: Value<'a>) {
        self.reserve(1);
        let ptr = self.ptr;
        let len = self.len;

        if self.tag.map_has_index() {
            let index = unsafe {
                &mut *self
                    .ptr
                    .sub(INDEX_ENTRY_COUNT as usize)
                    .cast::<ObjectKeyIndex>()
                    .as_ptr()
            };
            index.insert(self, key.as_bytes());
        }
        let ret = unsafe { ptr.add(len as usize) };
        unsafe {
            ret.write((key, value));
        }
        self.len += 1;
    }

    fn reserve(&mut self, size: u32) {
        if !self.try_reserve(size, true) {
            oom();
        }
    }

    /// Removes the first entry with the given key, preserving order.
    ///
    /// Returns the removed value, or [`None`] if the key was not found.
    pub fn remove(&mut self, key: &str) -> Option<Value<'a>> {
        if self.tag.map_has_index() {
            let index = unsafe {
                &mut *self
                    .ptr
                    .sub(INDEX_ENTRY_COUNT as usize)
                    .cast::<ObjectKeyIndex>()
                    .as_ptr()
            };
            let hash = index.hasher.hash_one(key.as_bytes());
            let entry_index = index.table.remove_entry(hash, |idx| {
                let (key, _) = unsafe { &*(self.ptr.add(*idx as usize).as_ptr()) };
                key.as_bytes() == key.as_bytes()
            })?;
            let ptr = unsafe { self.ptr.add(entry_index as usize) };
            let (_, value) = unsafe { ptr.read() };
            if self.len - 1 != entry_index {
                unsafe {
                    std::ptr::copy(
                        ptr.add(1).as_ptr(),
                        ptr.as_ptr(),
                        (self.len - entry_index - 1) as usize,
                    );
                }
                for bucket in unsafe { index.table.iter() } {
                    let bucket = unsafe { bucket.as_mut() };
                    if *bucket > entry_index {
                        *bucket -= 1;
                    }
                }
            }
            self.len -= 1;
            return Some(value);
        }
        for (entry_index, (entry_key, _)) in self.entries().iter().enumerate() {
            if key.as_bytes() == entry_key.as_bytes() {
                let entry_index = entry_index as u32;
                let ptr = unsafe { self.ptr.add(entry_index as usize) };
                let (_, value) = unsafe { ptr.read() };
                if self.len - 1 != entry_index {
                    unsafe {
                        std::ptr::copy(
                            ptr.add(1).as_ptr(),
                            ptr.as_ptr(),
                            (self.len - entry_index - 1) as usize,
                        );
                    }
                }
                self.len -= 1;
                return Some(value);
            }
        }
        None
    }

    /// Removes the first entry with the given key using swap-remove.
    ///
    /// This is faster than [`remove`] but does not preserve order. The last
    /// entry is moved into the removed entry's position.
    ///
    /// Returns the removed value, or [`None`] if the key was not found.
    ///
    /// [`remove`]: ValueMap::remove
    pub fn swap_remove(&mut self, key: &str) -> Option<Value<'a>> {
        if self.tag.map_has_index() {
            let index = unsafe {
                &mut *self
                    .ptr
                    .sub(INDEX_ENTRY_COUNT as usize)
                    .cast::<ObjectKeyIndex>()
                    .as_ptr()
            };
            let hash = index.hasher.hash_one(key.as_bytes());
            let entry_index = index.table.remove_entry(hash, |idx| {
                let (key, _) = unsafe { &*(self.ptr.add(*idx as usize).as_ptr()) };
                key.as_bytes() == key.as_bytes()
            })?;
            let (_, value) = unsafe { self.ptr.add(entry_index as usize).read() };
            if self.len - 1 != entry_index {
                unsafe {
                    let tail_index = self.len - 1;
                    let (tail_key, tail_value) = self.ptr.add(tail_index as usize).read();
                    let hash = index.hasher.hash_one(tail_key.as_bytes());
                    let tail_bucket_entry = index
                        .table
                        .find(hash, |v| *v == tail_index)
                        .expect("Tail to be indexed");
                    tail_bucket_entry.as_ptr().write(entry_index);
                    self.ptr
                        .add(entry_index as usize)
                        .write((tail_key, tail_value));
                }
            }
            self.len -= 1;
            return Some(value);
        }
        for (entry_index, (entry_key, _)) in self.entries().iter().enumerate() {
            if key.as_bytes() == entry_key.as_bytes() {
                let entry_index = entry_index as u32;
                let (_, value) = unsafe { self.ptr.add(entry_index as usize).read() };
                if self.len - 1 != entry_index {
                    unsafe {
                        let tail_index = self.len - 1;
                        let (tail_key, tail_value) = self.ptr.add(tail_index as usize).read();
                        self.ptr
                            .add(entry_index as usize)
                            .write((tail_key, tail_value));
                    }
                }
                self.len -= 1;
                return Some(value);
            }
        }
        None
    }
    /// Returns a mutable reference to the value for the key, inserting null if absent.
    ///
    /// # Panics
    ///
    /// Panics if memory allocation fails when inserting a new entry.
    pub fn get_mut_or_default(&mut self, key: &str) -> &mut Value<'a> {
        if let Some(value) = self.get_mut(key) {
            // workaround NLL limitation
            return unsafe { &mut *(value as *mut Value<'a>) };
        }
        self.insert(ValueString::from_owned(key.into()), Value::NULL);
        return &mut self.inner_entries_mut().last_mut().unwrap().1;
    }

    /// Returns a mutable reference to the value for the key, if present.
    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value<'a>> {
        if let Some(index) = self.key_index() {
            let hash = index.hasher.hash_one(key.as_bytes());
            let index = index.table.get(hash, |idx| {
                let (key, _) = unsafe { &*(self.ptr.add(*idx as usize).as_ptr()) };
                key.as_bytes() == key.as_bytes()
            })?;
            let (_, value) = unsafe { self.ptr.add(*index as usize).as_mut() };
            return Some(value);
        }
        for (entry_key, entry_value) in self.inner_entries_mut() {
            if key.as_bytes() == entry_key.as_bytes() {
                return Some(entry_value);
            }
        }
        None
    }
    /// Sorts all entries by key in ascending order.
    ///
    /// This is useful for deterministic serialization or comparison.
    pub fn sort(&mut self) {
        self.inner_entries_mut()
            .sort_unstable_by(|(k1, _), (k2, _)| k1.as_str().cmp(k2.as_str()));
        if self.tag.map_has_index() {
            let index = unsafe {
                &mut *self
                    .ptr
                    .sub(INDEX_ENTRY_COUNT as usize)
                    .cast::<ObjectKeyIndex>()
                    .as_ptr()
            };
            index.table.clear();
            for (i, (key, _)) in self.entries().iter().enumerate() {
                unsafe {
                    index
                        .table
                        .insert_no_grow(index.hasher.hash_one(key.as_bytes()), i as u32);
                }
            }
        }
    }

    /// Returns a reference to the value for the key, if present.
    ///
    /// If the key appears multiple times, returns the first occurrence.
    /// Use [`get_all`] to iterate over all values with duplicate keys.
    ///
    /// [`get_all`]: ValueMap::get_all
    pub fn get(&self, key: &str) -> Option<&Value<'a>> {
        if let Some(index) = self.key_index() {
            let hash = index.hasher.hash_one(key.as_bytes());
            let index = index.table.get(hash, |idx| {
                let (key, _) = unsafe { &*(self.ptr.add(*idx as usize).as_ptr()) };
                key.as_bytes() == key.as_bytes()
            })?;
            let (_, value) = unsafe { &*(self.ptr.add(*index as usize).as_ptr()) };
            return Some(value);
        }
        for (entry_key, entry_value) in self.entries() {
            if key.as_bytes() == entry_key.as_bytes() {
                return Some(&entry_value);
            }
        }
        None
    }
    /// Returns an iterator over all values matching the given key.
    ///
    /// Since `ValueMap` allows duplicate keys, this iterator yields all values
    /// associated with the key.
    #[inline]
    pub fn get_all<'m, 'k>(&'m self, key: &'k str) -> MatchIter<'a, 'm, 'k> {
        if let Some(index) = self.key_index() {
            let hash = index.hasher.hash_one(key.as_bytes());
            let iter = unsafe { index.table.iter_hash(hash) };
            MatchIter {
                inner: InnerMatchIter::Map {
                    iter,
                    key,
                    map: self,
                },
            }
        } else {
            MatchIter {
                inner: InnerMatchIter::Slice {
                    iter: self.entries().iter(),
                    key,
                },
            }
        }
    }
}

impl PartialEq for ValueMap<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.entries() == other.entries()
    }
}

impl From<String> for ValueString<'_> {
    fn from(value: String) -> Self {
        ValueString::from_owned(value.into())
    }
}
impl From<String> for Value<'_> {
    fn from(value: String) -> Self {
        Value::from(ValueString::from_owned(value.into()))
    }
}

impl std::fmt::Debug for ValueMap<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut builder = f.debug_map();
        for (k, v) in self.entries() {
            builder.entry(k, v);
        }
        builder.finish()
    }
}

impl<'a> std::ops::Index<&str> for ValueMap<'a> {
    type Output = Value<'a>;

    fn index(&self, index: &str) -> &Self::Output {
        if let Some(value) = self.get(index) {
            value
        } else {
            const { &Value::NULL }
        }
    }
}

impl<'a> std::ops::IndexMut<&str> for ValueMap<'a> {
    fn index_mut(&mut self, index: &str) -> &mut Self::Output {
        self.get_mut_or_default(index)
    }
}

/// A view into a single entry in a [`ValueMap`], which may be vacant or occupied.
///
/// This enum is returned by [`ValueMap::entry`].
pub enum Entry<'a, 'b> {
    /// An occupied entry.
    Occupied(OccupiedEntry<'a, 'b>),
    /// A vacant entry.
    Vacant(VacantEntry<'a, 'b>),
}

/// A view into an occupied entry in a [`ValueMap`].
///
/// Obtained via [`Entry::Occupied`] from [`ValueMap::entry`].
pub struct OccupiedEntry<'a, 'b> {
    map: &'a mut ValueMap<'b>,
    index: u32,
    hash: u64,
}

/// A view into a vacant entry in a [`ValueMap`].
///
/// Obtained via [`Entry::Vacant`] from [`ValueMap::entry`].
pub struct VacantEntry<'a, 'b> {
    map: &'a mut ValueMap<'b>,
    key: ValueString<'b>,
}

impl<'b> ValueMap<'b> {
    /// Returns an [`Entry`] for in-place manipulation of the value at the given key.
    ///
    /// # Examples
    ///
    /// ```
    /// use jsony_value::{ValueMap, Entry};
    ///
    /// let mut map = ValueMap::new();
    /// map.entry("counter").or_insert(0i64.into());
    ///
    /// if let Entry::Occupied(mut e) = map.entry("counter") {
    ///     *e.get_mut() = 1i64.into();
    /// }
    /// ```
    pub fn entry<'a>(&'a mut self, key: impl Into<ValueString<'b>>) -> Entry<'a, 'b> {
        let key = key.into();
        let hash = if let Some(index) = self.key_index() {
            index.hasher.hash_one(key.as_bytes())
        } else {
            foldhash::quality::RandomState::default().hash_one(key.as_bytes())
        };

        if let Some(index) = self.key_index() {
            if let Some(&entry_index) = index.table.get(hash, |idx| {
                let (entry_key, _) = unsafe { &*(self.ptr.add(*idx as usize).as_ptr()) };
                entry_key.as_bytes() == key.as_bytes()
            }) {
                return Entry::Occupied(OccupiedEntry {
                    map: self,
                    index: entry_index,
                    hash,
                });
            }
        } else {
            for (i, (entry_key, _)) in self.entries().iter().enumerate() {
                if entry_key.as_bytes() == key.as_bytes() {
                    return Entry::Occupied(OccupiedEntry {
                        map: self,
                        index: i as u32,
                        hash,
                    });
                }
            }
        }

        Entry::Vacant(VacantEntry { map: self, key })
    }
}

impl<'a, 'b> OccupiedEntry<'a, 'b> {
    /// Returns a reference to the entry's key.
    pub fn key(&self) -> &str {
        let (key, _) = unsafe { &*self.map.ptr.add(self.index as usize).as_ptr() };
        key.as_str()
    }

    /// Returns a reference to the entry's value.
    pub fn get(&self) -> &Value<'b> {
        let (_, value) = unsafe { &*self.map.ptr.add(self.index as usize).as_ptr() };
        value
    }

    /// Returns a mutable reference to the entry's value.
    pub fn get_mut(&mut self) -> &mut Value<'b> {
        let (_, value) = unsafe { &mut *self.map.ptr.add(self.index as usize).as_ptr() };
        value
    }

    /// Converts the entry into a mutable reference to its value.
    pub fn into_mut(self) -> &'a mut Value<'b> {
        let (_, value) = unsafe { &mut *self.map.ptr.add(self.index as usize).as_ptr() };
        value
    }

    /// Replaces the entry's value with the given value, returning the old value.
    pub fn insert(&mut self, value: Value<'b>) -> Value<'b> {
        let (_, slot) = unsafe { &mut *self.map.ptr.add(self.index as usize).as_ptr() };
        std::mem::replace(slot, value)
    }

    /// Removes the entry from the map and returns its value.
    pub fn remove(self) -> Value<'b> {
        self.remove_entry().1
    }

    /// Removes the entry from the map and returns the key-value pair.
    pub fn remove_entry(self) -> (ValueString<'b>, Value<'b>) {
        let entry_index = self.index;

        if self.map.tag.map_has_index() {
            let index = unsafe {
                &mut *self
                    .map
                    .ptr
                    .sub(INDEX_ENTRY_COUNT as usize)
                    .cast::<ObjectKeyIndex>()
                    .as_ptr()
            };
            index
                .table
                .remove_entry(self.hash, |idx| *idx == entry_index);
        }

        let ptr = unsafe { self.map.ptr.add(entry_index as usize) };
        let entry = unsafe { ptr.read() };

        if self.map.len - 1 != entry_index {
            unsafe {
                std::ptr::copy(
                    ptr.add(1).as_ptr(),
                    ptr.as_ptr(),
                    (self.map.len - entry_index - 1) as usize,
                );
            }
            if self.map.tag.map_has_index() {
                let index = unsafe {
                    &mut *self
                        .map
                        .ptr
                        .sub(INDEX_ENTRY_COUNT as usize)
                        .cast::<ObjectKeyIndex>()
                        .as_ptr()
                };
                for bucket in unsafe { index.table.iter() } {
                    let bucket = unsafe { bucket.as_mut() };
                    if *bucket > entry_index {
                        *bucket -= 1;
                    }
                }
            }
        }

        self.map.len -= 1;
        entry
    }
}

impl<'a, 'b> VacantEntry<'a, 'b> {
    /// Returns a reference to the entry's key.
    pub fn key(&self) -> &str {
        self.key.as_str()
    }

    /// Consumes the entry and returns its key.
    pub fn into_key(self) -> ValueString<'b> {
        self.key
    }

    /// Inserts a value into the entry and returns a mutable reference to it.
    ///
    /// # Panics
    ///
    /// Panics if memory allocation fails.
    pub fn insert(self, value: Value<'b>) -> &'a mut Value<'b> {
        self.map.reserve(1);

        if self.map.tag.map_has_index() {
            let index = unsafe {
                &mut *self
                    .map
                    .ptr
                    .sub(INDEX_ENTRY_COUNT as usize)
                    .cast::<ObjectKeyIndex>()
                    .as_ptr()
            };
            index.insert(self.map, self.key.as_bytes());
        }

        let len = self.map.len;
        let ret = unsafe { self.map.ptr.add(len as usize) };
        unsafe {
            ret.write((self.key, value));
        }
        self.map.len += 1;

        let (_, value) = unsafe { &mut *ret.as_ptr() };
        value
    }
}

impl<'a, 'b> Entry<'a, 'b> {
    /// Returns a reference to the entry's key.
    pub fn key(&self) -> &str {
        match self {
            Entry::Occupied(e) => e.key(),
            Entry::Vacant(e) => e.key(),
        }
    }

    /// Ensures a value is in the entry by inserting the default if empty.
    pub fn or_insert(self, default: Value<'b>) -> &'a mut Value<'b> {
        match self {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => e.insert(default),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the function if empty.
    pub fn or_insert_with<F: FnOnce() -> Value<'b>>(self, default: F) -> &'a mut Value<'b> {
        match self {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => e.insert(default()),
        }
    }

    /// Ensures a value is in the entry by inserting the result of the function if empty.
    ///
    /// The function receives the entry's key as an argument.
    pub fn or_insert_with_key<F: FnOnce(&str) -> Value<'b>>(self, default: F) -> &'a mut Value<'b> {
        match self {
            Entry::Occupied(e) => e.into_mut(),
            Entry::Vacant(e) => {
                let value = default(e.key());
                e.insert(value)
            }
        }
    }

    /// Modifies an existing value and returns the entry.
    ///
    /// If the entry is vacant, this is a no-op.
    pub fn and_modify<F: FnOnce(&mut Value<'b>)>(self, f: F) -> Self {
        match self {
            Entry::Occupied(mut e) => {
                f(e.get_mut());
                Entry::Occupied(e)
            }
            Entry::Vacant(e) => Entry::Vacant(e),
        }
    }

    /// Ensures a value is in the entry by inserting [`Value::NULL`] if empty.
    pub fn or_default(self) -> &'a mut Value<'b> {
        self.or_insert(Value::NULL)
    }
}
