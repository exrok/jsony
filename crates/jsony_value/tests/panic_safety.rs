#[cfg(miri)]
mod miri {
    use jsony_value::{Value, ValueList, ValueMap};
    use std::alloc::{GlobalAlloc, Layout, System};
    use std::panic::{catch_unwind, AssertUnwindSafe};
    use std::sync::{
        atomic::{AtomicBool, AtomicUsize, Ordering},
        Mutex,
    };

    struct FailingAllocator;

    static FAIL_ENABLED: AtomicBool = AtomicBool::new(false);
    static ALLOCS_UNTIL_FAIL: AtomicUsize = AtomicUsize::new(usize::MAX);
    static FAIL_LOCK: Mutex<()> = Mutex::new(());

    #[global_allocator]
    static ALLOCATOR: FailingAllocator = FailingAllocator;

    unsafe impl GlobalAlloc for FailingAllocator {
        unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
            if should_fail_alloc() {
                std::ptr::null_mut()
            } else {
                unsafe { System.alloc(layout) }
            }
        }

        unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
            unsafe { System.dealloc(ptr, layout) }
        }

        unsafe fn realloc(&self, ptr: *mut u8, layout: Layout, new_size: usize) -> *mut u8 {
            if should_fail_alloc() {
                std::ptr::null_mut()
            } else {
                unsafe { System.realloc(ptr, layout, new_size) }
            }
        }
    }

    fn should_fail_alloc() -> bool {
        if !FAIL_ENABLED.load(Ordering::SeqCst) {
            return false;
        }

        let mut remaining = ALLOCS_UNTIL_FAIL.load(Ordering::SeqCst);
        loop {
            if remaining == 0 {
                FAIL_ENABLED.store(false, Ordering::SeqCst);
                return true;
            }
            match ALLOCS_UNTIL_FAIL.compare_exchange(
                remaining,
                remaining - 1,
                Ordering::SeqCst,
                Ordering::SeqCst,
            ) {
                Ok(_) => return false,
                Err(next) => remaining = next,
            }
        }
    }

    #[test]
    fn value_list_clone_drops_only_initialized_elements_if_nested_clone_panics() {
        let _guard = FAIL_LOCK.lock().unwrap();
        let inner_a = Value::from(ValueList::from_iter([Value::from(1i64)]));
        let inner_b = Value::from(ValueList::from_iter([Value::from(2i64)]));
        let list = ValueList::from_iter([inner_a, inner_b]);

        ALLOCS_UNTIL_FAIL.store(1, Ordering::SeqCst);
        FAIL_ENABLED.store(true, Ordering::SeqCst);

        let result = catch_unwind(AssertUnwindSafe(|| {
            let _ = list.clone();
        }));

        FAIL_ENABLED.store(false, Ordering::SeqCst);
        assert!(result.is_err());
    }

    #[test]
    fn value_map_clone_panics_instead_of_writing_after_allocation_failure() {
        let _guard = FAIL_LOCK.lock().unwrap();
        let map = ValueMap::from_iter([("a", Value::from(1i64)), ("b", Value::from(2i64))]);

        ALLOCS_UNTIL_FAIL.store(0, Ordering::SeqCst);
        FAIL_ENABLED.store(true, Ordering::SeqCst);

        let result = catch_unwind(AssertUnwindSafe(|| {
            let _ = map.clone();
        }));

        FAIL_ENABLED.store(false, Ordering::SeqCst);
        assert!(result.is_err());
    }

    #[test]
    fn zero_capacity_construction_does_not_allocate() {
        let _guard = FAIL_LOCK.lock().unwrap();

        let result = catch_unwind(AssertUnwindSafe(|| {
            ALLOCS_UNTIL_FAIL.store(0, Ordering::SeqCst);
            FAIL_ENABLED.store(true, Ordering::SeqCst);

            let list = ValueList::with_capacity(0);
            assert!(list.as_slice().is_empty());

            let map = ValueMap::from_iter(std::iter::empty::<(&str, Value<'_>)>());
            assert!(map.entries().is_empty());

            FAIL_ENABLED.store(false, Ordering::SeqCst);
        }));

        FAIL_ENABLED.store(false, Ordering::SeqCst);
        assert!(result.is_ok());
    }
}
