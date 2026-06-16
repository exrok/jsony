//! Temporary leak-audit suite. Drives partial-initialization error paths that
//! the existing soundness suite does not, relying on a counting type plus
//! miri's allocation-leak detection.
use std::collections::HashMap;
use std::sync::Mutex;
use std::sync::atomic::{AtomicIsize, Ordering};

use jsony::Jsony;

static LIVE: AtomicIsize = AtomicIsize::new(0);
// The LIVE counter is process-global, so concurrent tests would corrupt each
// other's balance. Serialize the counted regions.
static LOCK: Mutex<()> = Mutex::new(());

/// Heap-owning, drop-counting field. A missed drop shows up two ways: LIVE
/// stays non-zero and miri reports the leaked Box.
#[derive(Debug, PartialEq)]
struct Tracked(#[allow(dead_code)] Box<u32>);

unsafe impl<'a> jsony::FromJson<'a> for Tracked {
    unsafe fn emplace_from_json(
        dest: std::ptr::NonNull<()>,
        parser: &mut jsony::parser::Parser<'a>,
    ) -> Result<(), &'static jsony::json::DecodeError> {
        let value = u32::decode_json(parser)?;
        LIVE.fetch_add(1, Ordering::SeqCst);
        unsafe {
            dest.cast::<Tracked>().write(Tracked(Box::new(value)));
        }
        Ok(())
    }
}
impl Drop for Tracked {
    fn drop(&mut self) {
        LIVE.fetch_sub(1, Ordering::SeqCst);
    }
}

#[track_caller]
fn assert_err_no_leak<T: for<'a> jsony::FromJson<'a>>(input: &str) {
    let _guard = LOCK.lock().unwrap_or_else(|e| e.into_inner());
    LIVE.store(0, Ordering::SeqCst);
    let r = jsony::from_json::<T>(input);
    assert!(r.is_err(), "expected error for {input:?}");
    assert_eq!(LIVE.load(Ordering::SeqCst), 0, "leak for input {input:?}");
}

#[track_caller]
fn assert_ok_no_leak<T: for<'a> jsony::FromJson<'a>>(input: &str) {
    let _guard = LOCK.lock().unwrap_or_else(|e| e.into_inner());
    LIVE.store(0, Ordering::SeqCst);
    {
        let r = jsony::from_json::<T>(input);
        assert!(r.is_ok(), "expected ok for {input:?}");
    }
    assert_eq!(LIVE.load(Ordering::SeqCst), 0, "leak for input {input:?}");
}

#[test]
fn vec_of_heap_struct_fails_midway() {
    #[derive(Jsony)]
    struct S {
        #[allow(dead_code)]
        a: Tracked,
        #[allow(dead_code)]
        b: Tracked,
    }
    // Second element's `b` is the wrong type -> element fails after several
    // Tracked already live across the vec and the partial struct.
    assert_err_no_leak::<Vec<S>>(r#"[{"a":1,"b":2},{"a":3,"b":"x"}]"#);
    // Truncated mid vec.
    assert_err_no_leak::<Vec<S>>(r#"[{"a":1,"b":2},{"a":3,"b":4"#);
    assert_ok_no_leak::<Vec<S>>(r#"[{"a":1,"b":2},{"a":3,"b":4}]"#);
}

#[test]
fn vec_direct_heap_element_fails() {
    assert_err_no_leak::<Vec<Tracked>>(r#"[1,2,3,"x"]"#);
    assert_err_no_leak::<Vec<Tracked>>(r#"[1,2,3"#);
}

#[test]
fn map_value_heap_fails_midway() {
    assert_err_no_leak::<HashMap<String, Tracked>>(r#"{"a":1,"b":2,"c":"x"}"#);
    assert_err_no_leak::<HashMap<String, Tracked>>(r#"{"a":1,"b":2"#);
    assert_ok_no_leak::<HashMap<String, Tracked>>(r#"{"a":1,"b":2}"#);
}

#[test]
fn flatten_map_fails_after_entries() {
    #[derive(Jsony)]
    struct S {
        #[allow(dead_code)]
        head: u32,
        #[jsony(flatten)]
        #[allow(dead_code)]
        rest: HashMap<String, Tracked>,
    }
    // head ok, flatten map accumulates entries, then a later field fails.
    assert_err_no_leak::<S>(r#"{"head":1,"x":2,"y":3,"z":"bad"}"#);
    // truncated after some flatten entries
    assert_err_no_leak::<S>(r#"{"head":1,"x":2,"y":3"#);
    // missing required head, but flatten map already has entries
    assert_err_no_leak::<S>(r#"{"x":2,"y":3}"#);
    assert_ok_no_leak::<S>(r#"{"head":1,"x":2,"y":3}"#);
}

#[test]
fn flatten_struct_fails_after_fields() {
    #[derive(Jsony)]
    #[jsony(Flattenable)]
    struct Inner {
        #[allow(dead_code)]
        x: Tracked,
        #[allow(dead_code)]
        y: Tracked,
    }
    #[derive(Jsony)]
    struct Outer {
        #[allow(dead_code)]
        head: Tracked,
        #[jsony(flatten)]
        #[allow(dead_code)]
        inner: Inner,
    }
    // head + inner.x live, inner.y fails.
    assert_err_no_leak::<Outer>(r#"{"head":1,"x":2,"y":"bad"}"#);
    // truncated after head + inner.x
    assert_err_no_leak::<Outer>(r#"{"head":1,"x":2"#);
    // missing required inner.y (complete() should fail) with head + x live
    assert_err_no_leak::<Outer>(r#"{"head":1,"x":2}"#);
    assert_ok_no_leak::<Outer>(r#"{"head":1,"x":2,"y":3}"#);
}

#[test]
fn validate_attribute_drops_on_failure() {
    #[derive(Jsony)]
    struct S {
        #[jsony(validate = jsony::require!(|v| *v.0 < 100, "too big"))]
        #[allow(dead_code)]
        a: Tracked,
        #[allow(dead_code)]
        b: Tracked,
    }
    // a decodes (heap live) then validation fails -> a must be dropped.
    assert_err_no_leak::<S>(r#"{"a":200,"b":2}"#);
    // a valid, b decodes, ok
    assert_ok_no_leak::<S>(r#"{"a":1,"b":2}"#);
}

#[test]
fn duplicate_field_drops_existing() {
    #[derive(Jsony)]
    struct S {
        #[allow(dead_code)]
        a: Tracked,
        #[allow(dead_code)]
        b: Tracked,
    }
    // duplicate `a` after b live -> error, both live values must drop.
    assert_err_no_leak::<S>(r#"{"a":1,"b":2,"a":3}"#);
}

#[test]
fn enum_content_multifield_fails() {
    #[derive(Jsony)]
    #[jsony(tag = "k", content = "c")]
    #[allow(dead_code)]
    enum E {
        V { a: Tracked, b: Tracked },
    }
    assert_err_no_leak::<E>(r#"{"k":"V","c":{"a":1,"b":"bad"}}"#);
    assert_err_no_leak::<E>(r#"{"k":"V","c":{"a":1,"b":2"#);
    assert_ok_no_leak::<E>(r#"{"k":"V","c":{"a":1,"b":2}}"#);
}

#[test]
fn enum_internal_multifield_fails() {
    #[derive(Jsony)]
    #[jsony(tag = "k")]
    #[allow(dead_code)]
    enum E {
        V { a: Tracked, b: Tracked },
    }
    assert_err_no_leak::<E>(r#"{"k":"V","a":1,"b":"bad"}"#);
    assert_err_no_leak::<E>(r#"{"k":"V","a":1,"b":2"#);
    assert_ok_no_leak::<E>(r#"{"k":"V","a":1,"b":2}"#);
}

#[test]
fn untagged_enum_partial_variant_then_fallthrough() {
    #[derive(Jsony)]
    #[jsony(untagged)]
    #[allow(dead_code)]
    enum E {
        // Tried first: decodes `a` (heap live) then fails on `b`, must clean up
        // before the next variant is attempted.
        Two { a: Tracked, b: Tracked },
        One { a: Tracked },
    }
    // matches `One` after `Two` partially decodes `a` and fails on missing `b`.
    assert_ok_no_leak::<E>(r#"{"a":1}"#);
    // neither variant matches; both attempts must clean up.
    assert_err_no_leak::<E>(r#"{"b":2}"#);
    assert_ok_no_leak::<E>(r#"{"a":1,"b":2}"#);
}

#[test]
fn array_of_heap_struct_fails() {
    #[derive(Jsony)]
    struct S {
        #[allow(dead_code)]
        a: Tracked,
    }
    assert_err_no_leak::<[S; 3]>(r#"[{"a":1},{"a":2},{"a":"bad"}]"#);
    assert_err_no_leak::<[S; 3]>(r#"[{"a":1},{"a":2}]"#);
    assert_ok_no_leak::<[S; 3]>(r#"[{"a":1},{"a":2},{"a":3}]"#);
}
