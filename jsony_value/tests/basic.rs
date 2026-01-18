use jsony_value::{Entry, Value, ValueMap, ValueString};

fn assert_static<T: 'static>(_: T) {}

#[test]
fn static_string() {
    let s = ValueString::from_static("hello world");
    assert_eq!(&*s, "hello world");

    let ptr_before = s.as_str().as_ptr();
    let owned = s.to_owned();
    let ptr_after = owned.as_str().as_ptr();
    assert_eq!(ptr_before, ptr_after, "to_owned should not clone static strings");
    assert_static(owned);

    let s2 = ValueString::from_static("clone test");
    let ptr_before = s2.as_str().as_ptr();
    let cloned = s2.clone();
    let ptr_after = cloned.as_str().as_ptr();
    assert_eq!(ptr_before, ptr_after, "clone should not allocate for static strings");

    drop(cloned);
    drop(s2);

    let empty_static = ValueString::from_static("");
    assert_eq!(&*empty_static, "");
    let owned_empty = empty_static.to_owned();
    assert_static(owned_empty);
}

#[test]
fn text() {
    let mut buffer = String::new();
    let empty: ValueString = buffer.as_str().into();
    assert_eq!(&*empty, "");
    let owned_empty = empty.to_owned();
    assert_eq!(&*owned_empty, "");
    let empty_cloned = owned_empty.clone();
    assert_static(owned_empty);
    assert_eq!(&*empty_cloned, "");

    buffer.push_str("some_data");
    let short: ValueString = buffer.as_str().into();
    assert_eq!(&*short, "some_data");
    let owned_short = short.to_owned();
    assert_eq!(&*owned_short, "some_data");
    let short_cloned = owned_short.clone();
    assert_static(owned_short);
    assert_eq!(&*short_cloned, "some_data");
}

#[test]
fn conversions() {
    macro_rules! assert_from_eq {
        ($value: expr, $expected_string: literal) => {
            assert_eq!(Value::from($value).to_string(), $expected_string)
        };
    }

    assert_from_eq!(0u8, "0");
    assert_from_eq!(42u8, "42");
    assert_from_eq!(-128i8, "-128");
    assert_from_eq!(-127i8, "-127");
    assert_from_eq!(433u16, "433");
    assert_from_eq!(-2342i16, "-2342");
    assert_from_eq!(1000_000_000u32, "1000000000");
    assert_from_eq!(-1000_000_000i32, "-1000000000");
    assert_from_eq!(1_000_000_000_000u64, "1000000000000");
    assert_from_eq!(-1_000_000_000_000i64, "-1000000000000");

    assert_from_eq!(0.0f32, "0");
    assert_from_eq!(-5.0f32, "-5");

    assert_from_eq!(0.0f64, "0");
    assert_from_eq!(-5.0f64, "-5");
    println!("{:?}", Value::from(true));

    assert_from_eq!(true, "true");
    assert_from_eq!(false, "false");

    assert_from_eq!(Some(false), "false");
    assert_from_eq!(Some(-44), "-44");
    assert_from_eq!(None::<i32>, "null");

    assert_from_eq!(
        Value::from_iter([("hello", true), ("nice", false)]),
        "{\"hello\": true, \"nice\": false}"
    );

    assert_from_eq!(Value::from_iter([-3, 42, 5000]), "[-3, 42, 5000]");
}

#[test]
fn map() {
    let mut map = ValueMap::from_iter([("hello", true), ("nice", false)]);
    map.insert("hy".into(), 43i32.into());
    map.insert("hy".into(), 54i32.into());
    for i in (0..100).rev() {
        map.insert(format!("{i}").into(), i.into());
    }
    assert!(map
        .get_all("hy")
        .eq(&[Value::from(43i32), Value::from(54i32)]));
    map.sort();
    assert!(map.entries().iter().map(|(x, _)| &**x).is_sorted());
    assert_eq!(map["43"], 43.into());
}

#[test]
fn entry_or_insert() {
    let mut map = ValueMap::new();
    map.entry("a").or_insert(1.into());
    assert_eq!(map.get("a"), Some(&Value::from(1)));
    map.entry("a").or_insert(2.into());
    assert_eq!(map.get("a"), Some(&Value::from(1)));
}

#[test]
fn entry_and_modify() {
    let mut map = ValueMap::new();
    map.entry("counter").or_insert(0.into());
    map.entry("counter")
        .and_modify(|v| *v = Value::from(v.as_i64().unwrap_or(0) + 1));
    assert_eq!(map.get("counter"), Some(&Value::from(1)));
}

#[test]
fn entry_remove() {
    let mut map = ValueMap::from_iter([("a", 1), ("b", 2), ("c", 3)]);
    if let Entry::Occupied(e) = map.entry("b") {
        assert_eq!(e.remove(), Value::from(2));
    }
    assert!(map.get("b").is_none());
    assert_eq!(map.entries().len(), 2);
}

#[test]
fn entry_large_map() {
    let mut map = ValueMap::new();
    for i in 0..20 {
        map.entry(format!("key{i}")).or_insert(Value::from(i as i64));
    }
    map.entry("key10")
        .and_modify(|v| *v = 100.into());
    assert_eq!(map.get("key10"), Some(&Value::from(100)));
}

#[test]
fn entry_or_insert_with() {
    let mut map = ValueMap::new();
    map.entry("computed").or_insert_with(|| Value::from(42));
    assert_eq!(map.get("computed"), Some(&Value::from(42)));
}

#[test]
fn entry_or_insert_with_key() {
    let mut map = ValueMap::new();
    map.entry("length").or_insert_with_key(|k| Value::from(k.len() as i64));
    assert_eq!(map.get("length"), Some(&Value::from(6)));
}

#[test]
fn entry_key() {
    let mut map = ValueMap::new();
    let entry = map.entry("test_key");
    assert_eq!(entry.key(), "test_key");
}

#[test]
fn entry_vacant_into_key() {
    let mut map = ValueMap::new();
    if let Entry::Vacant(e) = map.entry("my_key") {
        let key = e.into_key();
        assert_eq!(key.as_str(), "my_key");
    } else {
        panic!("expected vacant entry");
    }
}

#[test]
fn entry_occupied_insert() {
    let mut map = ValueMap::from_iter([("key", 1)]);
    if let Entry::Occupied(mut e) = map.entry("key") {
        let old = e.insert(Value::from(99));
        assert_eq!(old, Value::from(1));
        assert_eq!(e.get(), &Value::from(99));
    }
}

#[test]
fn entry_remove_entry() {
    let mut map = ValueMap::from_iter([("a", 1), ("b", 2)]);
    if let Entry::Occupied(e) = map.entry("a") {
        let (key, value) = e.remove_entry();
        assert_eq!(key.as_str(), "a");
        assert_eq!(value, Value::from(1));
    }
    assert!(map.get("a").is_none());
    assert_eq!(map.get("b"), Some(&Value::from(2)));
}

#[test]
fn entry_or_default() {
    let mut map = ValueMap::new();
    let val = map.entry("default").or_default();
    assert_eq!(*val, Value::NULL);
}
