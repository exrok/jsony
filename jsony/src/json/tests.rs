use crate::from_json;

use super::{FromJson, DecodeError};

fn assert_formattable<T: std::fmt::Display + PartialEq + for<'a> FromJson<'a>>(values: &[T]) {
    let type_name = std::any::type_name::<T>();
    for value in values {
        let encoded_value = format!("{}", value);
        match from_json::<T>(&encoded_value) {
            Err(err) => {
                panic!(
                    "assert_formattable<{}>({}): Failed to decode {:?} from {:?}",
                    type_name, value, err, encoded_value
                );
            }
            Ok(decoded_value) => {
                if &decoded_value != value {
                    panic!(
                        "assert_formattable<{}>({}): Incorrect value: {} from {:?}",
                        type_name, value, decoded_value, encoded_value
                    )
                }
            }
        }
    }
}

#[test]
fn ints() {
    assert_formattable(&[7u8, u8::MIN, u8::MAX]);
    assert_formattable(&[7u16, u16::MIN, u16::MAX]);
    assert_formattable(&[7u32, u32::MIN, u32::MAX]);
    assert_formattable(&[7u64, u64::MIN, u64::MAX]);
    assert_formattable(&[7u128, u128::MIN, u128::MAX]);

    assert_formattable(&[7i8, i8::MIN, i8::MAX]);
    assert_formattable(&[7i16, i16::MIN, i16::MAX]);
    assert_formattable(&[7i32, i32::MIN, i32::MAX]);
    assert_formattable(&[7i64, i64::MIN, i64::MAX]);
    assert_formattable(&[7i128, i128::MIN, i128::MAX]);
}
#[test]
fn bools() {
    assert_eq!(from_json::<bool>("true").unwrap(), true);
    assert_eq!(from_json::<bool>("   \n\t true  \n\t").unwrap(), true);
    assert!(from_json::<bool>("TRUE").is_err());

    assert!(from_json::<bool>("t rue").is_err());
    assert!(from_json::<bool>("tru e").is_err());
    assert!(from_json::<bool>("tru").is_err());

    assert_eq!(from_json::<bool>("false").unwrap(), false);
    assert_eq!(from_json::<bool>("   \n\t false  \n\t").unwrap(), false);
    assert!(from_json::<bool>("FALSE").is_err());
    assert!(from_json::<bool>("f alse").is_err());
    assert!(from_json::<bool>("fal se").is_err());
    assert!(from_json::<bool>("fal").is_err());
}
#[track_caller]
fn assert_err_contains(value: Option<&'static DecodeError>, expected: &str) {
    if let Some(err) = value {
        if !err.message.contains(expected) {
            panic!(
                "Expected error to contain '{}', got '{}'",
                expected, err.message
            );
        }
    } else {
        panic!("Expected error to contain '{}', got Ok", expected);
    }
}

#[test]
fn array() {
    assert!(from_json::<Vec<u8>>("[]").unwrap().is_empty());
    assert!(from_json::<Vec<u8>>(" \t  [\n\t ]\n  ").unwrap().is_empty());
    assert!(from_json::<Vec<u8>>(" \t  [\n\t ]\n  ").unwrap().is_empty());

    assert_eq!(
        from_json::<Vec<Vec<Vec<u8>>>>("[[[]]]").unwrap(),
        vec![vec![vec![]]]
    );

    assert_eq!(
        from_json::<(bool, &str)>("[true, \"text\"]").unwrap(),
        (true, "text")
    );

    assert_err_contains(
        from_json::<(bool, &str)>("[true, \"text\", faluse]").err(),
        "length",
    );

    assert_err_contains(from_json::<(bool, &str)>("[true]").err(), "length");

    assert_err_contains(from_json::<(bool, &str)>("[]").err(), "length");
    assert_eq!(from_json::<(u32,)>("[234]").unwrap(), (234,));
    assert_eq!(
        from_json::<(u8, bool, &str, Option<u16>, i8, i8)>("[ 1,  false, \"hi\", null, -1, -43 ]")
            .unwrap(),
        (1, false, "hi", None, -1, -43)
    );
}
