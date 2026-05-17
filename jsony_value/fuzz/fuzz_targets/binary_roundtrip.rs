#![no_main]

use jsony_value::Value;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let Ok(value) = jsony::from_binary::<Value<'_>>(data) else {
        return;
    };

    let encoded = jsony::to_binary(&value);
    let decoded: Value<'_> =
        jsony::from_binary(&encoded).expect("encoded jsony_value::Value should decode");
    let reencoded = jsony::to_binary(&decoded);

    assert_eq!(encoded, reencoded);
});
