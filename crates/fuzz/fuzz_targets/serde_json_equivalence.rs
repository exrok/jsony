#![no_main]

use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    jsony_fuzz::assert_serde_json_equivalence(data);
});
