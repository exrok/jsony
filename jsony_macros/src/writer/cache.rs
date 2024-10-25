use super::IdentCacheEntry;
use proc_macro::{Punct, Spacing};
pub static
BLIT_SRC: &[u8] = b"c\x03\x06\n\t\x02b\x02\x01\x88J^\t\x04\x9d\n\t\x1d\n\t\x11\x03\x06\x08\x02qyI\x05\x00\x95\x88f\x97\t\n\t\x9b\n\t\x94\n\t\x81\x03\xa2\x02\x01\xa0\t\x04\n\t\xa0\n\tD\x03\x06\x08\x02q\n\tI\x95\x88`\x97\t\n\t\x9b\n\t\x94\n\t\x81\x03\xa2\x02\x01\xa0\t\x04\x9d\x08\x02\n\t\x9b\n\t8\n\t\x17\x03\xa2\x01\x04\x06;\n\t\xa1\n\t\x9a\n\t{\x02\x95c\n\tZb\tZ\x01\x88K\x04\x8d\x01}\t\x04\x9d\x1b\x03?\x02\n\t\x8eb\n\t\x8e\x01yo\x05\n\t\x8e\x02\n\to\x00\x88\x92\x04\x8d\x01\x9f\t\x04\x9d\xa1\n\tG\x08\x02q\n\to\n\tZ\x02\n\tK\x02\n\tJ\n\tn\x03\x06\n\t\x13\x03\x06\n\t\x8f\n\t\'\x02\n\t`\n\t\x8f\n\t\x12.\t\x011\t\n\t\x9b\n\t\x85\n\th\x0b\x01|\t\x9b\n\t\x85\n\t>\x9b\n\t\x94\n\tN\n\t\x03\x02\x98\x95\x88\x0e\x9d\x0c\x94\t\n\t\x9b\n\t\x94\n\t\x81\x03\xa2\x02\x0c\x9e\x8b\t\x94\r\x90\xa2\r\x87\n\t\xa1\n\t\x8f\n\t\x19\n\t\xa1\n\t\x8f\n\t\x15(\t\x04\x01$\t\x04\x01!\t\x04\x9e\x9dv\x05\x03\x98\n\t\xa1\n\t\x9a\n\tB\x02\n\tf\x97\rM\x01\xa0\x01\x00t\n\t\x03\x06\x02\xa2\r|\x97\x01\xa0\x01r\x04\x9dv\x98\n\t\xa1\n\tn\x03\x06\x02\x02\n\t`\x9f\r\x8a\x98\n\t\xa1\n\t\x8e\x02\n\t\x92\x04\x8d\r\x9f\r:\xa2\x00\x9f\rS\xa2\x9f\ra\xa2\x9f\r\x7f\xa2\x00\x00\x03\x9f\r\x84\xa2\x83\x8c\t\xa2\x05\x83\x95\x88t\x03\x06\x02\xa2\x08\x02\n\t\xa1\n\t\x8f\n\tp\x03\x06d\t\x04\x83\x01i\t\n\t\x9b\n\te\n\tY\x97\x01\xa0\x01X\xa1\n\t\x8f\n\tA\"\t\x97\x01~\tt\n\t\x03\x06\x02\xa2\x01\x1e\t\x016\t\xa1\n\t\x8f\n\tA\x03\x06\x93\x8d\x07\x02\x9f\r\x84\xa2\x00q\n\to\xa3ys\x00\x9e~\x05\n\t\xa1\n\t\x8f\n\tpd\t\x83\x01i\t\n\t\x9b\n\te\n\tY\x01\x00\x9e\x9d\x86\x05\n\t\x9b\n\t\x85\n\tW\n\t\x03s\x02\n\tm\xa2\x00\x9e\x9dl\x05\n\t\x9b\n\t\x85\n\tW\n\t\x03\x02\n\tm\xa2\x00\x9e\x9dv\x05\x03\n\t\x9b\n\t\x94\n\t\x81\n\tgl\r\\\xa2\r\x90\xa2\x00O\x9e\x9c\x05~\r|\x86\r\\\xa2\r\x90\xa2\x99\x9c\x9ex\x05\x86\r]\xa2\x00\x97\r\x90\n\t\x03\x02\xa2\r\x87\tx\r\tl\r]\xa2\x01u\x06\x80\x00\x9e\x9d\x86\x05\n\t\x9b\n\t\x85\n\tW\n\t\x03s\x02\n\tm\xa2\x00O\x9e\x9c\x01\xa0\x01X\x01\x93\x03\x02\x02\n\t \x01\x9c\x9ek\x05\xa0\rk\xa2\x00\x06\x80\t\xa0\r7\x04k\x04\n\t\xa1\n\t\x9a\n\t{w\t\x9e\x89\x05\x93\xa0\r\x07\x02\x8b\x01\x9c\x07\x02\x99\x9c\x9e\x96\xa2_\x04\xa1\n\t\x9a\n\t{\x93\x89\x8c\x07\x02\xa0\rT\x89\rU\xa2\x00\x99\x9c\x04\n\t\xa1\n\t\xa0\n\tH\xa0\r#\xa2\x9e\x91\x05\x93\xa0\r0\xa2\x99\x96\x01\x96\x00\x9b\n\t\x94\n\tN\n\t\x03\x97\r\x90\xa2\r\x1c\xa2\x00\x99\x91\x00\x93\xa0\r2\xa2\n\t\xa1\n\t\xa0\n\tE\n\t\x14\n\t\xa1\n\t\xa0\n\tE\n\tF\x07\x02\x93\xa0\r5\xa2\x9e\x8b\x05\x93\x89\x00\x99\x96}\rj\x93^\r\x1f\xa2\r3\xa2\x00\rS\xa2\x00\r4\ra\xa2\x00\n\t\xa1\n\t\x9a\n\t\x1a\n\t/\n\t\xa1\n\t\x9a\n\tC\n\tQ\n\t\xa1\n\t\x9a\n\t\x10\n\tQ\x9e\x8c\t\n\t\xa1\n\t\x9a\n\t\x18\x05\r\x92\rP\xa2\x00\x9e\x8c\t\n\t\xa1\n\t\x9a\n\t\x16\x05\r-\xa2\x00\rP\xa2\x00\x9e\x8c\t\n\t\xa1\n\t\x9a\n\t\x0f\x05\r,\xa2\x00V\n\t\xa1\n\t\x8e\x00\x9e\x9dR\t\x04\x9d\n\t\xa1\n\t\x9a\n\tC\x05L\x05R\r*\xa2\x00L\r9\xa2\x00F\n\t)V\n\t\xa1\n\t\x8e\x00\x9e\x9d[\x05\xa1\n\tG\n\t@\x05\x04\x9d[\x00[\r+\xa2";
pub const IDENT_SIZE: usize = 147;
static NAMES: &[&str] = &[
    "ArrayValue",
    "ArrayWriter",
    "Decoder",
    "Field",
    "FromBinary",
    "Object",
    "ObjectSchemaInner",
    "ObjectValue",
    "Result",
    "StringValue",
    "UnsafeReturn",
    "ValueWriter",
    "Vec",
    "as_mut",
    "binary",
    "bitset",
    "byte",
    "decode_json",
    "defaults",
    "destination",
    "discard_remaining_object_fields",
    "drops",
    "enter_object",
    "enter_seen_object",
    "erase",
    "fields",
    "from",
    "inner_writer",
    "into_string",
    "join_array_with_next_value",
    "join_object_with_next_value",
    "name",
    "new",
    "object_step",
    "offset",
    "peek",
    "push_colon",
    "push_unchecked_ascii",
    "read_seen_string_unescaped",
    "required",
    "restore_for_retry",
    "result",
    "smart_object_comma",
    "start_json_array",
    "static",
    "tag_query_at_content_next_object",
    "tag_query_next_object",
    "transmute",
    "u8",
    "with_capacity",
    "DynamicFieldDecoder",
    "FromJsonFieldVisitor",
    "ObjectWriter",
    "Parser",
    "Peek",
    "String",
    "TextWriter",
    "UNKNOWN_VARIANT",
    "Vistor",
    "binary_decode",
    "binary_encode",
    "builder",
    "byte_add",
    "drop_in_place",
    "if",
    "join_parent_json_value_with_next",
    "non_terminating",
    "object_writer",
    "push_comma",
    "report_error",
    "to_string",
    "use",
    "MaybeUninit",
    "None",
    "PhantomData",
    "ToBinary",
    "_builder",
    "as_mut_ptr",
    "assume_init",
    "decoder",
    "else",
    "emplace_from_json",
    "end_json_array",
    "for",
    "impl",
    "inner",
    "marker",
    "new_field_visitor",
    "new_unchecked",
    "offset_of",
    "phantom",
    "push",
    "snapshot",
    "temp_flatten",
    "uninit",
    "FromJson",
    "Kind",
    "ObjectSchema",
    "Self",
    "Some",
    "__TEMP",
    "__schema_inner",
    "break",
    "flatten_visitor",
    "message",
    "temp2",
    "type",
    "where",
    "DecodeError",
    "decode",
    "encoder",
    "schema",
    "start_json_object",
    "success",
    "NonNull",
    "_err",
    "const",
    "end_json_object",
    "mem",
    "temp",
    "write",
    "fn",
    "variant",
    "push_str",
    "value",
    "_",
    "self",
    "ToJson",
    "__internal",
    "cast",
    "err",
    "jsony_to_json_into",
    "match",
    "ptr",
    "unsafe",
    "Ok",
    "dst",
    "as",
    "return",
    "json",
    "std",
    "Err",
    "mut",
    "let",
    "out",
    "parser",
    "jsony",
];
pub fn ident_cache_initial_state() -> Box<[IdentCacheEntry; IDENT_SIZE]> {
    unsafe {
        let mut cache =
            std::alloc::alloc(std::alloc::Layout::array::<IdentCacheEntry>(IDENT_SIZE).unwrap())
                as *mut IdentCacheEntry;
        for (i, &name) in NAMES.iter().enumerate() {
            std::ptr::write(cache.add(i), IdentCacheEntry::Empty(name));
        }
        Box::from_raw(cache as *mut [IdentCacheEntry; IDENT_SIZE])
    }
}
pub const PUNCT_SIZE: usize = 15;
pub fn punct_cache_initial_state() -> [Punct; PUNCT_SIZE] {
    [
        Punct::new(';', Spacing::Alone),
        Punct::new(',', Spacing::Alone),
        Punct::new('>', Spacing::Alone),
        Punct::new('<', Spacing::Alone),
        Punct::new('&', Spacing::Alone),
        Punct::new('=', Spacing::Alone),
        Punct::new('\'', Spacing::Joint),
        Punct::new('=', Spacing::Joint),
        Punct::new('-', Spacing::Joint),
        Punct::new(':', Spacing::Alone),
        Punct::new(':', Spacing::Joint),
        Punct::new('!', Spacing::Alone),
        Punct::new('|', Spacing::Alone),
        Punct::new('.', Spacing::Alone),
        Punct::new('*', Spacing::Alone),
    ]
}
