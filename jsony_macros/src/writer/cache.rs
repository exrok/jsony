use super::IdentCacheEntry;
use proc_macro::{Punct, Spacing};
pub static
BLIT_SRC: &[u8] = b"f\x03\x06\n\t\x02z\x02\x01\x8fMb\t\x05\xa3\n\t\x1c\n\t\x11\x03\x06\x08\x02u~L\x04\x00\x97\x8fj\x9e\t\n\t\xa1\n\t\x95\n\t\x88\x03\xa8\x02\x01\xa4\t\x05\n\t\xa4\n\tF\x03\x06\x08\x02u\n\tL\x97\x8fd\x9e\t\n\t\xa1\n\t\x95\n\t\x88\x03\xa8\x02\x01\xa4\t\x05\xa3\x08\x02\n\t\xa1\n\t7\n\t\x16\x03\xa8\x01\x05\x06;\n\t\xa7\n\t\xa0\n\t\x80\x02\x97f\n\t^z\t^\x01\x8fN\x05\x96\x01\x82\t\x05\xa3\x1a\x03?\x02\n\t\x90z\n\t\x90\x01~s\x04\n\t\x90\x02\n\ts\x00\x8f\x9b\x05\x96\x01\xa5\t\x05\xa3\xa7\n\tJ\x08\x02u\n\ts\n\t^\x02\n\tN\x02\n\tM\n\tr\x03\x06\n\t\x13\x03\x06\n\t\x9a\n\t&\x02\n\td\n\t\x9a\n\t\x12.\t\x011\t\n\t\xa1\n\t\x8c\n\tl\x0b\x01\x81\t\xa1\n\t\x8c\n\t>\xa1\n\t\x95\n\tQ\n\t\x03\x02\x9d\x97\x8f\x0e\xa3\x0c\x95\t\n\t\xa1\n\t\x95\n\t\x88\x03\xa8\x02\x0c\xa6\x98\t\x95\r\x92\xa8\r\x8e\n\t\xa7\n\t\x9a\n\t\x18\n\t\xa7\n\t\x9a\n\t\x15\'\t\x05\x01#\t\x05\x01 \t\x05\xa6\xa3\x8b\x04\x03\x9d\n\t\xa7\n\t\xa0\n\tC\x02\n\tj\x9e\rP\x01\xa4\x01\x00x\n\t\x03\x06\x02\xa8\r\x81\x9e\x01\xa4\x01v\x05\xa3\x8b\x9d\n\t\xa7\n\tr\x03\x06\x02\x02\n\td\xa5\r\x84\x9d\n\t\xa7\n\t\x90\x02\n\t\x9b\x05\x96\r\xa5\r:\xa8\x00\xa5\r|\xa8\xa5\re\xa8\xa5\r|\xa8\x00T\x01\x98\r,\xa8\xa6\x99\t\n\t\xa7\n\t\xa0\n\tI\x04T\r\x9b\x00\xa5\rW\xa8\x00\x98\r\x9b\x00\xa5\r|\xa8\x00\xa5\rh\xa8\x00\xa6\x99\t\n\t\xa7\n\t\xa0\n\tD\x04\xa5\rS\xa8\x00\xa5\r\x86\xa8\x00\xa5\r\x83\xa8\x8a\x99\t\xa8\x04\x8a\x97\x8fx\x03\x06\x02\xa8\x08\x02\n\t\xa7\n\t\x9a\n\tt\x03\x06g\t\x05\x8a\x01m\t\n\t\xa1\n\ti\n\t]\x9e\x01\xa4\x01\\\xa7\n\t\x9a\n\tB!\t\x9e\x01\x85\tx\n\t\x03\x06\x02\xa8\x01\x1d\t\x015\t\xa7\n\t\x9a\n\tB\x03\x06\x94\x96\x07\x02\xa5\r\x83\xa8\x00u\n\ts\xa9~w\x00\xa6\x85\x04\n\t\xa7\n\t\x9a\n\ttg\t\x8a\x01m\t\n\t\xa1\n\ti\n\t]\x01\x00\xa6\xa3\x8d\x04\n\t\xa1\n\t\x8c\n\t[\n\t\x03w\x02\n\tq\xa8\x00\xa6\xa3p\x04\n\t\xa1\n\t\x8c\n\t[\n\t\x03\x02\n\tq\xa8\x00\xa6\xa3\x8b\x04\x03\n\t\xa1\n\t\x95\n\t\x88\n\tkp\r`\xa8\r\x92\xa8\xa6\xa3\x8b\x04\xa7\n\t\x9a\n\t\x178\t\x01@\t\x8bR\xa6\xa2\x04\x85\r\x81\x8d\r`\xa8\r\x92\xa8\x9f\xa2\xa6}\x04\x8d\ra\xa8\x00\x9e\r\x92\n\t\x03\x02\xa8\r\x8e\t}\r\tp\ra\xa8\x01y\x06\x87\x00\xa6\xa3\x8d\x04\n\t\xa1\n\t\x8c\n\t[\n\t\x03w\x02\n\tq\xa8\x00R\xa6\xa2\x01\xa4\x01\\\x01\x94\x03\x02\x02\n\t\x1f\x01\xa2\xa6o\x04\xa4\ro\xa8\x00\x06\x87\t\xa4\r6\x05o\x05\n\t\xa7\n\t\xa0\n\t\x80{\t\xa6\x91\x04\x94\xa4\r\x07\x02\x98\x01\xa2\x07\x02\x9f\xa2\xa6\x9c\xa8c\x05\xa7\n\t\xa0\n\t\x80\x94\x91\x99\x07\x02\xa6\x91\x04\x91\rY\xa8\x00\xa4\rX\x00\x9f\xa2\x05\n\t\xa7\n\t\xa4\n\tK\xa4\r\"\xa8\xa6\x93\x04\x94\xa4\r0\xa8\x9f\x9c\x01\x9c\x00\xa1\n\t\x95\n\tQ\n\t\x03\x9e\r\x92\xa8\r\x1b\xa8\x00\x9f\x93\x00\x94\xa4\r2\xa8\n\t\xa7\n\t\xa4\n\tG\n\t\x14\n\t\xa7\n\t\xa4\n\tG\n\tH\x07\x02\x94\xa4\r4\xa8\xa6\x98\x04\x94\x91\x00\x9f\x9c\x82\rn\x94b\r\x1e\xa8\r3\re\xa8\x00\n\t\xa7\n\t\xa0\n\t\x19\n\t/\n\t\xa7\n\t\xa0\n\tE\n\tU\n\t\xa7\n\t\xa0\n\t\x10\n\tU\rh\xa8\x00\xa6\x99\t\n\t\xa7\n\t\xa0\n\t\x0f\x04\r-\xa8\x00Z\n\t\xa7\n\t\x90\x00\xa6\xa3V\t\x05\xa3\n\t\xa7\n\t\xa0\n\tE\x04O\x04V\r*\xa8\x00O\r9\xa8\x00H\n\t(Z\n\t\xa7\n\t\x90\x00\xa6\xa3_\x04\xa7\n\tJ\n\tA\x04\x05\xa3_\x00_\r+\xa8";
pub const IDENT_SIZE: usize = 153;
static NAMES: &[&str] = &[
    "ArrayValue",
    "ArrayWriter",
    "Decoder",
    "Field",
    "FromBinary",
    "Object",
    "ObjectSchemaInner",
    "Result",
    "SkipFieldVisitor",
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
    "in",
    "inner_writer",
    "into_string",
    "iter",
    "join_array_with_next_value",
    "name",
    "new",
    "object_step",
    "offset",
    "peek",
    "push_unchecked_ascii",
    "read_seen_string_unescaped",
    "required",
    "restore_for_retry",
    "result",
    "skipped_field",
    "smart_object_comma",
    "start_json_array",
    "static",
    "tag_query_at_content_next_object",
    "tag_query_next_object",
    "transmute",
    "u8",
    "visitor",
    "with_capacity",
    "DynamicFieldDecoder",
    "FromJsonFieldVisitor",
    "ObjectValue",
    "ObjectWriter",
    "Parser",
    "Peek",
    "String",
    "StringValue",
    "TextWriter",
    "UNKNOWN_VARIANT",
    "Vistor",
    "binary_decode",
    "binary_encode",
    "builder",
    "byte_add",
    "drop_in_place",
    "if",
    "join_object_with_next_value",
    "key",
    "non_terminating",
    "object_writer",
    "push_colon",
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
    "impl",
    "inner",
    "join_parent_json_value_with_next",
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
    "for",
    "message",
    "push_comma",
    "temp2",
    "type",
    "where",
    "DecodeError",
    "decode",
    "encoder",
    "end_json_object",
    "push_str",
    "schema",
    "start_json_object",
    "success",
    "NonNull",
    "_err",
    "const",
    "flatten_visitor",
    "mem",
    "temp",
    "write",
    "fn",
    "ToJson",
    "variant",
    "cast",
    "err",
    "match",
    "ptr",
    "self",
    "unsafe",
    "value",
    "_",
    "__internal",
    "jsony_to_json_into",
    "Ok",
    "as",
    "dst",
    "return",
    "json",
    "std",
    "Err",
    "mut",
    "parser",
    "out",
    "let",
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
        Punct::new('=', Spacing::Alone),
        Punct::new('&', Spacing::Alone),
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
