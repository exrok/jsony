use super::IdentCacheEntry;
use proc_macro::{Punct, Spacing};
pub static
BLIT_SRC: &[u8] = b"g\x03\x06\n\t\x01f\x01\x02\x8bJb\t\x05\xa0\n\t\x1d\n\t\x11\x03\x06\x08\x01t|I\x04\x00\x98\x8bj\x9a\t\n\t\x9e\n\t\x97\n\t\x84\x03\xa5\x01\x02\xa3\t\x05\n\t\xa3\n\tD\x03\x06\x08\x01t\n\tI\x98\x8bd\x9a\t\n\t\x9e\n\t\x97\n\t\x84\x03\xa5\x01\x02\xa3\t\x05\xa0\x08\x01\n\t\x9e\n\t8\n\t\x17\x03\xa5\x02\x05\x06;\n\t\xa4\n\t\x9d\n\t~\x01\x98g\n\t^f\t^\x02\x8bK\x05\x90\x02\x80\t\x05\xa0\x1b\x03?\x01\n\t\x91f\n\t\x91\x02|r\x04\n\t\x91\x01\n\tr\x00\x8b\x95\x05\x90\x02\xa2\t\x05\xa0\xa4\n\tG\x08\x01t\n\tr\n\t^\x01\n\tK\x01\n\tJ\n\tq\x03\x06\n\t\x13\x03\x06\n\t\x92\n\t(\x01\n\td\n\t\x92\n\t\x12/\t\x022\t\n\t\x9e\n\t\x88\n\tl\x0b\x02\x7f\t\x9e\n\t\x88\n\t>\x9e\n\t\x97\n\tN\n\t\x03\x01\x9b\x98\x8b\x0e\xa0\x0c\x97\t\n\t\x9e\n\t\x97\n\t\x84\x03\xa5\x01\x0c\xa1\x8e\t\x97\r\x93\xa5\r\x8a\n\t\xa4\n\t\x92\n\t\x19\n\t\xa4\n\t\x92\n\t\x15)\t\x05\x02%\t\x05\x02\"\t\x05\xa1\xa0y\x04\x03\x9b\n\t\xa4\n\t\x9d\n\tB\x01\n\tj\x9a\rM\x02\xa3\x02\x00w\n\t\x03\x06\x01\xa5\r\x7f\x9a\x02\xa3\x02u\x05\xa0y\x9b\n\t\xa4\n\tq\x03\x06\x01\x01\n\td\xa2\r\x8d\x9b\n\t\xa4\n\t\x91\x01\n\t\x95\x05\x90\r\xa2\r:\xa5\x00\xa2\rV\xa5\xa2\re\xa5\xa2\r\x82\xa5\x00\x00\x03\xa2\r\x87\xa5\x86\x8f\t\xa5\x04\x86\x98\x8bw\x03\x06\x01\xa5\x08\x01\n\t\xa4\n\t\x92\n\ts\x03\x06h\t\x05\x86\x02m\t\n\t\x9e\n\ti\n\t]\x9a\x02\xa3\x02\\\xa4\n\t\x92\n\tA#\t\x9a\x02\x81\tw\n\t\x03\x06\x01\xa5\x02\x1e\t\x027\t\xa4\n\t\x92\n\tA\x03\x06\x96\x90\x07\x01\xa2\r\x87\xa5\x00t\n\tr\xa6|v\x00\xa1\x81\x04\n\t\xa4\n\t\x92\n\tsh\t\x86\x02m\t\n\t\x9e\n\ti\n\t]\x02\x00\xa1\xa0\x89\x04\n\t\x9e\n\t\x88\n\t[\n\t\x03v\x01\n\tp\xa5\x00\xa1\xa0o\x04\n\t\x9e\n\t\x88\n\t[\n\t\x03\x01\n\tp\xa5\x00\xa1\xa0y\x04\x03\n\t\x9e\n\t\x97\n\t\x84\n\tko\r`\xa5\r\x93\xa5\x00O\xa1\x9f\x04\x81\r\x7f\x89\r`\xa5\r\x93\xa5\x9c\x9f\xa1{\x04\x89\ra\xa5\x00\x9a\r\x93\n\t\x03\x01\xa5\r\x8a\t{\r\to\ra\xa5\x02x\x06\x83\x00\xa1\xa0\x89\x04\n\t\x9e\n\t\x88\n\t[\n\t\x03v\x01\n\tp\xa5\x00O\xa1\x9f\x02\xa3\x02\\\x02\x96\x03\x01\x01\n\t!\x02\x9f\xa1Q\x04\xa3\rP\x00\xa1R\x04\xa3\rW\x00\x06\x83\t\xa3\rP\x04Q\x00\xa3\rW\x04R\x00\x05\n\t\xa4\n\t\x9d\n\t~z\t\xa3\r \xa5\x00\x99\xa1\x8c\x04\x96\xa3\r\x07\x01\x8e\x02\x9f\x07\x01\x9c\x9f\xa1\x99\xa5c\x05\xa4\n\t\x9d\n\t~\x96\x8c\x8f\x07\x01\xa3\rX\x8c\rY\xa5\x00\x9c\x9f\x05\n\t\xa4\n\t\xa3\n\tH\xa3\r$\xa5\xa1\x94\x04\x96\xa3\r1\xa5\x9c\x99\x02\x99\x00\x9e\n\t\x97\n\tN\n\t\x03\x9a\r\x93\xa5\r\x1c\xa5\x00\x9c\x94\x00\x96\xa3\r3\xa5\n\t\xa4\n\t\xa3\n\tE\n\t\x14\n\t\xa4\n\t\xa3\n\tE\n\tF\x07\x01\x96\xa3\r6\xa5\xa1\x8e\x04\x96\x8c\x00\x9c\x99\x80\rn\x96b\r\x1f\xa5\r4\xa5\x00\rV\xa5\x00\r5\re\xa5\x00\n\t\xa4\n\t\x9d\n\t\x1a\n\t0\n\t\xa4\n\t\x9d\n\tC\n\tT\n\t\xa4\n\t\x9d\n\t\x10\n\tT\xa1\x8f\t\n\t\xa4\n\t\x9d\n\t\x18\x04\r\x95\rS\xa5\x00\xa1\x8f\t\n\t\xa4\n\t\x9d\n\t\x16\x04\r.\xa5\x00\rS\xa5\x00\xa1\x8f\t\n\t\xa4\n\t\x9d\n\t\x0f\x04\r-\xa5\x00Z\n\t\xa4\n\t\x91\x00\xa1\xa0U\t\x05\xa0\n\t\xa4\n\t\x9d\n\tC\x04L\x04U\r+\xa5\x00L\r9\xa5\x00F\n\t*Z\n\t\xa4\n\t\x91\x00\xa1\xa0_\x04\xa4\n\tG\n\t@\x04\x05\xa0_\x00_\r,\xa5";
pub const IDENT_SIZE: usize = 150;
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
    "clear_error",
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
    "index",
    "initial_index",
    "initial_remaining_depth",
    "join_parent_json_value_with_next",
    "non_terminating",
    "object_writer",
    "push_comma",
    "remaining_depth",
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
        Punct::new('>', Spacing::Alone),
        Punct::new(',', Spacing::Alone),
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
