use super::IdentCacheEntry;
use proc_macro::{Punct, Spacing};
pub static
BLIT_SRC: &[u8] = b"x\x03\x07\n\t\x01\x8d\x01\x02\x9f`u\t\x05\xba\n\t\"\n\t\x13\x03\x07\x0e\x01\x84\x94\\\x04\x00\xab\x9fz\xb3\t\n\t\xb6\n\t\xa7\n\t\x9c\x03\xc0\x01\x02\xbc\t\x05\n\t\xbc\n\tn\x03\x07\x0e\x01\x84\n\t\\\xab\x9fv\xb3\t\n\t\xb6\n\t\xa7\n\t\x9c\x03\xc0\x01\x02\xbc\t\x05\xba\x0e\x01\n\t\xb6\n\tF\n\t\x1a\x03\xc0\x02\x05\x07J\n\t\xbf\n\t\xb5\n\t\x96\x01\xabx\n\to\x8d\to\x02\x9fb\x05\xae\x02\x98\t\x05\xba\n\t\xbf\n\t\x12\n\t\xa6\x8d\n\t\xa6\x02\x94\x82\x04\n\t\xa6\x01\n\t\x82\x00\x9f\xad\x05\xae\x02\xbd\t\x05\xba\xbf\n\t[\x0e\x01\x84\n\t\x82\n\to\x01\n\tb\n\t`\n\t\x16\x03\x07\n\t\xb1\n\t.\n\t\x03\x05\xba\n\t\xbf\n\t\xbc\n\tn\x03\x07\x02\xaa\x01\n\ta\n\t\xb1\n\t2\n\t\x03\x07\x01\xc0\n\t\xb1\n\t\x15;\t\x02>\t\n\t\xb6\n\t\xa0\n\t\x91\x08\x02\x89\t\n\t\xbf\n\t\xb1\n\t1\n\t\x03\x0c\xa7\t\n\t\xb6\n\t\xa7\n\t\x9c\x03\xc0\x01\x0c\xbe\xb4\t\xa7\r\xaf\xc0\r\xa8\n\t\xbf\n\t\xb1\n\t\x1d\n\t\xbf\n\t\xb1\n\t\x193\t\x05\x02-\t\x05\x02)\t\x05\xb2\n\t\xbf\n\t\x81\x03\x07\x01\x01\n\tv\rt\xbd\r\xa3\n\t\x8b\xb2\n\t\xbf\n\t\xa6\x01\n\t\xad\x05\xae\r\xbd\rI\xc0\x00\xbd\r\xa1\xc0\xbd\rw\xc0\xbd\r\xa1\xc0\x00\xa9\x08f\x02\xb4\r9\xc0\xbe\xaa\t\n\t\xbf\n\t\xb5\n\tT\x04f\r\xad\x00\xbd\ri\xc0\x00\xb4\r\xad\x00\xbd\r\xa1\xc0\x00\xbd\ry\xc0\x00\xbe\xaa\t\n\t\xbf\n\t\xb5\n\tS\x04\xbd\re\xc0\x00\xbe_\x04\x0c\x0c\x0e\x01\x00_\xc0\x14\n\t(\xc0\xbd\r\x9a\xc0\x00\xbd\r\xa5\xc0\xa4\xab\x9fp\x03\x07\x01\xc0\x0b\x01\n\t\xbf\n\t\xb1\n\t\x97\x03\x07\n\t\xbf\n\t\xb1\n\t\x97\n\t\x03\x07\x8e\t\x05\xa4\x02\x92\t\n\t\xb6\n\t\x8f\n\t\x83\xbe\xba]\x04\x03\xb2\n\t\xbf\n\t\xb5\n\tV\x01\n\tz\xb3\rt\x02\xbc\x02\xbe^\x04p\n\t\x03\x07\r\x89\xb3\x02\xbc\x02\x05\xba]\x00\xa9\xbe\xbb\x04^\xb9\xbb\r\xaf\n\t\x03\x01\xc0\r\xa8\xbf\n\t\xb1\n\tU*\t\xb3\x02\x99\tp\n\t\x03\x07\x01\xc0\x02#\t\x02D\t\xbf\n\t\xb1\n\tU\x03\x07\xb0\xae\x06\x01\xbd\r\xa5\xc0\x00\x84\n\t\x82\xc1\x94\x86\x00\xbe\x99\x04\n\t\xbf\n\t\xb1\n\t\x97\x8e\t\xa4\x02\x92\t\n\t\xb6\n\t\x8f\n\t\x83\x02\x00\xbe\xba\xa2\x04\n\t\xb6\n\t\xa0\n\tl\n\t\x03\x86\x01\n\t\x80\xc0\x00\xbe\xba\x7f\x04\n\t\xb6\n\t\xa0\n\tl\n\t\x03\x01\n\t\x80\xc0\x00\xbe\xba\x8c\x04\x03\n\t\xb6\n\t\xa7\n\t\x9c\n\t{\x7f\rr\xc0\r\xaf\xc0\xbe\xba\x8c\x04\xbf\n\t\xb1\n\t\x1bG\t\x02Q\t\x8c\x04\x99\r\x89\xa2\rr\xc0\r\xaf\xc0\x02\xbc\x02\x85\x05\xba\x8c\xbe\x93\x04\xa2\rs\xc0\x00\xb3\r\xaf\n\t\x03\t\x93\r\t\x7f\rs\xc0\x02\x88\x07\x9b\x00\xbe\xba\xa2\x04\n\t\xb6\n\t\xa0\n\tl\n\t\x03\x86\x01\n\t\x80\xc0\x00\xa9\xbe\xbb\x02\xbc\x02m\x02\xbec\x04\xab\x05\x0f\xac\xb2\x0f\xa4K\x00\xbe|\x04\xb0\x03\xb2\n\t\xbf\n\tO\n\t\x17\x01\n\t5\x05\xba\xbc\r!\r\'\x02c\x06\x01\xb4\x02\xbb\x06\x01\xb9\xbb\xa9\x08\x9e\x05\n\t\xbf\n\t\xbc\n\tW\x04\xbc\rj\xc0\xb0\xbc\rN\xc0\xbe\xb4\x04\xb0\xac\xaa\x06\x01\xa9\x9e\t|\xbe\xac\x04\xac\rP\xc0\x00\xbc\rC\x00\xb9\xbb\x05\n\t\xbf\n\t\xbc\n\t\x1c\xbe~\x04\xbc\r~\xc0\x00\x07\x9b\t\xbc\rE\x05~\x05\n\t\xbf\n\t\xb5\n\t\x96\x90\t\xbe\x9e\t$\x00\xbe\xac\x04\xb0\xbc\r\xb4\x02d\x9e\x04d\x00\xb4\xbe\xb7\xc0\x8a\x05\xbf\n\t\xb5\n\t\x96\xbc\r+\xc0\xbe\xb8\x04\xb0\xbc\r=\xc0\xb9\xb7\x02\xb7\x00\xb6\n\t\xa7\n\t,\n\t\x03\xb3\r\xaf\xc0\r \xc0\x00\xb9\xb8\x00\xb0\xbc\r?\xc0\n\t\xbf\n\t\xbc\n\tY\n\t\x18\n\t\xbf\n\t\xbc\n\tY\n\tZ\x06\x01\xb0\xab\xbc\rB\xc0\x00\xb9\xb7\n\t\x81\x01\n\tv\x98\r}\xb0u\r%\xc0&\n\tA\xa4\xaa\t\xc0\x04\r@\rw\xc0\x00\n\t\xbf\n\t\xb5\n\t\x1e\n\t<\n\t\xbf\n\t\xb5\n\tX\n\tg\n\t\xbf\n\t\xb5\n\t\x11\n\tg\ry\xc0\x00\xbe\xaa\t\n\t\xbf\n\t\xb5\n\t\x10\x04\r:\xc0\x00k\n\t\xbf\n\t\xa6\x00\xbe\xbah\t\x05\xba\n\t\xbf\n\t\xb5\n\tX\x04\x04h\r7\xc0\x00\rH\xc0\x00Z\n\t4k\n\t\xbf\n\t\xa6\x00\xbe\xbaq\x04\xbf\n\t[\n\tR\x04\x05\xbaq\x00q\r8\xc0";
pub const IDENT_SIZE: usize = 176;
static NAMES: &[&str] = &[
    "AlwaysArray",
    "ArrayWriter",
    "BytesWriter",
    "Decoder",
    "Default",
    "Field",
    "FromBinary",
    "FromText",
    "Object",
    "ObjectSchemaInner",
    "Result",
    "SkipFieldVisitor",
    "UNKNOWN_VARIANT",
    "UnsafeReturn",
    "ValueWriter",
    "allow",
    "as_mut",
    "at",
    "binary",
    "bitset",
    "bool",
    "byte",
    "clippy",
    "ctx",
    "default",
    "defaults",
    "destination",
    "discard_remaining_object_fields",
    "drop_in_place",
    "drops",
    "emplace_json_for_with_attribute",
    "enter_object",
    "enter_seen_object",
    "erased_drop_in_place",
    "erased_emplace_from_json",
    "fields",
    "from",
    "from_text",
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
    "question_mark",
    "read_seen_string",
    "report_error",
    "required",
    "restore_for_retry",
    "result",
    "skipped_field",
    "smart_object_comma",
    "start_json_array",
    "static",
    "str",
    "tag_query_at_content_next_object",
    "tag_query_next_object",
    "take_string",
    "text",
    "to_string",
    "visitor",
    "with_capacity",
    "AlwaysObject",
    "AlwaysString",
    "DynamicFieldDecoder",
    "FromJsonFieldVisitor",
    "MISSING_CONTENT_TAG",
    "ObjectWriter",
    "Peek",
    "String",
    "TextWriter",
    "Vistor",
    "__flatten_visitor_jsony",
    "__result",
    "__scope_jsony",
    "decode_binary",
    "decode_json",
    "encode_binary",
    "erased",
    "is_at_content",
    "join_object_with_next_value",
    "key",
    "non_terminating",
    "object_writer",
    "push_colon",
    "skip_value",
    "use",
    "MaybeUninit",
    "None",
    "Parser",
    "ToBinary",
    "__schema_inner",
    "_builder",
    "as_mut_ptr",
    "assume_init",
    "byte_add",
    "decoder",
    "emplace_from_json",
    "end_json_array",
    "impl",
    "join_parent_json_value_with_next",
    "new_field_visitor",
    "new_unchecked",
    "other_tag",
    "push",
    "snapshot",
    "temp_flatten",
    "uninit",
    "FromJson",
    "Kind",
    "PhantomData",
    "Self",
    "Some",
    "__TEMP",
    "automatically_derived",
    "break",
    "decode",
    "else",
    "encode_json",
    "flatten_visitor",
    "for",
    "inner",
    "marker",
    "message",
    "offset_of",
    "phantom",
    "temp2",
    "type",
    "where",
    "DecodeError",
    "ObjectSchema",
    "encoder",
    "schema",
    "start_json_object",
    "success",
    "NonNull",
    "_err",
    "at_content",
    "fn",
    "mem",
    "push_comma",
    "temp",
    "push_str",
    "const",
    "end_json_object",
    "ToJson",
    "ptr",
    "write",
    "if",
    "_",
    "unsafe",
    "variant",
    "encode_json__jsony",
    "self",
    "cast",
    "match",
    "__internal",
    "as",
    "dst",
    "value",
    "json",
    "std",
    "Ok",
    "err",
    "return",
    "mut",
    "Err",
    "parser",
    "out",
    "let",
    "jsony",
];
pub fn ident_cache_initial_state() -> Box<[IdentCacheEntry; IDENT_SIZE]> {
    unsafe {
        let cache =
            std::alloc::alloc(std::alloc::Layout::array::<IdentCacheEntry>(IDENT_SIZE).unwrap())
                as *mut IdentCacheEntry;
        for (i, &name) in NAMES.iter().enumerate() {
            std::ptr::write(cache.add(i), IdentCacheEntry::Empty(name));
        }
        Box::from_raw(cache as *mut [IdentCacheEntry; IDENT_SIZE])
    }
}
pub const PUNCT_SIZE: usize = 16;
pub fn punct_cache_initial_state() -> [Punct; PUNCT_SIZE] {
    [
        Punct::new(';', Spacing::Alone),
        Punct::new('>', Spacing::Alone),
        Punct::new(',', Spacing::Alone),
        Punct::new('<', Spacing::Alone),
        Punct::new('=', Spacing::Alone),
        Punct::new('&', Spacing::Alone),
        Punct::new('=', Spacing::Joint),
        Punct::new('\'', Spacing::Joint),
        Punct::new('!', Spacing::Alone),
        Punct::new(':', Spacing::Alone),
        Punct::new(':', Spacing::Joint),
        Punct::new('#', Spacing::Joint),
        Punct::new('|', Spacing::Alone),
        Punct::new('.', Spacing::Alone),
        Punct::new('-', Spacing::Joint),
        Punct::new('*', Spacing::Alone),
    ]
}
