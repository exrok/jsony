use super::IdentCacheEntry;
use proc_macro::{Punct, Spacing};
pub static
BLIT_SRC: &[u8] = b"v\x03\x07\n\t\x02\x88\x02\x01\x9b\\t\t\x05\xb6\n\t \n\t\x12\x03\x07\x0c\x02\x81\x90X\x04\x00\xa9\x9bx\xaf\t\n\t\xb2\n\t\xa3\n\t\x98\x03\xbc\x02\x01\xb7\t\x05\n\t\xb7\n\tm\x03\x07\x0c\x02\x81\n\tX\xa9\x9b_\xaf\t\n\t\xb2\n\t\xa3\n\t\x98\x03\xbc\x02\x01\xb7\t\x05\xb6\x0c\x02\n\t\xb2\n\tB\n\t\x19\x03\xbc\x01\x05\x07F\n\t\xbb\n\t\xb1\n\t\x92\x02\xa9v\n\tn\x88\tn\x01\x9b]\x05\xa8\x01\x94\t\x05\xb6\n\t\xbb\n\t\x11\n\t\xa2\x88\n\t\xa2\x01\x90\x7f\x04\n\t\xa2\x02\n\t\x7f\x00\x9b\xa7\x05\xa8\x01\xb9\t\x05\xb6\xbb\n\tW\x0c\x02\x81\n\t\x7f\n\tn\x02\n\t]\n\t\\\n\t\x15\x03\x07\n\t\xae\n\t+\n\t\x03\x05\xb6\n\t\xbb\n\t\xb7\n\tm\x03\x07\x01\xa6\x02\n\tc\n\t\xae\n\t/\n\t\x03\x07\x02\xbc\n\t\xae\n\t\x148\t\x01;\t\n\t\xb2\n\t\x9c\n\t\x8d\x08\x01\x85\t\n\t\xbb\n\t\xae\n\t.\n\t\x03\x0b\xa3\t\n\t\xb2\n\t\xa3\n\t\x98\x03\xbc\x02\x0b\xba\xb0\t\xa3\r\xac\xbc\r\xa4\n\t\xbb\n\t\xae\n\t\x1c\n\t\xbb\n\t\xae\n\t\x180\t\x05\x01*\t\x05\x01&\t\x05\xab\n\t\xbb\n\tj\x03\x07\x02\x02\n\t_\rs\xb9\r\x9f\n\t\x8a\xab\n\t\xbb\n\t\xa2\x02\n\t\xa7\x05\xa8\r\xb9\rE\xbc\x00\xb9\r\x9d\xbc\xb9\ru\xbc\xb9\r\x9d\xbc\x00\xa5\x08d\x01\xb0\r6\xbc\xba\xa6\t\n\t\xbb\n\t\xb1\n\tP\x04d\r\xa7\x00\xb9\rg\xbc\x00\xb0\r\xa7\x00\xb9\r\x9d\xbc\x00\xb9\rw\xbc\x00\xba\xa6\t\n\t\xbb\n\t\xb1\n\tO\x04\xb9\rb\xbc\x00\xba[\x04\x0b\x0b\x0c\x02\x00[\xbc\x13\n\t%\xbc\xb9\r\x96\xbc\x00\xb9\r\xa1\xbc\xa0\xa6\t\xbc\x04\xa0\xa9\x9bo\x03\x07\x02\xbc\x0c\x02\n\t\xbb\n\t\xae\n\t\x93\x03\x07\n\t\xbb\n\t\xae\n\t\x93\n\t\x03\x07\x89\t\x05\xa0\x01\x8e\t\n\t\xb2\n\t\x8b\n\t\x80\xba\xb6Y\x04\x03\xab\n\t\xbb\n\t\xb1\n\tR\x02\n\tx\xaf\rs\x01\xb7\x01\xbaZ\x04o\n\t\x03\x07\r\x85\xaf\x01\xb7\x01\x05\xb6Y\x00\xa5\xba\xb8\x04Z\xb5\xb8\r\xac\n\t\x03\x02\xbc\r\xa4\xbb\n\t\xae\n\tQ\'\t\xaf\x01\x95\to\n\t\x03\x07\x02\xbc\x01!\t\x01@\t\xbb\n\t\xae\n\tQ\x03\x07\xad\xa8\x06\x02\xb9\r\xa1\xbc\x00\x81\n\t\x7f\xbd\x90\x83\x00\xba\x95\x04\n\t\xbb\n\t\xae\n\t\x93\x89\t\xa0\x01\x8e\t\n\t\xb2\n\t\x8b\n\t\x80\x01\x00\xba\xb6\x9e\x04\n\t\xb2\n\t\x9c\n\tk\n\t\x03\x83\x02\n\t~\xbc\x00\xba\xb6}\x04\n\t\xb2\n\t\x9c\n\tk\n\t\x03\x02\n\t~\xbc\x00\xba\xb6\x87\x04\x03\n\t\xb2\n\t\xa3\n\t\x98\n\ty}\rq\xbc\r\xac\xbc\xba\xb6\x87\x04\xbb\n\t\xae\n\t\x1aC\t\x01M\t\x87\x04\x95\r\x85\x9e\rq\xbc\r\xac\xbc\x01\xb7\x01\x82\x05\xb6\x87\xba\x8f\x04\x9e\rr\xbc\x00\xaf\r\xac\n\t\x03\t\x8f\r\t}\rr\xbc\x01\x84\x07\x97\x00\xba\xb6\x9e\x04\n\t\xb2\n\t\x9c\n\tk\n\t\x03\x83\x02\n\t~\xbc\x00\xa5\xba\xb8\x01\xb7\x01l\x01\xba`\x04\xa9\x05\x0e\xaa\xab\x0e\xa0G\x00\xbaz\x04\xad\x03\xab\n\t\xbb\n\tK\n\t\x16\x02\n\t2\x05\xb6\xb7\r\x1f\r$\x01`\x06\x02\xb0\x01\xb8\x06\x02\xb5\xb8\xa5\x08\x9a\x05\n\t\xbb\n\t\xb7\n\tS\x04\xb7\rh\xbc\xad\xb7\rJ\xbc\xba\xb0\x04\xad\xaa\xa6\x06\x02\xa5\x9a\tz\xba\xaa\x04\xaa\rL\xbc\x00\xb7\r?\x00\xb5\xb8\x05\n\t\xbb\n\t\xb7\n\t\x1b\xba|\x04\xb7\r|\xbc\x00\x07\x97\t\xb7\rA\x05|\x05\n\t\xbb\n\t\xb1\n\t\x92\x8c\t\xba\x9a\t\"\x00\xba\xaa\x04\xad\xb7\r\xb0\x01a\x9a\x04a\x00\xb0\xba\xb3\xbc\x86\x05\xbb\n\t\xb1\n\t\x92\xb7\r(\xbc\xba\xb4\x04\xad\xb7\r:\xbc\xb5\xb3\x01\xb3\x00\xb2\n\t\xa3\n\t)\n\t\x03\xaf\r\xac\xbc\r\x1e\xbc\x00\xb5\xb4\x00\xad\xb7\r<\xbc\n\t\xbb\n\t\xb7\n\tU\n\t\x17\n\t\xbb\n\t\xb7\n\tU\n\tV\x06\x02\xad\xa9\xb7\r>\xbc\x00\xb5\xb3\x94\r{\xadt\r#\xbc\r=\ru\xbc\x00\n\t\xbb\n\t\xb1\n\t\x1d\n\t9\n\t\xbb\n\t\xb1\n\tT\n\te\n\t\xbb\n\t\xb1\n\t\x10\n\te\rw\xbc\x00\xba\xa6\t\n\t\xbb\n\t\xb1\n\t\x0f\x04\r7\xbc\x00i\n\t\xbb\n\t\xa2\x00\xba\xb6f\t\x05\xb6\n\t\xbb\n\t\xb1\n\tT\x04^\x04f\r4\xbc\x00^\rD\xbc\x00V\n\t1i\n\t\xbb\n\t\xa2\x00\xba\xb6p\x04\xbb\n\tW\n\tN\x04\x05\xb6p\x00p\r5\xbc";
pub const IDENT_SIZE: usize = 173;
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
    "as_mut",
    "at",
    "binary",
    "bitset",
    "bool",
    "byte",
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
    "binary_decode",
    "binary_encode",
    "builder",
    "emplace_from_json",
    "erased",
    "is_at_content",
    "join_object_with_next_value",
    "json_decode",
    "key",
    "non_terminating",
    "object_writer",
    "push_colon",
    "skip_value",
    "use",
    "FromJson",
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
    "Kind",
    "PhantomData",
    "Self",
    "Some",
    "__TEMP",
    "break",
    "decode",
    "else",
    "flatten_visitor",
    "for",
    "inner",
    "json_encode",
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
    "json_encode__jsony",
    "self",
    "unsafe",
    "variant",
    "as",
    "cast",
    "match",
    "__internal",
    "dst",
    "value",
    "json",
    "std",
    "Ok",
    "err",
    "return",
    "mut",
    "parser",
    "Err",
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
pub const PUNCT_SIZE: usize = 15;
pub fn punct_cache_initial_state() -> [Punct; PUNCT_SIZE] {
    [
        Punct::new(';', Spacing::Alone),
        Punct::new(',', Spacing::Alone),
        Punct::new('>', Spacing::Alone),
        Punct::new('<', Spacing::Alone),
        Punct::new('=', Spacing::Alone),
        Punct::new('&', Spacing::Alone),
        Punct::new('=', Spacing::Joint),
        Punct::new('\'', Spacing::Joint),
        Punct::new('!', Spacing::Alone),
        Punct::new(':', Spacing::Alone),
        Punct::new(':', Spacing::Joint),
        Punct::new('|', Spacing::Alone),
        Punct::new('-', Spacing::Joint),
        Punct::new('.', Spacing::Alone),
        Punct::new('*', Spacing::Alone),
    ]
}
