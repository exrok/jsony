use proc_macro::{Punct, Spacing};
pub static BLIT_SRC: &[u8] =
b"\xa2\x03\x07\n\t\x01\xa1\x01\x02\xb2e\x81\t\x04\xc6\n\t#\n\t\x13\x03\x07\x0b\x01\x9e\xa5`\x05\x00\xb8\xb2\x85\xc2\t\n\t\xc4\n\t\xb5\n\t\xa6\x03\xce\x01\x02\xc9\t\x04\n\t\xc9\n\ty\x03\x07\x0b\x01\x9e\n\t`\xb8\xb2\x82\xc2\t\n\t\xc4\n\t\xb5\n\t\xa6\x03\xce\x01\x02\xc9\t\x04\xc6\x0b\x01\n\t\xc4\n\tI\n\t]\x03\xce\x02\x04\x07u\n\t\xcd\n\t\xc0\n\tw\x01\xb8\xa2\n\tz\xa1\tz\x02\xb2j\x04\xbd\x02\xa0\t\x04\xc6\n\t\xcd\n\t\x12\n\t\xb3\xa1\n\t\xb3\x02\xa5\x8e\x05\n\t\xb3\x01\n\t\x8e\x00\xb2\xba\x04\xbd\x02\xca\t\x04\xc6\xcd\n\t_\x0b\x01\x9e\n\t\x8e\n\tz\x01\n\tj\n\te\n\t\x16\x03\x07\n\t\xbe\n\t/\n\t\x03\x04\xc6\n\t\xcd\n\t\xc9\n\ty\x03\x07\x02\xb9\x01\n\tf\n\t\xbe\n\t3\n\t\x03\x07\x01\xce\n\t\xbe\n\t\x15>\t\x02@\t\n\t\xc4\n\t\xaa\n\t\x99\x0c\x02\x93\t\n\t\xcd\n\t\xbe\n\t2\n\t\x03\n\t\xcd\n\t\xbe\n\t+\n\t\x03\x0e\xb5\t\n\t\xc4\n\t\xb5\n\t\xa6\x03\xce\x01\x0e\xcc\xc3\t\xb5\r\xbf\xce\r\xb6\n\t\xcd\n\t\xbe\n\t\x1f\n\t\xcd\n\t\xbe\n\t\x1c4\t\x04\x02.\t\x04\x02,\t\x04\xbc\n\t\xcd\n\t\x8d\x03\x07\x01\x01\n\t\x82\r\x80\xca\r\xaf\n\t\x95\xbc\n\t\xcd\n\t\xb3\x01\n\t\xba\x04\xbd\r\xca\rL\xce\x00\xca\r\xab\xce\xca\r\x83\xce\xca\r\xab\xce\x00\xb7\x0cn\x02\xc3\r;\xce\xcc\xb9\t\n\t\xcd\n\t\xc0\n\tV\x05n\r\xba\x00\xca\rt\xce\x00\xc3\r\xba\x00\xca\r\xab\xce\x00\xca\r\x84\xce\x00\xcc\xb9\t\n\t\xcd\n\t\xc0\n\tU\x05\xca\rm\xce\x00\xccc\x05\x0e\x0e\x0b\x01W\n\tg\xce\x00c\xce\xca\r\xa4\xce\x00\xca\r\xb1\xce\xb0\xb8\xb2|\x03\x07\x01\xce\x08\x01\n\t\xcd\n\t\xbe\n\t\x9d\x03\x07\n\t\xcd\n\t\xbe\n\t\x9d\n\t\x03\x07\x97\t\x04\xb0\x02\x9a\t\n\t\xc4\n\t\x98\n\t\x90\xcc\xc6a\x05\x03\xbc\n\t\xcd\n\t\xc0\n\tY\x01\n\t\x85\xc2\r\x80\x02\xc9\x02\xccb\x05|\n\t\x03\x07\xc2\x02\xc9\x02\x04\xc6a\x00\xb7\xcc\xcb\x05b\xc8\xcb\r\xbf\n\t\x03\x01\xce\r\xb6\xcd\n\t\xbe\n\tX-\t\xc2\x02\xa3\t|\n\t\x03\x07\x01\xce\x02$\t\x02G\t\xcd\n\t\xbe\n\tX\x03\x07\xc1\xbd\x06\x01\xca\r\xb1\xce\x00\x9e\n\t\x8e\xcf\xa5\x91\x00\xcc\xa3\x05\n\t\xcd\n\t\xbe\n\t\x9d\x97\t\xb0\x02\x9a\t\n\t\xc4\n\t\x98\n\t\x90\x02\x00\xcc\xc6\xad\x05\n\t\xc4\n\t\xaa\n\tx\n\t\x03\x91\x01\n\t\x8c\xce\x00\xcc\xc6\x8b\x05\n\t\xc4\n\t\xaa\n\tx\n\t\x03\x01\n\t\x8c\xce\x00\xcc\xc6\x96\x05\x03\n\t\xc4\n\t\xb5\n\t\xa6\n\t\x86\x8b\r~\xce\r\xbf\xce\xcc\xc6\x96\x05\xcd\n\t\xbe\n\t\x1dJ\t\x02S\t\x96\x05\xa3\r\x93\xad\r~\xce\r\xbf\xce\x02\xc9\x02\x9f\x04\xc6\x96\xcc\x9c\x05\xad\r\x7f\xce\x00\xc2\r\xbf\n\t\x03\t\x9c\r\t\x8b\r\x7f\xce\x02\x92\x07\xac\x00\xcc\xc6\xad\x05\n\t\xc4\n\t\xaa\n\tx\n\t\x03\x91\x01\n\t\x8c\xce\x00\xb7\xcc\xcb\x02\xc9\x02\x8f\x02\xcck\x05\xb8\x04\x0f\xbb\xbc\x0f\xb0\x9b\x00\xcc\x87\x05\xc1\x03\xbc\n\t\xcd\n\tP\n\t\x18\x01\n\t7\x04\xc6\xc9\r\"\r)\x02k\x06\x01\xc3\x02\xcb\x06\x01\xc8\xcb\xb7\x0c\xa8\x04\n\t\xcd\n\t\xb4\n\tZ\x05\xc9\r\x89\xce\xc1\xc9\rO\xce\xcc\xc3\x05\xc1\xbb\xb9\x06\x01\xb7\xa8\t\x87\xc1\xc9\rr\xce\x04\xcd\n\t\xb4\n\t\x1a\x02\xc5\xbb\x05p\x00(\x00\xcc\xbb\x05\xbb\rR\xce\x00\xc9\rF\x00\xc8\xcb\x04\n\t\xcd\n\t\xb4\n\t\x1e\xcc\x8a\x05\xc9\r\x8a\xce\x00\x07\xac\t\xc9\rH\x04\x8a\x04\n\t\xcd\n\t\xc0\n\two\t\xcc\xa8\t%\x00\xcc\xbb\x05\xc1\xc9\r\xc3\x02l\xa8\x05l\x00\xc3\xcc\xc5\xce\x94\x04\n\t\xcd\n\t\xb4\n\t\x14\x07\xac\t=\xc9\rh\xce\xc1\xc9\rh\xce\x06\x01\xc8\xc5\x02\xc7\x06\x01\xc4\n\t\xb5\n\ti\n\t\x03\xc2\r\xbf\xce\rd\xce\x00\xc8\xc7\xcc\xc7\x05\xc1\xc9\rr\xce\x04\xcd\n\t\xb4\n\t\x19\x00\xc4\n\t\xb5\n\ti\n\t\x03\x00\xc8\xc7\x00\xc1\xc9\rA\xce\n\t\xcd\n\t\xc9\n\t\\\n\t\x1b\n\t\xcd\n\t\xc9\n\t\\\n\t^\x06\x01\xc1\xb8\xc9\rE\xce\x00\xc8\xc5\x04\xcd\n\t\xc0\n\tw\n\t\x8d\x01\n\t\x82\xae\xd0B\xb2Q\x0b\x01\x04\x07u\x9b\n\t\xc4\n\t\x9b\n\t\x17\xa1\xa5\xcb\x05\n\t\xcd\n\t\xb4\n\t{\x00\xb26\xc3\t\x04\x9b\x0b\x01]\x03\x9e\x02\n\t\xcd\n\t\xb4\n\t{\x01\xc1\xc3\xb9\x06\x01\xc8\xcb\xa0\r\x88\xc1\x81\r&\xce\'\n\tD\xb0\xb9\t\xce\x05\rC\r\x83\xce\x00\n\t\xcd\n\t\xc0\n\t \n\t?\n\t\xcd\n\t\xc0\n\t[\n\tq\n\t\xcd\n\t\xc0\n\t\x11\n\tq\r\x84\xce\x00\xcc\xb9\t\n\t\xcd\n\t\xc0\n\t\x10\x05\r<\xce\x00v\n\t\xcd\n\t\xb3\x00\xcc\xc6s\t\x04\xc6\n\t\xcd\n\t\xc0\n\t[\x05\x05s\r9\xce\x00\rK\xce\x00^\n\t5v\n\t\xcd\n\t\xb3\x00\xcc\xc6}\x05\xcd\n\t_\n\tT\x05\x04\xc6}\x00}\r:\xce";
pub const IDENT_SIZE: usize = 190;
pub static NAMES: [&str; 190] = [
    "AlwaysArray",
    "ArrayWriter",
    "BytesWriter",
    "Decoder",
    "EMPTY_OBJECT_FOR_EXTERNALLY_TAGGED_ENUM",
    "Field",
    "FromBinary",
    "FromStr",
    "FromText",
    "MULTIPLE_FIELDS_FOR_EXTERNALLY_TAGGED_ENUM",
    "NO_FIELD_MATCHED_AN_ENUM_VARIANT",
    "Object",
    "ObjectSchemaInner",
    "SkipFieldVisitor",
    "UNKNOWN_VARIANT",
    "UnsafeReturn",
    "ValueWriter",
    "allow",
    "at",
    "binary",
    "bitset",
    "bool",
    "byte",
    "clippy",
    "continue",
    "ctx",
    "decode_with_alias",
    "default_default",
    "defaults",
    "destination",
    "drops",
    "emplace_json_for_with_attribute",
    "enter_object",
    "enter_seen_object",
    "erased_drop_in_place",
    "erased_emplace_from_json",
    "fields",
    "from",
    "from_str",
    "from_text",
    "in",
    "inner_writer",
    "into_string",
    "iter",
    "join_array_with_next_value",
    "loop",
    "name",
    "new",
    "offset",
    "peek",
    "pub",
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
    "tag_query_at_content_next_object",
    "tag_query_next_object",
    "take_string",
    "text",
    "to_str",
    "to_string",
    "visitor",
    "with_capacity",
    "AlwaysObject",
    "AlwaysString",
    "Default",
    "DynamicFieldDecoder",
    "FromJsonFieldVisitor",
    "MISSING_CONTENT_TAG",
    "ObjectWriter",
    "Peek",
    "Result",
    "String",
    "TextWriter",
    "Visitor",
    "__flatten_visitor_jsony",
    "__result",
    "__scope_jsony",
    "as_mut",
    "decode_binary",
    "decode_json",
    "default",
    "discard_remaining_object_fields",
    "drop_in_place",
    "encode_binary",
    "erased",
    "is_at_content",
    "join_object_with_next_value",
    "key",
    "message",
    "next_field",
    "non_terminating",
    "object_step",
    "object_writer",
    "push_colon",
    "static",
    "use",
    "DecodeError",
    "MaybeUninit",
    "Parser",
    "ToBinary",
    "UnknownVariant",
    "__schema_inner",
    "_builder",
    "as_mut_ptr",
    "assume_init",
    "byte_add",
    "decoder",
    "emplace_from_json",
    "end_json_array",
    "join_parent_json_value_with_next",
    "new_field_visitor",
    "new_unchecked",
    "other_tag",
    "push",
    "skip_value",
    "snapshot",
    "temp_flatten",
    "uninit",
    "FromJson",
    "Kind",
    "None",
    "PhantomData",
    "__TEMP",
    "break",
    "decode",
    "else",
    "encode_json",
    "flatten_visitor",
    "inner",
    "marker",
    "offset_of",
    "phantom",
    "str",
    "temp2",
    "ObjectSchema",
    "Self",
    "Some",
    "encoder",
    "for",
    "impl",
    "schema",
    "start_json_object",
    "type",
    "NonNull",
    "_err",
    "at_content",
    "automatically_derived",
    "mem",
    "push_comma",
    "success",
    "temp",
    "where",
    "push_str",
    "const",
    "end_json_object",
    "fn",
    "ToJson",
    "error",
    "ptr",
    "write",
    "if",
    "unsafe",
    "_",
    "encode_json__jsony",
    "variant",
    "as",
    "self",
    "__internal",
    "cast",
    "json",
    "match",
    "dst",
    "value",
    "std",
    "Ok",
    "mut",
    "err",
    "return",
    "parser",
    "out",
    "Err",
    "let",
    "jsony",
];
pub const PUNCT_SIZE: usize = 16;
pub fn punct_cache_initial_state() -> [Punct; PUNCT_SIZE] {
    [
        Punct::new(';', Spacing::Alone),
        Punct::new('>', Spacing::Alone),
        Punct::new(',', Spacing::Alone),
        Punct::new('<', Spacing::Alone),
        Punct::new('&', Spacing::Alone),
        Punct::new('=', Spacing::Alone),
        Punct::new('=', Spacing::Joint),
        Punct::new('\'', Spacing::Joint),
        Punct::new('#', Spacing::Joint),
        Punct::new(':', Spacing::Alone),
        Punct::new(':', Spacing::Joint),
        Punct::new('-', Spacing::Joint),
        Punct::new('!', Spacing::Alone),
        Punct::new('.', Spacing::Alone),
        Punct::new('|', Spacing::Alone),
        Punct::new('*', Spacing::Alone),
    ]
}
