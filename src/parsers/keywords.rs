pub enum VerilogKeyword {
    AcceptOn,
    Alias,
    AlwaysComb,
    AlwaysFf,
    AlwaysLatch,
    Assert,
    Assume,
    Before,
    Bind,
    Bins,
    Binsof,
    Bit,
    Break,
    Byte,
    Chandle,
    Checker,
    Class,
    Clocking,
    Const,
    Constraint,
    Context,
    Continue,
    Cover,
    Covergroup,
    Coverpoint,
    Cross,
    Dist,
    Do,
    Endchecker,
    Endclass,
    Endclocking,
    Endgroup,
    Endinterface,
    Endpackage,
    Endprogram,
    Endproperty,
    Endsequence,
    Enum,
    Eventually,
    Expect,
    Export,
    Extends,
    Extern,
    Final,
    FirstMatch,
    Foreach,
    Forkjoin,
    Global,
    Iff,
    IgnoreBins,
    IllegalBins,
    Implies,
    Import,
    Inside,
    Int,
    Interface,
    Intersect,
    JoinAny,
    JoinNone,
    Let,
    Local,
    Logic,
    Longint,
    Matches,
    Modport,
    New,
    Nexttime,
    Null,
    Package,
    Packed,
    Priority,
    Program,
    Property,
    Protected,
    Pure,
    Rand,
    Randc,
    Randcase,
    Randsequence,
    Ref,
    RejectOn,
    Restrict,
    Return,
    SAlways,
    SEventually,
    SNexttime,
    SUntil,
    SUntilWith,
    Sequence,
    Shortint,
    Shortreal,
    Solve,
    Static,
    String,
    Strong,
    Struct,
    Super,
    SyncAcceptOn,
    SyncRejectOn,
    Tagged,
    This,
    Throughout,
    Timeprecision,
    Timeunit,
    Type,
    Typedef,
    Union,
    Unique,
    Unique0,
    Until,
    UntilWith,
    Untypted,
    Var,
    Virtual,
    Void,
    WaitOrder,
    Weak,
    Wildcard,
    With,
    Within,
}

pub fn keyword_from_string(input: &str) -> Option<VerilogKeyword> {
    match input {
        "accept_on" => Some(VerilogKeyword::AcceptOn),
        "alias" => Some(VerilogKeyword::Alias),
        "always_comb" => Some(VerilogKeyword::AlwaysComb),
        "always_ff" => Some(VerilogKeyword::AlwaysFf),
        "always_latch" => Some(VerilogKeyword::AlwaysLatch),
        "assert" => Some(VerilogKeyword::Assert),
        "assume" => Some(VerilogKeyword::Assume),
        "before" => Some(VerilogKeyword::Before),
        "bind" => Some(VerilogKeyword::Bind),
        "bins" => Some(VerilogKeyword::Bins),
        "binsof" => Some(VerilogKeyword::Binsof),
        "bit" => Some(VerilogKeyword::Bit),
        "break" => Some(VerilogKeyword::Break),
        "byte" => Some(VerilogKeyword::Byte),
        "chandle" => Some(VerilogKeyword::Chandle),
        "checker" => Some(VerilogKeyword::Checker),
        "class" => Some(VerilogKeyword::Class),
        "clocking" => Some(VerilogKeyword::Clocking),
        "const" => Some(VerilogKeyword::Const),
        "constraint" => Some(VerilogKeyword::Constraint),
        "context" => Some(VerilogKeyword::Context),
        "continue" => Some(VerilogKeyword::Continue),
        "cover" => Some(VerilogKeyword::Cover),
        "covergroup" => Some(VerilogKeyword::Covergroup),
        "coverpoint" => Some(VerilogKeyword::Coverpoint),
        "cross" => Some(VerilogKeyword::Cross),
        "dist" => Some(VerilogKeyword::Dist),
        "do" => Some(VerilogKeyword::Do),
        "endchecker" => Some(VerilogKeyword::Endchecker),
        "endclass" => Some(VerilogKeyword::Endclass),
        "endclocking" => Some(VerilogKeyword::Endclocking),
        "endgroup" => Some(VerilogKeyword::Endgroup),
        "endinterface" => Some(VerilogKeyword::Endinterface),
        "endpackage" => Some(VerilogKeyword::Endpackage),
        "endprogram" => Some(VerilogKeyword::Endprogram),
        "endproperty" => Some(VerilogKeyword::Endproperty),
        "endsequence" => Some(VerilogKeyword::Endsequence),
        "enum" => Some(VerilogKeyword::Enum),
        "eventually" => Some(VerilogKeyword::Eventually),
        "expect" => Some(VerilogKeyword::Expect),
        "export" => Some(VerilogKeyword::Export),
        "extends" => Some(VerilogKeyword::Extends),
        "extern" => Some(VerilogKeyword::Extern),
        "final" => Some(VerilogKeyword::Final),
        "first_match" => Some(VerilogKeyword::FirstMatch),
        "foreach" => Some(VerilogKeyword::Foreach),
        "forkjoin" => Some(VerilogKeyword::Forkjoin),
        "global" => Some(VerilogKeyword::Global),
        "iff" => Some(VerilogKeyword::Iff),
        "ignore_bins" => Some(VerilogKeyword::IgnoreBins),
        "illegal_bins" => Some(VerilogKeyword::IllegalBins),
        "implies" => Some(VerilogKeyword::Implies),
        "import" => Some(VerilogKeyword::Import),
        "inside" => Some(VerilogKeyword::Inside),
        "int" => Some(VerilogKeyword::Int),
        "interface" => Some(VerilogKeyword::Interface),
        "intersect" => Some(VerilogKeyword::Intersect),
        "join_any" => Some(VerilogKeyword::JoinAny),
        "join_none" => Some(VerilogKeyword::JoinNone),
        "let" => Some(VerilogKeyword::Let),
        "local" => Some(VerilogKeyword::Local),
        "logic" => Some(VerilogKeyword::Logic),
        "longint" => Some(VerilogKeyword::Longint),
        "matches" => Some(VerilogKeyword::Matches),
        "modport" => Some(VerilogKeyword::Modport),
        "new" => Some(VerilogKeyword::New),
        "nexttime" => Some(VerilogKeyword::Nexttime),
        "null" => Some(VerilogKeyword::Null),
        "package" => Some(VerilogKeyword::Package),
        "packed" => Some(VerilogKeyword::Packed),
        "priority" => Some(VerilogKeyword::Priority),
        "program" => Some(VerilogKeyword::Program),
        "property" => Some(VerilogKeyword::Property),
        "protected" => Some(VerilogKeyword::Protected),
        "pure" => Some(VerilogKeyword::Pure),
        "rand" => Some(VerilogKeyword::Rand),
        "randc" => Some(VerilogKeyword::Randc),
        "randcase" => Some(VerilogKeyword::Randcase),
        "randsequence" => Some(VerilogKeyword::Randsequence),
        "ref" => Some(VerilogKeyword::Ref),
        "reject_on" => Some(VerilogKeyword::RejectOn),
        "restrict" => Some(VerilogKeyword::Restrict),
        "return" => Some(VerilogKeyword::Return),
        "s_always" => Some(VerilogKeyword::SAlways),
        "s_eventually" => Some(VerilogKeyword::SEventually),
        "s_nexttime" => Some(VerilogKeyword::SNexttime),
        "s_until" => Some(VerilogKeyword::SUntil),
        "s_until_with" => Some(VerilogKeyword::SUntilWith),
        "sequence" => Some(VerilogKeyword::Sequence),
        "shortint" => Some(VerilogKeyword::Shortint),
        "shortreal" => Some(VerilogKeyword::Shortreal),
        "solve" => Some(VerilogKeyword::Solve),
        "static" => Some(VerilogKeyword::Static),
        "string" => Some(VerilogKeyword::String),
        "strong" => Some(VerilogKeyword::Strong),
        "struct" => Some(VerilogKeyword::Struct),
        "super" => Some(VerilogKeyword::Super),
        "sync_accept_on" => Some(VerilogKeyword::SyncAcceptOn),
        "sync_reject_on" => Some(VerilogKeyword::SyncRejectOn),
        "tagged" => Some(VerilogKeyword::Tagged),
        "this" => Some(VerilogKeyword::This),
        "throughout" => Some(VerilogKeyword::Throughout),
        "timeprecision" => Some(VerilogKeyword::Timeprecision),
        "timeunit" => Some(VerilogKeyword::Timeunit),
        "type" => Some(VerilogKeyword::Type),
        "typedef" => Some(VerilogKeyword::Typedef),
        "union" => Some(VerilogKeyword::Union),
        "unique" => Some(VerilogKeyword::Unique),
        "unique0" => Some(VerilogKeyword::Unique0),
        "until" => Some(VerilogKeyword::Until),
        "until_with" => Some(VerilogKeyword::UntilWith),
        "untypted" => Some(VerilogKeyword::Untypted),
        "var" => Some(VerilogKeyword::Var),
        "virtual" => Some(VerilogKeyword::Virtual),
        "void" => Some(VerilogKeyword::Void),
        "wait_order" => Some(VerilogKeyword::WaitOrder),
        "weak" => Some(VerilogKeyword::Weak),
        "wildcard" => Some(VerilogKeyword::Wildcard),
        "with" => Some(VerilogKeyword::With),
        "within" => Some(VerilogKeyword::Within),
        _ => None,
    }
}

pub const ALL_KEYWORDS: &[&str] = &[
    "accept_on",
    "alias",
    "always_comb",
    "always_ff",
    "always_latch",
    "assert",
    "assume",
    "before",
    "bind",
    "bins",
    "binsof",
    "bit",
    "break",
    "byte",
    "chandle",
    "checker",
    "class",
    "clocking",
    "const",
    "constraint",
    "context",
    "continue",
    "cover",
    "covergroup",
    "coverpoint",
    "cross",
    "dist",
    "do",
    "endchecker",
    "endclass",
    "endclocking",
    "endgroup",
    "endinterface",
    "endpackage",
    "endprogram",
    "endproperty",
    "endsequence",
    "enum",
    "eventually",
    "expect",
    "export",
    "extends",
    "extern",
    "final",
    "first_match",
    "foreach",
    "forkjoin",
    "global",
    "iff",
    "ignore_bins",
    "illegal_bins",
    "implies",
    "import",
    "inside",
    "int",
    "interface",
    "intersect",
    "join_any",
    "join_none",
    "let",
    "local",
    "logic",
    "longint",
    "matches",
    "modport",
    "new",
    "nexttime",
    "null",
    "package",
    "packed",
    "priority",
    "program",
    "property",
    "protected",
    "pure",
    "rand",
    "randc",
    "randcase",
    "randsequence",
    "ref",
    "reject_on",
    "restrict",
    "return",
    "s_always",
    "s_eventually",
    "s_nexttime",
    "s_until",
    "s_until_with",
    "sequence",
    "shortint",
    "shortreal",
    "solve",
    "static",
    "string",
    "strong",
    "struct",
    "super",
    "sync_accept_on",
    "sync_reject_on",
    "tagged",
    "this",
    "throughout",
    "timeprecision",
    "timeunit",
    "type",
    "typedef",
    "union",
    "unique",
    "unique0",
    "until",
    "until_with",
    "untypted",
    "var",
    "virtual",
    "void",
    "wait_order",
    "weak",
    "wildcard",
    "with",
    "within",
];

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_keyword_from_string() {
        assert_eq!(keyword_from_string("accept_on"), Some(VerilogKeyword::AcceptOn));
        assert_eq!(keyword_from_string("alias"), Some(VerilogKeyword::Alias));
        assert_eq!(keyword_from_string("always_comb"), Some(VerilogKeyword::AlwaysComb));
        assert_eq!(keyword_from_string("always_ff"), Some(VerilogKeyword::AlwaysFf));
        assert_eq!(keyword_from_string("always_latch"), Some(VerilogKeyword::AlwaysLatch));
        assert_eq!(keyword_from_string("assert"), Some(VerilogKeyword::Assert));
        assert_eq!(keyword_from_string("assume"), Some(VerilogKeyword::Assume));
        assert_eq!(keyword_from_string("before"), Some(VerilogKeyword::Before));
        assert_eq!(keyword_from_string("bind"), Some(VerilogKeyword::Bind));
        assert_eq!(keyword_from_string("bins"), Some(VerilogKeyword::Bins));
        assert_eq!(keyword_from_string("binsof"), Some(VerilogKeyword::Binsof));
        assert_eq!(keyword_from_string("bit"), Some(VerilogKeyword::Bit));
        assert_eq!(keyword_from_string("break"), Some(VerilogKeyword::Break));
        assert_eq!(keyword_from_string("byte"), Some(VerilogKeyword::Byte));
        assert_eq!(keyword_from_string("chandle"), Some(VerilogKeyword::Chandle));
        assert_eq!(keyword_from_string("checker"), Some(VerilogKeyword::Checker));
        assert_eq!(keyword_from_string("class"), Some(VerilogKeyword::Class));
        assert_eq!(keyword_from_string("clocking"), Some(VerilogKeyword::Clocking));
        assert_eq!(keyword_from_string("const"), Some(VerilogKeyword::Const));
        assert_eq!(keyword_from_string("constraint"), Some(VerilogKeyword::Constraint));
        assert_eq!(keyword_from_string("context"), Some(VerilogKeyword::Context));
        assert_eq!(keyword_from_string("continue"), Some(VerilogKeyword::Continue));
        assert_eq!(keyword_from_string("cover"), Some(VerilogKeyword::Cover));
        assert_eq!(keyword_from_string("covergroup"), Some(VerilogKeyword::Covergroup));
        assert_eq!(keyword_from_string("coverpoint"), Some(VerilogKeyword::Coverpoint));
        assert_eq!(keyword_from_string("cross"), Some(VerilogKeyword::Cross));
        assert_eq!(keyword_from_string("dist"), Some(VerilogKeyword::Dist));
        assert_eq!(keyword_from_string("do"), Some(VerilogKeyword::Do));
        assert_eq!(keyword_from_string("endchecker"), Some(VerilogKeyword::Endchecker));
        assert_eq!(keyword_from_string("endclass"), Some(VerilogKeyword::Endclass));
        assert_eq!(keyword_from_string("endclocking"), Some(VerilogKeyword::Endclocking));
        assert_eq!(keyword_from_string("endgroup"), Some(VerilogKeyword::Endgroup));
        assert_eq!(keyword_from_string("endinterface"), Some(VerilogKeyword::Endinterface));
        assert_eq!(keyword_from_string("endpackage"), Some(VerilogKeyword::Endpackage));
        assert_eq!(keyword_from_string("endprogram"), Some(VerilogKeyword::Endprogram));
        assert_eq!(keyword_from_string("endproperty"), Some(VerilogKeyword::Endproperty));
        assert_eq!(keyword_from_string("endsequence"), Some(VerilogKeyword::Endsequence));
        assert_eq!(keyword_from_string("enum"), Some(VerilogKeyword::Enum));
        assert_eq!(keyword_from_string("eventually"), Some(VerilogKeyword::Eventually));
        assert_eq!(keyword_from_string("expect"), Some(VerilogKeyword::Expect));
        assert_eq!(keyword_from_string("export"), Some(VerilogKeyword::Export));
        assert_eq!(keyword_from_string("extends"), Some(VerilogKeyword::Extends));
        assert_eq!(keyword_from_string("extern"), Some(VerilogKeyword::Extern));
        assert_eq!(keyword_from_string("final"), Some(VerilogKeyword::Final));
        assert_eq!(keyword_from_string("first_match"), Some(VerilogKeyword::FirstMatch));
        assert_eq!(keyword_from_string("foreach"), Some(VerilogKeyword::Foreach));
        assert_eq!(keyword_from_string("forkjoin"), Some(VerilogKeyword::Forkjoin));
        assert_eq!(keyword_from_string("global"), Some(VerilogKeyword::Global));
        assert_eq!(keyword_from_string("iff"), Some(VerilogKeyword::Iff));
        assert_eq!(keyword_from_string("ignore_bins"), Some(VerilogKeyword::IgnoreBins));
        assert_eq!(keyword_from_string("illegal_bins"), Some(VerilogKeyword::IllegalBins));
        assert_eq!(keyword_from_string("implies"), Some(VerilogKeyword::Implies));
        assert_eq!(keyword_from_string("import"), Some(VerilogKeyword::Import));
        assert_eq!(keyword_from_string("inside"), Some(VerilogKeyword::Inside));
        assert_eq!(keyword_from_string("int"), Some(VerilogKeyword::Int));
        assert_eq!(keyword_from_string("interface"), Some(VerilogKeyword::Interface));
        assert_eq!(keyword_from_string("intersect"), Some(VerilogKeyword::Intersect));
        assert_eq!(keyword_from_string("join_any"), Some(VerilogKeyword::JoinAny));
        assert_eq!(keyword_from_string("join_none"), Some(VerilogKeyword::JoinNone));
        assert_eq!(keyword_from_string("let"), Some(VerilogKeyword::Let));
        assert_eq!(keyword_from_string("local"), Some(VerilogKeyword::Local));
        assert_eq!(keyword_from_string("logic"), Some(VerilogKeyword::Logic));
        assert_eq!(keyword_from_string("longint"), Some(VerilogKeyword::Longint));
        assert_eq!(keyword_from_string("matches"), Some(VerilogKeyword::Matches));
        assert_eq!(keyword_from_string("modport"), Some(VerilogKeyword::Modport));
        assert_eq!(keyword_from_string("new"), Some(VerilogKeyword::New));
        assert_eq!(keyword_from_string("nexttime"), Some(VerilogKeyword::Nexttime));
        assert_eq!(keyword_from_string("null"), Some(VerilogKeyword::Null));
        assert_eq!(keyword_from_string("package"), Some(VerilogKeyword::Package));
        assert_eq!(keyword_from_string("packed"), Some(VerilogKeyword::Packed));
        assert_eq!(keyword_from_string("priority"), Some(VerilogKeyword::Priority));
        assert_eq!(keyword_from_string("program"), Some(VerilogKeyword::Program));
        assert_eq!(keyword_from_string("property"), Some(VerilogKeyword::Property));
        assert_eq!(keyword_from_string("protected"), Some(VerilogKeyword::Protected));
        assert_eq!(keyword_from_string("pure"), Some(VerilogKeyword::Pure));
        assert_eq!(keyword_from_string("rand"), Some(VerilogKeyword::Rand));
        assert_eq!(keyword_from_string("randc"), Some(VerilogKeyword::Randc));
        assert_eq!(keyword_from_string("randcase"), Some(VerilogKeyword::Randcase));
        assert_eq!(keyword_from_string("randsequence"), Some(VerilogKeyword::Randsequence));
        assert_eq!(keyword_from_string("ref"), Some(VerilogKeyword::Ref));
        assert_eq!(keyword_from_string("reject_on"), Some(VerilogKeyword::RejectOn));
        assert_eq!(keyword_from_string("restrict"), Some(VerilogKeyword::Restrict));
        assert_eq!(keyword_from_string("return"), Some(VerilogKeyword::Return));
        assert_eq!(keyword_from_string("s_always"), Some(VerilogKeyword::SAlways));
        assert_eq!(keyword_from_string("s_eventually"), Some(VerilogKeyword::SEventually));
        assert_eq!(keyword_from_string("s_nexttime"), Some(VerilogKeyword::SNexttime));
        assert_eq!(keyword_from_string("s_until"), Some(VerilogKeyword::SUntil));
        assert_eq!(keyword_from_string("s_until_with"), Some(VerilogKeyword::SUntilWith));
        assert_eq!(keyword_from_string("sequence"), Some(VerilogKeyword::Sequence));
        assert_eq!(keyword_from_string("shortint"), Some(VerilogKeyword::Shortint));
        assert_eq!(keyword_from_string("shortreal"), Some(VerilogKeyword::Shortreal));
        assert_eq!(keyword_from_string("solve"), Some(VerilogKeyword::Solve));
        assert_eq!(keyword_from_string("static"), Some(VerilogKeyword::Static));
        assert_eq!(keyword_from_string("string"), Some(VerilogKeyword::String));
        assert_eq!(keyword_from_string("strong"), Some(VerilogKeyword::Strong));
        assert_eq!(keyword_from_string("struct"), Some(VerilogKeyword::Struct));
        assert_eq!(keyword_from_string("super"), Some(VerilogKeyword::Super));
        assert_eq!(keyword_from_string("sync_accept_on"), Some(VerilogKeyword::SyncAcceptOn));
        assert_eq!(keyword_from_string("sync_reject_on"), Some(VerilogKeyword::SyncRejectOn));
        assert_eq!(keyword_from_string("tagged"), Some(VerilogKeyword::Tagged));
        assert_eq!(keyword_from_string("this"), Some(VerilogKeyword::This));
        assert_eq!(keyword_from_string("throughout"), Some(VerilogKeyword::Throughout));
        assert_eq!(keyword_from_string("timeprecision"), Some(VerilogKeyword::Timeprecision));
        assert_eq!(keyword_from_string("timeunit"), Some(VerilogKeyword::Timeunit));
        assert_eq!(keyword_from_string("type"), Some(VerilogKeyword::Type));
        assert_eq!(keyword_from_string("typedef"), Some(VerilogKeyword::Typedef));
        assert_eq!(keyword_from_string("union"), Some(VerilogKeyword::Union));
        assert_eq!(keyword_from_string("unique"), Some(VerilogKeyword::Unique));
        assert_eq!(keyword_from_string("unique0"), Some(VerilogKeyword::Unique0));
        assert_eq!(keyword_from_string("until"), Some(VerilogKeyword::Until));
        assert_eq!(keyword_from_string("until_with"), Some(VerilogKeyword::UntilWith));
        assert_eq!(keyword_from_string("untypted"), Some(VerilogKeyword::Untypted));
        assert_eq!(keyword_from_string("var"), Some(VerilogKeyword::Var));
        assert_eq!(keyword_from_string("virtual"), Some(VerilogKeyword::Virtual));
        assert_eq!(keyword_from_string("void"), Some(VerilogKeyword::Void));
        assert_eq!(keyword_from_string("wait_order"), Some(VerilogKeyword::WaitOrder));
        assert_eq!(keyword_from_string("weak"), Some(VerilogKeyword::Weak));
        assert_eq!(keyword_from_string("wildcard"), Some(VerilogKeyword::Wildcard));
        assert_eq!(keyword_from_string("with"), Some(VerilogKeyword::With));
        assert_eq!(keyword_from_string("within"), Some(VerilogKeyword::Within));
        assert_eq!(keyword_from_string("nonexistent"), None);
    }

    #[test]
    fn test_all_keywords() {
        for kw in ALL_KEYWORDS {
            assert!(
                keyword_from_string(kw).is_some(),
                "Keyword {} failed to parse",
                kw
            );
        }
    }
}
