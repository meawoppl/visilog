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
