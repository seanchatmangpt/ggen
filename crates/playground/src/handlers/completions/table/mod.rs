pub mod capability_fields;
pub use capability_fields::CAPABILITY_FIELDS;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Domain {
    Lifecycle,
    TextSync,
    Navigation,
    Symbols,
    Editing,
    Diagnostics,
    CodeLens,
    SemanticTokens,
    Workspace,
    Window,
    Max,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct MethodEntry {
    pub fn_name: &'static str,
    pub lsp_method: &'static str,
    pub params_type: &'static str,
    pub return_type: &'static str,
    pub capability_field: Option<&'static str>,
    pub domain: Domain,
}

const PARTS: &[&[MethodEntry]] = &[
    include!("lifecycle_sync.rs"),
    include!("navigation.rs"),
    include!("symbols_codelens.rs"),
    include!("editing_diagnostics.rs"),
    include!("workspace_window.rs"),
];

const TOTAL_LEN: usize = {
    let mut len = 0;
    let mut i = 0;
    while i < PARTS.len() {
        len += PARTS[i].len();
        i += 1;
    }
    len
};

pub static METHODS: &[MethodEntry] = &{
    let mut arr = [MethodEntry {
        fn_name: "",
        lsp_method: "",
        params_type: "",
        return_type: "",
        capability_field: None,
        domain: Domain::Lifecycle,
    }; TOTAL_LEN];

    let mut arr_idx = 0;
    let mut p_idx = 0;
    while p_idx < PARTS.len() {
        let part = PARTS[p_idx];
        let mut part_idx = 0;
        while part_idx < part.len() {
            arr[arr_idx] = part[part_idx];
            arr_idx += 1;
            part_idx += 1;
        }
        p_idx += 1;
    }
    arr
};

pub fn domain_label(d: &Domain) -> &'static str {
    match d {
        Domain::Lifecycle => "Lifecycle",
        Domain::TextSync => "Text Synchronization",
        Domain::Navigation => "Navigation",
        Domain::Symbols => "Symbols",
        Domain::Editing => "Editing",
        Domain::Diagnostics => "Diagnostics",
        Domain::CodeLens => "Code Lens & Links",
        Domain::SemanticTokens => "Semantic Tokens",
        Domain::Workspace => "Workspace",
        Domain::Window => "Window",
        Domain::Max => "tower-lsp-max Extensions",
    }
}

pub fn domain_sort_key(d: &Domain) -> u8 {
    match d {
        Domain::Lifecycle => 0,
        Domain::TextSync => 1,
        Domain::Editing => 2,
        Domain::Navigation => 3,
        Domain::Symbols => 4,
        Domain::Diagnostics => 5,
        Domain::CodeLens => 6,
        Domain::SemanticTokens => 7,
        Domain::Workspace => 8,
        Domain::Window => 9,
        Domain::Max => 10,
    }
}
