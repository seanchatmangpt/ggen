//! AST visitor for extracting code entities

use crate::ontology::{CodeEntity, EntityKind, Visibility, RelationshipKind};
use std::path::{Path, PathBuf};
use syn::{
    File, Item, ItemFn, ItemStruct, ItemEnum, ItemTrait, ItemImpl,
    ItemMod, ItemConst, ItemStatic, ItemType, ItemMacro,
    Attribute, Visibility as SynVisibility,
};

/// Visitor that traverses the AST and extracts code entities
pub struct EntityVisitor {
    file_path: PathBuf,
    entities: Vec<CodeEntity>,
    current_module: Vec<String>,
}

impl EntityVisitor {
    /// Create a new entity visitor
    pub fn new(file_path: &Path) -> Self {
        Self {
            file_path: file_path.to_path_buf(),
            entities: Vec::new(),
            current_module: Vec::new(),
        }
    }

    /// Get the extracted entities
    pub fn entities(self) -> Vec<CodeEntity> {
        self.entities
    }

    /// Visit a file
    pub fn visit_file(&mut self, file: &File) {
        for item in &file.items {
            self.visit_item(item);
        }
    }

    /// Visit an item
    fn visit_item(&mut self, item: &Item) {
        match item {
            Item::Fn(func) => self.visit_function(func),
            Item::Struct(struct_item) => self.visit_struct(struct_item),
            Item::Enum(enum_item) => self.visit_enum(enum_item),
            Item::Trait(trait_item) => self.visit_trait(trait_item),
            Item::Impl(impl_item) => self.visit_impl(impl_item),
            Item::Mod(mod_item) => self.visit_module(mod_item),
            Item::Const(const_item) => self.visit_const(const_item),
            Item::Static(static_item) => self.visit_static(static_item),
            Item::Type(type_item) => self.visit_type(type_item),
            Item::Macro(macro_item) => self.visit_macro(macro_item),
            _ => {}
        }
    }

    /// Visit a function
    fn visit_function(&mut self, func: &ItemFn) {
        let name = func.sig.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id, name.clone(), EntityKind::Function);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&func.vis);
        entity.documentation = self.extract_doc_comments(&func.attrs);

        // Extract additional metadata
        if func.sig.asyncness.is_some() {
            entity.add_attribute("async".to_string(), "true".to_string());
        }

        if func.sig.unsafety.is_some() {
            entity.add_attribute("unsafe".to_string(), "true".to_string());
        }

        // Add to current module
        if !self.current_module.is_empty() {
            let module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, module_id);
        }

        self.entities.push(entity);
    }

    /// Visit a struct
    fn visit_struct(&mut self, struct_item: &ItemStruct) {
        let name = struct_item.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id, name.clone(), EntityKind::Struct);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&struct_item.vis);
        entity.documentation = self.extract_doc_comments(&struct_item.attrs);

        // Extract fields
        for field in &struct_item.fields {
            if let Some(field_name) = &field.ident {
                let field_id = format!("{}::{}", name, field_name);
                entity.add_relationship(RelationshipKind::HasField, field_id);
            }
        }

        if !self.current_module.is_empty() {
            let module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, module_id);
        }

        self.entities.push(entity);
    }

    /// Visit an enum
    fn visit_enum(&mut self, enum_item: &ItemEnum) {
        let name = enum_item.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id, name.clone(), EntityKind::Enum);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&enum_item.vis);
        entity.documentation = self.extract_doc_comments(&enum_item.attrs);

        if !self.current_module.is_empty() {
            let module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, module_id);
        }

        self.entities.push(entity);
    }

    /// Visit a trait
    fn visit_trait(&mut self, trait_item: &ItemTrait) {
        let name = trait_item.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id, name.clone(), EntityKind::Trait);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&trait_item.vis);
        entity.documentation = self.extract_doc_comments(&trait_item.attrs);

        if !self.current_module.is_empty() {
            let module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, module_id);
        }

        self.entities.push(entity);
    }

    /// Visit an impl block
    fn visit_impl(&mut self, _impl_item: &ItemImpl) {
        // Simplified: In full implementation, extract methods and trait implementations
    }

    /// Visit a module
    fn visit_module(&mut self, mod_item: &ItemMod) {
        let name = mod_item.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id.clone(), name.clone(), EntityKind::Module);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&mod_item.vis);
        entity.documentation = self.extract_doc_comments(&mod_item.attrs);

        if !self.current_module.is_empty() {
            let parent_module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, parent_module_id);
        }

        self.entities.push(entity);

        // Visit module contents if inline
        if let Some((_, items)) = &mod_item.content {
            self.current_module.push(name);
            for item in items {
                self.visit_item(item);
            }
            self.current_module.pop();
        }
    }

    /// Visit a const
    fn visit_const(&mut self, const_item: &ItemConst) {
        let name = const_item.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id, name, EntityKind::Const);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&const_item.vis);
        entity.documentation = self.extract_doc_comments(&const_item.attrs);

        if !self.current_module.is_empty() {
            let module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, module_id);
        }

        self.entities.push(entity);
    }

    /// Visit a static
    fn visit_static(&mut self, static_item: &ItemStatic) {
        let name = static_item.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id, name, EntityKind::Static);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&static_item.vis);
        entity.documentation = self.extract_doc_comments(&static_item.attrs);

        if !self.current_module.is_empty() {
            let module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, module_id);
        }

        self.entities.push(entity);
    }

    /// Visit a type alias
    fn visit_type(&mut self, type_item: &ItemType) {
        let name = type_item.ident.to_string();
        let id = self.make_id(&name);

        let mut entity = CodeEntity::new(id, name, EntityKind::Type);
        entity.file_path = self.file_path.to_string_lossy().to_string();
        entity.visibility = self.convert_visibility(&type_item.vis);
        entity.documentation = self.extract_doc_comments(&type_item.attrs);

        if !self.current_module.is_empty() {
            let module_id = self.current_module.join("::");
            entity.add_relationship(RelationshipKind::DefinedIn, module_id);
        }

        self.entities.push(entity);
    }

    /// Visit a macro
    fn visit_macro(&mut self, macro_item: &ItemMacro) {
        if let Some(ident) = &macro_item.ident {
            let name = ident.to_string();
            let id = self.make_id(&name);

            let mut entity = CodeEntity::new(id, name, EntityKind::Macro);
            entity.file_path = self.file_path.to_string_lossy().to_string();
            entity.documentation = self.extract_doc_comments(&macro_item.attrs);

            if !self.current_module.is_empty() {
                let module_id = self.current_module.join("::");
                entity.add_relationship(RelationshipKind::DefinedIn, module_id);
            }

            self.entities.push(entity);
        }
    }

    /// Extract documentation comments from attributes
    fn extract_doc_comments(&self, attrs: &[Attribute]) -> Option<String> {
        let mut docs = Vec::new();

        for attr in attrs {
            if attr.path().is_ident("doc") {
                if let Ok(meta) = attr.meta.require_name_value() {
                    if let syn::Expr::Lit(expr_lit) = &meta.value {
                        if let syn::Lit::Str(lit_str) = &expr_lit.lit {
                            docs.push(lit_str.value());
                        }
                    }
                }
            }
        }

        if docs.is_empty() {
            None
        } else {
            Some(docs.join("\n").trim().to_string())
        }
    }

    /// Convert syn visibility to our visibility type
    fn convert_visibility(&self, vis: &SynVisibility) -> Visibility {
        match vis {
            SynVisibility::Public(_) => Visibility::Public,
            SynVisibility::Restricted(restricted) => {
                if restricted.path.is_ident("crate") {
                    Visibility::Crate
                } else {
                    Visibility::Module
                }
            }
            SynVisibility::Inherited => Visibility::Private,
        }
    }

    /// Create a unique ID for an entity
    fn make_id(&self, name: &str) -> String {
        if self.current_module.is_empty() {
            name.to_string()
        } else {
            format!("{}::{}", self.current_module.join("::"), name)
        }
    }
}
