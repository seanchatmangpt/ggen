// Equivalent of Hygen's:
//   ---
//   to: components/<%= name %>/index.jsx
//   ---
//   import React from 'react'
//
//   export const <%= name %> = ({ children }) => (
//     <div className="<%= h.changeCase.paramCase(name) %>">{children}</div>"
//   )
//
// With name `HelloWorld`, Hygen's example compiles to a `hello-world`
// CSS class. ggen does have a `kebab_case` filter (`crates/ggen-engine/
// src/template.rs`, Inflector-backed); this Rust analog nonetheless uses
// `snake_case` for the module path (`hello_world.rs`, Rust's own file-
// naming convention) and `pascal_case` for the exported type, since the
// target here is Rust, not the CSS-class string Hygen's JSX example
// produces — the case conversions that fit ggen's actual output language.

pub struct HelloWorld {
    pub children: String,
}

impl HelloWorld {
    pub fn render(&self) -> String {
        format!("<div class=\"{}\">{}</div>", "hello_world", self.children)
    }
}
