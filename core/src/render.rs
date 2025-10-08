use crate::frontmatter::TemplateSpec;
use std::collections::BTreeMap;
use tera::{Context, Tera};
use utils::error::Result;

pub fn render_template(spec: &TemplateSpec, vars: &BTreeMap<String, String>) -> Result<String> {
    let mut tera = Tera::default();
    tera.add_raw_template("template", &spec.content)?;

    let mut context = Context::new();
    for (k, v) in vars {
        context.insert(k, v);
    }

    let rendered = tera.render("template", &context)?;
    Ok(rendered)
}
