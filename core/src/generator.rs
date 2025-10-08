use anyhow::Result;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::PathBuf;
use tera::Context;

use crate::pipeline::Pipeline;
use crate::template::Template;

pub struct GenContext {
    pub template_path: PathBuf,
    pub output_root: PathBuf,
    pub vars: BTreeMap<String, String>,
    pub global_prefixes: BTreeMap<String, String>,
    pub base: Option<String>,
    pub dry_run: bool,
}

impl GenContext {
    pub fn new(template_path: PathBuf, output_root: PathBuf) -> Self {
        Self {
            template_path, output_root,
            vars: BTreeMap::new(),
            global_prefixes: BTreeMap::new(),
            base: None,
            dry_run: false,
        }
    }
    pub fn with_vars(mut self, vars: BTreeMap<String, String>) -> Self { self.vars = vars; self }
    pub fn with_prefixes(mut self, prefixes: BTreeMap<String, String>, base: Option<String>) -> Self {
        self.global_prefixes = prefixes; self.base = base; self
    }
    pub fn dry(mut self, dry: bool) -> Self { self.dry_run = dry; self }
}

pub struct Generator {
    pub pipeline: Pipeline,
    pub ctx: GenContext,
}

impl Generator {
    pub fn new(pipeline: Pipeline, ctx: GenContext) -> Self { Self { pipeline, ctx } }

    pub fn generate(&mut self) -> Result<PathBuf> {
        let input = fs::read_to_string(&self.ctx.template_path)?;
        let mut tmpl = Template::parse(&input)?;

        // Context
        let mut tctx = Context::from_serialize(&self.ctx.vars)?;
        insert_env(&mut tctx);

        // Merge global + template prefixes for SPARQL (before frontmatter render)
        let mut merged = self.ctx.global_prefixes.clone();
        for (k, v) in &tmpl.front.prefixes { merged.insert(k.clone(), v.clone()); }
        self.pipeline.register_prefixes(tmpl.front.base.as_deref().or(self.ctx.base.as_deref()), &merged);

        // Render frontmatter (SPARQL functions now available)
        tmpl.render_frontmatter(&mut self.pipeline.tera, &tctx)?;

        // Graph ops
        tmpl.process_graph(&mut self.pipeline.graph, &mut self.pipeline.tera, &tctx)?;

        // Render body
        let rendered = self.pipeline.render_body(&tmpl.body, &tctx)?;

        // Determine output path
        let out_path = if let Some(to_path) = &tmpl.front.to {
            let rendered_to = self.pipeline.tera.render_str(to_path, &tctx)?;
            self.ctx.output_root.join(rendered_to)
        } else {
            self.ctx.output_root.join("out.txt")
        };

        // Write output unless dry run
        if !self.ctx.dry_run {
            if let Some(parent) = out_path.parent() {
                fs::create_dir_all(parent)?;
            }
            fs::write(&out_path, &rendered)?;
        }

        Ok(out_path)
    }
}

fn insert_env(ctx: &mut Context) {
    for (k, v) in env::vars() {
        ctx.insert(&k, &v);
    }
}