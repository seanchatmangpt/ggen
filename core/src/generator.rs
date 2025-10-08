use anyhow::Result;
use regex::Regex;
use std::collections::BTreeMap;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use tera::Context;

use crate::pipeline::Pipeline;
use crate::template::{Frontmatter, Template};

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
        tmpl.process_graph(&self.pipeline.graph, &mut self.pipeline.tera, &tctx)?;

        // Vars from frontmatter
        for (k, v) in &tmpl.front.vars { tctx.insert(k, v); }

        // Conditional skip
        if tmpl.front.to.as_deref() == Some("null") || tmpl.front.to.is_none() {
            self.shell(&tmpl.front.sh_before, Some(""), "pre")?;
            self.shell(&tmpl.front.sh_after, Some(""), "post")?;
            return Ok(self.ctx.output_root.join("skipped"));
        }

        // Body render
        let mut body = self.pipeline.render_body(&tmpl.body, &tctx)?;
        if tmpl.front.eof_last { ensure_eof_in_place(&mut body); }

        // Shell pre with body piped
        self.shell(&tmpl.front.sh_before, Some(&body), "pre")?;

        // Resolve output target
        let out_path = self.ctx.output_root.join(tmpl.front.to.clone().unwrap());

        // Inject vs addition
        if tmpl.front.inject {
            let current = fs::read_to_string(&out_path).unwrap_or_default();
            if should_skip_inject(&current, tmpl.front.skip_if.as_deref()) {
                self.note(format!("inject skipped (skip_if): {}", out_path.display()).as_str());
            } else {
                let new_content = apply_injection(&current, &body, &tmpl.front);
                self.write_or_preview(&out_path, &new_content)?;
            }
        } else {
            // Addition semantics
            if out_path.exists() {
                if tmpl.front.unless_exists {
                    self.note(format!("skip (unless_exists): {}", out_path.display()).as_str());
                    self.shell(&tmpl.front.sh_after, Some(&body), "post")?;
                    return Ok(out_path);
                }
                if !tmpl.front.force {
                    self.note(format!("skip (exists, force=false): {}", out_path.display()).as_str());
                    return Ok(out_path);
                }
            }
            self.write_or_preview(&out_path, &body)?;
        }

        // Shell post
        self.shell(&tmpl.front.sh_after, Some(&body), "post")?;
        Ok(out_path)
    }

    fn write_or_preview(&self, path: &Path, rendered: &str) -> Result<()> {
        if self.ctx.dry_run {
            println!("--- [DRY RUN] would write: {} ---", path.display());
            print!("{}", rendered);
            println!("\n--- [END DRY RUN] ---");
            return Ok(());
        }
        if let Some(parent) = path.parent() { fs::create_dir_all(parent)?; }
        fs::write(path, rendered)?;
        Ok(())
    }
    fn shell(&self, cmd: &Option<String>, stdin_data: Option<&str>, phase: &str) -> Result<()> {
        if let Some(c) = cmd {
            if self.ctx.dry_run {
                println!("--- [DRY RUN] would execute {} ---\n{}\n--- [END DRY RUN] ---", phase, c);
                return Ok(());
            }
            let mut child = Command::new("sh")
                .arg("-c").arg(c)
                .stdin(if stdin_data.is_some() { Stdio::piped() } else { Stdio::null() })
                .stdout(Stdio::inherit())
                .stderr(Stdio::inherit())
                .spawn()?;
            if let Some(data) = stdin_data {
                use std::io::Write;
                let mut stdin = child.stdin.take().unwrap();
                stdin.write_all(data.as_bytes())?;
            }
            let _ = child.wait()?;
        }
        Ok(())
    }
    fn note(&self, msg: &str) {
        if self.ctx.dry_run { println!("--- [DRY RUN] {} ---", msg); }
    }
}

/* ---------- helpers ---------- */

fn insert_env(ctx: &mut Context) {
    let mut env_map: BTreeMap<String, String> = BTreeMap::new();
    for (k, v) in env::vars() { env_map.insert(k, v); }
    ctx.insert("env", &env_map);
    if let Ok(cwd) = env::current_dir() { ctx.insert("cwd", &cwd.display().to_string()); }
}

fn ensure_eof_in_place(s: &mut String) {
    if !s.ends_with('\n') { s.push('\n'); }
}

fn ensure_eof(s: &str, eof_last: bool) -> String {
    if eof_last && !s.ends_with('\n') { let mut x = s.to_owned(); x.push('\n'); x } else { s.to_string() }
}

fn apply_injection(current: &str, injection: &str, fm: &Frontmatter) -> String {
    // prepend
    if fm.prepend {
        let mut out = String::new();
        out.push_str(injection);
        if !injection.ends_with('\n') { out.push('\n'); }
        out.push_str(current);
        return ensure_eof(&out, fm.eof_last);
    }
    // append
    if fm.append {
        let mut out = String::new();
        out.push_str(current);
        if !current.ends_with('\n') && !current.is_empty() { out.push('\n'); }
        out.push_str(injection);
        return ensure_eof(&out, fm.eof_last);
    }
    // at_line
    if let Some(n) = fm.at_line {
        let n = n.max(1) as usize;
        let mut lines: Vec<&str> = current.split_inclusive('\n').collect();
        let inj = if injection.ends_with('\n') { injection.to_string() } else { format!("{injection}\n") };
        if n - 1 >= lines.len() {
            lines.push(Box::leak(inj.into_boxed_str()));
        } else {
            lines.insert(n - 1, Box::leak(inj.into_boxed_str()));
        }
        return ensure_eof(&lines.concat(), fm.eof_last);
    }
    // before/after
    if let Some(pat) = fm.before.as_ref().or(fm.after.as_ref()) {
        let re = Regex::new(pat).unwrap();
        if let Some(m) = re.find(current) {
            let mut s = String::with_capacity(current.len() + injection.len() + 2);
            if fm.before.is_some() {
                s.push_str(&current[..m.start()]);
                s.push_str(injection);
                if !injection.ends_with('\n') { s.push('\n'); }
                s.push_str(&current[m.start()..]);
            } else {
                s.push_str(&current[..m.end()]);
                if !current[..m.end()].ends_with('\n') { s.push('\n'); }
                s.push_str(injection);
                s.push_str(&current[m.end()..]);
            }
            return ensure_eof(&s, fm.eof_last);
        }
        // fallback: append
        let mut s = String::new();
        s.push_str(current);
        if !current.ends_with('\n') && !current.is_empty() { s.push('\n'); }
        s.push_str(injection);
        return ensure_eof(&s, fm.eof_last);
    }
    // default: append
    let mut s = String::new();
    s.push_str(current);
    if !current.ends_with('\n') && !current.is_empty() { s.push('\n'); }
    s.push_str(injection);
    ensure_eof(&s, fm.eof_last)
}

fn should_skip_inject(current: &str, skip_if: Option<&str>) -> bool {
    match skip_if {
        Some(p) if !p.is_empty() => Regex::new(p).map(|re| re.is_match(current)).unwrap_or(false),
        _ => false,
    }
}
