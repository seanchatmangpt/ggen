//! Next.js and Nuxt.js project generator
//!
//! This module generates JavaScript/TypeScript projects for Next.js and Nuxt.js
//! frameworks. It creates the complete project structure including package.json,
//! configuration files, and initial page components.
//!
//! ## Supported Project Types
//!
//! - **NextJs**: React-based framework with server-side rendering
//! - **Nuxt**: Vue.js-based framework with server-side rendering
//!
//! ## Features
//!
//! - **TypeScript support**: Full TypeScript configuration
//! - **ESLint integration**: Pre-configured ESLint rules
//! - **Modern tooling**: Latest versions of frameworks and dependencies
//! - **Development setup**: Ready-to-run development scripts
//!
//! ## Examples
//!
//! ### Generating a Next.js Project
//!
//! ```rust,no_run
//! use ggen_core::project_generator::{ProjectConfig, ProjectType};
//! use ggen_core::project_generator::nextjs::NextJsGenerator;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let config = ProjectConfig {
//!     name: "my-app".to_string(),
//!     project_type: ProjectType::NextJs,
//!     framework: None,
//!     path: PathBuf::from("."),
//! };
//!
//! let generator = NextJsGenerator::new();
//! let structure = generator.generate(&config)?;
//! # Ok(())
//! # }
//! ```
//!
//! ### Generating a Nuxt Project
//!
//! ```rust,no_run
//! use ggen_core::project_generator::{ProjectConfig, ProjectType};
//! use ggen_core::project_generator::nextjs::NextJsGenerator;
//! use std::path::PathBuf;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let config = ProjectConfig {
//!     name: "my-nuxt-app".to_string(),
//!     project_type: ProjectType::Nuxt,
//!     framework: None,
//!     path: PathBuf::from("."),
//! };
//!
//! let generator = NextJsGenerator::new();
//! let structure = generator.generate(&config)?;
//! # Ok(())
//! # }
//! ```

use super::{ProjectConfig, ProjectGenerator, ProjectStructure, ProjectType};
use ggen_utils::error::Result;

pub struct NextJsGenerator;

impl Default for NextJsGenerator {
    fn default() -> Self {
        Self
    }
}

impl NextJsGenerator {
    pub fn new() -> Self {
        Self
    }

    fn generate_package_json(&self, config: &ProjectConfig) -> String {
        match config.project_type {
            ProjectType::NextJs => {
                format!(
                    r#"{{
  "name": "{}",
  "version": "0.1.0",
  "private": true,
  "scripts": {{
    "dev": "next dev",
    "build": "next build",
    "start": "next start",
    "lint": "next lint"
  }},
  "dependencies": {{
    "next": "14.0.0",
    "react": "^18.0.0",
    "react-dom": "^18.0.0"
  }},
  "devDependencies": {{
    "@types/node": "^20.0.0",
    "@types/react": "^18.0.0",
    "@types/react-dom": "^18.0.0",
    "typescript": "^5.0.0",
    "eslint": "^8.0.0",
    "eslint-config-next": "14.0.0"
  }}
}}
"#,
                    config.name
                )
            }
            ProjectType::Nuxt => {
                format!(
                    r#"{{
  "name": "{}",
  "version": "0.1.0",
  "private": true,
  "scripts": {{
    "dev": "nuxt dev",
    "build": "nuxt build",
    "generate": "nuxt generate",
    "preview": "nuxt preview"
  }},
  "devDependencies": {{
    "nuxt": "^3.0.0",
    "vue": "^3.3.0",
    "vue-router": "^4.0.0"
  }}
}}
"#,
                    config.name
                )
            }
            _ => String::new(),
        }
    }

    fn generate_index_page(&self, config: &ProjectConfig) -> String {
        match config.project_type {
            ProjectType::NextJs => {
                format!(
                    r#"export default function Home() {{
  return (
    <div style={{{{ padding: '2rem' }}}}>
      <h1>Welcome to {}</h1>
      <p>Get started by editing pages/index.tsx</p>
    </div>
  );
}}
"#,
                    config.name
                )
            }
            ProjectType::Nuxt => {
                format!(
                    r#"<template>
  <div>
    <h1>Welcome to {}</h1>
    <p>Get started by editing pages/index.vue</p>
  </div>
</template>

<script setup lang="ts">
// Your logic here
</script>

<style scoped>
div {{
  padding: 2rem;
}}
</style>
"#,
                    config.name
                )
            }
            _ => String::new(),
        }
    }

    fn generate_tsconfig(&self) -> String {
        r#"{
  "compilerOptions": {
    "target": "ES2020",
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "jsx": "preserve",
    "module": "ESNext",
    "moduleResolution": "bundler",
    "resolveJsonModule": true,
    "allowJs": true,
    "strict": true,
    "noEmit": true,
    "esModuleInterop": true,
    "skipLibCheck": true,
    "forceConsistentCasingInFileNames": true,
    "incremental": true,
    "paths": {
      "@/*": ["./*"]
    }
  },
  "include": ["next-env.d.ts", "**/*.ts", "**/*.tsx"],
  "exclude": ["node_modules"]
}
"#
        .to_string()
    }

    fn generate_next_config(&self) -> String {
        r#"/** @type {import('next').NextConfig} */
const nextConfig = {
  reactStrictMode: true,
}

module.exports = nextConfig
"#
        .to_string()
    }

    fn generate_nuxt_config(&self) -> String {
        r#"// https://nuxt.com/docs/api/configuration/nuxt-config
export default defineNuxtConfig({
  devtools: { enabled: true }
})
"#
        .to_string()
    }

    fn generate_gitignore(&self) -> String {
        r#"# Dependencies
node_modules/
/.pnp
.pnp.js

# Testing
/coverage

# Next.js
/.next/
/out/

# Nuxt.js
.nuxt
.output

# Production
/build
/dist

# Misc
.DS_Store
*.pem

# Debug
npm-debug.log*
yarn-debug.log*
yarn-error.log*

# Local env files
.env*.local
.env

# Vercel
.vercel

# TypeScript
*.tsbuildinfo
next-env.d.ts
"#
        .to_string()
    }

    fn generate_readme(&self, config: &ProjectConfig) -> String {
        let framework = match config.project_type {
            ProjectType::NextJs => "Next.js",
            ProjectType::Nuxt => "Nuxt.js",
            _ => "JavaScript",
        };

        format!(
            r#"# {}

A {} project created with ggen.

## Getting Started

First, run the development server:

```bash
npm run dev
```

Open [http://localhost:3000](http://localhost:3000) with your browser to see the result.

## Learn More

To learn more about {}, take a look at the following resources:

- [Documentation](https://nextjs.org/docs) - learn about features and API
- [Learn Tutorial](https://nextjs.org/learn) - an interactive tutorial

## Deploy

The easiest way to deploy your app is to use the Vercel Platform.
"#,
            config.name, framework, framework
        )
    }
}

impl ProjectGenerator for NextJsGenerator {
    fn generate(&self, config: &ProjectConfig) -> Result<ProjectStructure> {
        let mut files = vec![
            (
                "package.json".to_string(),
                self.generate_package_json(config),
            ),
            ("tsconfig.json".to_string(), self.generate_tsconfig()),
            (".gitignore".to_string(), self.generate_gitignore()),
            ("README.md".to_string(), self.generate_readme(config)),
        ];

        let mut directories = vec!["pages".to_string(), "public".to_string()];

        // Add framework-specific files
        match config.project_type {
            ProjectType::NextJs => {
                files.push((
                    "pages/index.tsx".to_string(),
                    self.generate_index_page(config),
                ));
                files.push(("next.config.js".to_string(), self.generate_next_config()));
                files.push((
                    "pages/_app.tsx".to_string(),
                    r#"import type { AppProps } from 'next/app'

export default function App({ Component, pageProps }: AppProps) {
  return <Component {...pageProps} />
}
"#
                    .to_string(),
                ));
                directories.push("styles".to_string());
            }
            ProjectType::Nuxt => {
                files.push((
                    "pages/index.vue".to_string(),
                    self.generate_index_page(config),
                ));
                files.push(("nuxt.config.ts".to_string(), self.generate_nuxt_config()));
                directories.push("components".to_string());
                directories.push("composables".to_string());
            }
            _ => {}
        }

        Ok(ProjectStructure { files, directories })
    }

    fn supported_types(&self) -> Vec<ProjectType> {
        vec![ProjectType::NextJs, ProjectType::Nuxt]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};
    use std::path::PathBuf;

    test!(test_generate_nextjs_project, {
        let generator = NextJsGenerator::new();
        let config = ProjectConfig {
            name: "test-nextjs".to_string(),
            project_type: ProjectType::NextJs,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config).unwrap();

        // Check files
        assert!(structure
            .files
            .iter()
            .any(|(path, _)| path == "package.json"));
        assert!(structure
            .files
            .iter()
            .any(|(path, _)| path == "pages/index.tsx"));
        assert!(structure
            .files
            .iter()
            .any(|(path, _)| path == "next.config.js"));

        // Check content
        let package_json = structure
            .files
            .iter()
            .find(|(path, _)| path == "package.json")
            .map(|(_, content)| content)
            .unwrap();

        assert!(package_json.contains(r#""name": "test-nextjs""#));
        assert!(package_json.contains("next"));
    });

    test!(test_generate_nuxt_project, {
        let generator = NextJsGenerator::new();
        let config = ProjectConfig {
            name: "test-nuxt".to_string(),
            project_type: ProjectType::Nuxt,
            framework: None,
            path: PathBuf::from("/tmp"),
        };

        let structure = generator.generate(&config).unwrap();

        // Check files
        assert!(structure
            .files
            .iter()
            .any(|(path, _)| path == "pages/index.vue"));
        assert!(structure
            .files
            .iter()
            .any(|(path, _)| path == "nuxt.config.ts"));

        // Check directories
        assert!(structure.directories.contains(&"components".to_string()));
    });
}
