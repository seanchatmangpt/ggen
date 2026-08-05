# Hygen & hygen-create — Combined Reference

> Compiled from the official Hygen docs (`hygen.io` / `github.com/jondot/hygen`,
> `hygen.io/docs/*.md`) and the `hygen-create` README
> (`github.com/ronp001/hygen-create`). Transcribed near-verbatim from the source
> markdown for reference while designing ggen's own pack-scaffolding
> constructor (see `packs/ggen-self-pack/` design work). Not ggen documentation
> itself — an external reference snapshot.

**Last compiled:** 2026-08-04

---

## Table of Contents

- [Part 1: Hygen](#part-1-hygen)
  - [1. Overview](#1-overview)
  - [2. Installation](#2-installation)
  - [3. Quick Start](#3-quick-start)
  - [4. Generators](#4-generators)
  - [5. Templates](#5-templates)
  - [6. Extensibility (`.hygen.js`)](#6-extensibility-hygenjs)
  - [7. Packages (`hygen-add`)](#7-packages-hygen-add)
  - [8. Standalone Installation](#8-standalone-installation)
  - [9. FAQ](#9-faq)
- [Part 2: hygen-create](#part-2-hygen-create)
  - [1. Overview](#1-overview-1)
  - [2. Installation](#2-installation-1)
  - [3. Workflow](#3-workflow)
  - [4. Full Example Session](#4-full-example-session)
  - [5. Iteratively Improving a Generator](#5-iteratively-improving-a-generator)
  - [6. Previous Versions of Generators](#6-previous-versions-of-generators)
  - [7. Configuration and Options](#7-configuration-and-options)
  - [8. Limitations](#8-limitations)
  - [9. Command Reference](#9-command-reference)
- [Part 3: How the Two Fit Together](#part-3-how-the-two-fit-together)

---

# Part 1: Hygen

Source: `hygen.io/docs/*` (mirrored in `github.com/jondot/hygen/hygen.io/docs/`)
and `github.com/jondot/hygen/README.md`.

## 1. Overview

Hygen is described as "the simple, fast, and scalable code generator that
lives *in* your project." It enables developers to build ad-hoc generators
and project scaffolds locally within their own codebase, rather than as a
separate, independently-maintained tool.

Key capabilities:

- Creating generators quickly for both small templates and full project
  scaffolds.
- Local generator storage per project (with optional global configuration).
- Built-in scaffolding for rapid generator creation (`hygen init self`,
  `hygen generator new`).
- Template logic and rendering through [EJS](https://github.com/tj/ejs).
- Interactive prompts for argument collection.
- Automatic CLI argument processing (any `--flag` becomes a template
  variable).
- File creation *and* injection into existing files.
- Shell command execution as a side effect of generation.

Development philosophy: rather than creating dedicated generator projects
that require separate maintenance and testing, Hygen keeps templates
project-local, letting developers "scratch your own itch" by generating code
contextually within their current workflow. Templates are meant to be
checked into the same repo/PR review flow as the code they generate.

Notable adoption cited in the docs: Wix, Airbnb, Mercedes-Benz, Accenture,
among others.

## 2. Installation

Multiple installation paths:

```bash
# Homebrew (macOS)
$ brew tap jondot/tap && brew install hygen

# npm / yarn (global)
$ npm i -g hygen
$ yarn global add hygen

# npx (no install, one-time use)
$ npx hygen ...

# Standalone binary releases — see §8 below
```

## 3. Quick Start

Basic invocation pattern: `hygen [generator] [action] [NAME]`.

> New in hygen 4.0.0: a positional `NAME` parameter to save a bit of typing.

Bootstrapping a project's own generator-of-generators:

```bash
$ hygen init self
$ hygen generator new awesome-generator
$ hygen awesome-generator new hello
```

`hygen init self` creates a `_templates` folder in the current project,
seeded with hygen's own built-in "generator" generator — i.e. a generator
whose job is to scaffold *other* generators. This is the direct precedent
for a "canonical self-pack that generates packs" design: hygen ships its own
meta-generator, materializes it into the project via `init self`, and from
then on the project owns and can edit that copy.

Once a generator exists, it supports several invocation patterns:

- Generate all files in an action, passing variables via `--flag value`.
- Target a specific file (or subset) within an action by substring or regex
  match on the action's sub-path (see [Selecting Parts of a Generator](#selecting-parts-of-a-generator)).

## 4. Generators

Every time a repetitive task or a hidden structural pattern in the files
you're editing shows up, that's a signal to make a new generator.

```
$ hygen generator new --name mailer
                              `-------- just a name you pick.

Loaded templates: _templates
       added: _templates/mailer/new/hello.ejs.t
                                       `------ your template file.


$ hygen

Error: please specify a generator.

Available actions:
generator: new, with-prompt
mailer: new
    \
     `----------- your new generator is already here!
```

The moment a generator directory is added under `_templates`, it is
immediately usable — no registration step. The scaffolded `hello.ejs.t`:

```javascript
---
to: app/hello.js
---
const hello = `
Hello!
This is your first hygen template.

Learn what it can do here:

https://github.com/jondot/hygen
`

console.log(hello)
```

To build a real generator, copy/rename this file:

```bash
$ mv _templates/mailer/new/{hello.js,html.ejs.t}
$ cp _templates/mailer/new/{html.ejs.t,text.ejs.t}
```

The `.t` suffix on template files exists mainly so editors don't try to be
"smart" about the file type — it's a convention, not a requirement. Hygen
does not care about file names or file types in generator folders; it only
cares about **folder structure** and the **contents** of files (their
frontmatter + body).

### Structure

```
_templates/
  mailer/
    new/
      html.ejs.t
      text.ejs.t
app/
  index.js
package.json
```

Calling `hygen mailer new` picks up the closest `_templates` folder and
renders every file under `mailer/new`. Hygen recursively walks the template
folder, so generators can be structured arbitrarily deep.

**Core design principle: "command structure is folder structure."** Hygen
picks up the `_templates` directory relative to the current working
directory (bubbling-up lookup — see §6 for the analogous `.hygen.js` search).

### CLI Arguments

```bash
$ hygen mailer new --name foobar --message hello --version 1
```

Any `--flag value` on the CLI becomes a variable (`name`, `message`,
`version`) usable in every template rendered by that action.

```yaml
---
to: app/emails/<%= name %>.html
---
<h1>Hello <%= name %></h1>
<%= message %>
(version <%= version %>)
```

### Interactive Prompt

Add a `prompt.js` at the generator-action root to make it interactive:

```
_templates/
  mailer/
    new/           <-- the mailer new generator
      prompt.js    <-- your prompt file!
      html.ejs.t
      text.ejs.t
```

```javascript
module.exports = [
  {
    type: 'input',
    name: 'message',
    message: "What's your message?"
  }
]
```

Format is based on [enquirer](https://github.com/enquirer/enquirer#prompt-options).
Prompted values become template variables exactly like CLI flags do.

### Advanced Interactive Prompt

For multi-step prompting, conditional skipping, or reshaping CLI args before
templates see them, replace `prompt.js` with an `index.js`:

```
my-generator
  my-action/
    index.js
    template1.ejs.t
    template2.ejs.t
```

```javascript
// my-generator/my-action/index.js
module.exports = {
  prompt: ({ prompter, args }) =>
    prompter
      .prompt({
        type: 'input',
        name: 'email',
        message: "What's your email?"
      })
      .then(({ email }) =>
        prompter.prompt({
          type: 'input',
          name: 'emailConfirmation',
          message: `Please type your email [${email}] again:`
        })
      )
}
```

Skip prompting conditionally:

```javascript
module.exports = {
  prompt: ({ prompter, args }) => {
    if (args.age > 18) {
      return Promise.resolve({ allow: true })
    }
    return prompter.prompt({
      type: 'input',
      name: 'age',
      message: 'whats your age?'
    })
  }
}
```

Reshape CLI args without prompting, via `params`:

```javascript
module.exports = {
  params: ({ args }) => {
    return { moreConvenientName: args.foobamboozle }
  }
}
```

> Params and Prompts are conceptually the same thing — both produce new
> parameters — but are split into two functions (`prompt` / `params`) to
> keep the API future-proof.

### Documenting Your Generators

A special `message:` frontmatter prop can be used (in a template with no
`to:`) purely to print help text — making a generator self-documenting:

```
_templates/
  mailer/
    help/
      index.ejs.t
    new/
      prompt.js
      html.ejs.t
      text.ejs.t
```

```yaml
---
message: |
  - hygen {bold mailer} new --name [NAME]
---
```

`message:` supports inline color/style tags: `{bold ...}`, `{red ...}`,
`{underline ...}`, `{green ...}` (backed by [chalk](https://github.com/chalk/chalk#styles)).

### Selecting Parts of a Generator

Full invocation form:

```
$ hygen GENERATOR ACTION:SUBACTION
```

`SUBACTION` is a regex or plain substring matched against the action's
template files, letting you run only part of a generator:

```bash
$ hygen mailer new:text --name textual-mailer      # matches text.ejs.t
$ hygen mailer new:.*xt --name textual-mailer       # regex form
```

## 5. Templates

A hygen template is a **frontmatter** header (YAML, markdown-style
delimiters) + an **EJS** body:

```yaml
---                            <----- frontmatter section
to: app/emails/<%= name %>.html
---

Hello <%= name %>,
<%= message %>                 <----- body, ejs
(version <%= version %>)
```

### Frontmatter

Delimited top and bottom by matching `---` lines, containing YAML. The
frontmatter itself is **rendered** (its values can contain EJS), before the
body is rendered:

```yaml
---
to: app/<%=section%>/emails.js
foo: <%= bar %>
---
```

```bash
$ hygen mailer campaign --section weekend --bar ping
```

produces, behind the scenes:

```yaml
---
to: app/weekend/emails.js
foo: ping
---
```

> **Frontmatter cleans up our act.** Other generator engines use file names,
> folder structure, or arbitrary side-config files to store metadata; hygen
> uses the frontmatter, so metadata lives directly beside the template it
> describes.

### Template Body

```javascript
---
to: app/workers/<%=name%>.js
---

class <%= Name %> {
    work(){
        // your code here!
    }
}
```

`name` is a "blessed" variable: `Name` (capitalized) is automatically
available for free. To get a capitalized form of any *other* variable,
compute it explicitly in an EJS scriptlet:

```javascript
---
to: app/workers/<%=name%>.js
---
<%
 Message = message.toUpperCase()
%>

class <%= Name %> {
    work(){
        return "<%= Message %>"
    }
}
```

### Helpers and Inflections

Built-in helpers via `h`:

```javascript
class <%= Name %> {
    work(){
        return "<%= h.capitalize(message) %>"
    }
}
```

`h.inflection.*` (backed by [node.inflection](https://github.com/dreamerslab/node.inflection)):

```
pluralize( str, plural )
singularize( str, singular )
inflect( str, count, singular, plural )
camelize( str, low_first_letter )
underscore( str, all_upper_case )
humanize( str, low_first_letter )
capitalize( str )
dasherize( str )
titleize( str )
demodulize( str )
tableize( str )
classify( str )
foreign_key( str, drop_id_ubar )
ordinalize( str )
transform( str, arr )
```

### Change-Case Helpers

`h.changeCase.*` (backed by [change-case](https://github.com/blakeembrey/change-case)):

```
camel( str )
constant( str )
dot( str )
header( str )
isLower( str )
isUpper( str )
lower( str )
lcFirst( str )
no( str )
param( str )
pascal( str )
path( str )
sentence( str )
snake( str )
swap( str )
title( str )
upper( str )
```

Example (React component generator):

```yaml
---
to: components/<%= name %>/index.jsx
---
import React from 'react'

export const <%= name %> = ({ children }) => (
  <div className="<%= h.changeCase.paramCase(name) %>">{children}</div>"
)
```

With `name = HelloWorld`, compiles to:

```jsx
import React from 'react'

export const HelloWorld = ({ children }) => (
  <div className="hello-world">{children}</div>"
)
```

### Local Variables

Two ways to reference a variable:

```javascript
Hello <%= message %>
```

Bare reference — throws a reference error if `message` was never supplied
(i.e. this form implicitly requires the variable).

```javascript
Hello <%= locals.message %>
```

Via the `locals` object — safe to check for existence first:

```javascript
<% if(locals.message){ -%>
    message: <%= message %>
<% } -%>
```

`-%>` slurps the trailing newline so conditional blocks don't leave stray
blank lines in the output. (See [EJS docs](https://github.com/mde/ejs) for
full scriptlet syntax.)

### Predefined Variables

Given `hygen component new:story`:

| Variable       | Content                   | Example                       |
| -------------- | -------------------------- | ------------------------------ |
| `templates`    | Templates path (absolute)  | `/User/.../project/_templates` |
| `actionfolder` | Action path                 | `/.../component/new`           |
| `generator`    | Generator name              | `component`                    |
| `action`       | Action name                 | `new`                          |
| `subaction`    | Sub-action name             | `story`                        |
| `cwd`          | Process working directory   | `/User/.../project`            |

### Addition (writing new files)

`to:` tells hygen where to write the rendered body as a new file.
`force: true` overwrites an existing file without prompting (default
`force: false`, which prompts).

```yaml
---
to: app/index.js
force: true
---
console.log('this is index!')
```

`unless_exists: true` skips rendering entirely if the target already exists
(no prompt, no overwrite):

```yaml
---
to: app/index.js
unless_exists: true
---
will not render if target exists
```

### From & Shared Templates

`from:` loads the template *body* from a different file (relative to
`_templates`), while the frontmatter of the current file still governs
output location/behavior. The body text in the file itself is then ignored:

```yaml
---
to: app/readme.md
from: shared/docs/readme.md
---
THIS BODY IS IGNORED !!!
```

### Injection

`inject: true` (plus inject-specific frontmatter props) writes into an
*existing* file instead of creating a new one:

```yaml
---
inject: true
to: package.json
after: dependencies
skip_if: react-native-fs
---
"react-native-fs":"*",
```

> **Regular expressions everywhere promote flexibility.** `after:
> dependencies` is itself a regex, matched against file content (here it
> matches the `"dependencies":{` block of a `package.json`).

Available inject-specific properties:

- `before` / `after` — regex locating a line; the injected content is
  placed immediately before/after the matched line.
- `prepend` / `append` (boolean) — add at the very start/end of the file.
- `at_line` — inject at an exact line number.
- `skip_if` — regex/text; if a match is found anywhere in the target,
  injection is skipped entirely (idempotency guard — almost always wanted
  alongside `inject: true` to avoid double-injection on repeat runs).

### Shell

`sh:` frontmatter turns a template into a shell action:

```yaml
---
sh: "mkdir -p <%= cwd %>/given/app/shell && cat > <%= cwd %>/given/app/shell/hello.piped"
---
hello, this was piped!
```

The rendered body is *piped* into the shell command's stdin. `cwd` is
pre-available as the working directory.

`sh:` can be combined with any other action (addition or injection) as a
side effect that fires after that action completes — e.g. add a dependency
and then run install:

```yaml
---
inject: true
to: package.json
after: dependencies
skip_if: lodash
sh: cd <%= cwd %> && yarn install
---
"lodash":"*",
```

### Conditional Rendering

If `to:` evaluates to `null`, that template is skipped entirely (no file
written):

```yaml
---
to: "<%= message ? `where/to/render/${name}.js` : null %>"
---
conditionally rendering template
```

### All Frontmatter Properties

| Property         | Type         | Default   | Example                                |
|------------------|--------------|-----------|-----------------------------------------|
| `to:`            | String (url) | undefined | `my-project/readme.md`                  |
| `from:`          | String (url) | undefined | `shared/docs/readme.md`                 |
| `force:`         | Boolean      | `false`   | `true`                                  |
| `unless_exists:` | Boolean      | `false`   | `true`                                  |
| `inject:`        | Boolean      | `false`   | `true`                                  |
| `before:`        | Regex        | undefined | `devDependencies`                       |
| `after:`         | Regex        | undefined | `devDependencies`                       |
| `prepend:`       | Boolean      | undefined | `true`                                  |
| `append:`        | Boolean      | undefined | `true`                                  |
| `at_line:`       | Number       | undefined | `123`                                   |
| `eof_last:`      | Boolean      | undefined | `true`                                  |
| `skip_if:`       | Regex        | undefined | `myPackage`                             |
| `sh:`            | String       | undefined | `echo: "Hello this is a shell command!"`|

## 6. Extensibility (`.hygen.js`)

An optional `.hygen.js` file at (or above) the project root can extend
hygen's behavior:

- Add custom helper functions available to templates (beyond the built-in
  `h.*`).
- Customize the logger, template location, and shell executor.
- *(WIP at time of writing)* custom generator operations beyond the built-in
  `add` / `inject` / `shell`.

Hygen searches **upward** from the current working directory for
`.hygen.js` and stops at the first one found (bubbling-up lookup — same
strategy as the `_templates` folder search). This means:

- A single `.hygen.js` can govern an entire project.
- Sub-projects can have their own `.hygen.js` for different behavior.
- A global `.hygen.js` can live in the user's home folder.

If two or more `.hygen.js` files exist along the upward path, **the first
one found wins**; the rest are ignored.

Example: a template referencing a not-yet-existing helper `h.extended`:

```yaml
---
to: given/hygen-js/new.md
---
this demonstrates hygen loaded up .hygen.js and extended helpers.
<%= h.extended('hello') %>
```

Defining it in `.hygen.js` at the project root:

```
src/
package.json
.hygen.js
```

```javascript
module.exports = {
    helpers: {
        extended: s => s.toUpperCase()
    }
}
```

Any project code can be `require`'d and exposed here — not limited to
inline functions.

## 7. Packages (`hygen-add`)

For sharing generators across projects/teams beyond simple copy-paste,
Hygen defines **Packages** — a compiled set of generators published as an
npm module, installed via a dedicated `hygen-add` tool.

```bash
$ yarn global add hygen-add
```

Popular package example cited: [`hygen-cra`](https://github.com/jondot/hygen-CRA)
(generates component + storybook + test scaffolding for Create React App
projects).

Installing (module named `hygen-acme-generators` on npm, installed without
the `hygen-` prefix):

```bash
$ hygen-add acme-generators
```

This installs `acme-generators` (via `yarn` under the hood, so it's
versioned/locked) and **copies** its generators into the current project's
local `_templates` directory — deliberately a copy, not a live reference,
because copying is more resilient to upstream changes. The npm dependency
can then be removed, or left in place to periodically re-sync.

Installing directly from GitHub (name inferred from the repo URL, or given
explicitly):

```bash
$ hygen-add https://github.com/acme/acme-generators
$ hygen-add https://github.com/acme/archive --name acme-generators
```

Avoiding name clashes when installing multiple same-named packages:

```bash
$ hygen-add acme-react
$ hygen-add awesome-react --prefix awsm
```

## 8. Standalone Installation

Hygen can be installed as a standalone binary, without Node.js:

- macOS: via Homebrew.
- Linux / Windows / non-Homebrew macOS: download a binary from the GitHub
  releases page (auto-updated on each release).

Reasons to prefer standalone installation (per the docs): avoiding Node.js
entirely, wanting a single global binary, integrating hygen into other
software or containers, building tools that invoke hygen programmatically,
and a performance edge — "the standalone package is a little bit faster
(because it snapshots code)."

## 9. FAQ

**Why should I use Hygen?**
Addresses developer effectiveness in complex monorepo environments — teams
can "create many different generators, suiting any kind of development
workflow, and they could change, evolve, improve and adapt their generators
to their ever evolving project." Unlike Yeoman's community-package-focused
model, Hygen keeps generator code beside project code, so it goes through
the same team review process as everything else.

**How do I lowercase, uppercase, or transform variables?**
Because bodies are plain EJS/JavaScript, do it inline:

```yaml
---
to: app/reducers/<%= reducer.toLowerCase() %>.js
---
<%
    defaulted = reducer || 'my-reducer'
%>
Hello <%= defaulted %>.
```

**Can injection regexes span multiple lines?**
Hygen operates line-by-line by design. From v2.1.2 onward, `before:`,
`after:`, and `skip_if:` support multi-line regex: `skip_if` evaluates
against the whole file; `before`/`after` try a single-line match first,
falling back to multi-line matching if that fails.

**I want to use generators from a single place.**
Set `HYGEN_TMPLS` per invocation via `package.json` scripts so generators
can be run without `cd`-ing into a subfolder:

```json
"g:client": "HYGEN_TMPLS=src/client/_templates hygen",
"g:server": "HYGEN_TMPLS=src/server/_templates hygen",
```

**Should I check in my templates?**
Yes — templates are part of the codebase, meant to be reviewed and evolved
like any other source.

**Can I force hygen to always overwrite?**
Set `HYGEN_OVERWRITE=1` to bypass the overwrite-confirmation prompt:

```bash
$ HYGEN_OVERWRITE=1 hygen generator new --name foobar
```

---

# Part 2: hygen-create

Source: `github.com/ronp001/hygen-create/README.md`, plus the summary on
`hygen.io/docs/create.md`.

## 1. Overview

> `hygen-create` simplifies creation of [hygen](http://www.hygen.io)
> templates from existing projects.

**Why it exists** (per the README): "Because creating templates from
existing projects is annoying." Maintaining a generator by hand means: fix a
bug in generated code → manually port that fix back into the EJS template →
remember to re-insert the right `<%= ... %>` placeholders everywhere the
templatized word appeared. `hygen-create` automates that reverse direction:
point it at a *working, concrete* set of files, tell it which literal word
identifies the "name" to templatize, and it emits ready-to-use hygen
template files with the right EJS placeholders substituted in automatically.

In a nutshell: `hygen-create` takes a set of existing project files and uses
them to create `hygen` template files, replacing a selected word with
appropriate placeholders (such as `<%= name.toLowerCase() %>`,
`<%= h.inflection.camelize(name, true) %>`, etc.). The resulting templates
can be used as-is via `hygen <generator> new`, or hand-edited further before
use.

This does **not** install `hygen` itself — `hygen-create` only produces
template files; running them still requires hygen.

## 2. Installation

```bash
$ yarn global add hygen-create
# or
$ npm install -g hygen-create
```

## 3. Workflow

There are several steps to generating a generator:

1. Start a session: `hygen-create start <generator-name>`
2. Select files to templatize: `hygen-create add <file> ...`
3. Choose the word to replace with placeholders:
   `hygen-create usename <name>` (recommended: use a CamelCased value — see
   [Limitations](#8-limitations))
4. *(optional)* Inspect planned replacements: `hygen-create status`
5. *(optional)* [Configure the target `_templates` directory](#setting-target-templates-directory)
6. Generate the new hygen generator: `hygen-create generate`

Result: a new hygen generator is created, usable as
`hygen <generator-name> new --name <target-name>`.

## 4. Full Example Session

Starting project:

```
/projects/hello
 |-package.json
 |-dist
    |-hello.js
```

`package.json`:

```json
{
  "name": "hello",
  "version": "1.0.0",
  "description": "an application that prints hello",
  "scripts" : {
    "hello": "node dist/hello.js"
  }
}
```

`dist/hello.js`:

```javascript
// This is hello.js
console.log("Hello!")
```

Working, if trivial, package (`npm run hello` prints `Hello!`).

### Creating the generator

```bash
$ hygen-create start greeter
created hygen-create.json
```

```bash
$ hygen-create add package.json dist/hello.js
adding:  package.json
adding:  dist/hello.js
```

`hygen-create.json` (auto-created, and auto-included as one of the
templatized files):

```json
{
  "about": "This is a hygen-create definitions file. The hygen-create utility creates generators that can be executed using hygen.",
  "hygen_create_version": "0.2.0",
  "name": "greeter",
  "files_and_dirs": {
    "hygen-create.json": true,
    "package.json": true,
    "dist/hello.js": true
  },
  "templatize_using_name": null,
  "gen_parent_dir": false
}
```

Set the templatization word:

```bash
$ hygen-create usename Hello
using 'Hello' as templatization word
6 matching lines found in 3 included files
```

Inspect status:

```bash
$ hygen-create status

Using the string "Hello" to templatize files (Change using 'hygen-create usename <name>')

The following files are included in the generator:
[included] - hygen-create.json [2 lines parameterized]
[included] - package.json [3 lines parameterized]
[included] - dist/hello.js [2 lines parameterized]

No target dir: HYGEN_CREATE_TMPLS not set, HYGEN_TMPLS not set, local dir (./_templates) does not exist

Parent dir generation: OFF (the resulting generator will add content to the current directory)
```

`hygen-create status -v <file>` shows a colorized diff of the planned
substitutions per file — e.g. on line 1 of a file, `hello` → `<%=
name.toLowerCase() %>`; on line 3, `Hello` → `<%= h.capitalize(name) %>` —
i.e. it recognizes multiple *case variants* of the same templatization word
and generates the matching helper call for each.

Set the target templates directory (required — see [§7](#setting-target-templates-directory)):

```bash
$ export HYGEN_CREATE_TMPLS=/tmp/_templates
```

Generate:

```bash
$ hygen-create generate
target path:  /tmp/_templates
generating: /tmp/_templates/greeter/new/hygen-create.json.ejs.t
generating: /tmp/_templates/greeter/new/package.json.ejs.t
generating: /tmp/_templates/greeter/new/dist_hello.js.ejs.t
```

### Using the generator

```bash
$ export HYGEN_TMPLS=/tmp/_templates
$ mkdir /tmp/dev/hola-greeter
$ cd /tmp/dev/hola-greeter
$ hygen greeter new --name Hola

Loaded templates: /tmp/_templates
       added: dist/hola.js
       added: hygen-create.json
       added: package.json
```

Result: a new app that prints `Hola!` instead of `Hello!` — driven entirely
by the `--name Hola` substitution through the templatized `Hello`/`hello`
occurrences.

## 5. Iteratively Improving a Generator

Because `hygen-create.json` is itself auto-included in every generated
output, a project produced by a `hygen-create`-built generator already
contains its own `hygen-create` session state. Running `hygen-create
status` inside a *generated* project shows the generator name and
templatization word already configured — no re-setup needed to iterate.

Workflow: edit the generated code directly (e.g. fix a bug in
`dist/hola.js`), verify the fix works in place, then re-run `hygen-create`
on the *modified, concrete* project to fold that fix back into the
generator:

```bash
$ hygen-create rename greeter2
$ hygen-create generate
target path:  /tmp/_templates
generating: /tmp/_templates/greeter2/new/hygen-create.json.ejs.t
generating: /tmp/_templates/greeter2/new/package.json.ejs.t
generating: /tmp/_templates/greeter2/new/dist_hola.js.ejs.t
```

The regenerated template file reflects the hand-made fix, re-templatized:

```
$ cat /tmp/_templates/greeter2/new/dist_hola.js.ejs.t
---
to: dist/<%= name.toLowerCase() %>.js
---
// This is the improved <%= name.toLowerCase() %>.js
console.log("<%= h.capitalize(name) %>! <%= h.capitalize(name) %>!")
```

(`rename` is used here to avoid colliding with the original `greeter`
generator name; if omitted, `hygen-create` auto-renames the *previous*
generator by version-suffixing it — see next section.)

## 6. Previous Versions of Generators

When `hygen-create generate` runs and the target generator directory
already exists with *different* content than what's about to be written,
the existing directory is renamed with a numeric version suffix rather than
overwritten:

- First run of generator `mygen` → creates `<templates-path>/mygen/new`.
- Second run → renames the existing `<templates-path>/mygen/new` to
  `<templates-path>/mygen/new.1`, writes a fresh `<templates-path>/mygen/new`.
- Third run → renames to `<templates-path>/mygen.2`, and so on.

If the newly generated content is **identical** to what's already there, no
new version is created (no-op).

## 7. Configuration and Options

### Setting Target Templates Directory

`hygen-create` looks for a hygen `_templates` directory in this order:

1. `HYGEN_CREATE_TMPLS` env var, if set and pointing to an existing
   directory.
2. `HYGEN_TMPLS` env var, if set and pointing to an existing directory.
3. An existing `_templates` directory in the current working directory.

`hygen-create` will **not** create a `_templates` directory on its own —
it aborts with an error if none of the above resolves to an existing path.

### Option: Parent Directory Generation

- Enable: `hygen-create setopt --gen-parent-dir`
- Disable: `hygen-create setopt --no-parent-dir`
- `.json` field: `"gen_parent_dir"` (boolean)
- Default: **off** for sessions started with `hygen-create` v0.2.0+ (on, for
  backward compatibility, if the session was started under v0.1.x).

**Off**: the generated content is written into the current directory
directly.

**On**: the generator creates a parent directory (named after the `--name`
value passed to `hygen ... new`) and writes all content inside it.

Example, `--no-parent-dir`:

```bash
$ hygen-create start mygen
$ hygen-create add file1
$ hygen-create usename xyz
$ hygen-create setopt --no-parent-dir
$ hygen-create generate    # creates _templates/mygen

$ hygen mygen new --name hi
Loaded templates: _templates
      added: file1
```

Example, `--gen-parent-dir`:

```bash
$ hygen-create start mygen
$ hygen-create add file1
$ hygen-create usename xyz
$ hygen-create setopt --gen-parent-dir
$ hygen-create generate    # creates _templates/mygen

$ hygen mygen new --name hi
Loaded templates: _templates
      added: hi/file1
```

## 8. Limitations

**String format (CamelCase, dash-cased, etc.) for `usename`:**

TL;DR — provide a **CamelCased** value both to `hygen-create usename` and to
`hygen ... --name`.

- `hygen-create` works best when given a CamelCased string to `usename`.
  Given a CamelCased name, it correctly recognizes dash-cased,
  underscore_cased, "Title Cased", and other variant forms of that same
  word throughout the codebase. This recognition is unreliable when the
  `usename` value itself is *not* CamelCased.
- There is a tight coupling between the case format given to `hygen-create
  usename` and the case format that must later be passed to
  `hygen <generator> new --name <name>` — they should match. This may be
  relaxed in a future version, but is a hard constraint today.

## 9. Command Reference

```
$ hygen-create

  Usage: hygen-create [options] [command]

  hygen-create - create hygen templates from an existing project


  Options:

    -V, --version             output the version number
    -v, --verbose             provide more info
    -p, --project <filename>  name of session definitions file (default: hygen-create.json)
    -h, --help                output usage information


  Commands:

    start [options] <generator-name>      initiate a definition session for the generator <generator-name>
    rename <generator-name>               change the name of the target generator to <generator-name>
    add <file|dir> [file|dir...]          add files or directories to be templatized
    remove|rm <file|dir> [file|dir...]    do not templatize specified files/directories
    usename <name>                        set <name> as the templatization param
    setopt [options]                      configure options for the generator
    status|s [options] [file] [files...]  show replacements to be made in (all|specified) files
    generate|g [options]                  generate a generator from the added files
```

Copyright (C) 2018 Ron Perry. [MIT License](https://github.com/ronp001/hygen-create/blob/master/LICENSE.txt).

---

# Part 3: How the Two Fit Together

Per `hygen.io/docs/create.md`, `hygen-create` is presented as an *answer to
generator-maintenance pain*, not a competing tool:

- **Hygen** is the runtime/engine: frontmatter + EJS templates, folder
  structure = command structure, addition/injection/shell actions, prompts.
  It has no opinion about *how* a template file gets written — a human
  writes `<%= name %>` placeholders by hand, or a tool like `hygen-create`
  does it for them.
- **hygen-create** is a *forward-generator generator*: given a concrete,
  already-working set of files (not a template), it reverse-engineers a
  hygen-compatible template set by substituting a chosen literal word
  (in all its case variants) with the matching EJS/helper expression, then
  writes the result into a `_templates/<name>/new/*.ejs.t` layout hygen can
  run directly.
- The **generator-of-generators pattern** hygen itself ships
  (`hygen init self` → `hygen generator new`) is structural/manual: it
  scaffolds an *empty* generator skeleton (one boilerplate `hello.ejs.t`)
  that a human then edits by hand into a real generator.
- `hygen-create` instead **derives** a generator from a real, working
  instance — "take an existing piece of code and automatically transform
  it into a generator" — and remains re-runnable: editing the *generated*
  output and re-running `hygen-create` on it folds fixes back into the
  generator (§5 above), because the generated project retains its own
  `hygen-create.json` session state.

Both tools converge on the same file-format contract (hygen's frontmatter +
EJS template files under `_templates/<generator>/<action>/`), which is what
makes them composable: `hygen-create`'s output is directly consumable by
plain `hygen`, and a `hygen-create`-authored generator can still be
hand-edited afterward using everything in Part 1 (injection, shell actions,
prompts, helpers, `.hygen.js` extensibility) exactly as if it had been
written by hand from the start.
