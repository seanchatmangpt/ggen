#!/bin/bash
# Basic Template Generation Example - Interactive Tutorial
# Demonstrates ggen's core template functionality

set -e  # Exit on error

# Colors for output
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
RED='\033[0;31m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
MAGENTA='\033[0;35m'
NC='\033[0m' # No Color

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$SCRIPT_DIR"

print_header() {
    echo -e "\n${BLUE}════════════════════════════════════════════════════════${NC}"
    echo -e "${BLUE}  $1${NC}"
    echo -e "${BLUE}════════════════════════════════════════════════════════${NC}\n"
}

print_success() {
    echo -e "${GREEN}✓${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

print_error() {
    echo -e "${RED}✗${NC} $1"
}

print_info() {
    echo -e "${CYAN}→${NC} $1"
}

print_section() {
    echo -e "\n${MAGENTA}▶ $1${NC}"
}

# Show welcome message
show_welcome() {
    clear
    print_header "ggen Basic Template Generation Tutorial"

    cat << 'EOF'
This interactive tutorial demonstrates ggen's core template functionality:

  • Template Anatomy (YAML frontmatter + Tera templating)
  • Variable Substitution and Filters
  • Conditional Rendering
  • Code Generation Patterns

The templates in this example generate production-quality Rust code.

EOF

    echo -e "${CYAN}Press Enter to continue...${NC}"
    read
}

# Show template structure
show_template_structure() {
    print_header "Template Anatomy"

    cat << 'EOF'
Each ggen template consists of TWO parts:

┌─────────────────────────────────────────┐
│ YAML FRONTMATTER                        │
│ ---                                     │
│ to: generated/{{ name | snake_case }}.rs│
│ vars:                                   │
│   name: string                          │
│   description: string                   │
│ ---                                     │
├─────────────────────────────────────────┤
│ TERA TEMPLATE BODY                      │
│                                         │
│ /// {{ description }}                   │
│ pub mod {{ name | snake_case }} {       │
│     // Your code here                   │
│ }                                       │
└─────────────────────────────────────────┘

EOF

    print_section "Key Components:"
    echo ""
    echo "  1. YAML Frontmatter (between --- markers)"
    echo "     • Defines configuration (output path, variables, etc.)"
    echo "     • Uses 'to:' to specify where generated code goes"
    echo "     • Declares required 'vars:' with types"
    echo ""
    echo "  2. Tera Template Body"
    echo "     • Contains actual code with {{ variable }} substitutions"
    echo "     • Supports filters like {{ name | snake_case }}"
    echo "     • Can include conditionals {% if %} and loops {% for %}"

    echo -e "\n${CYAN}Press Enter to see a real template...${NC}"
    read
}

# Show simple template example
show_simple_template() {
    print_header "Simple Template: rust-module.tmpl"

    print_section "Template Source:"
    echo ""

    # Show template with syntax highlighting
    if command -v bat &> /dev/null; then
        bat --style=numbers --color=always templates/rust-module.tmpl | head -40
    else
        head -40 templates/rust-module.tmpl
    fi

    echo ""
    print_section "What This Template Does:"
    echo ""
    echo "  • Generates a Rust module with error handling"
    echo "  • Uses snake_case filter for module names"
    echo "  • Uses pascal_case filter for type names"
    echo "  • Includes documentation and tests"
    echo "  • Creates: generated/{{ name | snake_case }}.rs"

    echo -e "\n${CYAN}Press Enter to see a complex template...${NC}"
    read
}

# Show complex template example
show_complex_template() {
    print_header "Complex Template: rust-struct.tmpl"

    print_section "Template Source (first 60 lines):"
    echo ""

    if command -v bat &> /dev/null; then
        bat --style=numbers --color=always templates/rust-struct.tmpl | head -60
    else
        head -60 templates/rust-struct.tmpl
    fi

    echo ""
    print_section "Advanced Features Demonstrated:"
    echo ""
    echo "  • Conditional rendering: {% if has_id %}"
    echo "  • Loops: {% for field in fields %}"
    echo "  • Multiple variable types (string, boolean, array)"
    echo "  • Builder pattern generation"
    echo "  • Automatic getter/setter methods"
    echo "  • Timestamp handling"

    echo -e "\n${CYAN}Press Enter to see variable substitution...${NC}"
    read
}

# Explain variable substitution
explain_variables() {
    print_header "Variable Substitution"

    cat << 'EOF'
Templates use Tera syntax for variable substitution:

┌─────────────────────────────────────────────────────────┐
│ SYNTAX                    │ RESULT                      │
├─────────────────────────────────────────────────────────┤
│ {{ name }}                │ Direct substitution         │
│ {{ name | snake_case }}   │ Converts to snake_case      │
│ {{ name | pascal_case }}  │ Converts to PascalCase      │
│ {{ name | upper }}        │ Converts to UPPERCASE       │
│ {{ name | lower }}        │ Converts to lowercase       │
└─────────────────────────────────────────────────────────┘

EXAMPLE TRANSFORMATION:
  Input:     name = "UserService"

  Outputs:
    • {{ name }}              = "UserService"
    • {{ name | snake_case }} = "user_service"
    • {{ name | pascal_case }}= "UserService"
    • {{ name | upper }}      = "USERSERVICE"
    • {{ name | lower }}      = "userservice"

EOF

    print_section "Variable Types in Frontmatter:"
    echo ""
    echo "  vars:"
    echo "    name: string              # Simple string"
    echo "    has_id: boolean          # true/false"
    echo "    count: number            # Numeric value"
    echo "    fields: array            # List of items"
    echo "    config: object           # Nested structure"

    echo -e "\n${CYAN}Press Enter to see conditional rendering...${NC}"
    read
}

# Explain conditional rendering
explain_conditionals() {
    print_header "Conditional Rendering"

    cat << 'EOF'
Templates can include code conditionally:

┌─────────────────────────────────────────────────────────┐
│ TEMPLATE CODE                                           │
├─────────────────────────────────────────────────────────┤
│ pub struct User {                                       │
│     {% if has_id %}                                     │
│     pub id: u64,                                        │
│     {% endif %}                                         │
│                                                         │
│     {% if has_timestamps %}                             │
│     pub created_at: SystemTime,                         │
│     pub updated_at: SystemTime,                         │
│     {% endif %}                                         │
│ }                                                       │
└─────────────────────────────────────────────────────────┘

EXAMPLE 1 - has_id=true, has_timestamps=false:
  pub struct User {
      pub id: u64,
  }

EXAMPLE 2 - has_id=true, has_timestamps=true:
  pub struct User {
      pub id: u64,
      pub created_at: SystemTime,
      pub updated_at: SystemTime,
  }

EOF

    print_section "Available Conditional Operators:"
    echo ""
    echo "  {% if condition %}...{% endif %}          # If block"
    echo "  {% if x %}...{% else %}...{% endif %}     # If-else"
    echo "  {% if x and y %}...{% endif %}            # Logical AND"
    echo "  {% if x or y %}...{% endif %}             # Logical OR"
    echo "  {% if not x %}...{% endif %}              # Logical NOT"

    echo -e "\n${CYAN}Press Enter to see loops...${NC}"
    read
}

# Explain loops
explain_loops() {
    print_header "Loop Rendering"

    cat << 'EOF'
Templates can iterate over arrays:

┌─────────────────────────────────────────────────────────┐
│ TEMPLATE CODE                                           │
├─────────────────────────────────────────────────────────┤
│ {% for field in fields %}                               │
│ /// {{ field.doc }}                                     │
│ pub {{ field.name }}: {{ field.type }},                 │
│ {% endfor %}                                            │
└─────────────────────────────────────────────────────────┘

INPUT VARIABLES:
  fields:
    - name: username
      type: String
      doc: User login name
    - name: email
      type: String
      doc: User email address
    - name: is_active
      type: bool
      doc: Whether account is active

GENERATED OUTPUT:
  /// User login name
  pub username: String,
  /// User email address
  pub email: String,
  /// Whether account is active
  pub is_active: bool,

EOF

    print_section "Loop Features:"
    echo ""
    echo "  • Access fields: {{ item.field }}"
    echo "  • Check position: {% if loop.first %}"
    echo "  • Check last: {% if loop.last %}"
    echo "  • Get index: {{ loop.index }}"
    echo "  • Nested loops supported"

    echo -e "\n${CYAN}Press Enter to see template best practices...${NC}"
    read
}

# Show best practices
show_best_practices() {
    print_header "Template Best Practices"

    cat << 'EOF'
✓ DO:
  • Use descriptive variable names
  • Include documentation comments
  • Apply appropriate filters (snake_case for Rust)
  • Define all required vars in frontmatter
  • Test with different variable combinations
  • Use conditionals to handle optional features
  • Include error handling in generated code

✗ DON'T:
  • Hardcode values that should be variables
  • Generate invalid code for any input
  • Forget to document complex templates
  • Mix multiple languages in one template
  • Leave unused variables in frontmatter
  • Generate unsafe code without warnings

EOF

    print_section "Code Quality Checklist:"
    echo ""
    echo "  □ Generated code compiles without errors"
    echo "  □ All variables are properly substituted"
    echo "  □ Filters are applied correctly"
    echo "  □ Conditionals work as expected"
    echo "  □ Documentation is included"
    echo "  □ Tests are generated (if applicable)"
    echo "  □ Error handling is present"

    echo -e "\n${CYAN}Press Enter to see usage examples...${NC}"
    read
}

# Show usage examples
show_usage_examples() {
    print_header "Usage Examples"

    print_section "Example 1: Rendering in Rust Code"
    cat << 'EOF'

use ggen_core::template::Template;
use tera::{Context, Tera};

// Parse template
let input = std::fs::read_to_string("rust-module.tmpl")?;
let mut template = Template::parse(&input)?;

// Set up variables
let mut tera = Tera::default();
let mut vars = Context::new();
vars.insert("name", "UserService");
vars.insert("description", "User management module");

// Render
template.render_frontmatter(&mut tera, &vars)?;
let output = template.render(&mut tera, &vars)?;

// Save to file specified in frontmatter
let output_path = template.front.to.unwrap();
std::fs::write(output_path, output)?;

EOF

    print_section "Example 2: Complex Variables"
    cat << 'EOF'

use serde_json::json;

let mut vars = Context::new();
vars.insert("name", "User");
vars.insert("description", "User entity");
vars.insert("has_id", &true);
vars.insert("has_timestamps", &true);

// Array of field objects
vars.insert("fields", &json!([
    {
        "name": "username",
        "type": "String",
        "doc": "User login name"
    },
    {
        "name": "email",
        "type": "String",
        "doc": "User email address"
    },
    {
        "name": "is_active",
        "type": "bool",
        "doc": "Whether the user account is active"
    }
]));

EOF

    echo -e "\n${CYAN}Press Enter to finish...${NC}"
    read
}

# Show summary
show_summary() {
    print_header "Summary"

    cat << 'EOF'
You've learned about ggen's template system:

✓ Template Structure
  • YAML frontmatter for configuration
  • Tera template body for code generation

✓ Core Features
  • Variable substitution with {{ var }}
  • Filters for text transformation
  • Conditional rendering with {% if %}
  • Loops with {% for %}

✓ Best Practices
  • Descriptive variable names
  • Comprehensive documentation
  • Proper error handling
  • Code quality validation

EOF

    print_section "Next Steps:"
    echo ""
    echo "  1. Examine the example templates in templates/"
    echo "  2. Modify variables and see how output changes"
    echo "  3. Create your own templates"
    echo "  4. Explore advanced examples in ../conditional-generation/"
    echo "  5. Read ggen documentation for more features"

    echo ""
    print_success "Tutorial complete!"
}

# Show template files
show_templates() {
    print_header "Template Files"

    for template in templates/*.tmpl; do
        if [ -f "$template" ]; then
            echo -e "\n${YELLOW}📄 $(basename "$template")${NC}"
            echo -e "${BLUE}────────────────────────────────────────────${NC}"

            if command -v bat &> /dev/null; then
                bat --style=plain --color=always "$template"
            else
                cat "$template"
            fi

            echo -e "${BLUE}────────────────────────────────────────────${NC}"
        fi
    done
}

# Explain template features
explain_features() {
    print_header "Template Features Explained"

    print_section "1. Frontmatter Fields"
    echo ""
    echo "  to: String             - Output file path (supports {{ vars }})"
    echo "  from: String           - Read body from external file"
    echo "  vars: Map              - Required variables with types"
    echo "  force: Boolean         - Overwrite existing files"
    echo "  inject: Boolean        - Inject into existing files"
    echo "  skip_if: String        - Skip if pattern found"
    echo ""

    print_section "2. Tera Filters"
    echo ""
    echo "  {{ var | snake_case }} - Converts to snake_case"
    echo "  {{ var | pascal_case }}- Converts to PascalCase"
    echo "  {{ var | upper }}      - Converts to UPPERCASE"
    echo "  {{ var | lower }}      - Converts to lowercase"
    echo "  {{ var | capitalize }} - Capitalizes first letter"
    echo "  {{ var | trim }}       - Removes whitespace"
    echo ""

    print_section "3. Control Structures"
    echo ""
    echo "  {% if condition %}     - Conditional rendering"
    echo "  {% for item in list %} - Loop rendering"
    echo "  {% set x = value %}    - Variable assignment"
    echo "  {{ comment }}          - Comments (not rendered)"
    echo ""

    print_section "4. Variable Types"
    echo ""
    echo "  string                 - Text values"
    echo "  boolean                - true/false"
    echo "  number                 - Numeric values"
    echo "  array                  - Lists"
    echo "  object                 - Nested structures"
}

# Show usage
show_usage() {
    cat << EOF
ggen Basic Template Generation Example

Usage: $0 [command]

Commands:
    tutorial    Run interactive tutorial (default)
    show        Show all template files
    explain     Explain template features
    help        Show this help message

Examples:
    $0              # Run interactive tutorial
    $0 show         # Display template files
    $0 explain      # Show feature reference

EOF
}

# Main script logic
case "${1:-tutorial}" in
    tutorial)
        show_welcome
        show_template_structure
        show_simple_template
        show_complex_template
        explain_variables
        explain_conditionals
        explain_loops
        show_best_practices
        show_usage_examples
        show_summary
        ;;
    show)
        show_templates
        ;;
    explain)
        explain_features
        ;;
    help|--help|-h)
        show_usage
        ;;
    *)
        print_error "Unknown command: $1"
        echo ""
        show_usage
        exit 1
        ;;
esac
