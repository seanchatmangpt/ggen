import glob, re

for f in glob.glob("crates/ggen-projection/tests/**/*.rs", recursive=True):
    with open(f, "r") as file:
        content = file.read()
    
    if "use ggen_projection::{EquationContext, \nuse ggen_projection::{EquationContext," in content:
        # manual cleanup is safer via regex
        pass
        
    content = re.sub(r'use ggen_projection::\{EquationContext, \nuse ggen_projection::\{EquationContext, ', r'use ggen_projection::{EquationContext, ', content)
    
    content = re.sub(r'EquationContext,\s*([\s\S]*?)EquationContext', r'EquationContext, \1', content)
    
    with open(f, "w") as file:
        file.write(content)
