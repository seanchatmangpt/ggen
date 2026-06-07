import glob

for f in glob.glob("crates/ggen-projection/tests/**/*.rs", recursive=True):
    with open(f, "r") as file:
        content = file.read()
    
    content = content.replace("&::default()", "&EquationContext::default()")
    
    if "dogfood_gc003" in f:
        content = content.replace("let mut expected_eq =  {", "let mut expected_eq = EquationContext {")
        content = content.replace("ReceiptValidationError", "")
        
    with open(f, "w") as file:
        file.write(content)

