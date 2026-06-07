import glob, re

for f in glob.glob("crates/ggen-projection/tests/**/*.rs", recursive=True):
    with open(f, "r") as file:
        content = file.read()
    
    # ensure EquationContext import
    if "EquationContext" not in content and "add_receipt" in content:
        content = content.replace("use ggen_projection::{", "use ggen_projection::{EquationContext, ")
        content = content.replace("use ggen_projection::ReceiptIndex;", "use ggen_projection::{ReceiptIndex, EquationContext};")
        content = content.replace("use crate::receipt::ReceiptIndex;", "use crate::receipt::{ReceiptIndex, EquationContext};")
        content = content.replace("use crate::receipt::{CryptographicReceipt, Receipt, ReceiptIndex};", "use crate::receipt::{CryptographicReceipt, Receipt, ReceiptIndex, EquationContext};")

    # In case there's an issue where 'EquationContext' is used but not imported
    if "EquationContext::" in content and "EquationContext," not in content and "{EquationContext}" not in content and " EquationContext " not in content:
        if "use ggen_projection::{" in content:
            content = content.replace("use ggen_projection::{", "use ggen_projection::{EquationContext, ")
        else:
            content = "use ggen_projection::EquationContext;\n" + content
    
    with open(f, "w") as file:
        file.write(content)
