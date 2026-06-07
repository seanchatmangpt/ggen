import os, glob, re

for f in glob.glob("crates/ggen-projection/tests/**/*.rs", recursive=True):
    with open(f, "r") as file:
        content = file.read()
    
    # ensure EquationContext import
    if "EquationContext" not in content and "add_receipt" in content:
        content = content.replace("use ggen_projection::{", "use ggen_projection::{EquationContext, ")
        content = content.replace("use ggen_projection::ReceiptIndex;", "use ggen_projection::{ReceiptIndex, EquationContext};")
        content = content.replace("use crate::receipt::ReceiptIndex;", "use crate::receipt::{ReceiptIndex, EquationContext};")
        content = content.replace("use crate::receipt::{CryptographicReceipt, Receipt, ReceiptIndex};", "use crate::receipt::{CryptographicReceipt, Receipt, ReceiptIndex, EquationContext};")
    
    # fix the specific tier3 cross feature manual break
    content = content.replace('receipts.add_receipt(\n        "customization-map.json".to_string(),\n        serde_json::to_string(&cust_map).unwrap().as_bytes(),\n        b"",\n    );', 'receipts.add_receipt(\n        "customization-map.json".to_string(),\n        serde_json::to_string(&cust_map).unwrap().as_bytes(),\n        b"",\n        &EquationContext::default(), None\n    );')
    
    # replace generic cases missing None 
    # e.g., add_receipt("src/cli.rs".to_string(), b"generated_1", b"template_1", &EquationContext::default()) -> add_receipt(..., None)
    content = re.sub(r'add_receipt\(([^,]+),\s*([^,]+),\s*([^,]+),\s*&EquationContext::default\(\)\)', r'add_receipt(\1, \2, \3, &EquationContext::default(), None)', content)

    # For any cases that were missing both equation and None
    content = re.sub(r'add_receipt\(([^,]+),\s*([^,]+),\s*(b?"[^"]*"|&?[a-zA-Z_0-9]+)\)', r'add_receipt(\1, \2, \3, &EquationContext::default(), None)', content)

    # For dogfood proofs, test_f8_t1_equation_enforcement* specifically needs `, None` appended because it passes `&eq`
    content = re.sub(r'add_receipt\("src/main\.rs"\.to_string\(\), b"generated_code", b"template_content", &eq\)', r'add_receipt("src/main.rs".to_string(), b"generated_code", b"template_content", &eq, None)', content)
    
    with open(f, "w") as file:
        file.write(content)
        print(f"Updated {f}")
