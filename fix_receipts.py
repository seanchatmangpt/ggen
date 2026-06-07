import os, glob, re

for f in glob.glob("crates/ggen-projection/**/*.rs", recursive=True):
    with open(f, "r") as file:
        content = file.read()
    
    # Check if there are instances of add_receipt(path, content)
    # We want to replace it with add_receipt(path, content, b"") 
    # as a simple fix for tests.
    new_content = re.sub(r'add_receipt\(([^,]+),\s*(b?"[^"]*"|&?[a-zA-Z_0-9]+)\)', r'add_receipt(\1, \2, b"")', content)
    
    if new_content != content:
        with open(f, "w") as file:
            file.write(new_content)
        print(f"Updated {f}")
