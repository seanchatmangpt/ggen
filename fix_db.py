import os

# fix symbol.rs
with open("src/symbol.rs", "r") as f:
    content = f.read()
content = content.replace("symbols.push(Symbol {", "symbols.push(Symbol { file_path: _path.to_string_lossy().to_string(),")

with open("src/symbol.rs", "w") as f:
    f.write(content)

# fix db.rs
with open("src/db.rs", "r") as f:
    content = f.read()

content = content.replace("class.as_str()", "format!(\"{:?}\", class)")
content = content.replace("Classification::DOC_ONLY", "Classification::DocOnly")

with open("src/db.rs", "w") as f:
    f.write(content)
