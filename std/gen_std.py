import os
from pathlib import Path

current_dir = Path(os.getcwd())
out_path = current_dir / "std.c"


out_content = ""

f = open(current_dir / "prelude.h")
out_content += f.read()
f.close()


for file in os.listdir(current_dir):
    if file.endswith(".c") and file != "std.c":
        print(file)
        f = open(file, "r")
        file_content = f.read()
        f.close()
        out_content += file_content


out_f = open(out_path, "w")
out_f.write(out_content)
out_f.close()