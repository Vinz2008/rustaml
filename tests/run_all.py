import os
import subprocess
import sys
from colorama import Fore, Style
from tabulate import tabulate
from yaspin import yaspin
from yaspin.spinners import Spinners


scripts_dir = os.path.dirname(os.path.realpath(__file__))
root_project_dir = os.getcwd()

if "Cargo.toml" not in os.listdir(root_project_dir):
    root_project_dir = os.path.join(os.getcwd(), os.pardir)

# TODO : add release mode support ?
exe_path = os.path.join(root_project_dir, "target", "debug", "rustaml")

excluded_files = [
    "parser_error.rml",
    "interpreter_error.rml"
]

def is_error(return_code : int) -> bool:
    return return_code != 0

def get_error_message(return_code : int, filename : str) -> str:
    match return_code:
        case 1:
            return "the compiler failed with an error"
        case 134:
            return "the compiler panicked"
        case 101:
            filename_without_ex, _ = os.path.splitext(filename)
            error_filename = filename_without_ex + "_error.ll"
            if os.path.exists(error_filename):
                return f"LLVM ERROR (check the {error_filename}) file for more details)"
            else:
                return get_error_message(1, filename)
        case _:
            sys.exit(f"Unknown error return code {return_code}")

def test_file(filename : str, is_compiling: bool):
    cmd = f"{exe_path} "
    if is_compiling:
        cmd += "compile -o out "
    else:
        cmd += "interpret "
    
    cmd += os.path.join(scripts_dir, filename)

    pipe = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out, _ = pipe.communicate()
    # print(out)
    return_code = pipe.returncode


    if is_error(return_code):
        error_message = get_error_message(return_code, filename)
        return "❌", error_message, out
        
    return "✅", "", out


def print_error(filename: str, error_message : str, out : bytes):
    print(f"{Fore.RED}ERROR :{Style.RESET_ALL} {error_message}")
    print(f"{out.decode("utf-8", errors="replace")}")


def test_all_files(is_compiling : bool):
    print(f"--\n{Fore.BLUE}START TESTING {"COMPILED" if is_compiling else "INTERPRETED"}{Style.RESET_ALL}")

    summary_filenames.clear()
    for file in os.listdir(scripts_dir):
        _, extension = os.path.splitext(file)
        if extension == ".rml" and file not in excluded_files:
            print(f"{file} ", end='', flush=True)
            summary_filenames.append(file)
            output_print, error_message, out = test_file(file, is_compiling)
            if is_compiling:
                summary_compiler.append(output_print)
            else:
                summary_interpreter.append(output_print)
                
            print(output_print)
            if error_message != "":
                print_error(file, error_message, out)
            
    print(f"{Fore.BLUE}END TESTING {"COMPILED" if is_compiling else "INTERPRETED"}{Style.RESET_ALL}")

def ensure_compiler_built():
    with yaspin(Spinners.dots, text="BUILDING RUSTAML...") as spinner:
        print("BUILDING RUSTAML... ", end='')
        pipe = subprocess.Popen("cargo build -F native -F stack-expand -F gc-test-collect", shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        
        
        out, _ = pipe.communicate()

        if is_error(pipe.returncode):
            sys.exit(f"failed when building the compiler :\n {out.decode("utf-8", errors="replace")}")
        

class COMMAND:
    ALL = 1
    INTERPRET = 2
    COMPILE = 3

summary_filenames = []
summary_compiler = []
summary_interpreter = []

def print_summary():
    first_row = ["Filename"]
    if summary_interpreter != []:
        first_row.append("Interpreted")
    if summary_compiler != []:
        first_row.append("Compiled") 

    table = [first_row]
    for i in range(max(len(summary_compiler), len(summary_interpreter))):
        current_row = [summary_filenames[i]]
        if summary_interpreter != []:
            current_row.append(summary_interpreter[i])
        if summary_compiler != []:
            current_row.append(summary_compiler[i])
        table.append(current_row)

    print(tabulate(table))


if __name__ == '__main__':
    command = COMMAND.ALL
    for arg in sys.argv:
        if arg == "--interpret":
            command = COMMAND.INTERPRET
        elif arg == "--compile":
            command = COMMAND.COMPILE
        elif arg == "--all":
            command = COMMAND.ALL
    
    ensure_compiler_built()
    match command:
        case COMMAND.ALL:
            test_all_files(False)
            test_all_files(True)
        case COMMAND.INTERPRET:
            test_all_files(False)
        case COMMAND.COMPILE:
            test_all_files(True)
    print_summary()

            
