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
exe_path = None

excluded_files = [
    "lexer_error.rml",
    "parser_error.rml",
    "types_error.rml",
    "interpreter_error.rml",
]

should_panic_files = {
 #    "panic.rml"
}

def is_error(return_code : int) -> bool:
    return return_code != 0

class COMMAND:
    ALL = 1
    INTERPRET = 2
    COMPILE = 3
    CHECK = 4


def get_error_message(command : COMMAND, return_code : int, filename : str, is_release_mode : bool) -> str:
    match command:
        case COMMAND.COMPILE:
            rustaml_name = "compiler"
        case COMMAND.INTERPRET:
            rustaml_name = "interpreter"
        case COMMAND.CHECK:
            rustaml_name = "checker"
        case _:
            sys.exit(f"Unknown command {command}")

    match return_code:
        case -6:
            return (f" Stack overflow of the {rustaml_name}" if not is_release_mode else "Error or Stack overflow (try in debug mode to have more infos)")
        case 1:
            return f"the {rustaml_name} failed with an error"
        case 134:
            return "the compiler panicked"
        case 101:
            filename_without_ex, _ = os.path.splitext(filename)
            error_filename = filename_without_ex + "_error.ll"
            if os.path.exists(error_filename):
                return f"LLVM ERROR (check the {error_filename}) file for more details)"
            else:
                return get_error_message(command, 1, filename, is_release_mode)
        case _:
            sys.exit(f"Unknown error return code {return_code}")

def test_file(filename : str, command : COMMAND, is_release_mode : bool, is_debuginfo : bool):
    cmd = f"{exe_path} "
    match command:
        case COMMAND.COMPILE:
            cmd += "compile -o out "
            if is_debuginfo:
                cmd += "-g "
        case COMMAND.INTERPRET:
            cmd += "interpret "
        case COMMAND.CHECK:
            cmd += "check "
    
    cmd += os.path.join(scripts_dir, filename)

    pipe = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    out, _ = pipe.communicate()
    # print(out)
    return_code = pipe.returncode

    should_panic = False
    if filename in should_panic_files:
        should_panic = True

    is_err = is_error(return_code)
    should_print_error_message = is_err if command != COMMAND.INTERPRET else (not should_panic and is_err) or (should_panic and not is_err)
    if should_print_error_message:
        error_message = get_error_message(command, return_code, filename, is_release_mode)
        return "❌", error_message, out
        
    return "✅", "", out


def print_error(filename: str, error_message : str, out : bytes):
    print(f"{Fore.RED}ERROR :{Style.RESET_ALL} {error_message}")
    print(f"{out.decode("utf-8", errors="replace")}")



def get_command_type_str(command : COMMAND) -> str:
    # ALL will never be passed
    match command:
        case COMMAND.INTERPRET:
            return "INTERPRETED"
        case COMMAND.COMPILE:
            return "COMPILED"
        case COMMAND.CHECK:
            return "CHECK"
    

def test_all_files(command : COMMAND, is_release_mode: bool, is_debuginfo : bool):
    command_type_str = get_command_type_str(command)
    print(f"--\n{Fore.BLUE}START TESTING {command_type_str}{Style.RESET_ALL}")

    summary_filenames.clear()
    for file in os.listdir(scripts_dir):
        _, extension = os.path.splitext(file)
        if extension == ".rml" and file not in excluded_files:
            print(f"{file} ", end='', flush=True)
            summary_filenames.append(file)
            output_print, error_message, out = test_file(file, command, is_release_mode, is_debuginfo)
            if command == COMMAND.COMPILE:
                summary_compiler.append(output_print)
            else:
                summary_interpreter.append(output_print)
                
            print(output_print)
            if error_message != "":
                print_error(file, error_message, out)
            
    print(f"{Fore.BLUE}END TESTING {command_type_str}{Style.RESET_ALL}")

def ensure_compiler_built(is_release_mode : bool):
    with yaspin(Spinners.dots, text="BUILDING RUSTAML...") as spinner:
        print("BUILDING RUSTAML... ", end='')
        pipe = subprocess.Popen("cargo build -F native -F stack-expand -F gc-test-collect" + (" --release" if is_release_mode else ""), shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        
        
        out, _ = pipe.communicate()

        if is_error(pipe.returncode):
            sys.exit(f"failed when building the compiler :\n {out.decode("utf-8", errors="replace")}")
        



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
    is_release_mode = False
    is_debuginfo = False
    for arg in sys.argv:
        if arg == "--interpret":
            command = COMMAND.INTERPRET
        elif arg == "--compile":
            command = COMMAND.COMPILE
        elif arg == "--all":
            command = COMMAND.ALL
        elif arg == "--check":
            command = COMMAND.CHECK
        elif arg == "--release":
            is_release_mode = True
        elif arg == "--debug":
            is_release_mode = False
        elif arg == "--debuginfo":
            is_debuginfo = True
    
    ensure_compiler_built(is_release_mode)
    

    exe_path = os.path.join(root_project_dir, "target", "release" if is_release_mode else "debug", "rustaml")
    
    match command:
        case COMMAND.ALL:
            test_all_files(COMMAND.INTERPRET, is_release_mode, is_debuginfo)
            test_all_files(COMMAND.COMPILE, is_release_mode, is_debuginfo)
        case COMMAND.INTERPRET:
            test_all_files(COMMAND.INTERPRET, is_release_mode, is_debuginfo)
        case COMMAND.COMPILE:
            test_all_files(COMMAND.COMPILE, is_release_mode, is_debuginfo)
        case COMMAND.CHECK:
            test_all_files(COMMAND.CHECK, is_release_mode, is_debuginfo)
    print_summary()

            
