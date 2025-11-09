import os
import subprocess
import sys
from colorama import Fore, Style
from tabulate import tabulate
from yaspin import yaspin
from yaspin.spinners import Spinners
from difflib import unified_diff


scripts_dir = os.path.basename(os.path.dirname(__file__))
scripts_dir = os.path.relpath(scripts_dir, start=os.getcwd()) # get relative path from cwd, not use realpath to not leak user infos in tests outputs

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


skip_check_output = [
    "rand.rml" # TODO : set a custom seed to make it work
]

replace_outputs = [] # will be appended during runtime

should_panic_files = [
 #    "panic.rml"
]

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

replace_all_outputs = False

class CheckOutputOutput:
    def __init__(self, success : bool, diff : str | None):
        self.success = success
        self.diff = diff

def check_output(command : COMMAND, filename : str, out_run : bytes) -> CheckOutputOutput:
    if filename in skip_check_output:
        return CheckOutputOutput(True, None)
    out_run = out_run.decode()

    # TODO : in the future, put only one (need to fix formatting before that)
    match command:
        case COMMAND.COMPILE:
            command_suffix = "compiler"
        case COMMAND.INTERPRET:
            command_suffix = "interpreter"
        case _:
            return CheckOutputOutput(True, None)
    output_filename = os.path.splitext(filename)[0] + "." + command_suffix + ".txt"
    output_filename = os.path.join("output_tests", output_filename)
    output_filename = os.path.join(scripts_dir, output_filename)

    if os.path.exists(output_filename) and not (filename in replace_outputs) and not replace_all_outputs:
        old_output_file = open(output_filename, mode="r")
        old_output_content = old_output_file.read()
        if old_output_content != out_run:
            diff = unified_diff(old_output_content.splitlines(), out_run.splitlines(), lineterm='')
            diff = '\n' + '\n'.join(list(diff)) + '\n'
            return CheckOutputOutput(False, diff)
    else:
        new_output_file = open(output_filename, mode="w")
        new_output_file.write(out_run)
    
    return CheckOutputOutput(True, None)

    

class WHEN_ERROR:
    INTERPRETER = 1
    COMPILER = 2
    COMPILER_RUN = 3
    CHECK = 4
    OUTPUT_CHECK = 5

class TestFileOutput:
    def __init__(self, output_print : str, out_process : bytes, error_message : str | None, when_error : WHEN_ERROR | None):
        self.output_print = output_print
        self.error_message = error_message
        self.when_error = when_error
        self.out_process = out_process

def test_file(filename : str, command : COMMAND, is_release_mode : bool, is_debuginfo : bool):
    cmd = f"{exe_path} "
    match command:
        case COMMAND.COMPILE:
            cmd += "compile -o out -Ltests "
            if is_debuginfo:
                cmd += "-g "
        case COMMAND.INTERPRET:
            cmd += "interpret "
        case COMMAND.CHECK:
            cmd += "check "
    
    cmd += os.path.join(scripts_dir, filename)

    env = os.environ.copy()
    env["LD_LIBRARY_PATH"] = scripts_dir + ":" + env.get("LD_LIBRARY_PATH", "")

    pipe = subprocess.Popen(cmd, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
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
        #return "❌", error_message, is_error_in_run, out
        match command:
            case COMMAND.COMPILE:
                when_error = WHEN_ERROR.COMPILER
            case COMMAND.INTERPRET:
                when_error = WHEN_ERROR.INTERPRETER
            case COMMAND.CHECK:
                when_error = WHEN_ERROR.CHECK
        return TestFileOutput("❌", out, error_message, when_error)

    match command:
        case COMMAND.COMPILE:
            cmd_run = "./out"
            env = os.environ.copy()
            env["LD_LIBRARY_PATH"] = scripts_dir + ":" + env.get("LD_LIBRARY_PATH", "")
            pipe = subprocess.Popen(cmd_run, shell=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, env=env)
            out_run, _ = pipe.communicate() # TODO : should it also use the output of the compiling ?
            return_code_run = pipe.returncode
            is_err_run = is_error(return_code_run)

            if is_err_run:
                return TestFileOutput("❌", out_run, "Error when running the compiled output", WHEN_ERROR.COMPILER_RUN)
            
        case COMMAND.INTERPRET | COMMAND.CHECK:
            out_run = out
    
    check_output_out = check_output(command, filename, out_run)
    if not check_output_out.success:
        return TestFileOutput("❌", check_output_out.diff.encode(), "Error when checking output", WHEN_ERROR.OUTPUT_CHECK)
    # return "✅", "", out
    return TestFileOutput("✅", out, None, None)

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
            #output_print, error_message, is_error_in_run, out = test_file(file, command, is_release_mode, is_debuginfo)
            output_test = test_file(file, command, is_release_mode, is_debuginfo)
            if command == COMMAND.COMPILE:
                summary_compiler.append(output_test.output_print)
            else:
                summary_interpreter.append(output_test.output_print)
                
            print(output_test.output_print)
            if output_test.error_message != None:
                print_error(file, output_test.error_message, output_test.out_process)
            
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
    for i in range(len(sys.argv)):
        arg = sys.argv[i]
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
        elif arg == "--replace-output":
            i += 1
            replace_outputs.append(sys.argv[i])
        elif arg == "--replace-all-outputs":
            replace_all_outputs = True
    
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

            
