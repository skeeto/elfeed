#!/bin/bash

# * makem.sh --- Script to aid building and testing Emacs Lisp packages

# https://github.com/alphapapa/makem.sh

# * Commentary:

# makem.sh is a script helps to build, lint, and test Emacs Lisp
# packages.  It aims to make linting and testing as simple as possible
# without requiring per-package configuration.

# It works similarly to a Makefile in that "rules" are called to
# perform actions such as byte-compiling, linting, testing, etc.

# Source and test files are discovered automatically from the
# project's Git repo, and package dependencies within them are parsed
# automatically.

# Output is simple: by default, there is no output unless errors
# occur.  With increasing verbosity levels, more detail gives positive
# feedback.  Output is colored by default to make reading easy.

# The script can run Emacs with the developer's local Emacs
# configuration, or with a clean, "sandbox" configuration that can be
# optionally removed afterward.  This is especially helpful when
# upstream dependencies may have released new versions that differ
# from those installed in the developer's personal configuration.

# * License:

# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.

# * Functions

function usage {
    cat <<EOF
$0 [OPTIONS] RULES...

Linter- and test-specific rules will error when their linters or tests
are not found.  With -vv, rules that run multiple rules will show a
message for unavailable linters or tests.

Rules:
  all      Run all lints and tests.
  compile  Byte-compile source files.

  lint           Run all linters, ignoring unavailable ones.
  lint-checkdoc  Run checkdoc.
  lint-compile   Byte-compile source files with warnings as errors.
  lint-indent    Run indent-lint.
  lint-package   Run package-lint.

  test, tests     Run all tests, ignoring missing test types.
  test-buttercup  Run Buttercup tests.
  test-ert        Run ERT tests.

  batch        Run Emacs in batch mode, loading project source and test files
               automatically, with remaining args (after "--") passed to Emacs.
  interactive  Run Emacs interactively, loading project source and test files
               automatically.

Options:
  -d, --debug    Print debug info.
  -h, --help     I need somebody!
  -v, --verbose  Increase verbosity, up to -vv.

  --debug-load-path  Print load-path from inside Emacs.

  -E, --emacs PATH  Run Emacs at PATH.
  -f, --file FILE   Check FILE in addition to discovered files.

  --no-color        Disable color output.
  -C, --no-compile  Don't compile files automatically.

Sandbox options:
  -s, --sandbox          Run Emacs with an empty config in a temporary
                         directory (removing directory on exit).
  -S, --sandbox-dir DIR  Use DIR for the sandbox directory (leaving it
                         on exit).  Implies -s.
  --install-deps         Automatically install package dependencies.
  --install-linters      Automatically install linters.
  -i, --install PACKAGE  Install PACKAGE before running rules.

Source files are automatically discovered from git, or may be
specified with options.  Package dependencies are discovered from
"Package-Requires" headers in source files and from a Cask file.
EOF
}

# ** Elisp

# These functions return a path to an elisp file which can be loaded
# by Emacs on the command line with -l or --load.

function elisp-buttercup-file {
    # The function buttercup-run, which is called by buttercup-run-discover,
    # signals an error if it can't find any Buttercup test suites.  We don't
    # want that to be an error, so we define advice which ignores that error.
    local file=$(mktemp)
    cat >$file <<EOF
(defun makem-buttercup-run (oldfun &rest r)
  "Call buttercup-run only if \`buttercup-suites' is non-nil."
  (when buttercup-suites
    (apply oldfun r)))

(advice-add #'buttercup-run :around #'makem-buttercup-run)
EOF
    echo $file
}

function elisp-checkdoc-file {
    # Since checkdoc doesn't have a batch function that exits non-zero
    # when errors are found, we make one.
    local file=$(mktemp)

    cat >$file <<EOF
(defvar makem-checkdoc-errors-p nil)

(defun makem-checkdoc-files-and-exit ()
  "Run checkdoc-file on files remaining on command line, exiting non-zero if there are warnings."
  (let* ((files (mapcar #'expand-file-name command-line-args-left))
         (checkdoc-create-error-function
          (lambda (text start end &optional unfixable)
            (let ((msg (concat (checkdoc-buffer-label) ":"
                               (int-to-string (count-lines (point-min) (or start (point-min))))
                               ": " text)))
              (message msg)
              (setq makem-checkdoc-errors-p t)
              (list text start end unfixable)))))
    (mapcar #'checkdoc-file files)
    (when makem-checkdoc-errors-p
      (kill-emacs 1))))

(makem-checkdoc-files-and-exit)
EOF
    echo $file
}

function elisp-package-initialize-file {
    local file=$(mktemp)

    cat >$file <<EOF
(require 'package)
(setq package-archives (list (cons "gnu" "https://elpa.gnu.org/packages/")
                             (cons "melpa" "https://melpa.org/packages/")
                             (cons "melpa-stable" "https://stable.melpa.org/packages/")))
$elisp_org_package_archive
(package-initialize)
(setq load-prefer-newer t)
EOF
    echo $file
}

# ** Emacs

function run_emacs {
    # NOTE: The sandbox args need to come before the package
    # initialization so Emacs will use the sandbox's packages.
    local emacs_command=(
        "${emacs_command[@]}"
        -Q
        "${args_sandbox[@]}"
        -l $package_initialize_file
        $arg_batch
        "${args_load_paths[@]}"
    )

    # Show debug message with load-path from inside Emacs.
    [[ $debug_load_path ]] \
        && debug $("${emacs_command[@]}" \
                       --batch \
                       --eval "(message \"LOAD-PATH: %s\" load-path)" \
                    2>&1)

    # Set output file.
    output_file=$(mktemp) || die "Unable to make output file."
    paths_temp+=("$output_file")

    # Run Emacs.
    debug "run_emacs: ${emacs_command[@]} $@ &>\"$output_file\""
    "${emacs_command[@]}" "$@" &>"$output_file"

    # Check exit code and output.
    exit=$?
    [[ $exit != 0 ]] \
        && debug "Emacs exited non-zero: $exit"

    [[ $verbose -gt 1 || $exit != 0 ]] \
        && cat $output_file

    return $exit
}

# ** Compilation

function batch-byte-compile {
    debug "batch-byte-compile: ERROR-ON-WARN:$compile_error_on_warn  FILES:$@"

    [[ $compile_error_on_warn ]] && local error_on_warn=(--eval "(setq byte-compile-error-on-warn t)")

    run_emacs \
        "${error_on_warn[@]}" \
        --funcall batch-byte-compile \
        "$@"
}

# ** Files

function files-project-elisp {
    # Echo list of Elisp files in project.
    git ls-files 2>/dev/null | egrep "\.el$" | filter-files-exclude
}

function files-project-source {
    # Echo list of Elisp files that are not tests.
    files-project-elisp | egrep -v "$test_files_regexp" | filter-files-feature
}

function files-project-test {
    # Echo list of Elisp test files.
    files-project-elisp | egrep "$test_files_regexp"
}

function dirs-project {
    # Echo list of directories to be used in load path.
    files-project-source | filter-files-dirs
    files-project-test | filter-files-dirs
}

function args-load-path {
    # Echo load-path arguments.
    for path in $(dirs-project | sort -u)
    do
        printf -- '-L %q ' "$path"
    done
}

function filter-files-dirs {
    # Echo directory names for files on STDIN.
    while read file
    do
        dirname "$file"
    done
}

function filter-files-exclude {
    # Filter out paths (STDIN) which should be excluded by default.
    egrep -v "(/\.cask/|-autoloads.el|.dir-locals)"
}

function filter-files-feature {
    # Read paths on STDIN and echo ones that (provide 'a-feature).
    while read path
    do
        debug "PATH: $path"
        egrep "^\\(provide '" "$path" &>/dev/null \
            && echo "$path"
    done
}

function args-load-files {
    # For file in $@, echo "--load $file".
    for file in "$@"
    do
        printf -- '--load %q ' "$file"
    done
}

function test-files-p {
    # Return 0 if $files_project_test is non-empty.
    [[ "${files_project_test[@]}" ]]
}

function buttercup-tests-p {
    # Return 0 if Buttercup tests are found.
    test-files-p || die "No tests found."
    debug "Checking for Buttercup tests..."

    grep "(require 'buttercup)" "${files_project_test[@]}" &>/dev/null
}

function ert-tests-p {
    # Return 0 if ERT tests are found.
    test-files-p || die "No tests found."
    debug "Checking for ERT tests..."

    # We check for this rather than "(require 'ert)", because ERT may
    # already be loaded in Emacs and might not be loaded with
    # "require" in a test file.
    grep "(ert-deftest" "${files_project_test[@]}" &>/dev/null
}

function dependencies {
    # Echo list of package dependencies.

    # Search package headers.
    egrep '^;; Package-Requires: ' $(files-project-source) $(files-project-test) \
        | egrep -o '\([^([:space:]][^)]*\)' \
        | egrep -o '^[^[:space:])]+' \
        | sed -r 's/\(//g' \
        | egrep -v '^emacs$'  # Ignore Emacs version requirement.

    # Search Cask file.
    if [[ -r Cask ]]
    then
        egrep '\(depends-on "[^"]+"' Cask \
            | sed -r -e 's/\(depends-on "([^"]+)".*/\1/g'
    fi
}

# ** Sandbox

function sandbox {
    # Initialize sandbox.

    # *** Sandbox arguments

    # Check or make user-emacs-directory.
    if [[ $sandbox_dir ]]
    then
        # Directory given as argument: ensure it exists.
        [[ -d $sandbox_dir ]] || die "Directory doesn't exist: $sandbox_dir"
    else
        # Not given: make temp directory, and delete it on exit.
        local sandbox_dir=$(mktemp -d) || die "Unable to make sandbox dir."
        paths_temp+=("$sandbox_dir")
    fi

    # Make argument to load init file if it exists.
    init_file="$sandbox_dir/init.el"
    [[ -r $init_file ]] \
        && local args_load_init_file=(--load "$init_file")

    # Set sandbox args.  This is a global variable used by the run_emacs function.
    args_sandbox=(
        --eval "(setq user-emacs-directory (file-truename \"$sandbox_dir\"))"
        --eval "(setq user-init-file (file-truename \"$init_file\"))"
        "${args_load_init_file[@]}"
    )

    # Add package-install arguments for dependencies.
    if [[ $install_deps ]]
    then
        local deps=($(dependencies))
        debug "Installing dependencies: ${deps[@]}"

        for package in "${deps[@]}"
        do
            args_sandbox_package_install+=(--eval "(package-install '$package)")
        done
    fi

    # *** Install packages into sandbox

    if [[ ${args_sandbox_package_install[@]} ]]
    then
        # Initialize the sandbox (installs packages once rather than for every rule).
        debug "Initializing sandbox..."

        run_emacs \
            --eval "(package-refresh-contents)" \
            "${args_sandbox_package_install[@]}" \
            || die "Unable to initialize sandbox."
    fi

    debug "Sandbox initialized."
}

# ** Utility

function cleanup {
    # Remove temporary paths (${paths_temp[@]}).

    for path in "${paths_temp[@]}"
    do
        if [[ $debug ]]
        then
            debug "Debugging enabled: not deleting temporary path: $path"
        elif [[ -r $path ]]
        then
            rm -rf "$path"
        else
            debug "Temporary path doesn't exist, not deleting: $path"
        fi
    done
}

function echo-unset-p {
    # Echo 0 if $1 is set, otherwise 1.  IOW, this returns the exit
    # code of [[ $1 ]] as STDOUT.
    [[ $1 ]]
    echo $?
}

function ensure-package-available {
    # If package $1 is available, return 0.  Otherwise, return 1, and
    # if $2 is set, give error otherwise verbose.  Outputting messages
    # here avoids repetition in callers.
    local package=$1
    local direct_p=$2

    if ! run_emacs --load $package &>/dev/null
    then
        if [[ $direct_p ]]
        then
            error "$package not available."
        else
            verbose 2 "$package not available."
        fi
        return 1
    fi
}

function ensure-tests-available {
    # If tests of type $1 (like "ERT") are available, return 0.  Otherwise, if
    # $2 is set, give an error and return 1; otherwise give verbose message.  $1
    # should have a corresponding predicate command, like ert-tests-p for ERT.
    local test_name=$1
    local test_command="${test_name,,}-tests-p"  # Converts name to lowercase.
    local direct_p=$2

    if ! $test_command
    then
        if [[ $direct_p ]]
        then
            error "$test_name tests not found."
        else
            verbose 2 "$test_name tests not found."
        fi
        return 1
    fi
}

function echo_color {
    # This allows bold, italic, etc. without needing a function for
    # each variation.
    local color_code="COLOR_$1"
    shift

    if [[ $color ]]
    then
        echo -e "${!color_code}${@}${COLOR_off}"
    else
        echo "$@"
    fi
}
function debug {
    if [[ $debug ]]
    then
        function debug {
            echo_color yellow "DEBUG ($(ts)): $@" >&2
        }
        debug "$@"
    else
        function debug {
            true
        }
    fi
}
function error {
    echo_color red "ERROR ($(ts)): $@" >&2
    ((errors++))
    return 1
}
function die {
    [[ $@ ]] && error "$@"
    exit $errors
}
function log {
    echo "LOG ($(ts)): $@" >&2
}
function log_color {
    local color_name=$1
    shift
    echo_color $color_name "LOG ($(ts)): $@" >&2
}
function success {
    if [[ $verbose -ge 2 ]]
    then
        log_color green "$@" >&2
    fi
}
function verbose {
    # $1 is the verbosity level, rest are echoed when appropriate.
    if [[ $verbose -ge $1 ]]
    then
        [[ $1 -eq 1 ]] && local color_name=blue
        [[ $1 -ge 2 ]] && local color_name=cyan

        shift
        log_color $color_name "$@" >&2
    fi
}

function ts {
    date "+%Y-%m-%d %H:%M:%S"
}

# * Rules

# These functions are intended to be called as rules, like a Makefile.
# Some rules test $1 to determine whether the rule is being called
# directly or from a meta-rule; if directly, an error is given if the
# rule can't be run, otherwise it's skipped.

function all {
    verbose 1 "Running all rules..."

    lint
    tests
}

function compile {
    [[ $compile ]] || return 0
    unset compile  # Only compile once.

    verbose 1 "Compiling..."
    debug "Byte-compile files: ${files_project_byte_compile[@]}"

    batch-byte-compile "${files_project_byte_compile[@]}" \
        && success "Compiling finished without errors." \
            || error "Compilation failed."
}

function batch {
    # Run Emacs with $args_batch and with project source and test files loaded.
    verbose 1 "Executing Emacs with arguments: ${args_batch[@]}"

    run_emacs \
        $(args-load-files "${files_project_source[@]}" "${files_project_test[@]}") \
        "${args_batch[@]}"
}

function interactive {
    # Run Emacs interactively.  Most useful with --sandbox and --install-deps.
    unset arg_batch
    run_emacs \
        $(args-load-files "${files_project_source[@]}" "${files_project_test[@]}")
    arg_batch="--batch"
}

function lint {
    verbose 1 "Linting..."

    lint-checkdoc
    lint-compile
    lint-indent
    lint-package
}

function lint-checkdoc {
    verbose 1 "Linting checkdoc..."

    local checkdoc_file="$(elisp-checkdoc-file)"
    paths_temp+=("$checkdoc_file")

    run_emacs \
        --load="$checkdoc_file" \
        "${files_project_source[@]}" \
        && success "Linting checkdoc finished without errors." \
            || error "Linting checkdoc failed."
}

function lint-compile {
    verbose 1 "Linting compilation..."

    compile_error_on_warn=true
    batch-byte-compile "${files_project_byte_compile[@]}" \
        && success "Linting compilation finished without errors." \
            || error "Linting compilation failed."
    unset compile_error_on_warn
}

function lint-indent {
    ensure-package-available indent-lint $1 || return $(echo-unset-p $1)

    verbose 1 "Linting indentation..."

    # FIXME: indent-lint outputs a summary line like:
    #    Diff finished (has differences).  Fri Jan 17 10:30:34 2020
    # which is unnecessary for our use and clutters output.

    # We load project source files as well, because they may contain
    # macros with (declare (indent)) rules which must be loaded to set
    # indentation.  However...

    # FIXME: This doesn't appear to actually work: macros that set
    # indentation and are correctly indented in the source files are
    # reported as having wrong indentation.  Not sure if bug in
    # indent-lint or here.

    run_emacs \
        --load indent-lint \
        $(args-load-files "${files_project_source[@]}" "${files_project_test[@]}") \
        --funcall indent-lint-batch \
        "${files_project_source[@]}" "${files_project_test[@]}" \
        && success "Linting indentation finished without errors." \
            || error "Linting indentation failed."
}

function lint-package {
    ensure-package-available package-lint $1 || return $(echo-unset-p $1)

    verbose 1 "Linting package..."

    run_emacs \
        --load package-lint \
        --funcall package-lint-batch-and-exit \
        "${files_project_source[@]}" \
        && success "Linting package finished without errors." \
            || error "Linting package failed."
}

function tests {
    verbose 1 "Running all tests..."

    test-ert
    test-buttercup
}

function test-buttercup {
    ensure-tests-available Buttercup $1 || return $(echo-unset-p $1)
    compile || die

    verbose 1 "Running Buttercup tests..."

    local buttercup_file="$(elisp-buttercup-file)"
    paths_temp+=("$buttercup_file")

    run_emacs \
        --load buttercup \
        --load "$buttercup_file" \
        -f buttercup-run-discover \
        && success "Buttercup tests finished without errors." \
            || error "Buttercup tests failed."
}

function test-ert {
    ensure-tests-available ERT $1 || return $(echo-unset-p $1)
    compile || die

    verbose 1 "Running ERT tests..."
    debug "Test files: ${files_project_test[@]}"

    run_emacs \
        $(args-load-files "${files_project_test[@]}") \
        -f ert-run-tests-batch-and-exit \
        && success "ERT tests finished without errors." \
            || error "ERT tests failed."
}

# * Defaults

test_files_regexp='^((tests?|t)/)|-test.el$|^test-'

emacs_command=("emacs")
errors=0
verbose=0
compile=true
arg_batch="--batch"

# MAYBE: Disable color if not outputting to a terminal.  (OTOH, the
# colorized output is helpful in CI logs, and I don't know if,
# e.g. GitHub Actions logging pretends to be a terminal.)
color=true

# TODO: Using the current directory (i.e. a package's repo root directory) in
# load-path can cause weird errors in case of--you guessed it--stale .ELC files,
# the zombie problem that just won't die.  It's incredible how many different ways
# this problem presents itself.  In this latest example, an old .ELC file, for a
# .EL file that had since been renamed, was present on my local system, which meant
# that an example .EL file that hadn't been updated was able to "require" that .ELC
# file's feature without error.  But on another system (in this case, trying to
# setup CI using GitHub Actions), the old .ELC was not present, so the example .EL
# file was not able to load the feature, which caused a byte-compilation error.

# In this case, I will prevent such example files from being compiled.  But in
# general, this can cause weird problems that are tedious to debug.  I guess
# the best way to fix it would be to actually install the repo's code as a
# package into the sandbox, but doing that would require additional tooling,
# pulling in something like Quelpa or package-build--and if the default recipe
# weren't being used, the actual recipe would have to be fetched off MELPA or
# something, which seems like getting too smart for our own good.

# TODO: Emit a warning if .ELC files that don't match any .EL files are detected.
args_load_paths=($(args-load-path))

# ** Colors

COLOR_off='\e[0m'
COLOR_black='\e[0;30m'
COLOR_red='\e[0;31m'
COLOR_green='\e[0;32m'
COLOR_yellow='\e[0;33m'
COLOR_blue='\e[0;34m'
COLOR_purple='\e[0;35m'
COLOR_cyan='\e[0;36m'
COLOR_white='\e[0;37m'

# ** Package system args

args_package_archives=(
    --eval "(add-to-list 'package-archives '(\"gnu\" . \"https://elpa.gnu.org/packages/\") t)"
    --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)"
)

args_org_package_archives=(
    --eval "(add-to-list 'package-archives '(\"org\" . \"https://orgmode.org/elpa/\") t)"
)

args_package_init=(
    --eval "(package-initialize)"
)

elisp_org_package_archive="(add-to-list 'package-archives '(\"org\" . \"https://orgmode.org/elpa/\") t)"

# * Project files

# MAYBE: Option to not byte-compile test files.  (OTOH, byte-compiling reveals many
# errors that would otherwise go unnoticed, so it's worth it to fix the warnings.)
files_project_source=($(files-project-source))
files_project_test=($(files-project-test))
files_project_byte_compile=("${files_project_source[@]}" "${files_project_test[@]}")

# * Args

args=$(getopt -n "$0" \
              -o dhE:i:sS:vf:CO \
              -l emacs:,install-deps,install-linters,debug,debug-load-path,help,install:,verbose,file:,no-color,no-compile,no-org-repo,sandbox,sandbox-dir: \
              -- "$@") \
    || { usage; exit 1; }
eval set -- "$args"

while true
do
    case "$1" in
        --install-deps)
            install_deps=true
            ;;
        --install-linters)
            args_sandbox_package_install+=(--eval "(package-install 'indent-lint)"
                                           --eval "(package-install 'package-lint)")
            ;;
        -d|--debug)
            debug=true
            verbose=2
            ;;
        --debug-load-path)
            debug_load_path=true
            ;;
        -h|--help)
            usage
            exit
            ;;
        -E|--emacs)
            shift
            emacs_command=($1)
            ;;
        -i|--install)
            shift
            args_sandbox_package_install+=(--eval "(package-install '$1)")
            ;;
        -s|--sandbox)
            sandbox=true
            ;;
        -S|--sandbox-dir)
            shift
            sandbox=true
            sandbox_dir="$1"
            ;;
        -v|--verbose)
            ((verbose++))
            ;;
        -f|--file)
            shift
            project_source_files+=("$1")
            project_byte_compile_files+=("$1")
            ;;
        -O|--no-org-repo)
            unset elisp_org_package_archive
            ;;
        --no-color)
            unset color
            ;;
        -C|--no-compile)
            unset compile
            ;;
        --)
            # Remaining args (required; do not remove)
            shift
            rest=("$@")
            break
            ;;
    esac

    shift
done

debug "ARGS: $args"
debug "Remaining args: ${rest[@]}"

# Set package elisp (which depends on --no-org-repo arg).
package_initialize_file="$(elisp-package-initialize-file)"
paths_temp+=("$package_initialize_file")

# * Main

trap cleanup EXIT INT TERM

if ! [[ ${files_project_source[@]} ]]
then
    error "No files specified and not in a git repo."
    exit 1
fi

# Initialize sandbox.
[[ $sandbox ]] && sandbox

# Run rules.
for rule in "${rest[@]}"
do
    if [[ $batch ]]
    then
        debug "Adding batch argument: $rule"
        args_batch+=("$rule")

    elif [[ $rule = batch ]]
    then
        # Remaining arguments are passed to Emacs.
        batch=true
    elif type -t "$rule" 2>/dev/null | grep function &>/dev/null
    then
        # Pass called-directly as $1 to indicate that the rule is
        # being called directly rather than from a meta-rule.
        $rule called-directly
    elif [[ $rule = test ]]
    then
        # Allow the "tests" rule to be called as "test".  Since "test"
        # is a shell builtin, this workaround is required.
        tests
    else
        error "Invalid rule: $rule"
    fi
done

# The batch rule.
[[ $batch ]] && batch

if [[ $errors -gt 0 ]]
then
    log_color red "Finished with $errors errors."
else
    success "Finished without errors."
fi

exit $errors
