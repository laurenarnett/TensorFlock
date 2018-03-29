#! /bin/bash

# Format functions lifed from here:
#bold://askubuntu.com/questions/528928/how-to-do-underline-bold-italic-strikethrough-color-background-and-size-i 
bold()          { ansi 1 "$@"; }
italic()        { ansi 3 "$@"; }
underline()     { ansi 4 "$@"; }
strikethrough() { ansi 9 "$@"; }
red()           { ansi 31 "$@"; }
green()         { ansi 32 "$@"; }
ansi()          { printf "\e[${1}m${*:2}\e[0m"; }

if [ ! $(which lli) ]
then
    echo $(bold $(red "LLVM not found."))
    echo $(bold $(red "Please run this script using make, or add the location of LLVM binaries to your path."))
    exit 1
fi


failures=""

function run_compile_test {
    # Param $1: file name
    passing_output_file=$(echo $1 | sed -E 's/(.*\/)(.*\.)(tf)/\1\2pass/')
    passing_output=$(cat $passing_output_file)
    generated_output=$(./toplevel.native -l $1 | lli)
    if [ $generated_output != $passing_output ]
    then
        failures+="$(bold "========================================")"
        failures+=$'\nTest '
        failures+="$f"
        failures+=$' failed.\n'
        failures+="$(underline "Expected:")"
        failures+=$'\n'
        failures+="$(green $passing_output)"
        failures+=$'\n\n'
        failures+="$(underline "Received:")"
        failures+=$'\n'
        failures+="$(red $generated_output)"
        failures+=$'\n\n'
    fi
}

function run_test {
    # Param $1: file name
    # Param $2: compiler flag
    # Param $3: pass/fail; 0 if pass, 1 if fail
    output=$(./toplevel.native -$2 $1 2>&1)
    ret_code=$?

    if [ $ret_code -eq 0 ] && [ $3 -eq 0 ]
    then
        pass_mode="pass"
    elif [ $ret_code -gt 0 ] && [ $3 -eq 1 ]
    then
        pass_mode="pass"
    else
        pass_mode="fail"
    fi

    if [ $pass_mode == "pass" ]
    then
        printf "."
    else
        printf "F"
        failures+="$(bold "========================================")"
        failures+=$'\nTest '
        failures+="$f"
        failures+=$' failed.\n'
        failures+="$output"
        failures+=$'\n'
        return
    fi
    
    # If the flag is set to compile and this test is intended to pass
    if [ "$2" == "c" ] && [ $3 -eq 0 ]
    then
        run_compile_test $1
    fi
}

# Register new tests below
for f in ./tests/syntax_tests/pass*.tf; do
    run_test $f a 0
done

for f in ./tests/syntax_tests/fail*.tf; do
    run_test $f a 1
done

for f in ./tests/semant_tests/pass/*.tf; do
    run_test $f s 0
done

for f in ./tests/codegen/pass/*.tf; do
    run_test $f c 0
done

echo
if [[ ! -z $failures ]]
then
    failures+="$(bold "========================================")"
    failures+=$'\n'
    printf "%s" "$failures"
    exit 1
fi

echo $(bold $(green "All tests pass."))
exit 0
