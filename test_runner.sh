#! /bin/bash

if [ ! $(which lli) ]
then
    echo "lli not found"
    echo "Please run this script from make, or add lli to your path"
    exit 1
fi

# Format functions lifed from here:
#bold://askubuntu.com/questions/528928/how-to-do-underline-bold-italic-strikethrough-color-background-and-size-i 
bold()          { ansi 1 "$@"; }
italic()        { ansi 3 "$@"; }
underline()     { ansi 4 "$@"; }
strikethrough() { ansi 9 "$@"; }
red()           { ansi 31 "$@"; }
green()         { ansi 32 "$@"; }
ansi()          { printf "\e[${1}m${*:2}\e[0m"; }


failures=""

function run_compile_test {
    # Param $1: file name
    passing_output_file=$(echo $1 | perl -pe 's/(.*\/)(.*\.)(tf)/\1\2pass/')
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
    # Param $3: passing exit code
    output=$(./toplevel.native -$2 $1 2>&1)
    ret_code=$?
    if [ $ret_code -eq $3 ]
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
    run_test $f a 2
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
