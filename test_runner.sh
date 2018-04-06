#! /bin/sh

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


# failures is used as a buffer to hold failure messages
# until the tests are finished running
failures=""

function run_test {
    # Param $1: file name
    # Param $2: compiler flag
    # Param $3: pass or fail - describes if the test should pass or fail 
    generated_output=$(./toplevel.native -$2 $1 2>&1)
    ret_code=$?

    if [ $2 == "c" ]
    then
        llc output.ll
        clang -Wno-override-module -lm  output.s _build/src/runtime.o -o output
        generated_output=$(./output)
    fi

    # Set a 'pass mode', this variable describes if a test does what
    # we expect, i.e. if we expect a test to fail, and it passes, then
    # the pass mode is fail.
    if [ $ret_code -eq 0 ] && [ $3 == "pass" ]
    then
        pass_mode="pass"
    elif [ $ret_code -gt 0 ] && [ $3 == "fail" ]
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
        failures+="$generated_output"
        failures+=$'\n'
        return
    fi

    # If an expected output file exists, compare the generated and expected output
    passing_output_file=$(echo $1 | sed -E 's/(.*\/)(.*\.)(tf)/\1\2expected/')
    if [ -f $passing_output_file ]; then
        passing_output=$(cat $passing_output_file)
        if [ "$generated_output" != "$passing_output" ]
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
    fi
}

# Register new tests below
for f in ./tests/syntax_tests/pass/*.tf; do
    run_test $f a pass
done

for f in ./tests/syntax_tests/fail/*.tf; do
    run_test $f a fail 
done

for f in ./tests/semant_tests/pass/*.tf; do
    run_test $f s pass 
done

for f in ./tests/semant_tests/fail/*.tf; do
    run_test $f s fail 
done

for f in ./tests/codegen/pass/*.tf; do
    run_test $f c pass
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
