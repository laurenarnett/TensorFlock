#! /bin/bash

failures=""

function run_test {
    # Param $1: file name
    # Param $2: compiler flag
    # Param $3: passing exit code
    output=$(./toplevel.native -$2 $1 2>&1)
    if [ $? -eq $3 ]
    then
        printf "."
    else
        printf "F"
        failures+=$'--------------------\n'
        failures+=$'Test '
        failures+="$f"
        failures+=$' failed.\n'
        failures+="$output"
        failures+=$'\n'
        failures+=$'--------------------\n'
    fi
}

for f in ./tests/syntax_tests/pass*.tf; do
    run_test $f a 0
done

for f in ./tests/syntax_tests/fail*.tf; do
    run_test $f a 2
done

for f in ./tests/semant_tests/pass/*.tf; do
    run_test $f s 0
done

echo
if [[ ! -z $failures ]]
then
    echo
    printf "%s" "$failures"
    exit 1
fi

echo "All tests pass."
exit 0
