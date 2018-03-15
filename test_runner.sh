#! /bin/bash

failures=""
for f in ./tests/syntax_tests/pass*.tf; do
    output=$(./toplevel.native -a $f 2>&1)
    if [ $? -eq 0 ]
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
done

for f in ./tests/syntax_tests/fail*.tf; do
    output=$(./toplevel.native -a $f 2>&1)
    if [ $? -eq 2 ]
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
done

for f in ./tests/semant_tests/pass/*.tf; do
    output=$(./toplevel.native -a $f 2>&1)
    if [ $? -eq 0 ]
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
