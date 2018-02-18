#! /bin/bash

failures=""
for f in tests/pass*.tf; do
    output=$(./toplevel.native -a $f 2>&1)
    if [ $? -eq 0 ]
    then
        printf "."
    else
        printf "F"
        failures+="--------------------\n"
        failures+="Test $f failed.\n"
        failures+="$output\n"
        failures+="--------------------\n"
    fi
done

for f in tests/fail*.tf; do
    output=$(./toplevel.native -a $f 2>&1)
    if [ $? -eq 2 ]
    then
        printf "."
    else
        printf "F"
        failures+="--------------------\n"
        failures+="Test $f failed.\n"
        failures+="$output\n"
        failures+="--------------------\n"
    fi
done

echo
if [[ ! -z $failures ]]
then
    echo
    echo "$failures"
    exit 1
fi

echo "All tests pass."
exit 0
