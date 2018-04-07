#!/bin/bash

if [ $(uname) == "Darwin" ]
then
    STAT_STR="stat -f %c"
else
    STAT_STR="stat --format=%Z"
fi

DIR_M_TIME=$($STAT_STR $1) ;
while true 
do 
    TEMP_TIME=$($STAT_STR $1); 
	if [ "$DIR_M_TIME" != "$TEMP_TIME" ]; 
	then 
	  make 
	  DIR_M_TIME=$TEMP_TIME; 
    fi; 
    sleep 3; 
done;
