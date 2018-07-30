#!/bin/bash

if [ ${#@} -eq 0 ]
then
    echo "31.21793 121.47113" | nc localhost 8080
else
    echo "${@}" | nc localhost 8080
fi
