#!/usr/bin/env bash

# Change directory to where presentations are stored
cd ALTER_PATH
echo Starting HTTP server on port 8000...
# Start the HTTP server
# Send all output to /dev/null and store process id to kill later
python -m SimpleHTTPServer >& /dev/null & var=`echo $!`
# Wait a while before loading browser
echo Waiting...
sleep 3
open http://localhost:8000/   ##FOR MAC##
echo Server up launched for your convenience
# Ask for input that we won't use to keep this active 
echo Press any key to kill server...
read nonsense
# Once we give input we can kill the server
kill `echo $var`
