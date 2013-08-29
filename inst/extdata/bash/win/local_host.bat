@echo off
cd ALTER_PATH
START /B python -m SimpleHTTPServer
SLEEP 3
START http://localhost:8000/