@echo off
echo ====================================
echo    The server startup script for the cat and mouse game
echo ====================================
echo.
echo The game server is starting up...
echo.

cd /d "%~dp0"

swipl -s game_server.pl -g "start_server(9178)"

pause



