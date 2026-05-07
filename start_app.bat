@echo off
cd /d D:\GitHub\SuperITOM2
echo Starting SuperITOM2...
echo R Version: 4.6.0
"D:\Tai_Programs\R-4.6.0\bin\Rscript.exe" -e "shiny::runApp('.', port=3838, host='0.0.0.0', launch.browser=FALSE)" 2>&1
echo Exit code: %ERRORLEVEL%
pause
