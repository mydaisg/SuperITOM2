@echo off
cd /d D:\GitHub\SuperITOM2
echo Starting app...
Rscript -e "shiny::runApp('.', port=3838, host='0.0.0.0', launch.browser=FALSE)" 2>&1
echo Exit code: %ERRORLEVEL%
pause
