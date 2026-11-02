@echo off
rem ============================================================
rem  HungFo data CLI entry (gh.cmd)
rem  Usage: gh "gh-list()" | gh "gh-get(hf_users,5)" | gh "gh-get(hf_users)" --fmt json
rem  Bundles absolute Rscript + hf_gh_bash.r paths, usable from any dir (add to PATH).
rem ============================================================
setlocal
set "RSCRIPT=D:\Tai_ProgramFiles\R\R-4.6.1\bin\x64\Rscript.exe"
set "GHBASH=d:\GitHub\SuperITOM2\Script\hungfo\hf_gh_bash.r"
"%RSCRIPT%" "%GHBASH%" %*
endlocal
