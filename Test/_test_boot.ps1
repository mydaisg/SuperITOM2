Write-Host "==== Rscript process status ===="
$p = Get-Process -Id 7544 -ErrorAction SilentlyContinue
if ($p) {
  Write-Host "PID 7544 ALIVE  CPU=$([Math]::Round($p.CPU,1))s  WS=$([Math]::Round($p.WorkingSet/1MB,1))MB"
} else {
  Write-Host "PID 7544 已退出"
}

Write-Host ""
Write-Host "==== All R procs ===="
foreach ($q in (Get-CimInstance Win32_Process -Filter "Name='R.exe' OR Name='Rterm.exe' OR Name='Rscript.exe'")) {
  $g = Get-Process -Id $q.ProcessId -ErrorAction SilentlyContinue
  Write-Host "PID=$($q.ProcessId) PPID=$($q.ParentProcessId) $($q.Name) CPU=$([Math]::Round($g.CPU,1))s WS=$([Math]::Round($g.WorkingSet/1MB,1))MB"
  Write-Host "  $($q.CommandLine)"
}

Write-Host ""
Write-Host "==== boot stdout log ===="
if (Test-Path 'D:\GitHub\SuperITOM2\Log\_boot_test_out.log') {
  Write-Host "Size: $((Get-Item 'D:\GitHub\SuperITOM2\Log\_boot_test_out.log').Length) bytes"
  Get-Content 'D:\GitHub\SuperITOM2\Log\_boot_test_out.log' -Encoding UTF8
} else { Write-Host "不存在" }

Write-Host ""
Write-Host "==== boot stderr log ===="
if (Test-Path 'D:\GitHub\SuperITOM2\Log\_boot_test_err.log') {
  Write-Host "Size: $((Get-Item 'D:\GitHub\SuperITOM2\Log\_boot_test_err.log').Length) bytes"
  Get-Content 'D:\GitHub\SuperITOM2\Log\_boot_test_err.log' -Encoding UTF8
} else { Write-Host "不存在" }

Write-Host ""
Write-Host "==== port 8899 ===="
Get-NetTCPConnection -LocalPort 8899 -ErrorAction SilentlyContinue | Select-Object State, LocalAddress, OwningProcess | Format-Table -AutoSize
