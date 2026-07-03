# SuperITOM2 管理 API 服务器
# 用法: powershell -ExecutionPolicy Bypass -File mgmt_api.ps1
# 监听: http://localhost:3839

$listener = New-Object System.Net.HttpListener
$listener.Prefixes.Add("http://localhost:3839/")
$listener.Start()
Write-Host "SuperITOM2 管理 API 已启动: http://localhost:3839" -ForegroundColor Green
Write-Host "管理控制台: http://localhost:3838/index.html" -ForegroundColor Cyan

$ROOT = "D:\GitHub\SuperITOM2"
$SHINY_PORT = 3838
$RScript = "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"

# 查找 Rscript
if (-not (Test-Path $RScript)) {
    $RScript = (Get-Command rscript.exe -ErrorAction SilentlyContinue).Source
}
if (-not $RScript) {
    $RScript = "rscript.exe"
}

function Write-JsonResponse($ctx, $obj) {
    $json = $obj | ConvertTo-Json -Compress -Depth 3
    $buf = [Text.Encoding]::UTF8.GetBytes($json)
    $ctx.Response.ContentType = "application/json; charset=utf-8"
    $ctx.Response.Headers.Add("Access-Control-Allow-Origin","*")
    $ctx.Response.OutputStream.Write($buf,0,$buf.Length)
    $ctx.Response.Close()
}

function Get-ShinyStatus {
    $proc = Get-Process Rscript -ErrorAction SilentlyContinue | Where-Object { $_.CommandLine -match "run_app\.R" }
    $listening = $false
    try {
        $req = [Net.HttpWebRequest]::Create("http://localhost:$SHINY_PORT/")
        $req.Timeout = 2000
        $resp = $req.GetResponse()
        $listening = ($resp.StatusCode -eq [Net.HttpStatusCode]::OK)
        $resp.Close()
    } catch { $listening = $false }
    @{
        running = $listening -or ($null -ne $proc)
        pid = if ($proc) { $proc.Id } else { $null }
        port = $SHINY_PORT
    }
}

function Stop-Shiny {
    $procs = Get-Process Rscript -ErrorAction SilentlyContinue | Where-Object { $_.CommandLine -match "run_app\.R" }
    if ($procs) {
        $procs | Stop-Process -Force
        Start-Sleep -Seconds 1
        return @{ ok = $true; msg = "已停止 Shiny (PID: $($procs.Id))" }
    }
    # fallback
    taskkill /F /IM Rscript.exe 2>$null
    return @{ ok = $true; msg = "已尝试停止所有 Rscript 进程" }
}

function Start-Shiny {
    $proc = Get-Process Rscript -ErrorAction SilentlyContinue | Where-Object { $_.CommandLine -match "run_app\.R" }
    if ($proc) { return @{ ok = $false; msg = "Shiny 已在运行 (PID: $($proc.Id))" } }
    $psi = New-Object Diagnostics.ProcessStartInfo
    $psi.FileName = $RScript
    $psi.Arguments = "$ROOT\run_app.R"
    $psi.WorkingDirectory = $ROOT
    $psi.UseShellExecute = $false
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.CreateNoWindow = $true
    $p = [Diagnostics.Process]::Start($psi)
    Start-Sleep -Seconds 3
    return @{ ok = $true; msg = "Shiny 启动中 (PID: $($p.Id))" }
}

function Get-EnvInfo {
    $rver = & $RScript --version 2>$null
    $git = & git --version 2>$null
    $psver = $PSVersionTable.PSVersion.ToString()
    @{
        ok = $true
        env = @(
            @{key="项目路径"; val=$ROOT}
            @{key="操作系统"; val=(Get-CimInstance Win32_OperatingSystem).Caption}
            @{key="PS 版本"; val=$psver}
            @{key="Shiny 端口"; val="http://localhost:$SHINY_PORT"}
            @{key="API  端口"; val="http://localhost:3839"}
            @{key="Rscript"; val=$RScript}
        )
        ver = @(
            @{key="R"; val=($rver -join ' ').Trim(); stat="OK"}
            @{key="Git"; val=$git.Trim(); stat="OK"}
            @{key="PowerShell"; val=$psver; stat="OK"}
        )
    }
}

function Get-Tree {
    function _tree($path, $indent) {
        $items = Get-ChildItem $path -ErrorAction SilentlyContinue | Where-Object { $_.Name -notin @('.git','.codebuddy','node_modules','__pycache__','.Rproj.user') }
        $lines = @()
        $cnt = $items.Count
        for ($i=0; $i -lt $cnt; $i++) {
            $isDir = $items[$i].PSIsContainer
            $prefix = if ($i -eq $cnt-1) { "└── " } else { "├── " }
            $name = $items[$i].Name
            $size = if (-not $isDir) { " (" + ("{0:N0}" -f $items[$i].Length) + " B)" } else { "" }
            $cls = if ($isDir) { "dir" } else { "file" }
            $lines += "$indent$prefix<span class='$cls'>$name</span><span class='size'>$size</span>"
            if ($isDir) {
                $childIndent = if ($i -eq $cnt-1) { "    " } else { "│   " }
                $lines += _tree $items[$i].FullName "$indent$childIndent"
            }
        }
        return $lines
    }
    $tree = (_tree $ROOT "") -join "<br>"
    return @{ ok = $true; tree = "<code style='font-family:Consolas,monospace'>$ROOT<br>$tree</code>" }
}

function Get-Logs {
    $logFiles = Get-ChildItem "$ROOT\Log" -Filter "*.log" -ErrorAction SilentlyContinue | Sort-Object LastWriteTime -Descending | Select-Object -First 3
    $logContent = ""
    foreach ($f in $logFiles) {
        $logContent += "=== $($f.Name) ===`n"
        $logContent += (Get-Content $f.FullName -Tail 30 -ErrorAction SilentlyContinue | Out-String)
        $logContent += "`n"
    }
    return @{ ok = $true; logs = if ($logContent) { $logContent } else { "暂无日志" } }
}

function Test-Login($username, $password) {
    $rscript = $RScript
    $authScript = Join-Path $ROOT "Script\auth_api.r"
    $result = & $rscript $authScript $username $password 2>$null | Out-String
    if ($result -match "OK:(\w+)") {
        return @{ ok = $true; role = $Matches[1] }
    }
    return @{ ok = $false }
}

function Submit-Quick($type, $text) {
    # 通过 Shiny 的 HTTP API 提交（如果 Shiny 支持）
    # 这里是占位，Shiny 需要相应的 HTTP handler
    return @{ ok = $false; error = "快速提交需通过 Shiny 首页操作" }
}

while ($listener.IsListening) {
    $ctx = $listener.GetContext()
    $path = $ctx.Request.Url.AbsolutePath
    $method = $ctx.Request.HttpMethod
    
    if ($method -eq "OPTIONS") {
        $ctx.Response.Headers.Add("Access-Control-Allow-Origin","*")
        $ctx.Response.Headers.Add("Access-Control-Allow-Methods","GET,POST,OPTIONS")
        $ctx.Response.Headers.Add("Access-Control-Allow-Headers","Content-Type")
        $ctx.Response.Close()
        continue
    }
    
    try {
        switch ($path) {
            "/login"   {
                $body = (New-Object IO.StreamReader($ctx.Request.InputStream)).ReadToEnd()
                $data = $body | ConvertFrom-Json
                $result = Test-Login $data.username $data.password
                Write-JsonResponse $ctx $result
            }
            "/status"  { Write-JsonResponse $ctx (Get-ShinyStatus) }
            "/start"   { Write-JsonResponse $ctx (Start-Shiny) }
            "/stop"    { Write-JsonResponse $ctx (Stop-Shiny) }
            "/env"     { Write-JsonResponse $ctx (Get-EnvInfo) }
            "/structure" { Write-JsonResponse $ctx (Get-Tree) }
            "/logs"    { Write-JsonResponse $ctx (Get-Logs) }
            "/quick"   {
                $body = (New-Object IO.StreamReader($ctx.Request.InputStream)).ReadToEnd()
                $data = $body | ConvertFrom-Json
                Write-JsonResponse $ctx (Submit-Quick $data.type $data.text)
            }
            default {
                $ctx.Response.StatusCode = 404
                Write-JsonResponse $ctx @{ error = "Not found" }
            }
        }
    } catch {
        Write-JsonResponse $ctx @{ error = $_.Exception.Message }
    }
}

$listener.Stop()
