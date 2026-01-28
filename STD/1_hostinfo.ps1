param(
    [string]$ConfigPath = "D:\GitHub\SuperITOM\config\config.json"
)

$ErrorActionPreference = "Stop"

function Write-Log {
    param(
        [string]$Message,
        [string]$Level = "INFO"
    )
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"
    Write-Output $logMessage
}

function Get-Config {
    param([string]$Path)
    if (Test-Path $Path) {
        return Get-Content $Path | ConvertFrom-Json
    } else {
        Write-Log "Config file not found: $Path" "ERROR"
        exit 1
    }
}

function Get-SystemInfo {
    try {
        Write-Log "Collecting system information..."
        $systemInfo = systeminfo /FO CSV | ConvertFrom-Csv
        return $systemInfo
    } catch {
        Write-Log "Failed to get system info: $_" "ERROR"
        return $null
    }
}

function Get-IPConfig {
    try {
        Write-Log "Collecting IP configuration..."
        $ipConfig = ipconfig /all
        return $ipConfig
    } catch {
        Write-Log "Failed to get IP config: $_" "ERROR"
        return $null
    }
}

function Get-InstalledPrograms {
    try {
        Write-Log "Collecting installed programs..."
        
        $programs = @()
        
        $registryPaths = @(
            "HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*",
            "HKLM:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\*",
            "HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*"
        )
        
        foreach ($path in $registryPaths) {
            $programs += Get-ItemProperty $path -ErrorAction SilentlyContinue | 
                Where-Object { $_.DisplayName } | 
                Select-Object DisplayName, DisplayVersion, Publisher, InstallDate, InstallLocation
        }
        
        return $programs
    } catch {
        Write-Log "Failed to get installed programs: $_" "ERROR"
        return $null
    }
}

function Get-AppxPackages {
    try {
        Write-Log "Collecting AppX packages..."
        $appxPackages = Get-AppxPackage -ErrorAction SilentlyContinue | 
            Select-Object Name, Version, Publisher, InstallLocation
        return $appxPackages
    } catch {
        Write-Log "Failed to get AppX packages: $_" "WARN"
        return $null
    }
}

function Get-NetworkInfo {
    try {
        Write-Log "Collecting network information..."
        
        $networkInfo = @()
        
        $adapters = Get-NetAdapter -ErrorAction SilentlyContinue
        foreach ($adapter in $adapters) {
            $ipAddresses = Get-NetIPAddress -InterfaceIndex $adapter.ifIndex -ErrorAction SilentlyContinue
            $dnsServers = Get-DnsClientServerAddress -InterfaceIndex $adapter.ifIndex -ErrorAction SilentlyContinue
            
            $networkInfo += [PSCustomObject]@{
                Name = $adapter.Name
                InterfaceDescription = $adapter.InterfaceDescription
                Status = $adapter.Status
                LinkSpeed = $adapter.LinkSpeed
                MACAddress = $adapter.MacAddress
                IPAddresses = ($ipAddresses | ForEach-Object { $_.IPAddress }) -join ", "
                DNSServers = ($dnsServers | ForEach-Object { $_.ServerAddresses }) -join ", "
            }
        }
        
        return $networkInfo
    } catch {
        Write-Log "Failed to get network info: $_" "ERROR"
        return $null
    }
}

function Get-Hotfixes {
    try {
        Write-Log "Collecting installed hotfixes..."
        $hotfixes = Get-HotFix -ErrorAction SilentlyContinue | 
            Select-Object HotFixID, Description, InstalledBy, InstalledOn
        return $hotfixes
    } catch {
        Write-Log "Failed to get hotfixes: $_" "ERROR"
        return $null
    }
}

function Get-Services {
    try {
        Write-Log "Collecting services..."
        $services = Get-Service -ErrorAction SilentlyContinue | 
            Select-Object Name, DisplayName, Status, StartType
        return $services
    } catch {
        Write-Log "Failed to get services: $_" "ERROR"
        return $null
    }
}

function Get-DiskInfo {
    try {
        Write-Log "Collecting disk information..."
        $disks = Get-PSDrive -PSProvider FileSystem -ErrorAction SilentlyContinue | 
            Select-Object Name, Used, Free, @{Name="UsedGB";Expression={[math]::Round($_.Used/1GB,2)}}, @{Name="FreeGB";Expression={[math]::Round($_.Free/1GB,2)}}
        return $disks
    } catch {
        Write-Log "Failed to get disk info: $_" "ERROR"
        return $null
    }
}

function Write-HostInfoLog {
    param(
        [hashtable]$Data,
        [string]$LogPath
    )
    
    try {
        $logContent = @"
========================================
HOST INFORMATION COLLECTION REPORT
========================================
Collection Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Hostname: $env:COMPUTERNAME
Username: $env:USERNAME
Domain: $env:USERDOMAIN

========================================
SYSTEM INFORMATION
========================================
$($Data.SystemInfo | ConvertTo-Json -Depth 3)

========================================
IP CONFIGURATION
========================================
$($Data.IPConfig -join "`n")

========================================
NETWORK ADAPTERS
========================================
$($Data.NetworkInfo | ConvertTo-Json -Depth 3)

========================================
INSTALLED PROGRAMS ($($Data.InstalledPrograms.Count) total)
========================================
$($Data.InstalledPrograms | Select-Object DisplayName, DisplayVersion, Publisher | ConvertTo-Json -Depth 3)

========================================
APPX PACKAGES ($($Data.AppxPackages.Count) total)
========================================
$($Data.AppxPackages | Select-Object Name, Version | ConvertTo-Json -Depth 3)

========================================
INSTALLED HOTFIXES ($($Data.Hotfixes.Count) total)
========================================
$($Data.Hotfixes | ConvertTo-Json -Depth 3)

========================================
SERVICES ($($Data.Services.Count) total)
========================================
$($Data.Services | ConvertTo-Json -Depth 3)

========================================
DISK INFORMATION
========================================
$($Data.DiskInfo | ConvertTo-Json -Depth 3)

========================================
END OF REPORT
========================================
"@
        
        Set-Content -Path $LogPath -Value $logContent -Encoding UTF8
        Write-Log "Host information log written to: $LogPath"
        return $true
    } catch {
        Write-Log "Failed to write host info log: $_" "ERROR"
        return $false
    }
}

function Upload-Log {
    param(
        [string]$SourcePath,
        [string]$DestPath,
        [string]$Prefix
    )
    
    try {
        if (-not (Test-Path $SourcePath)) {
            Write-Log "Source file not found: $SourcePath" "ERROR"
            return $false
        }
        
        if (-not (Test-Path $DestPath)) {
            Write-Log "Destination path not found: $DestPath" "ERROR"
            return $false
        }
        
        $filename = Split-Path $SourcePath -Leaf
        $destFile = Join-Path $DestPath "${Prefix}_${filename}"
        
        Copy-Item -Path $SourcePath -Destination $destFile -Force -ErrorAction Stop
        Write-Log "Log uploaded to: $destFile"
        return $true
    } catch {
        Write-Log "Failed to upload log: $_" "ERROR"
        return $false
    }
}

$config = Get-Config -Path $ConfigPath
$localDir = $config.paths.local_work_dir

Write-Log "=== Starting Host Information Collection ==="

if (-not (Test-Path $localDir)) {
    Write-Log "Local directory not found: $localDir" "ERROR"
    exit 1
}

$hostInfoLog = Join-Path $localDir "1_host.log"

$collectionData = @{
    SystemInfo = Get-SystemInfo
    IPConfig = Get-IPConfig
    InstalledPrograms = Get-InstalledPrograms
    AppxPackages = Get-AppxPackages
    NetworkInfo = Get-NetworkInfo
    Hotfixes = Get-Hotfixes
    Services = Get-Services
    DiskInfo = Get-DiskInfo
}

$writeResult = Write-HostInfoLog -Data $collectionData -LogPath $hostInfoLog

if ($writeResult) {
    $logUploadPath = $config.paths.log_upload_path
    $ipAddress = (Get-NetIPAddress -AddressFamily IPv4 -ErrorAction SilentlyContinue | Where-Object { $_.IPAddress -notlike "127.*" } | Select-Object -First 1).IPAddress
    
    if ($ipAddress) {
        $uploadResult = Upload-Log -SourcePath $hostInfoLog -DestPath $logUploadPath -Prefix $ipAddress
        
        if ($uploadResult) {
            Write-Log "=== Host Information Collection Completed Successfully ==="
            exit 0
        } else {
            Write-Log "=== Host Information Collection Completed but Upload Failed ===" "ERROR"
            exit 1
        }
    } else {
        Write-Log "Failed to get IP address for upload prefix" "ERROR"
        Write-Log "=== Host Information Collection Completed (No Upload) ==="
        exit 0
    }
} else {
    Write-Log "=== Host Information Collection Failed ===" "ERROR"
    exit 1
}
.Name] = param(
    [string]$ConfigPath = "D:\GitHub\SuperITOM\config\config.json"
)

$ErrorActionPreference = "Stop"

function Write-Log {
    param(
        [string]$Message,
        [string]$Level = "INFO"
    )
    $timestamp = Get-Date -Format "yyyy-MM-dd HH:mm:ss"
    $logMessage = "[$timestamp] [$Level] $Message"
    Write-Output $logMessage
}

function Get-Config {
    param([string]$Path)
    if (Test-Path $Path) {
        return Get-Content $Path | ConvertFrom-Json
    } else {
        Write-Log "Config file not found: $Path" "ERROR"
        exit 1
    }
}

function Get-SystemInfo {
    try {
        Write-Log "Collecting system information..."
        $systemInfo = systeminfo /FO CSV | ConvertFrom-Csv
        return $systemInfo
    } catch {
        Write-Log "Failed to get system info: $_" "ERROR"
        return $null
    }
}

function Get-IPConfig {
    try {
        Write-Log "Collecting IP configuration..."
        $ipConfig = ipconfig /all
        return $ipConfig
    } catch {
        Write-Log "Failed to get IP config: $_" "ERROR"
        return $null
    }
}

function Get-InstalledPrograms {
    try {
        Write-Log "Collecting installed programs..."
        
        $programs = @()
        
        $registryPaths = @(
            "HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*",
            "HKLM:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\*",
            "HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*"
        )
        
        foreach ($path in $registryPaths) {
            $programs += Get-ItemProperty $path -ErrorAction SilentlyContinue | 
                Where-Object { $_.DisplayName } | 
                Select-Object DisplayName, DisplayVersion, Publisher, InstallDate, InstallLocation
        }
        
        return $programs
    } catch {
        Write-Log "Failed to get installed programs: $_" "ERROR"
        return $null
    }
}

function Get-AppxPackages {
    try {
        Write-Log "Collecting AppX packages..."
        $appxPackages = Get-AppxPackage -ErrorAction SilentlyContinue | 
            Select-Object Name, Version, Publisher, InstallLocation
        return $appxPackages
    } catch {
        Write-Log "Failed to get AppX packages: $_" "WARN"
        return $null
    }
}

function Get-NetworkInfo {
    try {
        Write-Log "Collecting network information..."
        
        $networkInfo = @()
        
        $adapters = Get-NetAdapter -ErrorAction SilentlyContinue
        foreach ($adapter in $adapters) {
            $ipAddresses = Get-NetIPAddress -InterfaceIndex $adapter.ifIndex -ErrorAction SilentlyContinue
            $dnsServers = Get-DnsClientServerAddress -InterfaceIndex $adapter.ifIndex -ErrorAction SilentlyContinue
            
            $networkInfo += [PSCustomObject]@{
                Name = $adapter.Name
                InterfaceDescription = $adapter.InterfaceDescription
                Status = $adapter.Status
                LinkSpeed = $adapter.LinkSpeed
                MACAddress = $adapter.MacAddress
                IPAddresses = ($ipAddresses | ForEach-Object { $_.IPAddress }) -join ", "
                DNSServers = ($dnsServers | ForEach-Object { $_.ServerAddresses }) -join ", "
            }
        }
        
        return $networkInfo
    } catch {
        Write-Log "Failed to get network info: $_" "ERROR"
        return $null
    }
}

function Get-Hotfixes {
    try {
        Write-Log "Collecting installed hotfixes..."
        $hotfixes = Get-HotFix -ErrorAction SilentlyContinue | 
            Select-Object HotFixID, Description, InstalledBy, InstalledOn
        return $hotfixes
    } catch {
        Write-Log "Failed to get hotfixes: $_" "ERROR"
        return $null
    }
}

function Get-Services {
    try {
        Write-Log "Collecting services..."
        $services = Get-Service -ErrorAction SilentlyContinue | 
            Select-Object Name, DisplayName, Status, StartType
        return $services
    } catch {
        Write-Log "Failed to get services: $_" "ERROR"
        return $null
    }
}

function Get-DiskInfo {
    try {
        Write-Log "Collecting disk information..."
        $disks = Get-PSDrive -PSProvider FileSystem -ErrorAction SilentlyContinue | 
            Select-Object Name, Used, Free, @{Name="UsedGB";Expression={[math]::Round($_.Used/1GB,2)}}, @{Name="FreeGB";Expression={[math]::Round($_.Free/1GB,2)}}
        return $disks
    } catch {
        Write-Log "Failed to get disk info: $_" "ERROR"
        return $null
    }
}

function Write-HostInfoLog {
    param(
        [hashtable]$Data,
        [string]$LogPath
    )
    
    try {
        $logContent = @"
========================================
HOST INFORMATION COLLECTION REPORT
========================================
Collection Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Hostname: $env:COMPUTERNAME
Username: $env:USERNAME
Domain: $env:USERDOMAIN

========================================
SYSTEM INFORMATION
========================================
$($Data.SystemInfo | ConvertTo-Json -Depth 3)

========================================
IP CONFIGURATION
========================================
$($Data.IPConfig -join "`n")

========================================
NETWORK ADAPTERS
========================================
$($Data.NetworkInfo | ConvertTo-Json -Depth 3)

========================================
INSTALLED PROGRAMS ($($Data.InstalledPrograms.Count) total)
========================================
$($Data.InstalledPrograms | Select-Object DisplayName, DisplayVersion, Publisher | ConvertTo-Json -Depth 3)

========================================
APPX PACKAGES ($($Data.AppxPackages.Count) total)
========================================
$($Data.AppxPackages | Select-Object Name, Version | ConvertTo-Json -Depth 3)

========================================
INSTALLED HOTFIXES ($($Data.Hotfixes.Count) total)
========================================
$($Data.Hotfixes | ConvertTo-Json -Depth 3)

========================================
SERVICES ($($Data.Services.Count) total)
========================================
$($Data.Services | ConvertTo-Json -Depth 3)

========================================
DISK INFORMATION
========================================
$($Data.DiskInfo | ConvertTo-Json -Depth 3)

========================================
END OF REPORT
========================================
"@
        
        Set-Content -Path $LogPath -Value $logContent -Encoding UTF8
        Write-Log "Host information log written to: $LogPath"
        return $true
    } catch {
        Write-Log "Failed to write host info log: $_" "ERROR"
        return $false
    }
}

function Upload-Log {
    param(
        [string]$SourcePath,
        [string]$DestPath,
        [string]$Prefix
    )
    
    try {
        if (-not (Test-Path $SourcePath)) {
            Write-Log "Source file not found: $SourcePath" "ERROR"
            return $false
        }
        
        if (-not (Test-Path $DestPath)) {
            Write-Log "Destination path not found: $DestPath" "ERROR"
            return $false
        }
        
        $filename = Split-Path $SourcePath -Leaf
        $destFile = Join-Path $DestPath "${Prefix}_${filename}"
        
        Copy-Item -Path $SourcePath -Destination $destFile -Force -ErrorAction Stop
        Write-Log "Log uploaded to: $destFile"
        return $true
    } catch {
        Write-Log "Failed to upload log: $_" "ERROR"
        return $false
    }
}

$config = Get-Config -Path $ConfigPath
$localDir = $config.paths.local_work_dir

Write-Log "=== Starting Host Information Collection ==="

if (-not (Test-Path $localDir)) {
    Write-Log "Local directory not found: $localDir" "ERROR"
    exit 1
}

$hostInfoLog = Join-Path $localDir "1_host.log"

$collectionData = @{
    SystemInfo = Get-SystemInfo
    IPConfig = Get-IPConfig
    InstalledPrograms = Get-InstalledPrograms
    AppxPackages = Get-AppxPackages
    NetworkInfo = Get-NetworkInfo
    Hotfixes = Get-Hotfixes
    Services = Get-Services
    DiskInfo = Get-DiskInfo
}

$writeResult = Write-HostInfoLog -Data $collectionData -LogPath $hostInfoLog

if ($writeResult) {
    $logUploadPath = $config.paths.log_upload_path
    $ipAddress = (Get-NetIPAddress -AddressFamily IPv4 -ErrorAction SilentlyContinue | Where-Object { $_.IPAddress -notlike "127.*" } | Select-Object -First 1).IPAddress
    
    if ($ipAddress) {
        $uploadResult = Upload-Log -SourcePath $hostInfoLog -DestPath $logUploadPath -Prefix $ipAddress
        
        if ($uploadResult) {
            Write-Log "=== Host Information Collection Completed Successfully ==="
            exit 0
        } else {
            Write-Log "=== Host Information Collection Completed but Upload Failed ===" "ERROR"
            exit 1
        }
    } else {
        Write-Log "Failed to get IP address for upload prefix" "ERROR"
        Write-Log "=== Host Information Collection Completed (No Upload) ==="
        exit 0
    }
} else {
    Write-Log "=== Host Information Collection Failed ===" "ERROR"
    exit 1
}


function Get-SystemInfo {
    try {
        Write-Log "Collecting system information..."
        $systemInfo = systeminfo /FO CSV | ConvertFrom-Csv
        return $systemInfo
    } catch {
        Write-Log "Failed to get system info: $_" "ERROR"
        return $null
    }
}

function Get-IPConfig {
    try {
        Write-Log "Collecting IP configuration..."
        $ipConfig = ipconfig /all
        return $ipConfig
    } catch {
        Write-Log "Failed to get IP config: $_" "ERROR"
        return $null
    }
}

function Get-InstalledPrograms {
    try {
        Write-Log "Collecting installed programs..."
        
        $programs = @()
        
        $registryPaths = @(
            "HKLM:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*",
            "HKLM:\Software\WOW6432Node\Microsoft\Windows\CurrentVersion\Uninstall\*",
            "HKCU:\Software\Microsoft\Windows\CurrentVersion\Uninstall\*"
        )
        
        foreach ($path in $registryPaths) {
            $programs += Get-ItemProperty $path -ErrorAction SilentlyContinue | 
                Where-Object { $_.DisplayName } | 
                Select-Object DisplayName, DisplayVersion, Publisher, InstallDate, InstallLocation
        }
        
        return $programs
    } catch {
        Write-Log "Failed to get installed programs: $_" "ERROR"
        return $null
    }
}

function Get-AppxPackages {
    try {
        Write-Log "Collecting AppX packages..."
        $appxPackages = Get-AppxPackage -ErrorAction SilentlyContinue | 
            Select-Object Name, Version, Publisher, InstallLocation
        return $appxPackages
    } catch {
        Write-Log "Failed to get AppX packages: $_" "WARN"
        return $null
    }
}

function Get-NetworkInfo {
    try {
        Write-Log "Collecting network information..."
        
        $networkInfo = @()
        
        $adapters = Get-NetAdapter -ErrorAction SilentlyContinue
        foreach ($adapter in $adapters) {
            $ipAddresses = Get-NetIPAddress -InterfaceIndex $adapter.ifIndex -ErrorAction SilentlyContinue
            $dnsServers = Get-DnsClientServerAddress -InterfaceIndex $adapter.ifIndex -ErrorAction SilentlyContinue
            
            $networkInfo += [PSCustomObject]@{
                Name = $adapter.Name
                InterfaceDescription = $adapter.InterfaceDescription
                Status = $adapter.Status
                LinkSpeed = $adapter.LinkSpeed
                MACAddress = $adapter.MacAddress
                IPAddresses = ($ipAddresses | ForEach-Object { $_.IPAddress }) -join ", "
                DNSServers = ($dnsServers | ForEach-Object { $_.ServerAddresses }) -join ", "
            }
        }
        
        return $networkInfo
    } catch {
        Write-Log "Failed to get network info: $_" "ERROR"
        return $null
    }
}

function Get-Hotfixes {
    try {
        Write-Log "Collecting installed hotfixes..."
        $hotfixes = Get-HotFix -ErrorAction SilentlyContinue | 
            Select-Object HotFixID, Description, InstalledBy, InstalledOn
        return $hotfixes
    } catch {
        Write-Log "Failed to get hotfixes: $_" "ERROR"
        return $null
    }
}

function Get-Services {
    try {
        Write-Log "Collecting services..."
        $services = Get-Service -ErrorAction SilentlyContinue | 
            Select-Object Name, DisplayName, Status, StartType
        return $services
    } catch {
        Write-Log "Failed to get services: $_" "ERROR"
        return $null
    }
}

function Get-DiskInfo {
    try {
        Write-Log "Collecting disk information..."
        $disks = Get-PSDrive -PSProvider FileSystem -ErrorAction SilentlyContinue | 
            Select-Object Name, Used, Free, @{Name="UsedGB";Expression={[math]::Round($_.Used/1GB,2)}}, @{Name="FreeGB";Expression={[math]::Round($_.Free/1GB,2)}}
        return $disks
    } catch {
        Write-Log "Failed to get disk info: $_" "ERROR"
        return $null
    }
}

function Write-HostInfoLog {
    param(
        [hashtable]$Data,
        [string]$LogPath
    )
    
    try {
        $logContent = @"
========================================
HOST INFORMATION COLLECTION REPORT
========================================
Collection Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Hostname: $env:COMPUTERNAME
Username: $env:USERNAME
Domain: $env:USERDOMAIN

========================================
SYSTEM INFORMATION
========================================
$($Data.SystemInfo | ConvertTo-Json -Depth 3)

========================================
IP CONFIGURATION
========================================
$($Data.IPConfig -join "`n")

========================================
NETWORK ADAPTERS
========================================
$($Data.NetworkInfo | ConvertTo-Json -Depth 3)

========================================
INSTALLED PROGRAMS ($($Data.InstalledPrograms.Count) total)
========================================
$($Data.InstalledPrograms | Select-Object DisplayName, DisplayVersion, Publisher | ConvertTo-Json -Depth 3)

========================================
APPX PACKAGES ($($Data.AppxPackages.Count) total)
========================================
$($Data.AppxPackages | Select-Object Name, Version | ConvertTo-Json -Depth 3)

========================================
INSTALLED HOTFIXES ($($Data.Hotfixes.Count) total)
========================================
$($Data.Hotfixes | ConvertTo-Json -Depth 3)

========================================
SERVICES ($($Data.Services.Count) total)
========================================
$($Data.Services | ConvertTo-Json -Depth 3)

========================================
DISK INFORMATION
========================================
$($Data.DiskInfo | ConvertTo-Json -Depth 3)

========================================
END OF REPORT
========================================
"@
        
        Set-Content -Path $LogPath -Value $logContent -Encoding UTF8
        Write-Log "Host information log written to: $LogPath"
        return $true
    } catch {
        Write-Log "Failed to write host info log: $_" "ERROR"
        return $false
    }
}

function Upload-Log {
    param(
        [string]$SourcePath,
        [string]$DestPath,
        [string]$Prefix
    )
    
    try {
        if (-not (Test-Path $SourcePath)) {
            Write-Log "Source file not found: $SourcePath" "ERROR"
            return $false
        }
        
        if (-not (Test-Path $DestPath)) {
            Write-Log "Destination path not found: $DestPath" "ERROR"
            return $false
        }
        
        $filename = Split-Path $SourcePath -Leaf
        $destFile = Join-Path $DestPath "${Prefix}_${filename}"
        
        Copy-Item -Path $SourcePath -Destination $destFile -Force -ErrorAction Stop
        Write-Log "Log uploaded to: $destFile"
        return $true
    } catch {
        Write-Log "Failed to upload log: $_" "ERROR"
        return $false
    }
}

$config = Get-Config -Path $ConfigPath
$localDir = $config.paths.local_work_dir

Write-Log "=== Starting Host Information Collection ==="

if (-not (Test-Path $localDir)) {
    Write-Log "Local directory not found: $localDir" "ERROR"
    exit 1
}

$hostInfoLog = Join-Path $localDir "1_host.log"

$collectionData = @{
    SystemInfo = Get-SystemInfo
    IPConfig = Get-IPConfig
    InstalledPrograms = Get-InstalledPrograms
    AppxPackages = Get-AppxPackages
    NetworkInfo = Get-NetworkInfo
    Hotfixes = Get-Hotfixes
    Services = Get-Services
    DiskInfo = Get-DiskInfo
}

$writeResult = Write-HostInfoLog -Data $collectionData -LogPath $hostInfoLog

if ($writeResult) {
    $logUploadPath = $config.paths.log_upload_path
    $ipAddress = (Get-NetIPAddress -AddressFamily IPv4 -ErrorAction SilentlyContinue | Where-Object { $_.IPAddress -notlike "127.*" } | Select-Object -First 1).IPAddress
    
    if ($ipAddress) {
        $uploadResult = Upload-Log -SourcePath $hostInfoLog -DestPath $logUploadPath -Prefix $ipAddress
        
        if ($uploadResult) {
            Write-Log "=== Host Information Collection Completed Successfully ==="
            exit 0
        } else {
            Write-Log "=== Host Information Collection Completed but Upload Failed ===" "ERROR"
            exit 1
        }
    } else {
        Write-Log "Failed to get IP address for upload prefix" "ERROR"
        Write-Log "=== Host Information Collection Completed (No Upload) ==="
        exit 0
    }
} else {
    Write-Log "=== Host Information Collection Failed ===" "ERROR"
    exit 1
}



