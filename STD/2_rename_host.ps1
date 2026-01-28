param(
    [string]$ConfigPath = "D:\GitHub\SuperITOM\config\config.json",
    [string]$HostsCSVPath = "D:\GitHub\SuperITOM\config\hosts.csv"
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

function Get-HostMapping {
    param(
        [string]$CSVPath,
        [string]$CurrentHostname,
        [string]$IPAddress
    )
    
    try {
        if (-not (Test-Path $CSVPath)) {
            Write-Log "Hosts CSV file not found: $CSVPath" "ERROR"
            return $null
        }
        
        $hosts = Import-Csv -Path $CSVPath -ErrorAction Stop
        
        $hostEntry = $hosts | Where-Object { 
            $_.Hostname -eq $CurrentHostname -or 
            $_.IPAddress -eq $IPAddress 
        } | Select-Object -First 1
        
        if ($hostEntry) {
            Write-Log "Found host mapping for $CurrentHostname ($IPAddress)"
            Write-Log "Target hostname: $($hostEntry.Hostname)"
            Write-Log "Location: $($hostEntry.Location)"
            Write-Log "Department: $($hostEntry.Department)"
            return $hostEntry
        } else {
            Write-Log "No host mapping found for $CurrentHostname ($IPAddress)" "WARN"
            return $null
        }
    } catch {
        Write-Log "Failed to read hosts CSV: $_" "ERROR"
        return $null
    }
}

function Generate-Hostname {
    param(
        [string]$Prefix,
        [string]$Location,
        [string]$Department,
        [string]$Sequence
    )
    
    try {
        $hostname = "$Prefix-$Location-$Department-$Sequence"
        Write-Log "Generated hostname: $hostname"
        return $hostname
    } catch {
        Write-Log "Failed to generate hostname: $_" "ERROR"
        return $null
    }
}

function Rename-Computer {
    param(
        [string]$NewHostname,
        [string]$LogPath
    )
    
    try {
        $currentHostname = $env:COMPUTERNAME
        
        if ($currentHostname -eq $NewHostname) {
            Write-Log "Hostname already matches: $NewHostname"
            return $true
        }
        
        Write-Log "Renaming computer from $currentHostname to $NewHostname"
        
        Rename-Computer -NewName $NewHostname -Force -ErrorAction Stop
        
        $logEntry = @"
Hostname Rename Log
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Old Hostname: $currentHostname
New Hostname: $NewHostname
Status: Success - Reboot required
"@
        
        Add-Content -Path $LogPath -Value $logEntry
        Write-Log "Computer renamed successfully. Reboot required."
        Write-Log "Log entry added to: $LogPath"
        
        return $true
    } catch {
        Write-Log "Failed to rename computer: $_" "ERROR"
        return $false
    }
}

function Write-HostRenameLog {
    param(
        [string]$LogPath,
        [hashtable]$Data
    )
    
    try {
        $logContent = @"
========================================
HOSTNAME STANDARDIZATION LOG
========================================
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")

Current Hostname: $($Data.CurrentHostname)
IP Address: $($Data.IPAddress)
New Hostname: $($Data.NewHostname)
Location: $($Data.Location)
Department: $($Data.Department)
Status: $($Data.Status)

========================================
RENAME DETAILS
========================================
Rename Required: $($Data.RenameRequired)
Reboot Required: $($Data.RebootRequired)

========================================
END OF LOG
========================================
"@
        
        Set-Content -Path $LogPath -Value $logContent -Encoding UTF8
        Write-Log "Host rename log written to: $LogPath"
        return $true
    } catch {
        Write-Log "Failed to write host rename log: $_" "ERROR"
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

Write-Log "=== Starting Hostname Standardization ==="

if (-not (Test-Path $localDir)) {
    Write-Log "Local directory not found: $localDir" "ERROR"
    exit 1
}

$currentHostname = $env:COMPUTERNAME

try {
    Write-Log "Getting IP address..."
    $ipAddress = (Get-NetIPAddress -AddressFamily IPv4 -ErrorAction SilentlyContinue | Where-Object { $_.IPAddress -notlike "127.*" } | Select-Object -First 1).IPAddress
    
    if (-not $ipAddress) {
        Write-Log "Failed to get IP address, using default" "WARN"
        $ipAddress = "0.0.0.0"
    }
} catch {
    Write-Log "Error getting IP address: $_" "WARN"
    $ipAddress = "0.0.0.0"
}

Write-Log "Current hostname: $currentHostname"
Write-Log "IP Address: $ipAddress"

Write-Log "Looking for host mapping..."
$hostMapping = Get-HostMapping -CSVPath $HostsCSVPath -CurrentHostname $currentHostname -IPAddress $ipAddress

Write-Log "Host mapping result: $($hostMapping -ne $null)"

$newHostname = ""
$location = ""
$department = ""
$renameRequired = $false
$status = "No changes required"

if ($hostMapping) {
    $newHostname = $hostMapping.Hostname
    $location = $hostMapping.Location
    $department = $hostMapping.Department
    
    if ($currentHostname -ne $newHostname) {
        $renameRequired = $true
        $status = "Rename required"
        
        $hostRenameLog = Join-Path $localDir "2_host.log"
        
        $renameResult = Rename-Computer -NewHostname $newHostname -LogPath $hostRenameLog
        
        if ($renameResult) {
            $status = "Rename successful - Reboot required"
        } else {
            $status = "Rename failed"
        }
    } else {
        Write-Log "Hostname already matches standard: $currentHostname"
    }
} else {
    Write-Log "No host mapping found, skipping rename" "WARN"
    $status = "No mapping found"
}

$logData = @{
    CurrentHostname = $currentHostname
    IPAddress = $ipAddress
    NewHostname = $newHostname
    Location = $location
    Department = $department
    Status = $status
    RenameRequired = $renameRequired
    RebootRequired = $renameRequired
}

$hostRenameLog = Join-Path $localDir "2_host.log"
Write-HostRenameLog -LogPath $hostRenameLog -Data $logData

$logUploadPath = $config.paths.log_upload_path
$uploadResult = Upload-Log -SourcePath $hostRenameLog -DestPath $logUploadPath -Prefix $currentHostname

if ($uploadResult) {
    Write-Log "=== Hostname Standardization Completed ==="
    
    if ($renameRequired -and $status -like "*successful*") {
        Write-Log "WARNING: System reboot is required to complete hostname change" "WARN"
        Write-Log "Please run: Restart-Computer -Force" "WARN"
    }
    
    exit 0
} else {
    Write-Log "=== Hostname Standardization Completed but Upload Failed ===" "ERROR"
    exit 1
}
