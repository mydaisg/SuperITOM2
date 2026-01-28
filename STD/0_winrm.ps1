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
        $config = Get-Content $Path | ConvertFrom-Json
        $hashtable = @{}
        $config.PSObject.Properties | ForEach-Object {
            $hashtable[$_.Name] = $_.Value
        }
        return $hashtable
    } else {
        Write-Log "Config file not found: $Path" "ERROR"
        exit 1
    }
}

function Enable-WinRM {
    param(
        [hashtable]$Config
    )
    
    try {
        Write-Log "Configuring WinRM settings..."
        
        $winrmConfig = $Config.winrm
        
        Enable-PSRemoting -Force -ErrorAction Stop
        Write-Log "PSRemoting enabled"
        
        Set-Item WSMan:\localhost\Client\TrustedHosts "*" -Force -ErrorAction Stop
        Write-Log "TrustedHosts configured to accept all connections"
        
        Set-Item WSMan:\localhost\MaxEnvelopeSizekb $winrmConfig.max_envelop_size_kb -Force -ErrorAction Stop
        Write-Log "MaxEnvelopeSizekb set to $($winrmConfig.max_envelop_size_kb)"
        
        Set-Item WSMan:\localhost\MaxTimeoutms $winrmConfig.max_timeout_ms -Force -ErrorAction Stop
        Write-Log "MaxTimeoutms set to $($winrmConfig.max_timeout_ms)"
        
        $listener = Get-ChildItem WSMan:\localhost\Listener
        if ($listener) {
            Write-Log "WinRM listeners configured:"
            $listener | ForEach-Object {
                Write-Log "  - $($_.Name): $($_.Value)"
            }
        } else {
            Write-Log "No WinRM listeners found" "WARN"
        }
        
        $firewallRule = Get-NetFirewallRule -DisplayName "Windows Remote Management (HTTP-In)" -ErrorAction SilentlyContinue
        if ($firewallRule) {
            if ($firewallRule.Enabled -eq "False") {
                Enable-NetFirewallRule -DisplayName "Windows Remote Management (HTTP-In)" -ErrorAction Stop
                Write-Log "WinRM firewall rule enabled"
            } else {
                Write-Log "WinRM firewall rule already enabled"
            }
        } else {
            New-NetFirewallRule -DisplayName "Windows Remote Management (HTTP-In)" -Direction Inbound -LocalPort $winrmConfig.port -Protocol TCP -Action Allow -ErrorAction Stop
            Write-Log "WinRM firewall rule created"
        }
        
        Restart-Service WinRM -Force -ErrorAction Stop
        Write-Log "WinRM service restarted"
        
        return $true
    } catch {
        Write-Log "WinRM configuration failed: $_" "ERROR"
        return $false
    }
}

function Test-WinRM {
    param(
        [hashtable]$Config
    )
    
    try {
        Write-Log "Testing WinRM connectivity..."
        
        $testResult = Test-WSMan -ErrorAction Stop
        if ($testResult) {
            Write-Log "WinRM service is responding"
            Write-Log "ProductVersion: $($testResult.ProductVersion)"
            Write-Log "ProductVendor: $($testResult.ProductVendor)"
            
            $session = New-PSSession -ComputerName localhost -ErrorAction Stop
            if ($session) {
                Write-Log "Local PowerShell session created successfully"
                Remove-PSSession $session -ErrorAction SilentlyContinue
                Write-Log "Local PowerShell session test passed"
                return $true
            } else {
                Write-Log "Failed to create local PowerShell session" "ERROR"
                return $false
            }
        } else {
            Write-Log "WinRM service not responding" "ERROR"
            return $false
        }
    } catch {
        Write-Log "WinRM test failed: $_" "ERROR"
        return $false
    }
}

function Get-WinRMStatus {
    try {
        $service = Get-Service WinRM -ErrorAction Stop
        Write-Log "WinRM Service Status:"
        Write-Log "  - Status: $($service.Status)"
        Write-Log "  - StartType: $($service.StartType)"
        
        $config = Get-ChildItem WSMan:\localhost -Recurse -ErrorAction Stop
        Write-Log "WinRM Configuration:"
        $config | ForEach-Object {
            Write-Log "  - $($_.PSPath): $($_.Value)"
        }
        
        return $true
    } catch {
        Write-Log "Failed to get WinRM status: $_" "ERROR"
        return $false
    }
}

$config = Get-Config -Path $ConfigPath
$localDir = $config.paths.local_work_dir

Write-Log "=== Starting WinRM Configuration ==="

if (-not (Test-Path $localDir)) {
    Write-Log "Local directory not found: $localDir" "ERROR"
    exit 1
}

$adminCheck = Test-AdminPrivileges
if (-not $adminCheck) {
    Write-Log "This script requires administrator privileges" "ERROR"
    Write-Log "Please run as administrator" "ERROR"
    exit 1
}

$enableResult = Enable-WinRM -Config $config

if ($enableResult) {
    $testResult = Test-WinRM -Config $config
    
    $statusResult = Get-WinRMStatus
    
    $logFile = Join-Path $localDir "1_ps.log"
    $logEntry = @"
WinRM Configuration Log
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Configuration Status: Success
WinRM Service: $(Get-Service WinRM | Select-Object -ExpandProperty Status)
"@
    Add-Content -Path $logFile -Value $logEntry
    Write-Log "WinRM configuration log appended to: $logFile"
    
    if ($testResult) {
        Write-Log "=== WinRM Configuration Completed Successfully ==="
        exit 0
    } else {
        Write-Log "=== WinRM Configuration Completed but Tests Failed ===" "ERROR"
        exit 1
    }
} else {
    Write-Log "=== WinRM Configuration Failed ===" "ERROR"
    exit 1
}

