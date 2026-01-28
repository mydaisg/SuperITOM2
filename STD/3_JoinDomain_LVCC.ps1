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

function Test-AdminPrivileges {
    $currentUser = [Security.Principal.WindowsIdentity]::GetCurrent()
    $principal = New-Object Security.Principal.WindowsPrincipal($currentUser)
    $isAdmin = $principal.IsInRole([Security.Principal.WindowsBuiltinRole]::Administrator)
    
    if ($isAdmin) {
        Write-Log "Running with administrator privileges"
        return $true
    } else {
        Write-Log "Not running with administrator privileges" "ERROR"
        return $false
    }
}

function Test-DomainConnectivity {
    param(
        [string]$DomainName,
        [string[]]$DNSServers
    )
    
    try {
        Write-Log "Testing domain connectivity..."
        
        foreach ($dnsServer in $DNSServers) {
            $pingResult = Test-Connection -ComputerName $dnsServer -Count 2 -Quiet -ErrorAction SilentlyContinue
            if ($pingResult) {
                Write-Log "DNS server $dnsServer is reachable"
            } else {
                Write-Log "DNS server $dnsServer is not reachable" "WARN"
            }
        }
        
        $domainController = $DomainName
        $pingResult = Test-Connection -ComputerName $domainController -Count 2 -Quiet -ErrorAction SilentlyContinue
        
        if ($pingResult) {
            Write-Log "Domain controller $domainController is reachable"
            return $true
        } else {
            Write-Log "Domain controller $domainController is not reachable" "ERROR"
            return $false
        }
    } catch {
        Write-Log "Domain connectivity test failed: $_" "ERROR"
        return $false
    }
}

function Configure-DNS {
    param(
        [string[]]$DNSServers
    )
    
    try {
        Write-Log "Configuring DNS servers..."
        
        $adapters = Get-NetAdapter -ErrorAction SilentlyContinue | Where-Object { $_.Status -eq "Up" }
        
        foreach ($adapter in $adapters) {
            Write-Log "Configuring DNS for adapter: $($adapter.Name)"
            Set-DnsClientServerAddress -InterfaceIndex $adapter.ifIndex -ServerAddresses $DNSServers -ErrorAction Stop
            Write-Log "DNS servers configured: $($DNSServers -join ', ')"
        }
        
        Start-Sleep -Seconds 3
        return $true
    } catch {
        Write-Log "DNS configuration failed: $_" "ERROR"
        return $false
    }
}

function Join-Domain {
    param(
        [string]$DomainName,
        [string]$OUPath,
        [string]$Credential,
        [string]$LogPath
    )
    
    try {
        Write-Log "Joining domain: $DomainName"
        Write-Log "OU Path: $OUPath"
        
        $currentDomain = (Get-WmiObject Win32_ComputerSystem).Domain
        if ($currentDomain -eq $DomainName) {
            Write-Log "Computer is already joined to domain: $DomainName"
            return $true
        }
        
        $securePassword = ConvertTo-SecureString $Credential -AsPlainText -Force
        $credentialObject = New-Object System.Management.Automation.PSCredential($DomainName, $securePassword)
        
        Add-Computer -DomainName $DomainName -OUPath $OUPath -Credential $credentialObject -Force -ErrorAction Stop
        
        $logEntry = @"
Domain Join Log
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Domain: $DomainName
OU Path: $OUPath
Status: Success - Reboot required
"@
        
        Add-Content -Path $LogPath -Value $logEntry
        Write-Log "Domain join successful. Reboot required."
        Write-Log "Log entry added to: $LogPath"
        
        return $true
    } catch {
        Write-Log "Domain join failed: $_" "ERROR"
        
        $logEntry = @"
Domain Join Log
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Domain: $DomainName
OU Path: $OUPath
Status: Failed
Error: $_
"@
        
        Add-Content -Path $LogPath -Value $logEntry
        return $false
    }
}

function Test-DomainMembership {
    param([string]$DomainName)
    
    try {
        Write-Log "Verifying domain membership..."
        
        $computerSystem = Get-WmiObject Win32_ComputerSystem -ErrorAction Stop
        $currentDomain = $computerSystem.Domain
        $workgroup = $computerSystem.Workgroup
        
        Write-Log "Current domain: $currentDomain"
        Write-Log "Workgroup: $workgroup"
        
        if ($currentDomain -eq $DomainName) {
            Write-Log "Computer is joined to domain: $DomainName"
            
            $domainInfo = Get-ADDomain -Server $DomainName -ErrorAction SilentlyContinue
            if ($domainInfo) {
                Write-Log "Domain verified: $($domainInfo.Name)"
                Write-Log "Domain controllers: $($domainInfo.ReplicaDirectoryServers -join ', ')"
            }
            
            return $true
        } else {
            Write-Log "Computer is not joined to domain: $DomainName" "ERROR"
            return $false
        }
    } catch {
        Write-Log "Domain membership verification failed: $_" "ERROR"
        return $false
    }
}

function Get-GPOStatus {
    param([string]$LogPath)
    
    try {
        Write-Log "Checking GPO status..."
        
        $gpoResult = gpresult /R /SCOPE COMPUTER 2>&1
        
        if ($LASTEXITCODE -eq 0) {
            Write-Log "GPO information retrieved successfully"
            
            $logEntry = @"

========================================
GPO STATUS
========================================
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")

$($gpoResult -join "`n")

========================================
END OF GPO STATUS
========================================
"@
            
            Add-Content -Path $LogPath -Value $logEntry
            Write-Log "GPO status written to log"
            return $true
        } else {
            Write-Log "Failed to retrieve GPO information" "WARN"
            return $false
        }
    } catch {
        Write-Log "GPO status check failed: $_" "WARN"
        return $false
    }
}

function Write-DomainJoinLog {
    param(
        [string]$LogPath,
        [hashtable]$Data
    )
    
    try {
        $logContent = @"
========================================
DOMAIN JOIN LOG
========================================
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Hostname: $env:COMPUTERNAME

========================================
DOMAIN CONFIGURATION
========================================
Domain: $($Data.DomainName)
OU Path: $($Data.OUPath)
DNS Servers: $($Data.DNSServers -join ', ')

========================================
JOIN STATUS
========================================
Status: $($Data.Status)
Reboot Required: $($Data.RebootRequired)
Error: $($Data.Error)

========================================
VERIFICATION
========================================
Domain Membership: $($Data.DomainMembership)
GPO Status: $($Data.GPOStatus)

========================================
END OF LOG
========================================
"@
        
        Set-Content -Path $LogPath -Value $logContent -Encoding UTF8
        Write-Log "Domain join log written to: $LogPath"
        return $true
    } catch {
        Write-Log "Failed to write domain join log: $_" "ERROR"
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

Write-Log "=== Starting Domain Join Process ==="

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

$domainName = $config.domain.name
$ouPath = $config.domain.ou_path
$dnsServers = $config.domain.dns_servers
$adminPassword = $config.domain.admin_password

if ([string]::IsNullOrEmpty($adminPassword)) {
    Write-Log "Domain admin password not configured in config file" "ERROR"
    Write-Log "Please set 'domain.admin_password' in config.json" "ERROR"
    exit 1
}

$connectivityTest = Test-DomainConnectivity -DomainName $domainName -DNSServers $dnsServers

if ($connectivityTest) {
    $dnsConfigResult = Configure-DNS -DNSServers $dnsServers
    
    if ($dnsConfigResult) {
        $domainJoinLog = Join-Path $localDir "3_JoinDomain.log"
        
        $joinResult = Join-Domain -DomainName $domainName -OUPath $ouPath -Credential $adminPassword -LogPath $domainJoinLog
        
        if ($joinResult) {
            $domainMembership = Test-DomainMembership -DomainName $domainName
            $gpoStatus = Get-GPOStatus -LogPath $domainJoinLog
            
            $logData = @{
                DomainName = $domainName
                OUPath = $ouPath
                DNSServers = $dnsServers
                Status = "Success"
                RebootRequired = $true
                Error = ""
                DomainMembership = $domainMembership
                GPOStatus = $gpoStatus
            }
            
            Write-DomainJoinLog -LogPath $domainJoinLog -Data $logData
            
            $logUploadPath = $config.paths.log_upload_path
            $uploadResult = Upload-Log -SourcePath $domainJoinLog -DestPath $logUploadPath -Prefix $env:COMPUTERNAME
            
            if ($uploadResult) {
                Write-Log "=== Domain Join Process Completed Successfully ==="
                Write-Log "WARNING: System reboot is required to complete domain join" "WARN"
                Write-Log "Please run: Restart-Computer -Force" "WARN"
                exit 0
            } else {
                Write-Log "=== Domain Join Process Completed but Upload Failed ===" "ERROR"
                exit 1
            }
        } else {
            $logData = @{
                DomainName = $domainName
                OUPath = $ouPath
                DNSServers = $dnsServers
                Status = "Failed"
                RebootRequired = $false
                Error = "Domain join failed"
                DomainMembership = $false
                GPOStatus = $false
            }
            
            Write-DomainJoinLog -LogPath $domainJoinLog -Data $logData
            Write-Log "=== Domain Join Process Failed ===" "ERROR"
            exit 1
        }
    } else {
        Write-Log "=== Domain Join Process Failed (DNS Configuration) ===" "ERROR"
        exit 1
    }
} else {
    Write-Log "=== Domain Join Process Failed (Connectivity) ===" "ERROR"
    exit 1
}