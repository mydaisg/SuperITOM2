# Disable default administrator account
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

function Disable-DefaultAdminAccount {
    param(
        [string]$LogPath
    )
    
    try {
        Write-Log "Checking default administrator account..."
        
        $defaultAdmin = "Administrator"
        $adminExists = Get-LocalUser -Name $defaultAdmin -ErrorAction SilentlyContinue
        
        if ($adminExists) {
            Write-Log "Default administrator account found: $defaultAdmin"
            
            Disable-LocalUser -Name $defaultAdmin -ErrorAction Stop
            Write-Log "Disabled default administrator account: $defaultAdmin"
            
            $logEntry = @"
Default Admin Account Status
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Account: $defaultAdmin
Status: Disabled
"@
            
            Add-Content -Path $LogPath -Value $logEntry
            return $true
        } else {
            Write-Log "Default administrator account not found" "WARN"
            return $false
        }
    } catch {
        Write-Log "Failed to disable default admin account: $_" "ERROR"
        return $false
    }
}

function Rename-DefaultAdminAccount {
    param(
        [string]$NewName,
        [string]$LogPath
    )
    
    try {
        Write-Log "Renaming default administrator account..."
        
        $defaultAdmin = "Administrator"
        $adminExists = Get-LocalUser -Name $defaultAdmin -ErrorAction SilentlyContinue
        
        if ($adminExists) {
            Write-Log "Renaming $defaultAdmin to $NewName"
            
            Rename-LocalUser -Name $defaultAdmin -NewName $NewName -ErrorAction Stop
            Write-Log "Successfully renamed administrator account to: $NewName"
            
            $logEntry = @"
Admin Account Rename
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Old Name: $defaultAdmin
New Name: $NewName
Status: Success
"@
            
            Add-Content -Path $LogPath -Value $logEntry
            return $true
        } else {
            Write-Log "Default administrator account not found" "WARN"
            return $false
        }
    } catch {
        Write-Log "Failed to rename admin account: $_" "ERROR"
        return $false
    }
}

function Create-NewAdminAccount {
    param(
        [string]$Username,
        [string]$Password,
        [string]$Description,
        [string]$LogPath
    )
    
    try {
        Write-Log "Creating new administrator account: $Username"
        
        $userExists = Get-LocalUser -Name $Username -ErrorAction SilentlyContinue
        
        if ($userExists) {
            Write-Log "User $Username already exists" "WARN"
            
            $securePassword = ConvertTo-SecureString $Password -AsPlainText -Force
            Set-LocalUser -Name $Username -Password $securePassword -ErrorAction Stop
            Write-Log "Updated password for existing user: $Username"
        } else {
            $securePassword = ConvertTo-SecureString $Password -AsPlainText -Force
            New-LocalUser -Name $Username -Password $securePassword -Description $Description -ErrorAction Stop
            Write-Log "Created new user: $Username"
        }
        
        Add-LocalGroupMember -Group "Administrators" -Member $Username -ErrorAction Stop
        Write-Log "Added $Username to Administrators group"
        
        $logEntry = @"
Admin Account Creation
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Username: $Username
Description: $Description
Status: Success
"@
        
        Add-Content -Path $LogPath -Value $logEntry
        return $true
    } catch {
        Write-Log "Failed to create admin account: $_" "ERROR"
        return $false
    }
}

function Create-AdminGroup {
    param(
        [string]$GroupName,
        [string]$Description,
        [string]$LogPath
    )
    
    try {
        Write-Log "Creating administrator group: $GroupName"
        
        $groupExists = Get-LocalGroup -Name $GroupName -ErrorAction SilentlyContinue
        
        if ($groupExists) {
            Write-Log "Group $GroupName already exists" "WARN"
        } else {
            New-LocalGroup -Name $GroupName -Description $Description -ErrorAction Stop
            Write-Log "Created new group: $GroupName"
        }
        
        $logEntry = @"
Admin Group Creation
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Group Name: $GroupName
Description: $Description
Status: Success
"@
        
        Add-Content -Path $LogPath -Value $logEntry
        return $true
    } catch {
        Write-Log "Failed to create admin group: $_" "ERROR"
        return $false
    }
}

function Get-LocalAdmins {
    try {
        Write-Log "Retrieving local administrators..."
        
        $adminGroup = Get-LocalGroup -Name "Administrators" -ErrorAction Stop
        $adminMembers = Get-LocalGroupMember -Group $adminGroup -ErrorAction Stop
        
        Write-Log "Local administrators:"
        foreach ($member in $adminMembers) {
            Write-Log "  - $($member.Name) ($($member.ObjectClass))"
        }
        
        return $adminMembers
    } catch {
        Write-Log "Failed to retrieve local administrators: $_" "ERROR"
        return $null
    }
}

function Configure-LocalAdminPolicies {
    param(
        [string]$LogPath
    )
    
    try {
        Write-Log "Configuring local admin policies..."
        
        $policies = @(
            @{Name = "EnableAdminAccount"; Value = 0},
            @{Name = "NewAdministratorName"; Value = "DML_Admin"},
            @{Name = "DisableGuestAccount"; Value = 1}
        )
        
        foreach ($policy in $policies) {
            Write-Log "Setting policy: $($policy.Name) = $($policy.Value)"
            
            $registryPath = "HKLM:\SOFTWARE\Microsoft\Windows\CurrentVersion\Policies\System"
            
            if (-not (Test-Path $registryPath)) {
                New-Item -Path $registryPath -Force | Out-Null
            }
            
            Set-ItemProperty -Path $registryPath -Name $policy.Name -Value $policy.Value -ErrorAction Stop
            Write-Log "Policy set successfully: $($policy.Name)"
        }
        
        $logEntry = @"
Local Admin Policies
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Status: Success
Policies Configured: $($policies.Count)
"@
        
        Add-Content -Path $LogPath -Value $logEntry
        return $true
    } catch {
        Write-Log "Failed to configure local admin policies: $_" "ERROR"
        return $false
    }
}

function Write-LocalAdminLog {
    param(
        [string]$LogPath,
        [hashtable]$Data
    )
    
    try {
        $logContent = @"
========================================
LOCAL ADMINISTRATOR STANDARDIZATION LOG
========================================
Date: $(Get-Date -Format "yyyy-MM-dd HH:mm:ss")
Hostname: $env:COMPUTERNAME

========================================
ADMIN ACCOUNT CONFIGURATION
========================================
New Admin Name: $($Data.NewAdminName)
Disable Default Admin: $($Data.DisableDefaultAdmin)
Rename Default Admin: $($Data.RenameDefaultAdmin)
Create Admin Group: $($Data.CreateAdminGroup)
Admin Group Name: $($Data.AdminGroupName)

========================================
STATUS
========================================
Account Creation: $($Data.AccountCreationStatus)
Group Creation: $($Data.GroupCreationStatus)
Policy Configuration: $($Data.PolicyConfigurationStatus)

========================================
CURRENT LOCAL ADMINISTRATORS
========================================
$($Data.CurrentAdmins | Out-String)

========================================
END OF LOG
========================================
"@
        
        Set-Content -Path $LogPath -Value $logContent -Encoding UTF8
        Write-Log "Local admin log written to: $LogPath"
        return $true
    } catch {
        Write-Log "Failed to write local admin log: $_" "ERROR"
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

Write-Log "=== Starting Local Administrator Standardization ==="

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

$localAdminConfig = $config.local_admin
$newAdminName = $localAdminConfig.new_admin_name
$newAdminPassword = $localAdminConfig.new_admin_password
$disableDefaultAdmin = $localAdminConfig.disable_default_admin
$createAdminGroup = $localAdminConfig.create_admin_group

if ([string]::IsNullOrEmpty($newAdminPassword)) {
    Write-Log "New admin password not configured in config file" "ERROR"
    Write-Log "Please set 'local_admin.new_admin_password' in config.json" "ERROR"
    exit 1
}

$localAdminLog = Join-Path $localDir "4_LocalAdmin.log"

$accountCreationStatus = "Failed"
$groupCreationStatus = "Failed"
$policyConfigurationStatus = "Failed"

$createAccountResult = Create-NewAdminAccount -Username $newAdminName -Password $newAdminPassword -Description "Standard DML Administrator Account" -LogPath $localAdminLog
if ($createAccountResult) {
    $accountCreationStatus = "Success"
}

if ($createAdminGroup) {
    $createGroupResult = Create-AdminGroup -GroupName $createAdminGroup -Description "DML Administrators Group" -LogPath $localAdminLog
    if ($createGroupResult) {
        $groupCreationStatus = "Success"
    }
}

$policyConfigResult = Configure-LocalAdminPolicies -LogPath $localAdminLog
if ($policyConfigResult) {
    $policyConfigurationStatus = "Success"
}

if ($disableDefaultAdmin) {
    Disable-DefaultAdminAccount -LogPath $localAdminLog
}

$currentAdmins = Get-LocalAdmins

$logData = @{
    NewAdminName = $newAdminName
    DisableDefaultAdmin = $disableDefaultAdmin
    RenameDefaultAdmin = $false
    CreateAdminGroup = $createAdminGroup
    AdminGroupName = $createAdminGroup
    AccountCreationStatus = $accountCreationStatus
    GroupCreationStatus = $groupCreationStatus
    PolicyConfigurationStatus = $policyConfigurationStatus
    CurrentAdmins = $currentAdmins
}

Write-LocalAdminLog -LogPath $localAdminLog -Data $logData

$logUploadPath = $config.paths.log_upload_path
$uploadResult = Upload-Log -SourcePath $localAdminLog -DestPath $logUploadPath -Prefix $env:COMPUTERNAME

if ($uploadResult) {
    Write-Log "=== Local Administrator Standardization Completed ==="
    exit 0
} else {
    Write-Log "=== Local Administrator Standardization Completed but Upload Failed ===" "ERROR"
    exit 1
}



