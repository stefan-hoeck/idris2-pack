# Idris2 Pack installer for Windows
#
# Compatible with Windows PowerShell 5.1 and PowerShell 7+.
# Designed to run from a normal, non-elevated user session.
#
# If execution policy blocks this .ps1 file, invoke it from a child
# PowerShell process with `-ExecutionPolicy Bypass`. This applies only to that
# process and does not change persistent user or machine policy.

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

if (
    $PSVersionTable.PSVersion.Major -lt 5 -or
    (
        $PSVersionTable.PSVersion.Major -eq 5 -and
        $PSVersionTable.PSVersion.Minor -lt 1
    )
) {
    throw "This installer requires Windows PowerShell 5.1 or PowerShell 7+."
}

# ---------------------------------------------------------------------------
# Configuration
# ---------------------------------------------------------------------------

$DefaultMsys2Root = "C:\msys64"
$Msys2Root = if ($env:MSYS2_ROOT) { $env:MSYS2_ROOT } else { $DefaultMsys2Root }

# ---------------------------------------------------------------------------
# Common functions
# ---------------------------------------------------------------------------

function Test-CommandInstalled {
    param(
        [Parameter(Mandatory)]
        [string] $Command
    )

    return [bool](Get-Command $Command -ErrorAction SilentlyContinue)
}

function Require-Command {
    param(
        [Parameter(Mandatory)]
        [string] $Command
    )

    if (-not (Test-CommandInstalled $Command)) {
        throw "Required command was not found: $Command"
    }
}

function Invoke-Native {
    param(
        [Parameter(Mandatory)]
        [string] $Command,

        [Parameter(ValueFromRemainingArguments)]
        [string[]] $Arguments
    )

    Write-Host "+ $Command $($Arguments -join ' ')"

    # Native command stdout would otherwise become part of this function's
    # PowerShell return value. Display it explicitly instead.
    & $Command @Arguments | Out-Host

    if ($LASTEXITCODE -ne 0) {
        throw "Command failed with exit code ${LASTEXITCODE}: $Command $($Arguments -join ' ')"
    }
}

function Get-EnvironmentOrDefault {
    param(
        [Parameter(Mandatory)]
        [string] $Name,

        [Parameter(Mandatory)]
        [string] $Default
    )

    $value = [Environment]::GetEnvironmentVariable($Name)

    if ([string]::IsNullOrWhiteSpace($value)) {
        return $Default
    }

    return $value
}

function New-Directory {
    param(
        [Parameter(Mandatory)]
        [string] $Path
    )

    if (-not (Test-Path -LiteralPath $Path)) {
        New-Item -ItemType Directory -Path $Path -Force | Out-Null
    }
}

function Add-PathEntry {
    param(
        [Parameter(Mandatory)]
        [string] $Path
    )

    if (-not (Test-Path -LiteralPath $Path)) {
        return
    }

    $entries = $env:PATH -split ';'

    if ($entries -notcontains $Path) {
        $env:PATH = "$Path;$env:PATH"
    }
}

function Find-Executable {
    param(
        [Parameter(Mandatory)]
        [string] $Directory,

        [Parameter(Mandatory)]
        [string[]] $Names
    )

    foreach ($name in $Names) {
        $candidate = Join-Path $Directory $name

        if (Test-Path -LiteralPath $candidate) {
            return $candidate
        }
    }

    return $null
}

function Convert-ToMsysPath {
    param(
        [Parameter(Mandatory)]
        [string] $Path
    )

    $cygpath = Join-Path $Msys2Root "usr\bin\cygpath.exe"

    if (-not (Test-Path -LiteralPath $cygpath)) {
        throw "cygpath was not found at $cygpath"
    }

    $converted = & $cygpath -u $Path

    if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($converted)) {
        throw "Unable to convert Windows path to MSYS path: $Path"
    }

    return [string]$converted
}

function Invoke-Msys2Bash {
    param(
        [Parameter(Mandatory)]
        [string] $Command
    )

    $bash = Join-Path $Msys2Root "usr\bin\bash.exe"

    if (-not (Test-Path -LiteralPath $bash)) {
        throw "MSYS2 bash was not found at $bash"
    }

    # Start bash as an actual MINGW64 MSYS2 environment. Chez's configure
    # detects Windows from `uname` (MINGW*) and uses MSYSTEM to select the
    # 64-bit Windows machine type. Merely prepending /mingw64/bin to PATH is
    # not sufficient.
    $oldMsystem = $env:MSYSTEM
    $oldPathType = $env:MSYS2_PATH_TYPE
    $oldChere = $env:CHERE_INVOKING

    try {
        $env:MSYSTEM = "MINGW64"
        $env:MSYS2_PATH_TYPE = "inherit"
        $env:CHERE_INVOKING = "1"

        $MsysCommand = 'export PATH="/mingw64/bin:/usr/bin:$PATH"; ' + $Command

        Write-Host "+ MSYS2 (MINGW64): $MsysCommand"

        & $bash -lc $MsysCommand
    }
    finally {
        $env:MSYSTEM = $oldMsystem
        $env:MSYS2_PATH_TYPE = $oldPathType
        $env:CHERE_INVOKING = $oldChere
    }

    if ($LASTEXITCODE -ne 0) {
        throw "MSYS2 command failed with exit code ${LASTEXITCODE}: $MsysCommand"
    }
}

function Invoke-Msys2BashLogged {
    param(
        [Parameter(Mandatory)]
        [string] $Command,

        [Parameter(Mandatory)]
        [string] $LogFile
    )

    $bash = Join-Path $Msys2Root "usr\bin\bash.exe"

    if (-not (Test-Path -LiteralPath $bash)) {
        throw "MSYS2 bash was not found at $bash"
    }

    $MsysCommand = 'export PATH="/mingw64/bin:/usr/bin:$PATH"; ' + $Command

    Write-Host "+ MSYS2 (MINGW64): $MsysCommand"
    Write-Host "  log: $LogFile"

    $psi = New-Object System.Diagnostics.ProcessStartInfo
    $psi.FileName = $bash
    $psi.Arguments = '-lc "' + ($MsysCommand -replace '"', '\"') + '"'
    $psi.UseShellExecute = $false
    $psi.RedirectStandardOutput = $true
    $psi.RedirectStandardError = $true
    $psi.CreateNoWindow = $true

    # These are the environment markers that make MSYS2 bash behave as the
    # MINGW64 shell instead of the plain MSYS shell.
    $psi.EnvironmentVariables["MSYSTEM"] = "MINGW64"
    $psi.EnvironmentVariables["MSYS2_PATH_TYPE"] = "inherit"
    $psi.EnvironmentVariables["CHERE_INVOKING"] = "1"

    $process = New-Object System.Diagnostics.Process
    $process.StartInfo = $psi

    $writer = New-Object System.IO.StreamWriter($LogFile, $false, (New-Object System.Text.UTF8Encoding($false)))

    try {
        [void]$process.Start()

        while (-not $process.HasExited -or -not $process.StandardOutput.EndOfStream -or -not $process.StandardError.EndOfStream) {
            while (-not $process.StandardOutput.EndOfStream) {
                $line = $process.StandardOutput.ReadLine()
                Write-Host $line
                $writer.WriteLine($line)
                $writer.Flush()
            }

            while (-not $process.StandardError.EndOfStream) {
                $line = $process.StandardError.ReadLine()
                Write-Host $line
                $writer.WriteLine($line)
                $writer.Flush()
            }

            if (-not $process.HasExited) {
                Start-Sleep -Milliseconds 50
            }
        }

        $process.WaitForExit()

        if ($process.ExitCode -ne 0) {
            throw "MSYS2 command failed with exit code $($process.ExitCode): $MsysCommand`nSee log: $LogFile"
        }
    }
    finally {
        $writer.Dispose()
        $process.Dispose()
    }
}

function Install-Msys2WithWinget {
    Write-Host "Installing MSYS2 with winget..."

    Invoke-Native winget install `
        --id MSYS2.MSYS2 `
        --exact `
        --source winget `
        --scope user `
        --location $Msys2Root `
        --accept-package-agreements `
        --accept-source-agreements `
        --disable-interactivity
}

function Install-Msys2FromOfficialRelease {
    Write-Host "winget is unavailable; downloading the latest official MSYS2 installer..."

    $release = Invoke-RestMethod `
        -Uri "https://api.github.com/repos/msys2/msys2-installer/releases/latest" `
        -Headers @{ "User-Agent" = "idris2-pack-windows-installer" }

    $asset = $release.assets |
        Where-Object {
            $_.name -match '^msys2-x86_64-\d+\.exe$'
        } |
        Select-Object -First 1

    if (-not $asset) {
        throw "Could not locate the x86_64 MSYS2 installer in the latest official release."
    }

    $installer = Join-Path $env:TEMP $asset.name

    Invoke-WebRequest `
        -Uri $asset.browser_download_url `
        -OutFile $installer

    $signature = Get-AuthenticodeSignature -FilePath $installer

    if ($signature.Status -ne "Valid") {
        throw "The downloaded MSYS2 installer does not have a valid Authenticode signature."
    }

    Write-Host "Installing MSYS2 to $Msys2Root..."

    & $installer in `
        --confirm-command `
        --accept-messages `
        --root ($Msys2Root -replace '\\', '/')

    if ($LASTEXITCODE -ne 0) {
        throw "MSYS2 installer failed with exit code $LASTEXITCODE."
    }

    Remove-Item -LiteralPath $installer -Force -ErrorAction SilentlyContinue
}

function Ensure-Msys2 {
    $bash = Join-Path $Msys2Root "usr\bin\bash.exe"

    if (Test-Path -LiteralPath $bash) {
        Write-Host "Found MSYS2 at $Msys2Root"
        return
    }

    if (Test-CommandInstalled "winget") {
        Install-Msys2WithWinget
    }
    else {
        Install-Msys2FromOfficialRelease
    }

    if (-not (Test-Path -LiteralPath $bash)) {
        throw "MSYS2 installation completed, but bash was not found at $bash"
    }
}

function Initialize-Msys2Toolchain {
    Write-Host "Updating MSYS2 package database and base system..."

    # Running this twice is intentional. Core MSYS2 updates may update pacman/
    # runtime components first, and a second pass makes the installation converge.
    Invoke-Msys2Bash "pacman -Syu --noconfirm"
    Invoke-Msys2Bash "pacman -Syu --noconfirm"

    Write-Host "Installing Idris2 Windows build prerequisites..."

    # Use MSYS make (make.exe) because Idris2's build invokes POSIX shell tooling,
    # while GCC comes from the native MinGW64 environment.
    Invoke-Msys2Bash `
        "pacman -S --needed --noconfirm make tar mingw-w64-x86_64-gcc mingw-w64-x86_64-git"

    # MinGW binaries first, followed by MSYS POSIX utilities.
    Add-PathEntry (Join-Path $Msys2Root "usr\bin")
    Add-PathEntry (Join-Path $Msys2Root "mingw64\bin")

    $script:MakePath = Join-Path $Msys2Root "usr\bin\make.exe"
    $script:GccPath  = Join-Path $Msys2Root "mingw64\bin\gcc.exe"
    $script:GitPath  = Join-Path $Msys2Root "mingw64\bin\git.exe"
    $script:BashPath = Join-Path $Msys2Root "usr\bin\bash.exe"

    foreach ($tool in @(
        $script:MakePath,
        $script:GccPath,
        $script:GitPath,
        $script:BashPath
    )) {
        if (-not (Test-Path -LiteralPath $tool)) {
            throw "Required MSYS2 tool was not found: $tool"
        }
    }

    Write-Host "MSYS2 toolchain:"
    Write-Host "  make: $script:MakePath"
    Write-Host "  gcc : $script:GccPath"
    Write-Host "  git : $script:GitPath"
    Write-Host "  bash: $script:BashPath"
}

function Find-Scheme {
    foreach ($candidate in @(
        "chezscheme",
        "scheme",
        "chez",
        "racket"
    )) {
        $command = Get-Command $candidate -ErrorAction SilentlyContinue

        if ($command) {
            return $candidate
        }
    }

    # Common native Chez Scheme installation locations on Windows.
    $roots = @(
        "$env:ProgramFiles\Chez Scheme",
        "${env:ProgramFiles(x86)}\Chez Scheme",
        "$env:LOCALAPPDATA\Programs\Chez Scheme"
    ) | Where-Object { $_ -and (Test-Path $_) }

    foreach ($root in $roots) {
        $candidate = Get-ChildItem `
            -LiteralPath $root `
            -Recurse `
            -File `
            -Filter "scheme.exe" `
            -ErrorAction SilentlyContinue |
            Select-Object -First 1

        if ($candidate) {
            Add-PathEntry $candidate.DirectoryName
            return "scheme"
        }
    }

    return $null
}

function Install-ChezScheme {
    $ChezSourceRoot = Join-Path $HOME ".local\share\chez-scheme"
    $ChezBinDir = Join-Path $ChezSourceRoot "ta6nt\bin\ta6nt"
    $ChezExecutable = Join-Path $ChezBinDir "scheme.exe"

    if (Test-Path -LiteralPath $ChezExecutable) {
        Write-Host "Found locally bootstrapped Chez Scheme at $ChezExecutable"
        Add-PathEntry $ChezBinDir
        return "scheme"
    }

    Write-Host "Chez Scheme was not found."
    Write-Host "Building the latest Chez Scheme from the official source repository..."

    $ChezParent = Split-Path -Parent $ChezSourceRoot
    New-Directory $ChezParent

    if (Test-Path -LiteralPath $ChezSourceRoot) {
        Write-Host "Removing incomplete Chez Scheme source tree: $ChezSourceRoot"
        Remove-Item -LiteralPath $ChezSourceRoot -Recurse -Force
    }

    # Pin to the Chez version currently used by Idris2's Windows CI.
    # This avoids an untested moving target from Chez's main branch.
    Invoke-Native `
        $script:GitPath clone --depth=1 --branch v10.4.1 --recurse-submodules `
        "https://github.com/cisco/ChezScheme.git" `
        $ChezSourceRoot

    # Verify/update submodules explicitly with the native MinGW Git.
    # This avoids Chez's configure/build process falling back to MSYS /usr/bin/git.
    Push-Location $ChezSourceRoot
    try {
        Invoke-Native `
            $script:GitPath submodule update --init --recursive
    }
    finally {
        Pop-Location
    }

    $Cygpath = Join-Path $Msys2Root "usr\bin\cygpath.exe"

    if (-not (Test-Path -LiteralPath $Cygpath)) {
        throw "cygpath was not found at $Cygpath"
    }

    $ChezSourcePosix = & $Cygpath -u $ChezSourceRoot

    if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($ChezSourcePosix)) {
        throw "Unable to convert the Chez Scheme source path for MSYS2."
    }

    # Chez Scheme is bootstrapped. The source repository includes portable
    # bytecode boot files in boot/pb, but not necessarily native ta6nt boot
    # files. Follow the documented bootstrap path:
    #
    #   ./configure --pb
    #   make bootquick XM=ta6nt
    #   ./configure -m=ta6nt --threads
    #   make
    #
    # ta6nt is Chez's 64-bit Intel threaded Windows machine type.
    $ChezPbConfigureLog = Join-Path $ChezSourceRoot "configure-pb-windows.log"
    $ChezBootLog = Join-Path $ChezSourceRoot "boot-ta6nt-windows.log"
    $ChezConfigureLog = Join-Path $ChezSourceRoot "configure-ta6nt-windows.log"
    $ChezBuildLog = Join-Path $ChezSourceRoot "build-ta6nt-windows.log"

    Write-Host "Verifying MinGW64 build environment..."
    Invoke-Msys2BashLogged `
        -Command 'echo MSYSTEM=$MSYSTEM; echo uname=$(uname -s); echo gcc=$(command -v gcc); uname -s | grep -q "^MINGW"' `
        -LogFile (Join-Path $ChezSourceRoot "mingw64-environment.log")

    Write-Host "Configuring Chez Scheme portable bootstrap..."
    Invoke-Msys2BashLogged `
        -Command "cd '$ChezSourcePosix' && ./configure --pb" `
        -LogFile $ChezPbConfigureLog

    Write-Host "Generating native ta6nt boot files from portable bytecode..."
    Invoke-Msys2BashLogged `
        -Command "cd '$ChezSourcePosix' && make bootquick XM=ta6nt" `
        -LogFile $ChezBootLog

    Write-Host "Configuring threaded native Chez Scheme (ta6nt)..."
    Invoke-Msys2BashLogged `
        -Command "cd '$ChezSourcePosix' && ./configure -m=ta6nt --threads" `
        -LogFile $ChezConfigureLog

    Write-Host "Building threaded native Chez Scheme (ta6nt)..."
    Invoke-Msys2BashLogged `
        -Command "cd '$ChezSourcePosix' && make" `
        -LogFile $ChezBuildLog

    if (-not (Test-Path -LiteralPath $ChezExecutable)) {
        throw @"
Chez Scheme finished building, but scheme.exe was not found at:
$ChezExecutable
"@
    }

    Add-PathEntry $ChezBinDir

    Require-Command scheme

    Write-Host "Chez Scheme:"
    Write-Host "  scheme: $((Get-Command scheme).Source)"

    return "scheme"
}

function Ensure-Scheme {
    # Optional non-interactive override. This can be a command name such as
    # "scheme" / "racket", or a full executable path.
    $ConfiguredScheme = [Environment]::GetEnvironmentVariable("PACK_SCHEME")

    if (-not [string]::IsNullOrWhiteSpace($ConfiguredScheme)) {
        Require-Command $ConfiguredScheme
        return $ConfiguredScheme
    }

    $DetectedScheme = Find-Scheme

    if (-not [string]::IsNullOrWhiteSpace($DetectedScheme)) {
        return $DetectedScheme
    }

    return Install-ChezScheme
}

function Install-IdrisPackage {
    param(
        [Parameter(Mandatory)]
        [string] $Repository,

        [Parameter(Mandatory)]
        [string] $CloneDirectory,

        [Parameter(Mandatory)]
        [string] $PackageDirectory,

        [Parameter(Mandatory)]
        [string] $Ipkg,

        [Parameter(Mandatory)]
        [string] $BootPath
    )

    Invoke-Native $script:GitPath clone --depth=1 $Repository $CloneDirectory

    Push-Location $PackageDirectory

    try {
        Invoke-Native $BootPathWindows --install $Ipkg
    }
    finally {
        Pop-Location
    }
}

# ---------------------------------------------------------------------------
# Bootstrap the Windows build environment
# ---------------------------------------------------------------------------

if ($env:OS -ne "Windows_NT") {
    throw "This installer is intended for Windows."
}

# PowerShell exposes $HOME, but Windows does not guarantee that HOME exists as
# an environment variable. pack expects HOME (or PACK_DIR), so establish HOME
# explicitly for this process before invoking pack or any generated wrappers.
if ([string]::IsNullOrWhiteSpace($env:HOME)) {
    $env:HOME = ($HOME -replace '\\', '/')
}

Ensure-Msys2
Initialize-Msys2Toolchain

# ---------------------------------------------------------------------------
# Install directories
# ---------------------------------------------------------------------------

$HomeDirectory = $HOME

$ConfigHome = Get-EnvironmentOrDefault `
    -Name "XDG_CONFIG_HOME" `
    -Default (Join-Path $HomeDirectory ".config")

$StateHome = Get-EnvironmentOrDefault `
    -Name "XDG_STATE_HOME" `
    -Default (Join-Path $HomeDirectory ".local\state")

$CacheHome = Get-EnvironmentOrDefault `
    -Name "XDG_CACHE_HOME" `
    -Default (Join-Path $HomeDirectory ".cache")

$UserDir = Get-EnvironmentOrDefault `
    -Name "PACK_USER_DIR" `
    -Default (Join-Path $ConfigHome "pack")

$StateDir = Get-EnvironmentOrDefault `
    -Name "PACK_STATE_DIR" `
    -Default (Join-Path $StateHome "pack")

$CacheDir = Get-EnvironmentOrDefault `
    -Name "PACK_CACHE_DIR" `
    -Default (Join-Path $CacheHome "pack")

$BinDir = Get-EnvironmentOrDefault `
    -Name "PACK_BIN_DIR" `
    -Default (Join-Path $HomeDirectory ".local\bin")

$DbDir      = Join-Path $StateDir "db"
$InstallDir = Join-Path $StateDir "install"
$ClonesDir  = Join-Path $CacheDir "clones"

# ---------------------------------------------------------------------------
# Detect or provision Chez/Racket
# ---------------------------------------------------------------------------

$SchemeResults = @(Ensure-Scheme)

if ($SchemeResults.Count -eq 0) {
    throw "Scheme detection/install did not return a Scheme command."
}

$Scheme = [string]$SchemeResults[$SchemeResults.Count - 1]

Require-Command $Scheme
Write-Host "Using $Scheme for code generation"

# ---------------------------------------------------------------------------
# Check and create directories
# ---------------------------------------------------------------------------

if (Test-Path -LiteralPath $StateDir) {
    throw @"
Directory $StateDir exists.
Please remove it and rerun this script.
"@
}

New-Directory $UserDir
New-Directory $DbDir
New-Directory $InstallDir

# The clones directory is only a disposable build cache. A failed previous
# installer run can leave partially populated Git repositories here, which
# causes subsequent `git clone` commands to fail with "destination path
# already exists". Always start this cache clean while preserving the actual
# installed state and the separately cached Chez Scheme build.
if (Test-Path -LiteralPath $ClonesDir) {
    Write-Host "Removing stale clone cache: $ClonesDir"
    Remove-Item -LiteralPath $ClonesDir -Recurse -Force
}

New-Directory $ClonesDir
New-Directory $BinDir

Add-PathEntry $BinDir

# ---------------------------------------------------------------------------
# Install package collection
# ---------------------------------------------------------------------------

$PackDbClone = Join-Path $ClonesDir "idris2-pack-db"

Invoke-Native `
    git clone --depth=1 `
    "https://github.com/stefan-hoeck/idris2-pack-db.git" `
    $PackDbClone

$CollectionsDir = Join-Path $PackDbClone "collections"

Get-ChildItem -LiteralPath $CollectionsDir -File |
    Copy-Item -Destination $DbDir

$LatestDb = Get-ChildItem -LiteralPath $DbDir -Filter "nightly-*.toml" -File |
    Sort-Object Name |
    Select-Object -Last 1

if (-not $LatestDb) {
    throw "Could not find a nightly package collection in $DbDir"
}

$PackageCollection = $LatestDb.BaseName

Write-Host "Using package collection: $PackageCollection"

# ---------------------------------------------------------------------------
# Extract Idris2 commit from collection TOML
# ---------------------------------------------------------------------------

$CollectionContents = Get-Content -LiteralPath $LatestDb.FullName

$InIdris2Section = $false
$Idris2Commit = $null

foreach ($line in $CollectionContents) {
    if ($line -match '^\s*\[idris2\]\s*$') {
        $InIdris2Section = $true
        continue
    }

    if ($InIdris2Section -and $line -match '^\s*\[') {
        break
    }

    if (
        $InIdris2Section -and
        $line -match '^\s*commit\s*=\s*"([a-fA-F0-9]+)"'
    ) {
        $Idris2Commit = $Matches[1]
        break
    }
}

if ([string]::IsNullOrWhiteSpace($Idris2Commit)) {
    throw "Unable to determine Idris2 commit from $($LatestDb.FullName)"
}

Write-Host "Using Idris2 commit: $Idris2Commit"

# ---------------------------------------------------------------------------
# Bootstrap the Idris compiler
# ---------------------------------------------------------------------------

$IdrisClone = Join-Path $ClonesDir "Idris2"

Invoke-Native `
    git clone `
    "https://github.com/idris-lang/Idris2.git" `
    $IdrisClone

Push-Location $IdrisClone

try {
    Invoke-Native $script:GitPath checkout $Idris2Commit

    $PrefixPath = Join-Path `
        (Join-Path $InstallDir $Idris2Commit) `
        "idris2"

    # GNU make runs under MSYS2, so paths passed into Makefile variables must
    # use MSYS/POSIX syntax. Windows backslashes are interpreted as escapes by
    # shell recipes and corrupt destinations such as C:\Users\...
    $PrefixPathMsys = Convert-ToMsysPath $PrefixPath

    if ($Scheme -eq "racket") {
        $Cg = "racket"

        Invoke-Native `
            make `
            bootstrap-racket `
            "PREFIX=$PrefixPathMsys"
    }
    else {
        $Cg = "chez"

        Invoke-Native `
            make `
            bootstrap `
            "PREFIX=$PrefixPathMsys" `
            "SCHEME=$Scheme"
    }

    $env:IDRIS2_CG = $Cg

    Invoke-Native `
        make `
        install `
        "PREFIX=$PrefixPathMsys" `
        "IDRIS2_CG=$Cg"

    Invoke-Native make clean

    $BootBinDir = Join-Path $PrefixPath "bin"

    # Idris2 installs both:
    #
    #   idris2      - POSIX shell launcher for MSYS2/make
    #   idris2.cmd  - Windows launcher for PowerShell/CMD
    #
    # Keep them separate. PowerShell cannot directly execute the extensionless
    # POSIX launcher as a native command, while the Makefiles should use the
    # POSIX launcher under MSYS2.
    $BootPathWindows = Find-Executable `
        -Directory $BootBinDir `
        -Names @(
            "idris2.exe",
            "idris2.cmd"
        )

    $BootPathPosix = Find-Executable `
        -Directory $BootBinDir `
        -Names @(
            "idris2"
        )

    if (-not $BootPathWindows) {
        throw "Could not find the Windows Idris2 launcher under $BootBinDir"
    }

    if (-not $BootPathPosix) {
        throw "Could not find the POSIX Idris2 launcher under $BootBinDir"
    }

    $BootPathMsys = Convert-ToMsysPath $BootPathPosix

    Invoke-Native `
        make `
        all `
        "IDRIS2_BOOT=$BootPathMsys" `
        "PREFIX=$PrefixPathMsys" `
        "IDRIS2_CG=$Cg"

    Invoke-Native `
        make `
        install `
        "IDRIS2_BOOT=$BootPathMsys" `
        "PREFIX=$PrefixPathMsys" `
        "IDRIS2_CG=$Cg"

    Invoke-Native `
        make `
        install-with-src-libs `
        "IDRIS2_BOOT=$BootPathMsys" `
        "PREFIX=$PrefixPathMsys" `
        "IDRIS2_CG=$Cg"

    Invoke-Native `
        make `
        install-with-src-api `
        "IDRIS2_BOOT=$BootPathMsys" `
        "PREFIX=$PrefixPathMsys" `
        "IDRIS2_CG=$Cg"
}
finally {
    Pop-Location
}

# ---------------------------------------------------------------------------
# Install prerequisite Idris2 libraries
# ---------------------------------------------------------------------------

$Packages = @(
    @{
        Name = "algebra"
        Repository = "https://github.com/stefan-hoeck/idris2-algebra.git"
        Ipkg = "algebra.ipkg"
    },
    @{
        Name = "ref1"
        Repository = "https://github.com/stefan-hoeck/idris2-ref1.git"
        Ipkg = "ref1.ipkg"
    },
    @{
        Name = "array"
        Repository = "https://github.com/stefan-hoeck/idris2-array.git"
        Ipkg = "array.ipkg"
    },
    @{
        Name = "bytestring"
        Repository = "https://github.com/stefan-hoeck/idris2-bytestring.git"
        Ipkg = "bytestring.ipkg"
    },
    @{
        Name = "getopts"
        Repository = "https://github.com/idris-community/idris2-getopts.git"
        Ipkg = "getopts.ipkg"
    },
    @{
        Name = "elab-util"
        Repository = "https://github.com/stefan-hoeck/idris2-elab-util.git"
        Ipkg = "elab-util.ipkg"
    },
    @{
        Name = "refined"
        Repository = "https://github.com/stefan-hoeck/idris2-refined.git"
        Ipkg = "refined.ipkg"
    },
    @{
        Name = "literal"
        Repository = "https://github.com/stefan-hoeck/idris2-literal.git"
        Ipkg = "literal.ipkg"
    },
    @{
        Name = "finite"
        Repository = "https://github.com/stefan-hoeck/idris2-finite.git"
        Ipkg = "finite.ipkg"
    },
    @{
        Name = "enum"
        Repository = "https://github.com/stefan-hoeck/idris2-enum.git"
        Ipkg = "enum.ipkg"
    },
    @{
        Name = "filepath"
        Repository = "https://github.com/stefan-hoeck/idris2-filepath.git"
        Ipkg = "filepath.ipkg"
    }
)

foreach ($package in $Packages) {
    $clone = Join-Path $ClonesDir "idris2-$($package.Name)"

    Install-IdrisPackage `
        -Repository $package.Repository `
        -CloneDirectory $clone `
        -PackageDirectory $clone `
        -Ipkg $package.Ipkg `
        -BootPath $BootPathWindows
}

# ---------------------------------------------------------------------------
# Install ilex-core, ilex, and ilex-toml
# ---------------------------------------------------------------------------

$IlexClone = Join-Path $ClonesDir "idris2-ilex"

Invoke-Native `
    git clone --depth=1 `
    "https://github.com/stefan-hoeck/idris2-ilex.git" `
    $IlexClone

Push-Location (Join-Path $IlexClone "core")
try {
    Invoke-Native $BootPathWindows --install "ilex-core.ipkg"
}
finally {
    Pop-Location
}

Push-Location $IlexClone
try {
    Invoke-Native $BootPathWindows --install "ilex.ipkg"
}
finally {
    Pop-Location
}

Push-Location (Join-Path $IlexClone "toml")
try {
    Invoke-Native $BootPathWindows --install "ilex-toml.ipkg"
}
finally {
    Pop-Location
}

# ---------------------------------------------------------------------------
# Install pack
# ---------------------------------------------------------------------------

$PackClone = Join-Path $ClonesDir "idris2-pack"

Invoke-Native `
    git clone --depth=1 `
    "https://github.com/stefan-hoeck/idris2-pack.git" `
    $PackClone

# ---------------------------------------------------------------------------
# Windows fix: directory existence checks in pack
# ---------------------------------------------------------------------------
#
# Pack.Core.IO currently defines:
#
#   exists : Path Abs -> io Bool
#   exists = exists . interpolate
#
# The unqualified `exists` resolves to System.File.exists. Idris2's
# System.File.exists checks existence by attempting to open the path as a
# readable file. On Windows, that returns False for directories.
#
# This breaks pack's Git cache logic:
#
#   1. pack clones a repository into a directory successfully;
#   2. later, `missing cache` calls this helper;
#   3. the directory is reported as missing;
#   4. pack tries to clone into the already-populated directory again.
#
# Pack has a separate `fileExists` helper for actual files, so make `exists`
# correctly test directories by opening them with System.Directory.openDir.
#
$PackCoreIO = Join-Path $PackClone "src\Pack\Core\IO.idr"

if (-not (Test-Path -LiteralPath $PackCoreIO)) {
    throw "Could not find Pack.Core.IO at $PackCoreIO"
}

$PackCoreIOText = Get-Content -LiteralPath $PackCoreIO -Raw

$DirectoryExistsPattern = @'
(?ms)\|\|\| Checks if a file at the given location exists\.\r?\nexport %inline\r?\nexists : HasIO io => \(dir : Path Abs\) -> io Bool\r?\nexists = exists \. interpolate
'@

$DirectoryExistsReplacement = @'
||| Checks if a directory at the given location exists.
export
exists : HasIO io => (dir : Path Abs) -> io Bool
exists dir = do
  Right d <- System.Directory.openDir (interpolate dir)
    | Left _ => pure False
  System.Directory.closeDir d
  pure True
'@

$PatchedPackCoreIO = [regex]::Replace(
    $PackCoreIOText,
    $DirectoryExistsPattern,
    $DirectoryExistsReplacement,
    1
)

if ($PatchedPackCoreIO -eq $PackCoreIOText) {
    throw @"
Could not patch Pack.Core.IO directory existence check.

The upstream source may have changed. Refusing to continue because the
unpatched implementation causes pack's Git cache to be treated as missing
on Windows.
"@
}

# Make the file-specific helper explicit so future name-resolution changes do
# not accidentally route file checks through the new directory helper.
$PatchedPackCoreIO = $PatchedPackCoreIO -replace `
    'fileExists = exists \. interpolate', `
    'fileExists = System.File.Meta.exists . interpolate'

$Utf8NoBomPackPatch = New-Object System.Text.UTF8Encoding($false)

[System.IO.File]::WriteAllText(
    $PackCoreIO,
    $PatchedPackCoreIO,
    $Utf8NoBomPackPatch
)

Write-Host "Applied Windows directory-existence fix to idris2-pack."

Push-Location $PackClone
try {
    Invoke-Native $BootPathWindows --build "pack.ipkg"

    $PackBuildDir = Join-Path $PackClone "build\exec"

    Copy-Item `
        -Path (Join-Path $PackBuildDir "*") `
        -Destination $BinDir `
        -Recurse `
        -Force
}
finally {
    Pop-Location
}

$PackPath = Find-Executable `
    -Directory $BinDir `
    -Names @(
        "pack.exe",
        "pack.cmd",
        "pack"
    )

if (-not $PackPath) {
    throw "Could not find pack executable under $BinDir"
}

# ---------------------------------------------------------------------------
# Windows compatibility shell for pack
# ---------------------------------------------------------------------------
#
# pack invokes POSIX commands such as:
#
#   mkdir -p ...
#   rm -rf ...
#   cp ...
#   make ...
#
# through Idris2's System.system. On Windows, System.system ultimately calls
# the C runtime system(), which delegates to %COMSPEC% (normally cmd.exe).
# cmd.exe does not understand POSIX command-line syntax such as "mkdir -p".
#
# Install a tiny COMSPEC-compatible executable that accepts:
#
#   /c <command>
#
# The proxy preserves the raw Windows command line, translates Idris2's
# Windows/CMD-style escaping into Bash-compatible escaping, normalizes Windows
# path separators, writes the translated command to a temporary shell script,
# and executes that script with MSYS2 Bash.
#
# Using a temporary script avoids the additional Windows -> Bash quoting
# boundary that would be introduced by passing the command through
# `bash -lc <command>`.
#
# The pack launchers below set COMSPEC to this proxy only for pack and its
# children; the user's global COMSPEC is never modified.
#

$PackComspecSource = Join-Path $BinDir "pack-comspec.c"
$PackComspecExe = Join-Path $BinDir "pack-comspec.exe"

$PackComspecSourceText = @'
#include <windows.h>
#include <process.h>
#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * Return the raw command that follows COMSPEC's "/c" argument.
 *
 * Using the raw Windows command line instead of rebuilding it from argv
 * preserves quotes, pipes, redirections, &&, and paths containing spaces.
 */
static const char *command_after_c(void) {
    const char *p = GetCommandLineA();

    if (p == NULL) {
        return NULL;
    }

    /* Skip argv[0], respecting a quoted executable path. */
    if (*p == '"') {
        ++p;
        while (*p && *p != '"') {
            ++p;
        }
        if (*p == '"') {
            ++p;
        }
    } else {
        while (*p && *p != ' ' && *p != '\t') {
            ++p;
        }
    }

    while (*p == ' ' || *p == '\t') {
        ++p;
    }

    /* system() invokes COMSPEC as: <comspec> /c <command> */
    if ((p[0] == '/' || p[0] == '-') &&
        (p[1] == 'c' || p[1] == 'C')) {
        p += 2;
    } else {
        return NULL;
    }

    while (*p == ' ' || *p == '\t') {
        ++p;
    }

    return p;
}

/*
 * Idris2's System.escapeArg uses CMD escaping on Windows:
 *
 *   space -> ^
 *   &     -> ^&
 *   "     -> ^"
 *   etc.
 *
 * pack ultimately constructs commands using that escaping, but our COMSPEC
 * proxy executes them with Bash. Translate CMD caret escapes into Bash
 * backslash escapes, while converting Windows path separators to '/'.
 *
 * Examples:
 *
 *   C:\Program^ Files\foo  -> C:/Program\ Files/foo
 *   foo^&bar               -> foo\&bar
 */

/*
 * Return nonzero when position `pos` is inside the value of one of pack's
 * Idris2 path-list assignments:
 *
 *   IDRIS2_PACKAGE_PATH=...
 *   IDRIS2_LIBS=...
 *   IDRIS2_DATA=...
 *
 * Pack serializes these lists with ':' separators. Native Windows Idris2
 * expects ';'. We deliberately restrict the rewrite to these three variables
 * so custom build hooks and unrelated environment variables retain their
 * original POSIX semantics.
 */
static int in_idris_path_assignment(const char *src, size_t pos) {
    static const char *vars[] = {
        "IDRIS2_PACKAGE_PATH=",
        "IDRIS2_LIBS=",
        "IDRIS2_DATA="
    };

    const char *p = src + pos;
    const char *start = p;

    while (start > src &&
           start[-1] != ' ' &&
           start[-1] != '\t' &&
           start[-1] != '\n' &&
           start[-1] != '\r') {
        --start;
    }

    for (size_t i = 0; i < sizeof(vars) / sizeof(vars[0]); ++i) {
        size_t len = strlen(vars[i]);

        if (strncmp(start, vars[i], len) == 0 &&
            (size_t)(p - start) >= len) {
            return 1;
        }
    }

    return 0;
}

static char *cmd_to_bash(const char *src) {
    size_t n = strlen(src);
    char *dst = (char *)malloc(n * 2 + 1);
    size_t i = 0;
    size_t j = 0;

    if (dst == NULL) {
        return NULL;
    }

    while (i < n) {
        if (src[i] == '^' && i + 1 < n) {
            dst[j++] = '\\';
            dst[j++] = src[i + 1];
            i += 2;
            continue;
        }

        if (src[i] == '\\') {
            dst[j++] = '/';
            ++i;
            continue;
        }

        /*
         * Only rewrite path-list separators inside the values of
         * IDRIS2_PACKAGE_PATH, IDRIS2_LIBS, and IDRIS2_DATA.
         *
         * The temporary script is interpreted by Bash, so emit an escaped
         * semicolon. Bash consumes the backslash and passes the native Windows
         * Idris2 process the required ';' separator:
         *
         *   C:/one:C:/two  ->  C:/one\;C:/two  ->  C:/one;C:/two
         */
        if (src[i] == ':' &&
            i + 2 < n &&
            isalpha((unsigned char)src[i + 1]) &&
            src[i + 2] == ':' &&
            in_idris_path_assignment(src, i)) {
            dst[j++] = '\\';
            dst[j++] = ';';
            ++i;
            continue;
        }

        dst[j++] = src[i++];
    }

    dst[j] = '\0';
    return dst;
}

int main(void) {
    const char *bash = getenv("PACK_MSYS2_BASH");
    const char *raw = command_after_c();

    char temp_dir[MAX_PATH + 1];
    char temp_file[MAX_PATH + 1];

    FILE *fp;
    intptr_t result;
    char *command;
    char *script_arg;

    if (bash == NULL || *bash == '\0') {
        bash = "C:\\msys64\\usr\\bin\\bash.exe";
    }

    if (raw == NULL || *raw == '\0') {
        fprintf(stderr, "pack-comspec: expected /c <command>\n");
        return 2;
    }

    command = cmd_to_bash(raw);
    if (command == NULL) {
        fprintf(stderr, "pack-comspec: unable to allocate command buffer\n");
        return 2;
    }

    if (GetTempPathA(MAX_PATH, temp_dir) == 0 ||
        GetTempFileNameA(temp_dir, "pck", 0, temp_file) == 0) {
        fprintf(stderr, "pack-comspec: unable to create temporary script\n");
        free(command);
        return 2;
    }

    fp = fopen(temp_file, "wb");
    if (fp == NULL) {
        perror("pack-comspec");
        DeleteFileA(temp_file);
        free(command);
        return 2;
    }

    /*
     * Execute the command from a file instead of `bash -lc <command>`.
     * This completely avoids the Windows -> MSYS quoting boundary that was
     * truncating pack commands such as `mkdir -p ...`.
     */
    fputs("#!/usr/bin/env bash\n", fp);
    fputs(command, fp);
    fputc('\n', fp);

    fclose(fp);
    free(command);

    script_arg = _strdup(temp_file);
    if (script_arg == NULL) {
        DeleteFileA(temp_file);
        return 2;
    }

    {
        char *p = script_arg;
        while (*p) {
            if (*p == '\\') {
                *p = '/';
            }
            ++p;
        }
    }

    result =
        _spawnl(
            _P_WAIT,
            bash,
            bash,
            script_arg,
            NULL
        );

    free(script_arg);
    DeleteFileA(temp_file);

    if (result == -1) {
        perror("pack-comspec");
        return 127;
    }

    return (int)result;
}
'@

Set-Content `
    -LiteralPath $PackComspecSource `
    -Value $PackComspecSourceText `
    -Encoding ASCII

Invoke-Native `
    $script:GccPath `
    "-O2" `
    "-o" `
    $PackComspecExe `
    $PackComspecSource

if (-not (Test-Path -LiteralPath $PackComspecExe)) {
    throw "Failed to build pack COMSPEC compatibility shim."
}

Remove-Item -LiteralPath $PackComspecSource -Force -ErrorAction SilentlyContinue

# Preserve the launchers produced by Idris2 before replacing them with wrappers.
$PackRuntimeCmd = Join-Path $BinDir "pack-runtime.cmd"
$PackRuntimePosix = Join-Path $BinDir "pack-runtime"

$GeneratedPackCmd = Join-Path $BinDir "pack.cmd"
$GeneratedPackPosix = Join-Path $BinDir "pack"

if (Test-Path -LiteralPath $GeneratedPackCmd) {
    Move-Item `
        -LiteralPath $GeneratedPackCmd `
        -Destination $PackRuntimeCmd `
        -Force
}

if (Test-Path -LiteralPath $GeneratedPackPosix) {
    Move-Item `
        -LiteralPath $GeneratedPackPosix `
        -Destination $PackRuntimePosix `
        -Force
}

if (-not (Test-Path -LiteralPath $PackRuntimeCmd)) {
    throw "Expected generated pack.cmd was not found."
}

# Windows CMD/PowerShell launcher.
#
# Keep HOME in Windows drive-letter form, but normalize separators to forward
# slashes. Windows APIs accept C:/Users/... and MSYS2 tools also understand it.
$PackCmdWrapper = @"
@echo off
setlocal

set "HOME=$($HOME)"
set "PACK_USER_DIR=$UserDir"
set "PACK_STATE_DIR=$StateDir"
set "PACK_CACHE_DIR=$CacheDir"
set "PACK_BIN_DIR=$BinDir"

set "PACK_MSYS2_BASH=$($script:BashPath)"
set "COMSPEC=$PackComspecExe"

rem Ensure MSYS2 utilities are available in fresh Windows shells.
set "PATH=$Msys2Root\mingw64\bin;$Msys2Root\usr\bin;%PATH%"

call "$PackRuntimeCmd" %*
exit /b %ERRORLEVEL%
"@

Set-Content `
    -LiteralPath $GeneratedPackCmd `
    -Value $PackCmdWrapper `
    -Encoding ASCII

# Git Bash / MSYS2 launcher.
$PackPosixWrapperText = @'
#!/usr/bin/env bash

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export PATH="/mingw64/bin:/usr/bin:$PATH"

if command -v cygpath >/dev/null 2>&1; then
    export HOME="$(cygpath -w "${USERPROFILE:-$HOME}")"
    export PACK_USER_DIR="$(cygpath -w "$HOME/.config/pack")"
    export PACK_STATE_DIR="$(cygpath -w "$HOME/.local/state/pack")"
    export PACK_CACHE_DIR="$(cygpath -w "$HOME/.cache/pack")"
    export PACK_BIN_DIR="$(cygpath -w "$HOME/.local/bin")"

    export COMSPEC="$(cygpath -w "$SCRIPT_DIR/pack-comspec.exe")"
    export PACK_MSYS2_BASH="$(cygpath -w /usr/bin/bash.exe)"
fi

exec "$SCRIPT_DIR/pack-runtime" "$@"
'@

$PackPosixWrapperLf = $PackPosixWrapperText -replace "`r`n", "`n"
$Utf8NoBom = New-Object System.Text.UTF8Encoding($false)

[System.IO.File]::WriteAllText(
    $GeneratedPackPosix,
    $PackPosixWrapperLf,
    $Utf8NoBom
)

$GeneratedPackPosixMsys = Convert-ToMsysPath $GeneratedPackPosix
$PackRuntimePosixMsys = Convert-ToMsysPath $PackRuntimePosix

Invoke-Msys2Bash "chmod +x '$GeneratedPackPosixMsys' '$PackRuntimePosixMsys'"

# All later PowerShell-side calls should use our compatibility wrapper.
$PackPath = $GeneratedPackCmd

# ---------------------------------------------------------------------------
# Create Windows idris2 wrapper
# ---------------------------------------------------------------------------

$IdrisWrapper = Join-Path $BinDir "idris2.cmd"

$IdrisWrapperContents = @"
@echo off
setlocal

if not defined HOME set "HOME=%USERPROFILE%"

for /f "delims=" %%i in ('"$PackPath" app-path idris2') do set "APPLICATION=%%i"
for /f "delims=" %%i in ('"$PackPath" package-path') do set "IDRIS2_PACKAGE_PATH=%%i"
for /f "delims=" %%i in ('"$PackPath" libs-path') do set "IDRIS2_LIBS=%%i"
for /f "delims=" %%i in ('"$PackPath" data-path') do set "IDRIS2_DATA=%%i"

rem pack emits ':' between package/data/lib directories. Convert only the
rem separators immediately before another Windows drive prefix.
setlocal EnableDelayedExpansion
for %%D in (A B C D E F G H I J K L M N O P Q R S T U V W X Y Z) do (
    set "IDRIS2_PACKAGE_PATH=!IDRIS2_PACKAGE_PATH::%%D:=;%%D:!"
    set "IDRIS2_LIBS=!IDRIS2_LIBS::%%D:=;%%D:!"
    set "IDRIS2_DATA=!IDRIS2_DATA::%%D:=;%%D:!"
)

set "IDRIS2_CG=$Cg"

"%APPLICATION%" %*
"@

Set-Content `
    -LiteralPath $IdrisWrapper `
    -Value $IdrisWrapperContents `
    -Encoding ASCII

# ---------------------------------------------------------------------------
# Create POSIX idris2 wrapper for Git Bash / MSYS2
# ---------------------------------------------------------------------------
#
# Install an extensionless shell wrapper alongside idris2.cmd:
#
#   ~/.local/bin/idris2.cmd   -> PowerShell / CMD
#   ~/.local/bin/idris2       -> Git Bash / MSYS2
#
# The installer provides pack.cmd for PowerShell/CMD and an extensionless
# pack wrapper for Git Bash/MSYS2.
#

$IdrisPosixWrapper = Join-Path $BinDir "idris2"

$IdrisPosixWrapperContents = @'
#!/usr/bin/env bash

set -e

if [ -z "${HOME:-}" ] && [ -n "${USERPROFILE:-}" ]; then
    if command -v cygpath >/dev/null 2>&1; then
        HOME="$(cygpath -u "$USERPROFILE")"
    else
        HOME="$USERPROFILE"
    fi
    export HOME
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

PACK="$SCRIPT_DIR/pack"

if [ ! -x "$PACK" ]; then
    echo "Unable to find pack next to the idris2 launcher." >&2
    exit 1
fi

APPLICATION="$("$PACK" app-path idris2)"

IDRIS2_PACKAGE_PATH="$("$PACK" package-path)"
IDRIS2_LIBS="$("$PACK" libs-path)"
IDRIS2_DATA="$("$PACK" data-path)"

# pack emits ':' between directories. Native Windows Idris2 expects ';'.
# Replace only separators immediately preceding another drive-letter prefix.
for drive in {A..Z} {a..z}; do
    IDRIS2_PACKAGE_PATH="${IDRIS2_PACKAGE_PATH//:${drive}:/;${drive}:}"
    IDRIS2_LIBS="${IDRIS2_LIBS//:${drive}:/;${drive}:}"
    IDRIS2_DATA="${IDRIS2_DATA//:${drive}:/;${drive}:}"
done

export IDRIS2_PACKAGE_PATH
export IDRIS2_LIBS
export IDRIS2_DATA
export IDRIS2_CG="__IDRIS2_CG__"

# pack is a native Windows executable, so app-path may return a Windows path
# such as C:\Users\...\idris2.exe. Git Bash and MSYS2 provide cygpath, which
# converts it to the shell-native /c/Users/... representation before exec.
if command -v cygpath >/dev/null 2>&1; then
    APPLICATION="$(cygpath -u "$APPLICATION")"
fi

exec "$APPLICATION" "$@"
'@

$IdrisPosixWrapperContents = $IdrisPosixWrapperContents.Replace(
    "__IDRIS2_CG__",
    $Cg
)

# Use LF line endings for the POSIX shell wrapper.
$IdrisPosixWrapperContents = $IdrisPosixWrapperContents -replace "`r`n", "`n"

[System.IO.File]::WriteAllText(
    $IdrisPosixWrapper,
    $IdrisPosixWrapperContents,
    (New-Object System.Text.UTF8Encoding($false))
)

# Mark the extensionless wrapper executable in the MSYS2 environment.
$PosixWrapperForMsys = (& (Join-Path $Msys2Root "usr\bin\cygpath.exe") -u $IdrisPosixWrapper)

if ($LASTEXITCODE -ne 0) {
    throw "Unable to convert the POSIX idris2 wrapper path with cygpath."
}

Invoke-Msys2Bash "chmod +x '$PosixWrapperForMsys'"

# ---------------------------------------------------------------------------
# Initialize pack.toml files
# ---------------------------------------------------------------------------

$StateToml = Join-Path $StateDir "pack.toml"

$StateTomlContents = @"
# Warning: This file was auto-generated and is maintained by pack.
#          Any changes could be overwritten by pack at any time.
#          Custom settings should go to the global pack.toml file
#          or any pack.toml file local to a project.
collection = "$PackageCollection"
"@

Set-Content `
    -LiteralPath $StateToml `
    -Value $StateTomlContents `
    -Encoding UTF8

$UserToml = Join-Path $UserDir "pack.toml"

if (-not (Test-Path -LiteralPath $UserToml)) {
    $UserTomlContents = @"
[install]

# with-src = true
# with-docs = false
# use-katla = false
# safety-prompt = true
# gc-prompt = true
# gc-purge = false
# warn-depends = true
# whitelist = [ "pack", "idris2-lsp" ]
# libs = []
# apps = []

[pack]

# url = "https://github.com/stefan-hoeck/idris2-pack"
# commit = "latest:main"

[idris2]

# bootstrap = false
# bootstrap-stage3 = true

scheme = "$Scheme"

# codegen = "chez"
# repl.rlwrap = false
# repl.autoload = "installed"
# url = "https://github.com/idris-lang/Idris2"
# commit = "latest:main"
# git = false
# extra-args = []

[log]

# build             = "build"
# install-deps      = "build"
# typecheck         = "build"
# clean             = "build"
# cleanbuild        = "build"
# repl              = "warning"
# exec              = "warning"
# install           = "build"
# install-app       = "build"
# remove            = "build"
# remove-app        = "build"
# run               = "warning"
# test              = "warning"
# new               = "build"
# update            = "build"
# fetch             = "build"
# package-path      = "silence"
# libs-path         = "silence"
# data-path         = "silence"
# app-path          = "silence"
# switch            = "build"
# update-db         = "build"
# gc                = "info"
# info              = "cache"
# query             = "cache"
# fuzzy             = "cache"
# completion        = "silence"
# completion-script = "silence"
# uninstall         = "info"
# help              = "silence"
"@

    Set-Content `
        -LiteralPath $UserToml `
        -Value $UserTomlContents `
        -Encoding UTF8
}

# ---------------------------------------------------------------------------
# Cleanup
# ---------------------------------------------------------------------------

if (Test-Path -LiteralPath $ClonesDir) {
    Remove-Item `
        -LiteralPath $ClonesDir `
        -Recurse `
        -Force
}

$CleanupPatterns = @(
    "elab-util-*",
    "algebra-*",
    "getopts-*",
    "refined-*",
    "parser-*",
    "filepath-*",
    "ref1-*",
    "array-*",
    "bytestring-*"
)

$InstalledIdrisDirs = Get-ChildItem `
    -LiteralPath $PrefixPath `
    -Directory `
    -Filter "idris2-*" `
    -ErrorAction SilentlyContinue

foreach ($directory in $InstalledIdrisDirs) {
    foreach ($pattern in $CleanupPatterns) {
        Get-ChildItem `
            -LiteralPath $directory.FullName `
            -Directory `
            -Filter $pattern `
            -ErrorAction SilentlyContinue |
        Remove-Item `
            -Recurse `
            -Force
    }
}

# ---------------------------------------------------------------------------
# Persist ~/.local/bin to the user's PATH
# ---------------------------------------------------------------------------

$currentUserPath = [Environment]::GetEnvironmentVariable("Path", "User")

$userPathEntries = @()

if (-not [string]::IsNullOrWhiteSpace($currentUserPath)) {
    $userPathEntries = $currentUserPath -split ';'
}

if ($userPathEntries -notcontains $BinDir) {
    $newUserPath = if ([string]::IsNullOrWhiteSpace($currentUserPath)) {
        $BinDir
    }
    else {
        "$BinDir;$currentUserPath"
    }

    [Environment]::SetEnvironmentVariable(
        "Path",
        $newUserPath,
        "User"
    )

    Write-Host "Added $BinDir to the Windows user PATH."
}

# Keep HOME available only to this installer process. The generated launchers
# set HOME themselves, so no persistent user-level HOME variable is required.
$env:HOME = $HOME

# ---------------------------------------------------------------------------
# Final verification
# ---------------------------------------------------------------------------
#
# Run verification outside the caller's working tree. A repository-local
# pack.toml can override pack's configuration (for example with
# `commit = "latest:main"`), which should not influence bootstrap verification.
# Using a fresh temporary directory keeps this check limited to the installed
# global/state configuration without modifying pack's source code.
#
$VerificationDir = Join-Path `
    $env:TEMP `
    ("idris2-pack-verify-" + [Guid]::NewGuid().ToString("N"))

New-Directory $VerificationDir

Push-Location $VerificationDir
try {
    Invoke-Native $PackPath info
}
finally {
    Pop-Location
    Remove-Item `
        -LiteralPath $VerificationDir `
        -Recurse `
        -Force `
        -ErrorAction SilentlyContinue
}

Write-Host ""
Write-Host "pack installation completed."
Write-Host "MSYS2 root:       $Msys2Root"
Write-Host "Binary directory: $BinDir"
Write-Host "Scheme command:   $Scheme"
Write-Host "Chez version:     v10.4.1 (when auto-built)"
Write-Host ""
Write-Host "Installed launchers:"
Write-Host "  idris2.cmd  - PowerShell / CMD"
Write-Host "  idris2      - Git Bash / MSYS2"
Write-Host "  pack.cmd    - PowerShell / CMD"
Write-Host "  pack        - Git Bash / MSYS2"
Write-Host ""
Write-Host "Open a new terminal to pick up the persisted PATH change."
