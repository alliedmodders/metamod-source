<#
.SYNOPSIS
    Generate a .lib file for linking to a dll.
.DESCRIPTION
    This script generates a .lib from from a dynamic link library (dll) file.

    The .lib file can then be linked against to resolve symbols in the dll file.
.PARAMETER InputBinary
    The path to the dll file.
.PARAMETER OutPath
    The output folder in which to save the .lib file. Defaults to the directory of the dll file.

    The folder must already exist.
#>
[CmdletBinding(SupportsShouldProcess, ConfirmImpact = 'High')]
param(
    [Parameter(Mandatory, ValueFromPipeline)]
    [ValidateScript({($_ -match '\.dll$') -and (Test-Path -Path $_ -PathType Leaf)})]
    [string]$InputBinary,

    [Parameter()]
    [ValidateScript({Test-Path -Path $_ -PathType Container})]
    [string]$OutPath,

    [Parameter()]
    [switch]$Force
)

try {
    $dumpbinBin = (Get-Command -Name 'dumpbin.exe').Source
} catch {
    throw 'Could not find dumpbin.exe in PATH. (Are you running from a Developer Command Prompt?)'
}

Write-Verbose "Found dumpbin.exe at '$dumpbinBin'"

try {
    $libBin = (Get-Command -Name 'lib.exe').Source
} catch {
    throw 'Could not find lib.exe in PATH. (Are you running from a Developer Command Prompt?)'
}

Write-Verbose "Found lib.exe at '$libBin'"

$inputBinInfo = Get-Item -Path $InputBinary

$defTempPath = Join-Path -Path ([System.IO.Path]::GetTempPath()) -ChildPath ('{0}.def' -f $inputBinInfo.BaseName)

if (-not $OutPath) {
    $OutPath = $inputBinInfo.DirectoryName
}

Write-Verbose "Using output path '$OutPath'"

$outLibFullName = Join-Path -Path $OutPath -ChildPath ('{0}.lib' -f $inputBinInfo.BaseName)
if ((-not (Test-Path -Path $outLibFullName)) -or $PSCmdlet.ShouldProcess($outLibFullName, 'overwrite'))
{
    $archCode = ((. $dumpbinBin /headers $inputBinInfo.FullName | Select-String -Context 1 -SimpleMatch -Pattern 'FILE HEADER VALUES' -CaseSensitive).Context.PostContext | Select-String -Pattern '^\s*([A-Z0-9]+) machine \(.*\)$' -CaseSensitive).Matches[0].Groups[1].Value
    switch ($archCode)
    {
        '14C' { $arch = 'X86' }
        '8664' { $arch = 'X64' }
        'AA64' { $arch = 'ARM64' }
        default {
            if ([Environment]::Is64BitOperatingSystem) {
                $arch = 'X64'
            } else {
                $arch = 'X86'
            }
            # TODO: Add ARM64 detection fallback

            Write-Warning "Unable to determine arch of binary. Defaulting to $arch for lib."
        }
    }

    Write-Verbose "Using arch '$arch' for lib generation."

    $funcs = . $dumpbinBin /exports $inputBinInfo.FullName |
        Select-String -Pattern '^\s+\d+\s+[A-Z0-9]+\s+[A-Z0-9]+\s+(.+)$' -CaseSensitive |
        Select-Object -ExpandProperty 'Matches' |
        ForEach-Object -Process {
            $_.Groups[1].Value
        }

    Write-Verbose "Found $($funcs.Count) functions in '$InputBinary'"

    Write-Verbose "Creating def file at '$defTempPath'"
    'EXPORTS' | Out-File $defTempPath -Force
    $funcs | Out-File $defTempPath -Append

    Write-Verbose "Calling lib.exe to generate '$outLibFullName'"
    . $libBin /def:$defTempPath /OUT:$outLibFullName /MACHINE:$arch

    Write-Verbose "Removing temp def file at '$defTempPath'"
    Remove-Item -Path $defTempPath -Confirm:$false
}