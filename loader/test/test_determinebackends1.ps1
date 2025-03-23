Set-StrictMode -Version 3.0

$ErrorActionPreference = "Stop"
$PSNativeCommandUserErrorActionPerference = $true

# get the location of this file
$scriptpath = $MyInvocation.MyCommand.Path
# get the directory path to this file
$wd = Split-Path $scriptpath
# set the working directory as this file's directory
Push-Location $wd

function checkLastInvocation()
{
    if ($LastExitCode -eq 0)
	{
		Write-Host "Passed`n" -ForegroundColor Green
	}
	else
	{
		Write-Host "Failed`n" -ForegroundColor Red
	}
}

if ($args.Count -ge 1)
{
	$steamPath = $args[0]
}
if ($args.Count -ge 2 -and $args[1] -eq "-preferds")
{
	$preferDedicatedServers = $true
}
else
{
	$preferDedicatedServers = $false
}

Write-Host "Testing Alien Swarm" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Alien Swarm\bin;" + "${steamPath}\steamapps\common\Alien Swarm\swarm\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "swarm" -expectedbackend 9

checkLastInvocation

Write-Host "Testing Black Mesa" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Black Mesa\bin;" + "${steamPath}\steamapps\common\Black Mesa\bms\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "bms" -expectedbackend 21

checkLastInvocation

Write-Host "Testing Blade Symphony (32-bit)" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Blade Symphony\bin\win32;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "berimbau" -expectedbackend 18

checkLastInvocation

Write-Host "Testing Blade Symphony (64-bit)" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Blade Symphony\bin\win64;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir "berimbau" -expectedbackend 18

checkLastInvocation

if ($preferDedicatedServers)
{
	Write-Host "Testing Bloody Good Time Dedicated Server" -ForegroundColor yellow
	$env:Path="${steamPath}\steamapps\common\Bloody Good Time Dedicated Server\bin;" + "${steamPath}\steamapps\common\Bloody Good Time Dedicated Server\pm\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "pm" -expectedbackend 3
}
else
{
	Write-Host "Testing Bloody Good Time" -ForegroundColor yellow
	$env:Path="${steamPath}\steamapps\common\Bloody Good Time\bin;" + "${steamPath}\steamapps\common\Bloody Good Time\pm\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "pm" -expectedbackend 3
}

checkLastInvocation

Write-Host "Testing Contagion" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Contagion\engine;" + "${steamPath}\steamapps\common\Contagion\contagion\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir "contagion" -expectedbackend 20

checkLastInvocation

Write-Host "Counter-Strike Global Offensive" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Counter-Strike Global Offensive\bin;" + "${steamPath}\steamapps\common\Counter-Strike Global Offensive\csgo\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "csgo" -expectedbackend 11

checkLastInvocation

Write-Host "Counter-Strike Source" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Counter-Strike Source\bin\x64;" + "${steamPath}\steamapps\common\Counter-Strike Source\cstrike\bin\x64;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir "cstrike" -expectedbackend 5

checkLastInvocation

if ($preferDedicatedServers)
{
	Write-Host "Dark Messiah of Might and Magic Dedicated Server" -ForegroundColor yellow
	$env:Path="${steamPath}\steamapps\common\Dark Messiah of Might and Magic Dedicated Server\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "" -expectedbackend 1
}
else
{
	Write-Host "Dark Messiah of Might and Magic" -ForegroundColor yellow
	$env:Path="${steamPath}\steamapps\common\Dark Messiah of Might and Magic Multi-Player\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "" -expectedbackend 1
}

checkLastInvocation

Write-Host "Half-Life Source Deathmatch" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Half-Life 1 Source Deathmatch\bin\x64;" + "${steamPath}\steamapps\common\Half-Life 1 Source Deathmatch\hl1mp\bin\x64;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir "hl1mp" -expectedbackend 13

checkLastInvocation

Write-Host "Half-Life 2 Deathmatch" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Half-Life 2 Deathmatch\bin\x64;" + "${steamPath}\steamapps\common\Half-Life 2 Deathmatch\hl2mp\bin\x64;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir "hl2mp" -expectedbackend 13

checkLastInvocation

Write-Host "Left 4 Dead" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\left 4 dead\bin;" + "${steamPath}\steamapps\common\left 4 dead\left4dead\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "left4dead" -expectedbackend 7

checkLastInvocation

Write-Host "Left 4 Dead 2" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Left 4 Dead 2\bin;" + "${steamPath}\steamapps\common\Left 4 Dead 2\left4dead2\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "left4dead2" -expectedbackend 8

checkLastInvocation

Write-Host "Military Conflict Vietnam" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Military Conflict - Vietnam\bin\win64;" + "${steamPath}\steamapps\common\Military Conflict - Vietnam\vietnam\bin\win64;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir "vietnam" -expectedbackend 25

checkLastInvocation

Write-Host "Portal 2" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Portal 2\bin;" + "${steamPath}\steamapps\common\Portal 2\portal2\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "portal2" -expectedbackend 10

checkLastInvocation

Write-Host "Source SDK Base 2006 (Insurgency)" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Source SDK Base\bin;" + "${steamPath}\steamapps\common\Source SDK Base\insurgency\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "insurgency" -expectedbackend 0

checkLastInvocation

Write-Host "Source SDK Base 2007 (Age of Chivalry)" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Source SDK Base 2007\bin;" + "${steamPath}\steamapps\common\Source SDK Base 2007\ageofchivalry\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "ageofchivalry" -expectedbackend 2

checkLastInvocation

Write-Host "Team Fortress 2" -ForegroundColor yellow
$env:Path="${steamPath}\steamapps\common\Team Fortress 2\bin\x64;" + "${steamPath}\steamapps\common\Team Fortress 2\tf\bin\x64;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir "tf" -expectedbackend 15

checkLastInvocation

if ($preferDedicatedServers)
{
	Write-Host "Testing The Ship Dedicated Server" -ForegroundColor yellow
	$env:Path="${steamPath}\steamapps\common\The Ship Dedicated Server\bin;" + "${steamPath}\steamapps\common\The Ship Dedicated Server\ship\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "ship" -expectedbackend 0
}
else
{
	Write-Host "Testing The Ship" -ForegroundColor yellow
	$env:Path="${steamPath}\steamapps\common\The Ship\bin;" + "${steamPath}\steamapps\common\The Ship\ship\bin;" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir "ship" -expectedbackend 0
}

checkLastInvocation

Pop-Location
