Set-StrictMode -Version 3.0

$ErrorActionPreference = "Stop"
$PSNativeCommandUserErrorActionPerference = $true

# get the location of this file
$scriptpath = $MyInvocation.MyCommand.Path
# get the directory path to this file
$wd = Split-Path $scriptpath
# set the working directory as this file's directory
Push-Location $wd

class SteamGame {
	[string]$Name
	[string]$EnginePath
	[string]$GamePath
	[string]$GameDir
	[string]$Architecture
	[bool]$IsDedicatedServer
	[int]$ExpectedBackend
	[bool]$Skip

	SteamGame([string]$name, [string]$enginePath, [string]$gamePath, [string]$gameDir, [string]$architecture, [bool]$isDedicatedServer, [int]$expectedBackend, [bool]$skip) {
		$this.Name = $name
		$this.EnginePath = $enginePath
		$this.GamePath = $gamePath
		$this.GameDir = $gameDir
		$this.Architecture = $architecture
		$this.IsDedicatedServer = $isDedicatedServer
		$this.ExpectedBackend = $expectedBackend
		$this.Skip = $skip
    }
}

if ($args.Count -ge 1)
{
	$steamPath = $args[0]
}

$global:failureCount = 0

$gamesToTest = @{
	alienswarm = [SteamGame]::new("Alien Swarm", "common\Alien Swarm\bin", "common\Alien Swarm\swarm\bin", "swarm", "x86", $false, 9, $true)
	blackmesa = [SteamGame]::new("Black Mesa", "common\Black Mesa\bin", "common\Black Mesa\bms\bin", "bms", "x86", $false, 21, $false)
	bladesymphony_32 = [SteamGame]::new("Blade Symphony (x86)", "common\Blade Symphony\bin\win32", "", "berimbau", "x86", $false, 18, $false)
	bladesymphony_64 = [SteamGame]::new("Blade Symphony (x64)", "common\Blade Symphony\bin\win64", "", "berimbau", "x64", $false, 18, $false)
	bloodygoodtime = [SteamGame]::new("Bloody Good Time", "common\Bloody Good Time\bin", "common\Bloody Good Time\pm\bin", "pm", "x86", $false, 3, $true)
	bloodygoodtime_ds = [SteamGame]::new("Bloody Good Time Dedicated Server", "common\Bloody Good Time Dedicated Server\bin", "common\Bloody Good Time Dedicated Server\pm\bin", "pm", "x86", $false, 3, $true)
	contagion = [SteamGame]::new("Contagion", "common\Contagion\engine", "common\Contagion\bin", "contagion", "x64", $false, 18, $false)
	counterstrikeglobaloffensive = [SteamGame]::new("Counter-Strike Global Offensive", "common\Counter-Strike Global Offensive\bin", "common\Counter-Strike Global Offensive\csgo\bin", "csgo", "x86", $false, 11, $false)
	counterstrikesource = [SteamGame]::new("Counter-Strike Source", "common\Counter-Strike Source\bin\x64", "common\Counter-Strike Source\cstrike\bin\x64", "cstrike", "x64", $false, 5, $false)
	darkmessiahofmightandmagic = [SteamGame]::new("Dark Messiah of Might and Magic", "common\Dark Messiah Might and Magic Multi-Player\bin", "", "", "x86", $false, 1, $true)
	darkmessiahofmightandmagic_ds = [SteamGame]::new("Dark Messiah of Might and Magic Dedicated Server", "common\Dark Messiah Might and Magic Dedicated Server\bin", "", "", "x86", $false, 1, $true)
	dayofdefeatsource = [SteamGame]::new("Day of Defeat Source", "common\Day of Defeat Source\bin\x64", "common\Day of Defeat Source\dod\bin\x64", "dod", "x64", $false, 14, $false)
	# NOTE: fails because libogg.dll and libvorbis.dll both link MSVCR100.dll
	dayofinfamy_32 = [SteamGame]::new("Day of Infamy (x86)", "common\dayofinfamy\bin", "common\dayofinfamy\doi\bin", "doi", "x86", $false, 22, $true)
	dayofinfamy_64 = [SteamGame]::new("Day of Infamy (x64)", "common\dayofinfamy\bin\x64", "common\dayofinfamy\doi\bin\x64", "doi", "x64", $false, 22, $false)
	eyedivinecybermancy = [SteamGame]::new("E.Y.E: Divine Cybermancy", "common\EYE\bin", "common\EYE\EYE\bin", "EYE", "x86", $false, 4, $true)
	fistfuloffrags = [SteamGame]::new("Fistful of Frags", "common\Fistful of Frags\sdk\bin", "common\Fistful of Frags\fof\bin", "fof", "x86", $false, 17, $false)
	garrysmod = [SteamGame]::new("Garry's Mod", "common\GarrysMod\bin", "common\GarrysMod\garrysmod\bin", "garrysmod", "x86", $false, 17, $true)
	halflifesourcedeathmatch = [SteamGame]::new("Half-Life Source Deathmatch", "common\Half-Life 1 Source Deathmatch\bin\x64", "common\Half-Life 1 Source Deathmatch\hl1mp\bin\x64", "hl1mp", "x64", $false, 13, $false)
	halflifesourcedeathmatch_ds = [SteamGame]::new("Half-Life Source Deathmatch Dedicated Server", "common\Half-Life Deathmatch Source Dedicated Server\bin\x64", "common\Half-Life Deathmatch Source Dedicated Server\hl1mp\bin\x64", "hl1mp", "x64", $false, 13, $false)
	halflife2deathmatch = [SteamGame]::new("Half-Life 2 Deathmatch", "common\Half-Life 2 Deathmatch\bin\x64", "common\Half-Life 2 Deathmatch\hl2mp\bin\x64", "hl2mp", "x64", $false, 13, $false)
	# NOTE: fails because libogg.dll and libvorbis.dll both link MSVCR100.dll
	insurgency_32 = [SteamGame]::new("Insurgency (x86)", "common\insurgency2\bin", "common\insurgency2\insurgency\bin", "insurgency", "x86", $false, 19, $true)
	insurgency_64 = [SteamGame]::new("Insurgency (x64)", "common\insurgency2\bin\x64", "common\insurgency2\insurgency\bin\x64", "insurgency", "x64", $false, 19, $false)
	jabronibrawlepisode3 = [SteamGame]::new("Jabroni Brawl Episode 3", "common\Jabroni Brawl Episode 3\bin\win32", "common\Jabroni Brawl Episode 3\jbep3\bin\win32", "jbep3", "x86", $false, 11, $true)
	left4dead = [SteamGame]::new("Left 4 Dead", "common\left 4 dead\bin", "common\left 4 dead\left4dead\bin", "left4dead", "x86", $false, 7, $true)
	left4dead2 = [SteamGame]::new("Left 4 Dead 2", "common\Left 4 Dead 2\bin", "common\Left 4 Dead 2\left4dead2\bin", "left4dead2", "x86", $false, 8, $true)
	militaryconflictvietnam = [SteamGame]::new("Military Conflict Vietnam", "common\Military Conflict - Vietnam\bin\win64", "common\Military Conflict - Vietnam\vietnam\bin\win64", "vietnam", "x64", $false, 25, $false)
	nucleardawn = [SteamGame]::new("Nuclear Dawn", "common\Nuclear Dawn\bin", "common\Nuclear Dawn\nucleardawn\bin", "nucleardawn", "x86", $false, 16, $true)
	piratesvikingsandknightsii = [SteamGame]::new("Pirates, Vikings and Knights II", "common\pirates, vikings and knights ii\sdkbase_pvkii\bin", "common\pirates, vikings and knights ii\pvkii\bin", "pvkii", "x86", $false, 24, $false)
	portal2 = [SteamGame]::new("Portal 2", "common\Portal 2\bin", "common\Portal 2\portal2\bin", "portal2", "x86", $false, 10, $true)
	revelations2012 = [SteamGame]::new("Revelations 2012", "common\Revelations 2012\bin", "common\Revelations 2012\revelations\bin", "revelations", "x86", $false, 8, $true)
	sdk2006_insurgency = [SteamGame]::new("Source SDK Base 2006 (Insurgency)", "common\Source SDK Base\bin", "common\Source SDK Base\insurgency\bin", "insurgency", "x86", $false, 0, $false)
	sdk2007_ageofchivalry = [SteamGame]::new("Source SDK Base 2007 (Age of Chivalry)", "common\Source SDK Base 2007\bin", "common\Source SDK Base 2007\ageofchivalry\bin", "ageofchivalry", "x86", $false, 2, $true)
	teamfortress2 = [SteamGame]::new("Team Fortress 2", "common\Team Fortress 2\bin\x64", "common\Team Fortress 2\tf\bin\x64", "tf", "x64", $false, 15, $false)
	treason = [SteamGame]::new("Treason", "common\Treason\bin", "common\Treason\treason\bin", "treason", "x86", $false, 17, $false)
	theship = [SteamGame]::new("The Ship", "common\The Ship\bin", "common\The Ship\ship\bin", "ship", "x86", $false, 0, $true)
	theship_ds = [SteamGame]::new("The Ship Dedicated Server", "common\The Ship Dedicated Server\bin", "common\The Ship Dedicated Server\ship\bin", "ship", "x86", $false, 0, $true)
	zombiepanicsource = [SteamGame]::new("Zombie Panic Source", "common\Zombie Panic Source\bin", "common\Zombie Panic Source\zps\bin", "zps", "x86", $false, 17, $false)
}

function testGame([SteamGame]$game)
{
	Write-Host @("Testing", $game.Name) -ForegroundColor yellow

	$steamappsPath = "${steamPath}\steamapps"
	$enginePath = "${steamappsPath}\" + $game.EnginePath
	$gamePath = "${steamappsPath}\" + $game.GamePath

	if (!(Test-Path -Path $enginePath))
	{
		Write-Host "Skipped - game not found at ${enginePath}`n" -ForegroundColor DarkYellow

		Return
	}

	if ($gamePath -ne "" -And !(Test-Path -Path $gamePath))
	{
		Write-Host "Skipped - game not found at ${gamePath}`n" -ForegroundColor DarkYellow

		Return
	}

	# make gamedir the last parameter as something weird is going on where if it's "" then PowerShell is collapsing it and the next argument gets passed as gamedir
	if ($game.Architecture -eq "x86")
	{
		$env:Path=$enginePath + ";" + $gamePath + ";" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -expectedbackend $game.ExpectedBackend -gamedir $game.GameDir
	}
	else
	{
		$env:Path=$enginePath + ";" + $gamePath + ";" + $env:Path; ..\..\build\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -expectedbackend $game.ExpectedBackend -gamedir $game.GameDir
	}

	# some games don't seem to do the right thing when not initialised through the engine, so skip the known failures
	if ($game.Skip -eq $true)
	{
		Write-Host "Skipped - invalid result`n" -ForegroundColor DarkYellow

		Return
	}

	if ($LastExitCode -eq 0)
	{
		Write-Host "Passed`n" -ForegroundColor Green
	}
	else
	{
		Write-Host "Failed`n" -ForegroundColor Red
		$global:failureCount++
	}
}

foreach ($game in $gamesToTest.Keys | Sort-Object)
{
	testGame $gamesToTest[$game]
}

Write-Host "There were ${global:failureCount} failures"

Pop-Location
