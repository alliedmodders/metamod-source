
Building the tests
------------------

```
mkdir build
cd build/
python ../configure.py --sdks episode1 --enable-tests
ambuild
```

Running the tests (Windows)
---------------------------

```
cd build/
loader/test/test_loader/windows-x86/test_loader.exe
loader/test/test_loader/windows-x86_64/test_loader.exe
```


Runing the loader tests for `mm_DetermineBackendS1` (Windows)
-------------------------------------------------------------

Using GitBash:
```
PATH="C:\Program Files (x86)\Steam\steamapps\common\Black Mesa\bin":"C:\Program Files (x86)\Steam\steamapps\common\Black Mesa\bms\bin":$PATH loader/test/test_loader/windows-x86/test_loader.exe -testdbs1 -gamedir bms -expectedbackend 21

PATH="C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Global Offensive\bin":"C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Global Offensive\csgo\bin":$PATH loader/test/test_loader/windows-x86/test_loader.exe -testdbs1 -gamedir csgo -expectedbackend 11

PATH="C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\bin\x64":"C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\cstrike\bin\x64":$PATH loader/test/test_loader/windows-x86_64/test_loader.exe -testdbs1 -gamedir cstrike -expectedbackend 5

PATH="C:\Program Files (x86)\Steam\steamapps\common\Half-Life 2 Deathmatch\bin\x64":"C:\Program Files (x86)\Steam\steamapps\common\Half-Life 2 Deathmatch\hl2mp\bin\x64":$PATH loader/test/test_loader/windows-x86_64/test_loader.exe -testdbs1 -gamedir hl2mp -expectedbackend 13

PATH="C:\Program Files (x86)\Steam\steamapps\common\Military Conflict - Vietnam\bin\win64":"C:\Program Files (x86)\Steam\steamapps\common\Military Conflict - Vietnam\vietnam\bin\win64":$PATH loader/test/test_loader/windows-x86_64/test_loader.exe -testdbs1 -gamedir vietnam -expectedbackend 25

# NOTE: use Insurgency because server.dll in episodic/ doesn't load
PATH="C:\Program Files (x86)\Steam\steamapps\common\Source SDK Base":"C:\Program Files (x86)\Steam\steamapps\common\Source SDK Base\bin":"C:\Program Files (x86)\Steam\steamapps\common\Source SDK Base\insurgency\bin":$PATH loader/test/test_loader/windows-x86/test_loader.exe -testdbs1 -gamedir insurgency -expectedbackend 0

PATH="C:\Program Files (x86)\Steam\steamapps\common\Team Fortress 2\bin\x64":"C:\Program Files (x86)\Steam\steamapps\common\Team Fortress 2\tf\bin\x64":$PATH loader/test/test_loader/windows-x86_64/test_loader.exe -testdbs1 -gamedir tf -expectedbackend 15
```

Using PowerShell:
```
powershell -Command { $env:Path="C:\Program Files (x86)\Steam\steamapps\common\Black Mesa\bin;" + "C:\Program Files (x86)\Steam\steamapps\common\Black Mesa\bms\bin;" + $env:Path; .\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir bms -expectedbackend 21 }

powershell -Command { $env:Path="C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\bin\x64;" + "C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\cstrike\bin\x64;" + $env:Path; .\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir cstrike -expectedbackend 5 }
```