
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

PATH="C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\bin\x64":"C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\cstrike\bin\x64":$PATH loader/test/test_loader/windows-x86_64/test_loader.exe -testdbs1 -gamedir cstrike -expectedbackend 5
```

Using PowerShell:
```
powershell -Command { $env:Path="C:\Program Files (x86)\Steam\steamapps\common\Black Mesa\bin;" + "C:\Program Files (x86)\Steam\steamapps\common\Black Mesa\bms\bin;" + $env:Path; .\loader\test\test_loader\windows-x86\test_loader.exe -testdbs1 -gamedir bms -expectedbackend 21 }

powershell -Command { $env:Path="C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\bin\x64;" + "C:\Program Files (x86)\Steam\steamapps\common\Counter-Strike Source\cstrike\bin\x64;" + $env:Path; .\loader\test\test_loader\windows-x86_64\test_loader.exe -testdbs1 -gamedir cstrike -expectedbackend 5 }
```

You can run the following to test all supported games in Steam:
```
.\loader\test\test_determinebackends1.ps1 "C:\Program Files (x86)\Steam"
```
