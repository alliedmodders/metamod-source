Metamod:Source
==============

Metamod:Source - A C++ Plugin Environment and Detour Library for the Source Engine.

Build instructions
------------------

Make sure ambuild2 is installed: https://github.com/alliedmodders/ambuild

Clone the repo with submodules:
```
git clone --recurse-submodules https://github.com/alliedmodders/metamod-source
```

Clone the SDK dependencies:
```
cd ..
metamod-source/support/checkout-deps.sh
cd metamod-source
```

Configure the build:
```
mkdir build
cd build
python ../configure.py
```

Build:
```
ambuild
```

You can clone an individual SDK e.g.:
```
cd ..
metamod-source/support/checkout-deps.sh -s episode1
cd metamod-source
```

You can configure the build for an individual SDK e.g.:
```
mkdir build
cd build
python ../configure.py --sdks episode1
```

Stable build snapshots: <http://www.metamodsource.net/downloads.php/?branch=stable>

Development build snapshots: <http://www.metamodsource.net/downloads.php/?branch=master>

General documentation: <https://wiki.alliedmods.net/Category:Metamod:Source_Documentation>

Detouring with SourceHook: <https://wiki.alliedmods.net/SourceHook_Development>

Development: <https://wiki.alliedmods.net/Category:Metamod:Source_Development>