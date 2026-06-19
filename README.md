Metamod:Source
==============

Metamod:Source - A C++ Plugin Environment and Detour Library for the Source Engine.

This fork is maintained by **Snaximusss+** server infrastructure. Original project by **AlliedModders**.

Fork: Metamod:Source - Production Hardening for CS2/Source Dedicated Servers
---------------------------------------------------------------------------

This fork of Metamod:Source focuses on production hardening for CS2 and Source
defensive null checks, safer engine/game interface handling, improved lifecycle
logging, and build hardening — while keeping full upstream API/ABI compatibility
with SourceMod, CounterStrikeShadow, and other plugin ecosystems.

Maintainer
----------
- **Maintainer**: Snaximusss+
- **Original project**: [AlliedModders Metamod:Source](https://github.com/alliedmodders/metamod-source)

Purpose
-------
- Reduce crash risk from bad plugins (null handles, partial load failures, double-unload)
- Safer engine/SDK detection with clear error messages
- Defensive string and memory practices
- Production-safe compiler hardening (O2, warnings, no ffast-math)
- Improved debug logging around plugin/engine lifecycle events

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

### Production build (recommended)
```
ambuild
```
This compiles with `-O2`, safe warnings (`-Wall -Wextra -Wformat -Wnull-dereference`),
and `-fno-omit-frame-pointer` for crash post-mortem.

### Debug build
```
python ../configure.py --enable-debug
ambuild
```
This adds `-g3 -DDEBUG -fno-omit-frame-pointer` and keeps full debug symbols.
Do NOT strip symbols if you need to investigate crashes with gdb or core dumps.

### Keeping debug symbols
To keep symbols for crash investigation:
- Use the `--enable-debug` configure flag above.
- Do NOT run `strip` on the output binary.
- On Linux, install `dbg` or `debuginfo` packages as needed.
- Run the server under `catchsegv` or `gdb` for backtraces.

### Build for CS2 only
```
python ../configure.py --sdks cs2
ambuild
```

CS2 install path example
-----------------------
Typical CS2 dedicated server structure:
```
csgo/
├── addons/
│   └── metamam/
│       ├── metamod.vdf
│       └── bin/
│           └── linuxsteamrt64/
│               └── libserver.so  ← Metamod loader goes here
├── bin/
│   └── linuxsteamrt64/
│       └── libserver.so         ← Real game DLL
└── gameinfo.gi
```

Metamod plugin path convention: `addons/metamod/metaplugins.ini`

Troubleshooting
---------------

### undefined symbol
- A plugin exports a function that cannot be found by `dlsym`.
- Check that the plugin was compiled against a compatible Metamod:Source SDK.
- Verify the plugin's `.so`/`.dll` is for the correct architecture (linuxsteamrt64 for CS2).

### missing factory
- `CreateInterface` returned NULL for a requested engine interface.
- The engine may have been updated and no longer exports that interface.
- Check that your Metamod:Source build supports the current engine version.

### unsupported engine
- Log: `Could not detect engine version (game="..."). Unsupported or too-new Source engine.`
- The game's interface versions don't match any known engine branch.
- Update to a newer Metamod:Source build that supports the game.

### plugin fails to load
- Check the server log for `[META] Failed to load plugin` messages.
- Check `DisplayDevMsg` debug logs (build with `--enable-debug`) for load phase details.
- Verify the plugin file exists and has correct permissions.
- Ensure the plugin's `GetApiVersion()` returns >= 14 (PLAPI_MIN_VERSION).

### crash during startup
- Build with `--enable-debug` and run under gdb: `gdb --args ./srcds_run ...`
- Check for null factory pointers in the log.
- Verify gameinfo.gi paths are correct.

### crash during plugin unload
- Check for double-unload in the log (debug builds log unload attempts).
- Ensure plugins do not call `delete this` or unload from within their own callbacks.
- Build with `--enable-debug` to get full backtraces.

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
ambuild
```

Stable build snapshots: <http://www.metamodsource.net/downloads.php/?branch=stable>

Development build snapshots: <http://www.metamodsource.net/downloads.php/?branch=master>

General documentation: <https://wiki.alliedmods.net/Category:Metamod:Source_Documentation>

Detouring with SourceHook: <https://wiki.alliedmods.net/SourceHook_Development>

Development: <https://wiki.alliedmods.net/Category:Metamod:Source_Development>
