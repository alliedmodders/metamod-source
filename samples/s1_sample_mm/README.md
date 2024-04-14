For more information on compiling and reading the plugin's source code, see:

	http://wiki.alliedmods.net/Category:Metamod:Source_Development

Build instructions
------------------

Make sure ambuild2 is installed: https://github.com/alliedmodders/ambuild

Configure the build (where `--hl2sdk-root` specifies the path to the desired SDK installed by `support/checkout-deps.sh`):
```
mkdir build
cd build
python ../configure.py --hl2sdk-root C:/Users/user/Documents/GitHub/hl2sdk-episode1
```

Build:
```
ambuild
```