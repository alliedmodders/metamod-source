For more information on compiling and reading the plugin's source code, see:

	http://wiki.alliedmods.net/Category:Metamod:Source_Development

Build instructions
------------------

Make sure ambuild2 is installed: https://github.com/alliedmodders/ambuild

Configure the build (`--hl2sdk-root` specifies the path where the all SDKs have been installed by `support/checkout-deps.sh` and `--mms_path` is the path to Metamod: Source).

If you only want to compile using a specific SDK you can hack around the requirement to build for all SDKs by modifying the calls to the SDK constructor in the assignment to `PossibleSDKs` in `AMBuildScript` and setting the `platforms` parameter to \[\] for all SDKs that you don't want to compile for.

### Windows

Use Command Prompt as Gitbash doesn't handle the path arguments correctly.

```
mkdir build
cd build
python ../configure.py --hl2sdk-root C:\Users\user\Documents\GitHub --mms_path C:\Users\user\Documents\GitHub\metamod-source
```

Build:
```
ambuild
```