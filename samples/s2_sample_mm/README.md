## Manual building example

### Prerequisites
 * [hl2sdk](https://github.com/alliedmodders/hl2sdk) of the game you plan on writing plugin for (the current plugin build scripts allows only for 1 sdk and 1 platform at a time!);
 * [metamod-source](https://github.com/alliedmodders/metamod-source);
 * [python3](https://www.python.org/)
 * [ambuild](https://github.com/alliedmodders/ambuild), make sure ``ambuild`` command is available via the ``PATH`` environment variable;

### Setting up
 * ``mkdir build`` & ``cd build`` in the root of the plugin folder.
 * Open the [MSVC developer console](https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line) with the correct platform (x86 or x86_64) that you plan on targetting.
 * Run ``python3 ../configure.py --plugin-name={PLUGIN_NAME} --plugin-alias={PLUGIN_ALIAS} -s {SDKNAME} --targets={TARGET} --mms_path={MMS_PATH} --hl2sdk-root {HL2SDKROOT} --hl2sdk-manifests {HL2SDKMANIFESTS} `` where:
   * ``{PLUGIN_NAME}`` should be the plugin name which is used for the resulting binary name and folder naming scheme (this doesn't affect the plugin name you'd see in the plugin list if you don't modify the base plugin functions);
   * ``{PLUGIN_ALIAS}`` should be used to set the plugin alias that is used as a short hand version to load, unload, list info etc via the metamod-source menu (example being ``meta unload sample``, where ``sample`` is the alias);
   * ``{SDKNAME}`` should be the hl2sdk game name that you are building for;
   * ``{TARGET}`` should be the target platform you are targeting (``x86`` or ``x86_64``);
   * ``{MMS_PATH}`` should point to the root of the metamod-source folder;
   * ``{HL2SDKROOT}`` should point to the root of the hl2sdk's folders, note that it should **not** point to the actual ``hl2sdk-GAME`` folder but a root parent of it;
   * ``{HL2SDKMANIFESTS}`` should point to the root of a clone of the https://github.com/alliedmodders/hl2sdk-manifests repository. It is suggested that you use a submodule for this, as this folder must live within your project for AMBuild to be able to consume the helper script within;
   * Alternatively ``{MMS_PATH}`` & ``{HL2SDKROOT}`` could be put as a ``PATH`` environment variables, like ``MMSOURCE112=D:\mmsource-1.12`` & ``HL2SDKCS2=D:\hl2sdks\hl2sdk-cs2`` (note the metamod version and that here hl2sdk environment variable should point directly to the game's hl2sdk folder and not to the root of it!)
   * Example: ``python3 ../configure.py --plugin-name=sample_mm --plugin-alias=sample -s cs2 --targets=x86_64 --mms_path=D:\mmsource-1.12 --hl2sdk-root=D:\hl2sdks``
 * If the process of configuring was successful, you should be able to run ``ambuild`` in the ``\build`` folder to compile the plugin.
 * Once the plugin is compiled the files would be packaged and placed in ``\build\package`` folder.
 * To run the plugin on the server, place the files preserving the layout provided in ``\package``. Be aware that plugins get loaded either by corresponding ``.vdf`` files (automatic step) in the metamod folder, or by listing them in ``addons/metamod/metaplugins.ini`` file (manual step).
 
 ## Points of interest
 * To generate the VS solution of the plugin with the correct setup, use ``python3 ../configure {CONFIGURE_OPTIONS} --gen=vs``, where ``{CONFIGURE_OPTIONS}`` is your default configuring options that are used when building. As a result ``.vcxproj`` file would be created in the folder where the command was executed.
 * To update which ``.cpp`` files gets compiled in or to add new ones, look at ``AMBuilder`` script which has the ``sample_mm.cpp`` being built initially, you can add or edit this however you want to, running ``ambuild`` after editing this script would automatically catch up the changes, no need for the reconfiguration.
 * To change the name/version/author/etc that's displayed in the metamod-source menu, make sure to correctly overload and provide the wanted info to the metamod-source, like ``ISmmPlugin::GetAuthor``, ``ISmmPlugin::GetName`` and so on.
 * There are also additional arguments for the configuration step that aren't covered here, you can see them by running ``python3 ../configure -h`` from within the ``\build`` folder.
 * To add additional linking ``.libs``/defines/include directories, open ``AMBuildScript`` and at the top edit corresponding arrays.
 * Sometimes there could be problems with ``ambuild`` not catching up the changes in ``.h`` files, thus producing incorrect (outdated) binaries or even doesn't compile with the new changes. As there's [no full rebuild option](https://github.com/alliedmodders/ambuild/issues/145) to combat this, go to the ``/build`` folder and locate the folder named after the plugin name you've used, deleting that folder and building after should provide the clean build of the project and the described issues should be eliminated.


## For more information on compiling and reading the plugin's source code, see:

	http://wiki.alliedmods.net/Category:Metamod:Source_Development

