## Manual building example

### Prerequisites
 * [hl2sdk](https://github.com/alliedmodders/hl2sdk) of games you plan on writing plugin for (this sample only supports Source2 based games);
 * [hl2sdk-manifests](https://github.com/alliedmodders/hl2sdk-manifests);
 * [metamod-source](https://github.com/alliedmodders/metamod-source);
 * [python3](https://www.python.org/)
 * [ambuild](https://github.com/alliedmodders/ambuild), make sure ``ambuild`` command is available via the ``PATH`` environment variable;

### Setting up
 * Edit ``plugin-metadata.json`` with your plugin details (if you don't have that file run configure step once to generate it), where:
   * ``name``: Plugin name which is used for the resulting binary name and folder naming scheme;
   * ``alias``: Plugin alias which is used as a short hand name to load, unload, list info etc via the [metamod-source](https://github.com/alliedmodders/metamod-source) menu (example being ``meta unload sample``, where ``sample`` is the alias);
   * ``log_tag``: Plugin log tag, used for logging purposes;
   * Additional plugin info used when printing plugin info by ``meta list`` or alike:
     * ``display_name``: Display name;
     * ``description``: Description;
     * ``author``: Author;
     * ``url``: Url link;
     * ``license``: License;
     * ``version``: Version string, can also contain git revisions, like ``"1.0.0.{{git-shorthash}}"``:
       * ``{{git-shorthash}}``: Git shorthash of head commit (7 symbols wide);
       * ``{{git-longhash}}``: Git full hash of head commit;
       * ``{{git-count}}``: Git commit count to current head;
       * Would be 0 for all cases if no git info was located;
 * ``mkdir build`` & ``cd build`` in the root of the plugin folder.
 * Open the [MSVC developer console](https://learn.microsoft.com/en-us/cpp/build/building-on-the-command-line) with the correct platform (x86 or x86_64) that you plan on targetting.
 * Run ``python3 ../configure.py -s {SDKNAMES} --targets={TARGET} --mms_path={MMS_PATH} --hl2sdk-root {HL2SDKROOT} --hl2sdk-manifests {HL2SDKMANIFESTS}`` where:
   * ``{SDKNAMES}`` should be the hl2sdk game names that you are building for, separated by comma (e.g. ``cs2,dota``);
   * ``{TARGET}`` should be the target platform you are targeting (``x86`` or ``x86_64``, source2 games are mostly ``x86_64``);
   * ``{MMS_PATH}`` should point to the root of the [metamod-source](https://github.com/alliedmodders/metamod-source) folder;
   * ``{HL2SDKROOT}`` should point to the root of the [hl2sdk](https://github.com/alliedmodders/hl2sdk)'s folders, note that it should **not** point to the actual ``hl2sdk-GAME`` folder but a parent directory of it (Game specific folders inside the root needs to be named as ``hl2sdk-GAME`` where ``GAME`` is a game name like ``cs2``);
   * ``{HL2SDKMANIFESTS}`` should point to the root of a clone of the [hl2sdk-manifests](https://github.com/alliedmodders/hl2sdk-manifests) repository.
    > **Example**: ``python3 ../configure.py -s cs2,dota --targets=x86_64 --mms_path=D:\mmsource-1.12 --hl2sdk-root=D:\hl2sdks``
 * If the process of configuring was successful, you should be able to run ``ambuild`` in the ``\build`` folder to compile the plugin.
 * Once the plugin is compiled the files would be packaged and placed in ``\build\package`` folder in its own per game subfolders.
 * To run the plugin on the server, place the files preserving the layout provided in ``\package\{game}``. Be aware that plugins get loaded either by corresponding ``.vdf`` files (automatic step) in the metamod folder, or by listing them in ``addons\metamod\metaplugins.ini`` file (manual step).
 
 ### Environment variables setup
 To quickly configure various projects, it's recommended to setup an environment variables to point to certain locations used for building:
 * ``MMSOURCE20``/``MMSOURCE_DEV`` should point to root of [metamod-source](https://www.metamodsource.net/downloads.php?branch=dev) of the version 2.0 and higher;
 * ``HL2SDKMANIFESTS`` should point to [hl2sdk-manifests](https://github.com/alliedmodders/hl2sdk-manifests) directory.
 * ``HL2SDKROOT`` should point to root folder where [hl2sdk](https://github.com/alliedmodders/hl2sdk) directories are in;
 * Alternatively ``HL2SDK{GAME}`` (e.g. ``HL2SDKCS2``) can be used to point to game specific [hl2sdk](https://github.com/alliedmodders/hl2sdk) directories, in which case a direct path to its root needs to be provided;
> [!NOTE]
> If you have ``HL2SDKROOT`` defined as well, it will take priority over game specific environment variables!

 ## Points of interest
 * To generate the VS solution of the plugin with the correct setup, use ``python3 ../configure {CONFIGURE_OPTIONS} --gen=vs --vs-version=2022``, where ``{CONFIGURE_OPTIONS}`` is your default configuring options that are used when building. As a result ``.vcxproj`` files would be created in the folder where the command was executed separately for each sdk game provided.
 * To update which ``.cpp`` files gets compiled in or to add new ones, look at ``AMBuilder`` script which has the ``src/plugin.cpp`` being built initially, you can add or edit this however you want to, running ``ambuild`` after editing this script would automatically catch up the changes, no need for the reconfiguration step.
 * To add new ``.proto`` headers to the project, look at ``AMBuilder`` script which has ``os.path.join(sdk['path'], 'common', 'network_connection.proto')`` being built initially, you can add or edit this however you want to as with cpp files.
 * There are also additional arguments for the configuration step that aren't covered here, you can see them by running ``python3 ../configure -h`` from within the ``\build`` folder.
 * To add additional linking ``.libs``/defines/include directories, open ``AMBuildScript`` and at the top edit corresponding arrays.
 * Sometimes there could be problems with ``ambuild`` not catching up the changes in ``.h`` files, thus producing incorrect (outdated) binaries or even doesn't compile with the new changes. As there's [no full rebuild option](https://github.com/alliedmodders/ambuild/issues/145) to combat this, go to the ``/build`` folder and locate the folder named after the plugin name you've used, deleting that folder and building after should provide the clean build of the project and the described issues should be eliminated.
 * To add new files to the package output, like configs or any other, you can edit ``PackageScript`` at ``Add custom stuff here`` marker.


## For more information on compiling and reading the plugin's source code, see:

	http://wiki.alliedmods.net/Category:Metamod:Source_Development

