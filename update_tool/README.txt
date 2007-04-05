This is the README file for sourcemm_update_tool.

This tool will automatically correct gameinfo.txt when your server gets updated, and Valve's updater overwrites Metamod:Source's changes.  This tool is experimental, and is thus a separate download for now.

1.  INSTALLATION

 a.  Extract the entire package to your mod folder.  The structure should look like:

	<mod>/addons/metamod/bin/sourcemm_update_tool.dll
	<mod>/addons/metamod/bin/sourcemm_update_tool_i486.so
	<mod>/addons/metamod/README.txt
	<mod>/addons/sourcemm_update_tool.vdf
	<mod>/sourcemm_updater.conf

 b.  Open <mod>/sourcemm_updater.conf with your favorite text editor.  Change the
     "cstrike" folder to match your mod folder.  

2.  CONFIGURATION

   The sourcemm_updater.conf file has two configuration options.  

   mmpath     - Set this to the path Metamod:Source is located in.
                This defaults to addons/metamod/bin
   restart    - Set this to how the server should be restarted when 
                gameinfo.txt is patched.  There are three options:

                quit  - Execute "quit" in the console.  Currently does not work.
                never - Do not restart.
                error - Generate an error message.  Because of a bug in SourceDS,
                        this will generate a crash dump on Windows, but the server
                        will successfully quit.

3.  USAGE

   You do not need to do anything to use the updater tool.  When your server starts,
 it silently checks to see if Metamod:Source is loaded.  If not, it will make sure
 gameinfo.txt is correctly set.  Then, depending on how it's configured, it will 
 kill the server.  Most game server provides have auto-restart functionality on their
 servers; if not, you will need to manually restart the server.

   The update tool unloads itself immediately after the server starts, so it will not
 use any resources, and will not display when you type 'plugin_print'.  

4.  TROUBLESHOOTING

   This tool is currently experimental.  There are two possible problems.  For 
   any issue you encounter, you should post a report here:

   http://bugs.alliedmods.net/index.php?project=4

  a.  The updater tool does not patch gameinfo.txt

      Verify that the tool is loading.  You can do this by opening up the 
      sourcemm_update_tool.vdf file and copying its file path.  Then, enter
      the following command in your server console:

       plugin_load <path>

      Example:

       plugin_load cstrike\addons\metamod\bin\sourcemm_update_tool

      If you get the following reply:

       Failed to load plugin "cstrike\addons\sourcemm_update_tool"
       Unable to load plugin "cstrike\addons\sourcemm_update_tool"

      Then the tool is working, and you should post a bug report.  If instead, 
      you get:

       Unable to load plugin "cstrike\addons\sourcemm_update_tool"
       Unable to load plugin "cstrike\addons\sourcemm_update_tool"

      Then the tool is not loading properly, and the path you are trying to use
      is not correct.

  b.  The server always dies, and you can't start it at all

      The updater tool is either crashing or not repairing Metamod:Source 
      correctly.  First, try changing the 'restart' line in sourcemm_updater.conf
      to the following line:

	restart = never

      If that does not fix the problem, remove the .vdf file so the updater tool
      will not be loaded.  

      In either case, you should post a bug report containing your mod name and
      your gameinfo.txt as an attachment.
      