///////////////////////////////////////////////////////////////////////////////////////////
//                                                                                       //
//                                                                                       //
//     File: Default.js                                                                  //
//                                                                                       //
//                                                                                       //
///////////////////////////////////////////////////////////////////////////////////////////


///////////////////////////////////////////////////////////////////////////////////////////
// Use wizard.ReportError to display a message box for debugging
	
// wizard.ReportError("<your message here>", true);


///////////////////////////////////////////////////////////////////////////////////////////
// OnFinish

function OnFinish(selProj, selObj)
{
	try
	{
	  // Get the new project name and path
	  
		var strProjectName = wizard.FindSymbol('PROJECT_NAME');
		var strProjectPath = wizard.FindSymbol('PROJECT_PATH');

		var strProjectTemplateName = wizard.FindSymbol('PLUGIN_TEMPLATE_NAME'); // Defined in default.htm

    // Get the current date and add a symbol CURRENT_DATE
    
		var curr_time = new Date();
		
		var month = curr_time.getMonth() + 1;
    var day   = curr_time.getDate();
    var year  = curr_time.getFullYear();
    var today = year + '-' + month + '-' + day;
    
		wizard.AddSymbol('CURRENT_DATE', today);
		
    // Set the path to where the template is installed
    
		var strPluginPath = wizard.FindSymbol('PROJECT_TEMPLATE_PATH') + 
		                                      "\\AppWiz\\Metamod\\" +
		                                      strProjectTemplateName + "\\";

    wizard.AddSymbol('PLUGIN_INSTALL_PATH', strPluginPath);
		wizard.AddSymbol('PLUGIN_SOURCE_FILES', "Plugin Source Files");
		wizard.AddSymbol('PLUGIN_HEADER_FILES', "Plugin Header Files");
		wizard.AddSymbol('PLUGIN_RESOURCE_FILES', "Resource Files");

    // Create the custom project
    
		selProj = CreateCustomProject(strProjectName, strProjectPath);
		
		AddConfig(selProj, strProjectName);
		AddFilters(selProj);

    // Create the custom .inf file
    
		var InfFile = CreateCustomInfFile();
		
		AddFilesToCustomProj(selProj, strProjectName, strProjectPath, InfFile);
		PchSettings(selProj);
		
		InfFile.Delete();

    // Save the project
    
		selProj.Object.Save();
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		if(ex.description.length != 0)
	  {
			SetErrorInfo(ex);
		}
			
		return ex.number
	}
}

///////////////////////////////////////////////////////////////////////////////////////////
// CreateCustomProject

function CreateCustomProject(strProjectName, strProjectPath)
{
	try
	{
	  // Create the project's solution
	  
	  var strInstallPath  = wizard.FindSymbol('PLUGIN_INSTALL_PATH');
		var strProjTemplate = strInstallPath + '\\Templates\\1033\\plugin.vcproj';

		if(wizard.FindSymbol("CLOSE_SOLUTION"))
		{
		  var Solution = dte.Solution;
			Solution.Close();
			
			var strSolutionName = wizard.FindSymbol("VS_SOLUTION_NAME");
			
			if(strSolutionName.length)
			{
				var strSolutionPath = strProjectPath.substr(0, strProjectPath.length - strProjectName.length);
				
				Solution.Create(strSolutionPath, strSolutionName);
			}
		}

	  // Override the new solution with our custom plugin.vcproj file
	  
		var oTarget = wizard.FindSymbol("TARGET");
		var strProjectNameWithExt = strProjectName + '.vcproj';

		if(wizard.FindSymbol("WIZARD_TYPE") == vsWizardAddSubProject)
		{
			return oTarget.AddFromTemplate(strProjTemplate, strProjectNameWithExt).SubProject;
		}
		
		return oTarget.AddFromTemplate(strProjTemplate, strProjectPath, strProjectNameWithExt);
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		throw ex;
	}
}

///////////////////////////////////////////////////////////////////////////////////////////
// CreateCustomInfFile

function CreateCustomInfFile()
{
	try
	{
		var fso = new ActiveXObject('Scripting.FileSystemObject');
		var TemporaryFolder = 2;
		
		var tfolder = fso.GetSpecialFolder(TemporaryFolder);
		var strTempFolder = tfolder.Drive + '\\' + tfolder.Name;
		var strWizTempFile = strTempFolder + "\\" + fso.GetTempName();

		var strTemplatePath = wizard.FindSymbol('TEMPLATES_PATH');
		var strInfFile = strTemplatePath + '\\Templates.inf';
		
		wizard.RenderTemplate(strInfFile, strWizTempFile);

		return fso.GetFile(strWizTempFile);
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		throw ex;
	}
}

///////////////////////////////////////////////////////////////////////////////////////////
// DelFile

function DelFile(fso, strWizTempFile)
{
	try
	{
	  // Delete the file
	  
		if(fso.FileExists(strWizTempFile))
		{
			var tmpFile = fso.GetFile(strWizTempFile);
			
			tmpFile.Delete();
		}
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		throw ex;
	}
}

///////////////////////////////////////////////////////////////////////////////////////////
// AddFilters

function AddFilters(proj)
{
  // Our plugin.vcproj has filters
}

///////////////////////////////////////////////////////////////////////////////////////////
// AddConfig

function AddConfig(proj, strProjectName)
{
	try
	{
	  // Get the plugin dll name
	  
		var plugin_dll = wizard.FindSymbol('PLUGIN_DLL');
		
		if(plugin_dll.indexOf(".dll") < 0)
		{
		  plugin_dll = plugin_dll + ".dll";
		}
		
		var project_exports = strProjectName.toUpperCase() + "_EXPORTS";
		project_exports = project_exports.replace(/ /g, "_") + ";";
		
		// Debug - Original -> Configuration Properties -> C/C++ -> Preprocessor
		
		var config   = proj.Object.Configurations('Debug - Original');
		var CLTool   = config.Tools('VCCLCompilerTool');
		var LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		// Release - Original -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Release - Original');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		
		// Debug - Dark Messiah -> Configuration Properties -> C/C++ -> Preprocessor
		
		var config   = proj.Object.Configurations('Debug - Dark Messiah');
		var CLTool   = config.Tools('VCCLCompilerTool');
		var LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		// Release - Dark Messiah -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Release - Dark Messiah');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		
		// Debug - Orange Box -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Debug - Orange Box');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		// Release - Orange Box -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Release - Orange Box');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		
		// Debug - Orange Box Valve -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Debug - Orange Box Valve');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		// Release - Orange Box Valve -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Release - Orange Box Valve');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		
		// Debug - Left 4 Dead -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Debug - Left 4 Dead');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		// Release - Left 4 Dead -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Release - Left 4 Dead');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		
		// Debug - Left 4 Dead 2 -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Debug - Left 4 Dead 2');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
		
		// Release - Left 4 Dead 2 -> Configuration Properties -> C/C++ -> Preprocessor
		
		config   = proj.Object.Configurations('Release - Left 4 Dead 2');
		CLTool   = config.Tools('VCCLCompilerTool');
		LinkTool = config.Tools('VCLinkerTool');

		CLTool.PreprocessorDefinitions = project_exports + CLTool.PreprocessorDefinitions;
		LinkTool.OutputFile            = "$(ProjectDir)\\Bin\\$(ConfigurationName)\\" + plugin_dll;
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		throw ex;
	}
}

///////////////////////////////////////////////////////////////////////////////////////////
// PchSettings

function PchSettings(proj)
{
	// TODO: specify pch settings
}

///////////////////////////////////////////////////////////////////////////////////////////
// GetTargetName

function GetTargetName(strName)
{
	try
	{
		var strTarget      = strName;
    var strPluginName  = wizard.FindSymbol('PLUGIN_NAME');
    var strPluginClass = wizard.FindSymbol('PLUGIN_CLASS');
  
    // Target ReadMe File
    
		if(strName == "ReadMe.txt")
		{
			strTarget = "ReadMe.txt";
		}

    // Target StdMMS File
    
		else if(strName == "StdMMS.h")
		{
			strTarget = "StdMMS.h";
		}

    // Target plugin_mm File
    
		else if(strName == "plugin_mm.h")
		{
			strTarget = "Plugin\\" + strPluginClass + ".h";
		}
		
    // Target plugin_mm File
    
		else if(strName == "plugin_mm.cpp")
		{
			strTarget = "Plugin\\" + strPluginClass + ".cpp";
		}

    // Target plugin_hooks File
    
		else if(strName == "plugin_hooks.h")
		{
			strTarget = "Plugin\\" + strPluginClass + "Hooks.h";
		}
		
    // Target plugin_hooks File
    
		else if(strName == "plugin_hooks.cpp")
		{
			strTarget = "Plugin\\" + strPluginClass + "Hooks.cpp";
		}
		
    // Target plugin_engine File
    
		else if(strName == "plugin_engine.h")
		{
			strTarget = "Plugin\\" + strPluginClass + "Engine.h";
		}
		
    // Target plugin_mm File
    
		else if(strName == "plugin.vdf")
		{
		  if(strPluginName.length)
		  {
		    strPluginName = strPluginName.replace(/ /g, "");
		    
		    strTarget = "Bin\\" + strPluginName + ".vdf";
		  }
		}

		return strTarget; 
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		throw ex;
	}
}

///////////////////////////////////////////////////////////////////////////////////////////
// AddFilesToCustomProj

function AddFilesToCustomProj(proj, strProjectName, strProjectPath, InfFile)
{
	try
	{
		var vcProjectItems   = proj.ProjectItems;
		var vcProjectFilters = proj.Object.Filters;

    // Get references to the filters of the project
		
    var vcFilterSource = vcProjectFilters.Item(wizard.FindSymbol('PLUGIN_SOURCE_FILES'));
    var vcFilterHeader = vcProjectFilters.Item(wizard.FindSymbol('PLUGIN_HEADER_FILES'));
    var vcFilterRez    = vcProjectFilters.Item(wizard.FindSymbol('PLUGIN_RESOURCE_FILES'));

		var strTemplatePath = wizard.FindSymbol('PLUGIN_INSTALL_PATH') + "Templates\\1033\\";
		var FileStream      = InfFile.OpenAsTextStream(1, -2);

    // Process the template files
    
		while(!FileStream.AtEndOfStream)
		{
			var strLine = FileStream.ReadLine();
			
			if(strLine == '')
			{
			  continue;
			}
			
			// Retrieve the file parameters
			
			var strName         = strLine;
			var strTargetFile   = GetTargetName(strName);
			var strTemplateFile = strTemplatePath + '\\' + strLine;
			var strProjectFile  = strProjectPath + '\\' + strTargetFile;

      // "true" will only copy the file from template_file
      // to target_file without rendering / adding to the project
      
			var bCopyOnly = false;
			var strExt    = strName.substr(strName.lastIndexOf("."));
			
			if(strExt == ".bmp" ||
			   strExt == ".ico" || 
			   strExt == ".gif" ||
			   strExt == ".rtf" ||
			   strExt == ".css")
			{
				bCopyOnly = true;
		  }
				
			wizard.RenderTemplate(strTemplateFile, strProjectFile, bCopyOnly);

			if(!KeepOutsideProject(strName))
			{
			  // Add source files to the 'Plugin Source Files' filter
			  
		    if(strExt == '.cpp' ||
		       strExt == '.cxx' ||
		       strExt == '.c')
		    {
		      vcFilterSource.AddFile(strTargetFile);
		    }
		    
			  // Add header files to the 'Plugin Header Files' filter
			  
		    else if(strExt == '.hpp' ||
		            strExt == '.h')
		    {
		      vcFilterHeader.AddFile(strTargetFile);
		    }
		    
			  // Add resource files to the 'Resource Files' filter
			  
		    else if(strExt == '.rc2' || strExt == '.ico' ||
		            strExt == '.gif' || strExt == '.bmp' ||
		            strExt == '.vdf' || strExt == '.rc')
		    {
		      vcFilterRez.AddFile(strTargetFile);
		    }
		    
		    // Just add the rest to the project
		    
		    else 
		    {
		      proj.Object.AddFile(strTargetFile);	
		    }	
			}
		}
		
		FileStream.Close();
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		throw ex;
	}
}

///////////////////////////////////////////////////////////////////////////////////////////
// KeepOutsideProject

function KeepOutsideProject(strName)
{
	try
	{
    var bAdd = false;

    if(strName == "ReadMe.txt")
    {
      bAdd = true;
    } 
    
    return bAdd;
	}
	
	// Catch Exceptions
	
	catch(ex)
	{
		throw ex;
	}
}
