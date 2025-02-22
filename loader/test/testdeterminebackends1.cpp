#include <iostream>
#include <map>
#include <string>
#ifdef _LINUX
#include <unistd.h>
#elif _WIN32
#include <windows.h>
#endif
#include "loader.h"
#include "utility.h"

#ifdef _WIN64
bool g_64Bit = true;
#else
bool g_64Bit = false;
#endif

struct GameAttributes
{
	// none of these will really be PLATFORM_MAX_PATH but they're paths, so make them long enough
	char gamePath[PLATFORM_MAX_PATH];
	char gameDirectory[PLATFORM_MAX_PATH];
	char binPath[PLATFORM_MAX_PATH];
	MetamodBackend expectedBackend;
	bool is64Bit;
};

std::string GetSteamInstallationPath()
{
#ifdef _WIN32
	char pathBuffer[512];
	HKEY hKey;
	ULONG statusCode;
	DWORD bufferSize = sizeof(pathBuffer);

	LONG result = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Wow6432Node\\Valve\\Steam", 0, KEY_READ, &hKey);

	// if it's not find, try the 32-bit path
	if (result == ERROR_FILE_NOT_FOUND)
	{
		result = RegOpenKeyExA(HKEY_LOCAL_MACHINE, "SOFTWARE\\Valve\\Steam", 0, KEY_READ, &hKey);
	}

	statusCode = RegQueryValueExA(hKey, "InstallPath", 0, NULL, (LPBYTE)pathBuffer, &bufferSize);

	if (statusCode == ERROR_SUCCESS)
	{
		return pathBuffer;
	}
	else
	{
		std::cout << "Error: RegQueryValueExA returned " << statusCode << std::endl;

		return "";
	}
#else
	return "";
#endif
}

bool TestDetermineBackendS1(std::string &error)
{
	std::string steamInstallationPath = GetSteamInstallationPath();
	std::map<std::string, GameAttributes> games;
	char errorBuffer[128];
	char currentDirectoryBuffer[PLATFORM_MAX_PATH];
	char directoryBuffer[PLATFORM_MAX_PATH];
	void *serverLib = nullptr;
	void *engineLib = nullptr;
	QueryValveInterface serverQvi;
	QueryValveInterface engineQvi;

	if (steamInstallationPath == "")
	{
		std::cout << "Warning: exiting early as Steam installation not found" << std::endl;

		return true;
	}

	// TODO: when C++20 is supported, use designated initialisers
	games["Black Mesa"] = {
		/*.gamePath = */"steamapps\\common\\Black Mesa\\",
		/*.gameDirectory = */"bms",
		/*.binPath = */"bin",
		/*.expectedBackend = */MMBackend_BMS,
		/*.is64Bit = */false
	};
	games["Bloody Good Time"] = {
		/*.gamePath = */"steamapps\\common\\Bloody Good Time\\",
		/*.gameDirectory = */"pm",
		/*.binPath = */"bin",
		/*.expectedBackend = */MMBackend_BloodyGoodTime,
		/*.is64Bit = */false
	};
	games["Bloody Good Time Dedicated Server"] = {
		/*.gamePath = */"steamapps\\common\\Bloody Good Time Dedicated Server\\",
		/*.gameDirectory = */"pm",
		/*.binPath = */"bin",
		/*.expectedBackend = */MMBackend_BloodyGoodTime,
		/*.is64Bit = */false
	};
	games["Counter-Strike: Source"] = {
		/*.gamePath = */"steamapps\\common\\Counter-Strike Source\\",
		/*.gameDirectory = */"cstrike",
		/*.binPath = */"bin\\x64",
		/*.expectedBackend = */MMBackend_CSS,
		/*.is64Bit = */true
	};
	games["Counter-Strike: Global Offensive"] = {
		/*.gamePath = */"steamapps\\common\\Counter-Strike Global Offensive\\",
		/*.gameDirectory = */"csgo",
		/*.binPath = */"bin",
		/*.expectedBackend = */MMBackend_CSGO,
		/*.is64Bit = */false
	};
	games["Half-Life 2: Deathmatch"] = {
		/*.gamePath = */"steamapps\\common\\Half-Life 2 Deathmatch\\",
		/*.gameDirectory = */"hl2mp",
		/*.binPath = */"bin\\x64",
		/*.expectedBackend = */MMBackend_HL2DM,
		/*.is64Bit = */true
	};
	games["Military Conflict: Vietnam"] = {
		/*.gamePath = */"steamapps\\common\\Military Conflict - Vietnam\\",
		/*.gameDirectory = */"vietnam",
		/*.binPath = */"bin\\win64",
		/*.expectedBackend = */MMBackend_MCV,
		/*.is64Bit = */true
	};
	games["Portal 2"] = {
		/*.gamePath = */"steamapps\\common\\Portal 2\\",
		/*.gameDirectory = */"portal2",
		/*.binPath = */"bin",
		/*.expectedBackend = */MMBackend_Portal2,
		/*.is64Bit = */false
	};
	// TODO: skip for now as sourcetest is still 32-bit but the engine is 64-bit
	// games["Source SDK Base 2013 Multiplayer"] = {
	// 	/*.gamePath = */"steamapps\\common\\Source SDK Base 2013 Multiplayer\\",
	// 	/*.gameDirectory = */"sourcetest",
	// 	/*.binPath = */"bin",
	// 	/*.expectedBackend = */MMBackend_HL2DM,
	// 	/*.is64Bit = */true
	// };
	games["Team Fortress 2"] = {
		/*.gamePath = */"steamapps\\common\\Team Fortress 2\\",
		/*.gameDirectory = */"tf",
		/*.binPath = */"bin\\x64",
		/*.expectedBackend = */MMBackend_TF2,
		/*.is64Bit = */true
	};

#ifdef _LINUX
	getcwd(currentDirectoryBuffer, PLATFORM_MAX_PATH);
#elif _WIN32
	GetCurrentDirectory(PLATFORM_MAX_PATH, currentDirectoryBuffer);
#endif

	for (const auto& [gameName, gameAttributes] : games)
	{
		std::cout << "Checking " << gameName << std::endl;

		if (gameAttributes.is64Bit && !g_64Bit)
		{
			std::cout << "Skipping in 32-bit build" << std::endl << std::endl;
			continue;
		}
		else if (!gameAttributes.is64Bit && g_64Bit)
		{
			std::cout << "Skipping in 64-bit build" << std::endl << std::endl;
			continue;
		}

		sprintf(directoryBuffer, "%s\\%s\\%s\\", steamInstallationPath.c_str(), gameAttributes.gamePath, gameAttributes.binPath);
#ifdef _LINUX
		chdir(directoryBuffer);
#elif _WIN32
		SetCurrentDirectory(directoryBuffer);
#endif

		// get the server library
		sprintf(directoryBuffer, "%s\\%s\\%s\\%s\\server.dll", steamInstallationPath.c_str(), gameAttributes.gamePath, gameAttributes.gameDirectory, gameAttributes.binPath);
		serverLib = mm_LoadLibrary(directoryBuffer, errorBuffer, 128);

		if (serverLib == nullptr)
		{
			std::cout << "Error: mm_LoadLibrary server library " << errorBuffer << std::endl;
			continue;
		}

		// get the engine library
		sprintf(directoryBuffer, "%s\\%s\\%s\\engine.dll", steamInstallationPath.c_str(), gameAttributes.gamePath, gameAttributes.binPath);
		engineLib = mm_LoadLibrary(directoryBuffer, errorBuffer, 128);

		if (engineLib == nullptr)
		{
			std::cout << "Error: mm_LoadLibrary engine library " << errorBuffer << std::endl;
			continue;
		}

		serverQvi = (QueryValveInterface)mm_GetLibAddress(serverLib, "CreateInterface");

		if (serverQvi == nullptr)
		{
			std::cout << "Error: Failed to find CreateInterface in server" << std::endl;
			continue;
		}

		engineQvi = (QueryValveInterface)mm_GetLibAddress(engineLib, "CreateInterface");

		if (engineQvi == nullptr)
		{
			std::cout << "Error: Failed to find CreateInterface in engine" << std::endl;
			continue;
		}

		MetamodBackend actualBackend = mm_DetermineBackendS1(engineQvi, serverQvi, gameAttributes.gameDirectory);

		if (actualBackend != gameAttributes.expectedBackend)
		{
			std::cout << "Got MetamodBackend " << actualBackend << " instead of expected " << gameAttributes.expectedBackend << std::endl << std::endl;
		}
		else
		{
			std::cout << "Got MetamodBackend " << gameAttributes.expectedBackend << std::endl << std::endl;
		}
	}

#ifdef _LINUX
	chdir(currentDirectoryBuffer);
#elif _WIN32
	SetCurrentDirectory(directoryBuffer);
#endif

	return true;
}
