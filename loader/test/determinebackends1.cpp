#include <iostream>
#include <string>
#include "loader.h"
#include "utility.h"

#if defined _WIN32
#define SERVER_LIB_NAME "server.dll"
#define ENGINE_LIB_NAME "engine.dll"
#elif defined __linux__
#define SERVER_LIB_NAME "server" LIB_SUFFIX
#define ENGINE_LIB_NAME "engine" LIB_SUFFIX
#endif

bool DetermineBackendS1(const char *gameDir, int expectedBackend)
{
	char errorBuffer[128];
	void *serverLib = nullptr;
	void *engineLib = nullptr;
	QueryValveInterface serverQvi;
	QueryValveInterface engineQvi;

	std::cout << "Checking mm_DetermineBackendS1 with game directory " << gameDir << std::endl;

	// load the game code
	serverLib = mm_LoadLibrary(SERVER_LIB_NAME, errorBuffer, 128);

	if (serverLib == nullptr)
	{
		std::cout << "Error: mm_LoadLibrary server library " << SERVER_LIB_NAME << " " << errorBuffer << std::endl;
		return false;
	}

	serverQvi = (QueryValveInterface)mm_GetLibAddress(serverLib, "CreateInterface");

	if (serverQvi == nullptr)
	{
		std::cout << "Error: Failed to find CreateInterface in server" << std::endl;
		return false;
	}

	// load the engine code
	engineLib = mm_LoadLibrary(ENGINE_LIB_NAME, errorBuffer, 128);

	if (engineLib == nullptr)
	{
		std::cout << "Error: mm_LoadLibrary engine library " << ENGINE_LIB_NAME << " "  << errorBuffer << std::endl;
		return false;
	}

	engineQvi = (QueryValveInterface)mm_GetLibAddress(engineLib, "CreateInterface");

	if (engineQvi == nullptr)
	{
		std::cout << "Error: Failed to find CreateInterface in engine" << std::endl;
		return false;
	}

	MetamodBackend actualBackend = mm_DetermineBackendS1(engineQvi, serverQvi, gameDir);

	mm_UnloadLibrary(engineLib);
	mm_UnloadLibrary(serverLib);

	if (actualBackend != expectedBackend)
	{
		std::cout << "Got MetamodBackend " << actualBackend << " instead of expected " << expectedBackend << std::endl;
		return false;
	}

	std::cout << "Got MetamodBackend " << expectedBackend << " as expected " << std::endl;
	return true;
}
