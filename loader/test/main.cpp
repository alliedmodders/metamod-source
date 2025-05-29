#include <stdio.h>
#include <iostream>
#include <string>
#include <stdlib.h>
#include <string.h>

#define DECL_TEST(x) bool Test##x(std::string &error);

#define DO_TEST(x) \
	error.clear(); \
	if (Test##x(error)) \
	{ \
		++passed; \
		std::cout << "Test" << #x << " passed" << std::endl; \
	} \
	else \
	{ \
		++failed; \
		std::cout << "Test" << #x << " FAILED: " << error << std::endl; \
	} \


bool DetermineBackendS1(const char *gameDir, int expectedBackend);
DECL_TEST(TrimComments);

int main(int argc, char *argv[])
{
	bool testDetermineBackendS1 = false;
	char *gameDirectory = NULL;
	int expectedMetamodBackend = -1;

	for (int i = 0; i < argc; i++)
	{
		if (!strcmp(argv[i], "-testdbs1"))
		{
			testDetermineBackendS1 = true;
		}
		if (!strcmp(argv[i], "-gamedir") && argc > i + 1)
		{
			gameDirectory = argv[i + 1];
		}
		if (!strcmp(argv[i], "-expectedbackend") && argc > i + 1)
		{
			expectedMetamodBackend = atoi(argv[i + 1]);
		}
	}

	if (testDetermineBackendS1)
	{
		bool success = DetermineBackendS1(gameDirectory ? gameDirectory : "", expectedMetamodBackend);
		// invert it because return code from main has 0 as success
		return success != true;
	}

	std::string error;
	int passed = 0, failed = 0;

	DO_TEST(TrimComments);

	std::cout << std::endl << "----" << std::endl << "Passed: " << passed << std::endl << "Failed: " << failed << std::endl;
	std::cout << "Total: " << passed + failed << std::endl;

	if (failed)
		return 1;
	return 0;
}
