#include <stdio.h>
#include <iostream>
#include <string>
#include <stdlib.h>
#include <string.h>
#include <time.h>

using namespace std;
bool g_Verbose;

#define DECL_TEST(x) bool Test##x(std::string &error);

#define DO_TEST(x) \
	error.clear(); \
	if (Test##x(error)) \
	{ \
		++passed; \
		cout << "Test" << #x << " passed" << endl; \
	} \
	else \
	{ \
		++failed; \
		cout << "Test" << #x << " FAILED: " << error << endl; \
	} \


DECL_TEST(DetermineBackendS1);

int main(int argc, char *argv[])
{
	std::string error;

	g_Verbose = argc > 1 && strcmp(argv[1], "-v") == 0;

	int passed = 0, failed = 0;

	DO_TEST(DetermineBackendS1);

	cout << endl << "----" << endl << "Passed: " << passed << endl << "Failed: " << failed << endl;
	cout << "Total: " << passed + failed << endl;

	if (failed)
		return 1;
	return 0;
}
