// Hello BAIL!
//   hello pm how are you
// This is a test file

#include <stdio.h>
#include <iostream>
#include <string>
#include <list>
#include <stdlib.h>
#include <time.h>
#include "sh_tinyhash.h"
#include "sourcehook_impl.h"

using namespace std;

bool g_Verbose;

class Test
{
	typedef bool (*TestProto)(std::string&);
	TestProto m_Func;
	std::string m_Name;

	static std::list<Test *> ms_Tests;
public:
	Test(TestProto func, const char *name) : m_Func(func), m_Name(name)
	{
		ms_Tests.push_back(this);
	}

	bool operator()()
	{
		std::string error;
		if (!m_Func(error))
		{
			cout << "Test" << m_Name << " FAILED: " << error << endl;
			return false;
		}
		else
		{
			cout << "Test" << m_Name << " passed" << endl;
			return true;
		}
	}

	static void DoTests()
	{
		int passed=0, failed=0;
		for (std::list<Test*>::iterator iter = ms_Tests.begin(); iter != ms_Tests.end(); ++iter)
		{
			if ((**iter)())
				++passed;
			else
				++failed;
		}
		cout << endl << "----" << endl << "Passed: " << passed << endl << "Failed: " << failed << endl;
		cout << "Total: " << passed + failed << endl;
	}
};

std::list<Test *> Test::ms_Tests;

#define DO_TEST(x) \
	bool Test##x(std::string &error); \
	Test g_Test##x(Test##x, #x);

DO_TEST(Basic);
DO_TEST(VafmtAndOverload);
DO_TEST(ThisPtrOffs);
DO_TEST(PlugSys);
DO_TEST(Bail);

template <>
int SourceHook::HashFunction<int>(const int & k)
{
	return k;
}

template <>
int SourceHook::Compare<int>(const int & v1, const int & v2)
{
	if (v1 == v2)
		return 0;
	if (v1 < v2)
		return -1;
	if (v1 > v2)
		return 1;
	return 0;
}

int main(int argc, char *argv[])
{
	std::string error;
	int passed=0, failed=0;

	g_Verbose = argc > 1 && strcmp(argv[1], "-v") == 0;

	Test::DoTests();

	cout << "Press enter to continue" << endl;

	char x;
	cin.read(&x, 1);
}

