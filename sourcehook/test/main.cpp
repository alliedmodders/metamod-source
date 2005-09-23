// Hello BAIL!
//   hello pm how are you
//    I'm fine, what about you?
// This is a test file

#include <stdio.h>
#include <iostream>
#include <string>
#include <stdlib.h>
#include <time.h>
#include "sh_tinyhash.h"
#include "sh_list.h"
#include "sourcehook_impl.h"

using namespace std;

bool g_Verbose;

class Test
{
	typedef bool (*TestProto)(std::string&);
	TestProto m_Func;
	std::string m_Name;

	static SourceHook::List<Test *> ms_Tests;
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
		for (SourceHook::List<Test*>::iterator iter = ms_Tests.begin(); iter != ms_Tests.end(); ++iter)
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

SourceHook::List<Test *> Test::ms_Tests;

#define DO_TEST(x) \
	bool Test##x(std::string &error); \
	Test g_Test##x(Test##x, #x);

DO_TEST(List);
DO_TEST(Basic);
DO_TEST(VafmtAndOverload);
DO_TEST(ThisPtrOffs);
DO_TEST(PlugSys);
DO_TEST(Bail);

int main(int argc, char *argv[])
{
	std::string error;

	g_Verbose = argc > 1 && strcmp(argv[1], "-v") == 0;

	Test::DoTests();

	cout << "Press enter to continue" << endl;

	char x;
	cin.read(&x, 1);
}

