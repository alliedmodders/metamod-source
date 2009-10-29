// Hello BAIL!
//   hello pm how are you
//    I'm fine, what about you?
//     not bad, just looking for mem leaks
//      mem leaks in my code!? never! I have to preserve binary compatibility :(
// This is a test file

#include <stdio.h>
#include <iostream>
#include <string>
#include <stdlib.h>
#include <time.h>
#include "sh_tinyhash.h"
#include "sh_list.h"
#include "sourcehook_impl.h"
#include "sourcehook.h"

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
DO_TEST(Reentr);
DO_TEST(Manual);
DO_TEST(Recall);
DO_TEST(Multi);
DO_TEST(Ref);
DO_TEST(RefRet);
DO_TEST(VPHooks);

int main(int argc, char *argv[])
{
	std::string error;

	g_Verbose = argc > 1 && strcmp(argv[1], "-v") == 0;

	Test::DoTests();

	cout << "Press enter to continue" << endl;

	char x;
	cin.read(&x, 1);
}

SourceHook::ISourceHook *Test_Factory()
{
	return new SourceHook::CSourceHookImpl();
}

void Test_Delete(SourceHook::ISourceHook *shptr)
{
	delete static_cast<SourceHook::CSourceHookImpl *>(shptr);
}

void Test_CompleteShutdown(SourceHook::ISourceHook *shptr)
{
	static_cast<SourceHook::CSourceHookImpl *>(shptr)->CompleteShutdown();
}

bool Test_IsPluginInUse(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	return static_cast<SourceHook::CSourceHookImpl *>(shptr)->IsPluginInUse(plug);
}

void Test_UnloadPlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	static_cast<SourceHook::CSourceHookImpl *>(shptr)->UnloadPlugin(plug);
}

void Test_PausePlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	static_cast<SourceHook::CSourceHookImpl *>(shptr)->PausePlugin(plug);
}

void Test_UnpausePlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	static_cast<SourceHook::CSourceHookImpl *>(shptr)->UnpausePlugin(plug);
}
