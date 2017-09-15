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

#include "sourcehook_impl.h"
#include "sourcehook.h"
#include "sourcehook_hookmangen.h"

using namespace std;
bool g_Verbose;

struct Unloader : public SourceHook::Impl::UnloadListener
{
	void ReadyToUnload(SourceHook::Plugin) { }
} g_UnloadListener;

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


DECL_TEST(List);
DECL_TEST(Basic);
DECL_TEST(VafmtAndOverload);
DECL_TEST(ThisPtrOffs);
DECL_TEST(PlugSys);
DECL_TEST(Bail);
DECL_TEST(Reentr);
DECL_TEST(Manual);
DECL_TEST(Recall);
DECL_TEST(Multi);
DECL_TEST(Ref);
DECL_TEST(RefRet);
DECL_TEST(VPHooks);
DECL_TEST(CPageAlloc);					// in testhookmangen.cpp
DECL_TEST(HookManGen);
DECL_TEST(OddThunks);

int main(int argc, char *argv[])
{
	std::string error;

	g_Verbose = argc > 1 && strcmp(argv[1], "-v") == 0;

	int passed = 0, failed = 0;

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
	DO_TEST(CPageAlloc);
#if !defined( _M_AMD64 ) && !defined( __amd64__ ) && !defined(__x86_64__)	// TODO: Fix for 64-bit
	DO_TEST(HookManGen);
#endif
	DO_TEST(OddThunks);

	cout << endl << "----" << endl << "Passed: " << passed << endl << "Failed: " << failed << endl;
	cout << "Total: " << passed + failed << endl;

	if (failed)
		return 1;
	return 0;
}

SourceHook::ISourceHook *Test_Factory()
{
	return new SourceHook::Impl::CSourceHookImpl();
}

void Test_Delete(SourceHook::ISourceHook *shptr)
{
	delete static_cast<SourceHook::Impl::CSourceHookImpl *>(shptr);
}

void Test_CompleteShutdown(SourceHook::ISourceHook *shptr)
{
	static_cast<SourceHook::Impl::CSourceHookImpl *>(shptr)->CompleteShutdown();
}

class Listener : public SourceHook::Impl::UnloadListener
{
public:
	void ReadyToUnload(SourceHook::Plugin plug) override {
	}
} sListener;

void Test_UnloadPlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	static_cast<SourceHook::Impl::CSourceHookImpl *>(shptr)->UnloadPlugin(plug, &sListener);
}

void Test_PausePlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	static_cast<SourceHook::Impl::CSourceHookImpl *>(shptr)->PausePlugin(plug);
}

void Test_UnpausePlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug)
{
	static_cast<SourceHook::Impl::CSourceHookImpl *>(shptr)->UnpausePlugin(plug);
}

#if !defined( _M_AMD64 ) && !defined( __amd64__ ) && !defined(__x86_64__)
SourceHook::IHookManagerAutoGen *Test_HMAG_Factory(SourceHook::ISourceHook *shptr)
{
	return new SourceHook::Impl::CHookManagerAutoGen(shptr);
}

void Test_HMAG_Delete(SourceHook::IHookManagerAutoGen *ptr)
{
	delete static_cast<SourceHook::Impl::CHookManagerAutoGen*>(ptr);
}
#endif

