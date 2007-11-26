// This file is used for backwards compatibility testing
// It allows us to test binary backwards compatibility by using an older include file HERE:
#include "sourcehook.h"			// <-- here
// There. main.cpp which implements all of the following function is always usign sourcehook.h
// and the up-to-date sourcehook_impl.h/sourcehook.cpp. The tests use this file however.
// If the test needs an up-to-date version (like the recall test), it can simply
// #include "sourcehook.h" before including this, thus overriding our decision.


SourceHook::ISourceHook *Test_Factory();
void Test_Delete(SourceHook::ISourceHook *shptr);

template <class T>
struct CAutoPtrDestruction
{
	T *m_Ptr;
	CAutoPtrDestruction(T *p) : m_Ptr(p) { }
	~CAutoPtrDestruction() { if (m_Ptr) delete m_Ptr; }
	void clear() { m_Ptr = NULL; }
	void set(T *ptr) { m_Ptr = ptr; }
};

struct CSHPtrAutoDestruction
{
	SourceHook::ISourceHook *m_SHPtr;
	CSHPtrAutoDestruction(SourceHook::ISourceHook *shptr) : m_SHPtr(shptr) {}
	~CSHPtrAutoDestruction() { Test_Delete(m_SHPtr); }
};

#define GET_SHPTR(var) var = Test_Factory(); CSHPtrAutoDestruction ___autodestruction(var);

// Access to CSourceHookImpl functions
void Test_CompleteShutdown(SourceHook::ISourceHook *shptr);
void Test_UnloadPlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug);
void Test_PausePlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug);
void Test_UnpausePlugin(SourceHook::ISourceHook *shptr, SourceHook::Plugin plug);

SourceHook::IHookManagerAutoGen *Test_HMAG_Factory(SourceHook::ISourceHook *pSHPtr);
void Test_HMAG_Delete(SourceHook::IHookManagerAutoGen *ptr);

struct CHMAGAutoDestruction
{
	SourceHook::IHookManagerAutoGen *m_Ptr;
	CHMAGAutoDestruction(SourceHook::IHookManagerAutoGen *ptr) : m_Ptr(ptr) {}
	~CHMAGAutoDestruction() { Test_HMAG_Delete(m_Ptr); }
};


#define GET_HMAG(var, shptr) var = Test_HMAG_Factory(shptr); CHMAGAutoDestruction __hmagautodestruction(var);
