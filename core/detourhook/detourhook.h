#pragma once

#ifndef DH_GLOB_DHPTR
#define DH_GLOB_DHPTR g_DHPtr
#endif

#define DH_STATIC_DETOURHOOK(name, ret, ...) typedef ret(*name##FN)(__VA_ARGS__);

#define DH_STATIC_CALL(name, detourhook) ((name##FN)g_DHPtr->GetTrampoline(detourhook))

#define DH_MEMBER_DETOURHOOK(name, ret, ...) typedef ret(__thiscall *name##ThisFn)(void *thisptr, __VA_ARGS__);

#define DH_MEMBER_CALL(name, detourhook) ((name##ThisFn)g_DHPtr->GetTrampoline(detourhook))

class GenericClass {};
typedef void (GenericClass::*VoidFunc)();

inline void *GetCodeAddr(VoidFunc mfp)
{
	return *(void **)&mfp;
}

/**
* Converts a member function pointer to a void pointer.
* This relies on the assumption that the code address lies at mfp+0
* This is the case for both g++ and later MSVC versions on non virtual functions but may be different for other compilers
* Based on research by Don Clugston : http://www.codeproject.com/cpp/FastDelegate.asp
*/
#define GetCodeAddress(mfp) GetCodeAddr(reinterpret_cast<VoidFunc>(mfp))

namespace DetourHook
{
	class CDetourHook;

	class IDetourHook
	{
	public:
		virtual int GetIfaceVersion() = 0;
		virtual CDetourHook *CreateDetour(void *addr, void *callback) = 0;
		virtual void *GetTrampoline(CDetourHook *det) = 0;
		virtual void EnableDetour(CDetourHook *det) = 0;
		virtual void DisableDetour(CDetourHook *det) = 0;
		virtual void DeleteDetour(CDetourHook *det) = 0;
	};
};
