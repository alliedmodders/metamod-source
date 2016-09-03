#pragma once

#include "detourhook.h"
#include <sourcehook.h>
#include <sh_memory.h>

namespace DetourHook
{
	class CDetourHook
	{
	public:
		CDetourHook(void *addr, void *callback);
		~CDetourHook();
		void EnableDetour();
		void DisableDetour();
		void *GetTrampoline();
	private:
		void *pTrampoline;
		void *pFunc;
		void *pCallback;
		void *pOrginalBytes;
		int   bytes;
		bool  bEnabled;
	};

	class CDetourHookImpl : public IDetourHook
	{
		int GetIfaceVersion();
		CDetourHook *CreateDetour(void *addr, void *callback);
		void *GetTrampoline(CDetourHook *det);
		void EnableDetour(CDetourHook *det);
		void DisableDetour(CDetourHook *det);
		void DeleteDetour(CDetourHook *det);
	};
}

#define OP_JMP_SIZE 5

inline void inject_abs_jmp(unsigned char *target, void *dest)
{
	SourceHook::SetMemAccess(target, OP_JMP_SIZE + 1, SH_MEM_READ | SH_MEM_WRITE | SH_MEM_EXEC);

	target[0] = 0xFF;	/* JMP */
	target[1] = 0x25;	/* MEM32 */
	*(void **)(&target[2]) = dest;
}
