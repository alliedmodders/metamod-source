#include "detourhook_impl.h"
#include <metamod.h>

#include "asm\asm.h"

using namespace DetourHook;

int CDetourHookImpl::GetIfaceVersion()
{
	return 1;
}

CDetourHook::CDetourHook(void *addr, void *callback)
{
	this->pFunc = addr;
	this->pCallback = callback;
	this->bEnabled = false;

	this->bytes = copy_bytes((unsigned char *)this->pFunc, NULL, OP_JMP_SIZE + 1);
	this->pOrginalBytes = malloc(this->bytes);

	SourceHook::SetMemAccess(this->pFunc, this->bytes, SH_MEM_READ | SH_MEM_WRITE | SH_MEM_EXEC);
	memcpy(this->pOrginalBytes, this->pFunc, this->bytes);

	this->pTrampoline = malloc(this->bytes + OP_JMP_SIZE); //Allocate space

	SourceHook::SetMemAccess(this->pTrampoline, bytes + OP_JMP_SIZE, SH_MEM_READ | SH_MEM_WRITE | SH_MEM_EXEC);

	//Copy our original bytes to our trampoline
	//We use this to fix JMP's and whatever else is relative to the function.
	copy_bytes((unsigned char *)this->pFunc, (unsigned char *)this->pTrampoline, this->bytes);

	//JMP from trampoline +bytes to function + bytes
	inject_jmp((unsigned char*)this->pTrampoline + this->bytes, (unsigned char*)this->pFunc + this->bytes);
}

CDetourHook::~CDetourHook()
{
	this->DisableDetour();

	//Clean up
	free(this->pTrampoline);
	free(this->pOrginalBytes);
}

void *CDetourHook::GetTrampoline()
{
	return this->pTrampoline;
}

void CDetourHook::DisableDetour()
{
	if (this->bEnabled)
	{
		this->bEnabled = false;
		memcpy(this->pFunc, this->pOrginalBytes, this->bytes);
	}
}

void CDetourHook::EnableDetour()
{
	if (!this->bEnabled)
	{
		//Nop all the bytes needed
		fill_nop(this->pFunc, bytes);

		//JMP from function to our callback
		inject_abs_jmp((unsigned char *)this->pFunc, &this->pCallback);

		this->bEnabled = true;
	}
}

CDetourHook *CDetourHookImpl::CreateDetour(void *addr, void *callback)
{
	CDetourHook *ret = new CDetourHook(addr, callback);
	return ret;
}

void *CDetourHookImpl::GetTrampoline(CDetourHook *det)
{
	return det->GetTrampoline();
}

void CDetourHookImpl::EnableDetour(CDetourHook *det)
{
	det->EnableDetour();
}

void CDetourHookImpl::DisableDetour(CDetourHook *det)
{
	det->DisableDetour();
}

void CDetourHookImpl::DeleteDetour(CDetourHook *det)
{
	delete det;
}
