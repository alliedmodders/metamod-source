/**
 * vim: set ts=4 :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2023 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from
 * the use of this software.
 *
 * Permission is granted to anyone to use this software for any purpose,
 * including commercial applications, and to alter it and redistribute it
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not
 * claim that you wrote the original software. If you use this software in a
 * product, an acknowledgment in the product documentation would be
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 *
 * Version: $Id$
 */

#ifndef _INCLUDE_METAMOD_SOURCE_SOURCE2_PROVIDER_H_
#define _INCLUDE_METAMOD_SOURCE_SOURCE2_PROVIDER_H_

#include <string>
#include <vector>

#include "../provider_base.h"
#include "tier1/utlvector.h"
#include "IEngineService.h"

// TODO: is this still needed for Dota or CS2 on any platform?
#if SOURCE_ENGINE == SE_DOTA && defined( _WIN32 )
#define SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
#endif

class INetworkGameClient;
class ISource2WorldSession;

class Source2Provider : public BaseProvider
{
public:
	virtual void Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, CreateInterfaceFn serverFactory) override;
	virtual void Notify_DLLShutdown_Pre() override;
	virtual bool ProcessVDF(const char* file, char path[], size_t path_len, char alias[], size_t alias_len) override;
	virtual int DetermineSourceEngine() override;
	virtual const char* GetEngineDescription() const override;
	virtual void GetGamePath(char* pszBuffer, int len) override;
	virtual const char* GetGameDescription() override;
	virtual const char* GetCommandLineValue(const char* key, const char* defval) override;
	virtual void ConsolePrint(const char* msg) override;
	virtual void ClientConsolePrint(MMSPlayer_t player, const char* msg) override;
	virtual void ServerCommand(const char* cmd) override;
	virtual MetamodSourceConVar *CreateConVar(const char* name,
		const char* defval,
		const char* help,
		int flags) override;
	virtual const char* GetConVarString(MetamodSourceConVar *convar) override;
	virtual void SetConVarString(MetamodSourceConVar *convar, const char* str) override;
	virtual bool RegisterConCommand(ProviderConCommand *pCommand) override;
	virtual bool RegisterConVar(ProviderConVar *pVar) override;
	virtual void UnregisterConCommand(ProviderConCommand *pCommand) override;
	virtual void UnregisterConVar(ProviderConVar *pVar) override;
	virtual bool IsConCommandBaseACommand(ConCommandBase* pCommand) override;
public:
#ifdef SHOULD_OVERRIDE_ALLOWDEDICATED_SERVER
	bool Hook_AllowDedicatedServers(EUniverse universe) const;
#endif
	void Hook_RegisterLoopMode(const char* pszLoopModeName, ILoopModeFactory *pLoopModeFactory, void **ppGlobalPointer);
	void Hook_UnregisterLoopMode(const char* pszLoopModeName, ILoopModeFactory* pLoopModeFactory, void** ppGlobalPointer);
	ILoopMode *Hook_CreateLoopModePost();
	void Hook_DestroyLoopMode(ILoopMode*);
	bool Hook_LoopInitPost(KeyValues* pKeyValues, ILoopModePrerequisiteRegistry *pRegistry);
	void Hook_LoopShutdownPost();
	void Hook_ClientCommand(CPlayerSlot nSlot, const CCommand& args);
private:
	IFileSystem* baseFs = nullptr;
	std::vector<CConVar<CUtlString> *> m_RegisteredConVars;

	friend void LocalCommand_Meta(const CCommandContext& context, const CCommand& args);
};

#endif
