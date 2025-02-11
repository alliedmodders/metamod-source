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

#ifndef _INCLUDE_METAMOD_SOURCE_SOURCE1_PROVIDER_H_
#define _INCLUDE_METAMOD_SOURCE_SOURCE1_PROVIDER_H_

#include "../provider_base.h"
#include <string>
#include <vector>
#include <sh_list.h>

class IFileSystem;

class SourceProvider : public BaseProvider
{
public: // BaseProvider
	virtual void Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, CreateInterfaceFn serverFactory) override;
	virtual void Notify_DLLShutdown_Pre() override;
	virtual bool ProcessVDF(const char* file, char path[], size_t path_len, char alias[], size_t alias_len) override;
	virtual int DetermineSourceEngine() override;
	virtual const char* GetEngineDescription() const override;
	virtual void GetGamePath(char* pszBuffer, int len) override;
	virtual const char* GetGameDescription() override;
	virtual const char* GetCommandLineValue(const char* key, const char* defval) override;
	virtual void ConsolePrint(const char* msg) override;
	virtual void ClientConsolePrint(MMSPlayer_t client, const char* msg) override;
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
	virtual int GetUserMessageCount() override;
	virtual int FindUserMessage(const char* name, int* size = nullptr) override;
	virtual const char* GetUserMessage(int index, int* size = nullptr) override;
public: // IConCommandBaseAccessor
	class SourceConVarAccessor : public IConCommandBaseAccessor
	{
		virtual bool RegisterConCommandBase(ConCommandBase* pCommand) override;
	private:
		bool Register(ConCommandBase* pCommand);
		void Unregister(ConCommandBase* pCommand);
		void RemoveMetamodCommands();
#if SOURCE_ENGINE < SE_ORANGEBOX
		bool InitConCommandBaseList();
	private:
		ConCommandBase** m_TopConCommandBase = nullptr;
#endif
	private:
		SourceHook::List<ConCommandBase*> m_RegisteredCommands;

	friend class SourceProvider;
	} m_ConVarAccessor;
public:
	bool Hook_GameInit();
	bool Hook_LevelInit(char const* pMapName, char const* pMapEntities, char const* pOldLevel,
		char const* pLandmarkName, bool loadGame, bool background);
	void Hook_LevelShutdown();
#if SOURCE_ENGINE >= SE_ORANGEBOX
	void Hook_ClientCommand(edict_t* pEdict, const CCommand& args);
#else
	void Hook_ClientCommand(edict_t* pEdict);
#endif
private:
	struct UsrMsgInfo
	{
		UsrMsgInfo()
		{
		}
		UsrMsgInfo(int s, const char* t) : size(s), name(t)
		{
		}
		int size;
		std::string name;
	};
private:
	void CacheUserMessages();
	bool KVLoadFromFile(KeyValues* kv, IFileSystem* filesystem, const char* resourceName, const char* pathID = nullptr);
	inline bool IsUserMessageIterationSupported() const
	{
#if SOURCE_ENGINE == SE_CSGO || SOURCE_ENGINE == SE_BLADE || SOURCE_ENGINE == SE_MCV
		return false;
#else
		return true;
#endif
	}
private:
	IFileSystem* baseFs = nullptr;
	std::vector<UsrMsgInfo> usermsgs_list;
	bool bOriginalEngine = false;

#if SOURCE_ENGINE >= SE_ORANGEBOX
	friend void LocalCommand_Meta(const CCommand& args);
#else
	friend void LocalCommand_Meta();
#endif
};

#endif
