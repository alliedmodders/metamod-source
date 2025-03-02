/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2009 AlliedModders LLC and authors.
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
 */

#ifndef _INCLUDE_METAMOD_SOURCE_SUPPORT_H_
#define _INCLUDE_METAMOD_SOURCE_SUPPORT_H_

#include "ISmmAPI.h"

namespace SourceMM
{
	class MetamodSourceConVar;

	enum
	{
		ConVarFlag_None = 0,
		ConVarFlag_Notify = 1,
		ConVarFlag_SpOnly = 2,
	};

	/**
	 * @brief Abstracts command information, since the new engine fixes the 
	 * re-entrancy problems in the tokenization system.
	 */
	class IMetamodSourceCommandInfo
	{
	public:
		/**
		 * @brief Returns the argument count such that arguments 
		 * 1 to N are valid arguments, and 0 is the command name.
		 *
		 * @return					Argument count.
		 */
		virtual unsigned int GetArgCount() =0;

		/**
		 * @brief Returns the string of an argument number.
		 *
		 * @param num				Argument number.
		 * @return					Argument text.
		 */
		virtual const char *GetArg(unsigned int num) =0;

		/**
		 * @brief Returns the entire command argument string.
		 *
		 * @return					Argument string.
		 */
		virtual const char *GetArgString() =0;
	};

	/**
     * @brief Interface for Metamod:Source to provide callbacks to the 
	 * provider.
	 */
	class IMetamodSourceProviderCallbacks
	{
	public:
		/**
		 * @brief Called before the server DLL handles game initialization.
		 */
		virtual void OnGameInit() = 0;

		/**
		 * @brief Called after the server DLL has completed handling level/map initialization.
		 */
		virtual void OnLevelInit(char const* pMapName, char const* pMapEntities, char const* pOldLevel, char const* pLandmarkName, bool loadGame, bool background) = 0;

		/**
		 * @brief Called after the server DLL has completed handling level/map shut down.
		 */
		virtual void OnLevelShutdown() = 0;

		/**
		 * @brief Called when the ConCommand "meta" is executed
		 */
		virtual bool OnCommand_Meta(IMetamodSourceCommandInfo* info) = 0;

		/**
		 * @brief Called when a client executes "meta" as a ClientCommand
		 */
		virtual bool OnCommand_ClientMeta(MMSPlayer_t player, IMetamodSourceCommandInfo* info) = 0;
	};

	class IMetamodSourceProvider
	{
	public:
		/**
		 * @brief Set the callback interface for the provider to call into.
		 * 
		 * @param pCallbacks		Pointer to callback interface implementation.
		 */
		virtual void SetCallbacks(IMetamodSourceProviderCallbacks* pCallbacks) = 0;

		/**
		 * @brief Returns whether source engine build is compatible.
		 *
		 * @param build				Source engine build.
		 * @return					True if compatible, false otherwise.
		 */
		virtual bool IsSourceEngineBuildCompatible(int build) =0;

		/**
		 * @brief Logs a message via IVEngineServer::LogPrint.
		 * 
		 * @param buffer			Buffer to print.
		 * @return					True on success, false if not supported, 
		 * 							or IVEngineServer is not yet known.
		 */
		virtual bool LogMessage(const char *buffer) =0;

		/**
		 * @brief Returns the command line value of a parameter.  If ICvar 
		 * is not yet loaded, this uses an equivalent function.
		 *
		 * @param key				Parameter to look up.
		 * @param val				Default string to return if none found.
		 * @return					Parameter value.
		 */
		virtual const char *GetCommandLineValue(const char *key, const char *defval=NULL) =0;

		/**
		 * @brief Prints a string to the remote server console. 
		 * 
		 * Note: new lines are not appended.
		 *
		 * @param msg			 	Message string.
		 */
		virtual void ConsolePrint(const char *msg) =0;

		/**
		 * @brief Prints text in the specified client's console.
		 *
		 * @param player			Player identifier
		 * @param msg				Message string.
		 */
		virtual void ClientConsolePrint(MMSPlayer_t player, const char *msg) =0;

		/**
		 * @brief Halts the server with a fatal error message.
		 *
		 * No newline is appended.
		 *
		 * @param fmt				Formatted message string.
		 * @param ...				Format parameters.
		 */
		virtual void DisplayError(const char *fmt, ...) =0;

		/**
		 * @brief Sends the server a warning message.
		 *
		 * No newline is appended.
		 *
		 * @param fmt				Formatted message string.
		 * @param ...				Format parameters.
		 */
		virtual void DisplayWarning(const char *fmt, ...) =0;

		/**
		 * @brief Sends the server a developer message.
		 *
		 * No newline is appended.
		 *
		 * @param fmt				Formatted message string.
		 * @param ...				Format parameters.
		 */
		virtual void DisplayDevMsg(const char* fmt, ...) = 0;

		/**
		 * @brief Attempts to notify the provider of the gamedll version being 
		 * used.
		 *
		 * @param iface				Interface string.
		 * @return					Version number on success, 0 otherwise.
		 */
		virtual int TryServerGameDLL(const char *iface) =0;

		/**
		 * @brief Notifies the provider that the DLLInit pre-hook is almost done.
		 */
		virtual void Notify_DLLInit_Pre(CreateInterfaceFn engineFactory, CreateInterfaceFn serverFactory) =0;

		virtual void Notify_DLLShutdown_Pre() =0;

		/**
		 * @brief Wrapper around IVEngineServer::ServerCommand()
		 *
		 * @param cmd				Command string.
		 */
		virtual void ServerCommand(const char *cmd) =0;

		/**
		 * @brief Creates a ConVar pointer.
		 *
		 * @param name				ConVar name.
		 * @param defval			Default value string.
		 * @param flags				ConVar flags.
		 * @param help				Help text.
		 * @return					ConVar pointer.
		 */
		virtual MetamodSourceConVar *CreateConVar(const char *name,
			const char *defval, 
			const char *help,
			int flags) =0;

		/**
		 * @brief Returns the string value of a ConVar.
		 *
		 * @param convar			ConVar pointer.
		 * @return					String value.
		 */
		virtual const char *GetConVarString(MetamodSourceConVar *convar) =0;

		/**
		 * @brief Sets a ConVar string.
		 *
		 * @param convar			ConVar pointer.
		 * @param str				String pointer.
		 */
		virtual void SetConVarString(MetamodSourceConVar *convar, const char *str) =0;

		/**
		 * @brief Retrieves the absolute path to the game directory.
		 *
		 * @param buffer			Buffer in which to store path.
		 * @param maxlen			Maximum length of buffer.
		 */
		virtual void GetGamePath(char *buffer, int maxlen) = 0;

		/**
		 * @brief Retrieves the game description.
		 *
		 * @return					Game description.
		 */
		virtual const char *GetGameDescription() =0;

		/**
		 * @brief Registers a ConCommand.
		 *
		 * @param pCommand		ConCommand to register.
		 * @return				True if successful, false otherwise.
		 */
		virtual bool RegisterConCommand(ProviderConCommand *pCommand) =0;

		/**
		 * @brief Registers a ConVar.
		 *
		 * @param pVar			ConVar to register.
		 * @return				True if successful, false otherwise.
		 */
		virtual bool RegisterConVar(ProviderConVar *pVar) =0;

		/**
		 * @brief Unregisters a ConCommand.
		 *
		 * @param pCommand		ConCommand to unlink.
		 */
		virtual void UnregisterConCommand(ProviderConCommand *pCommand) =0;

		/**
		 * @brief Unregisters a ConVar.
		 *
		 * @param pVar			ConVar to unlink.
		 */
		virtual void UnregisterConVar(ProviderConVar *pVar) =0;

		/**
		 * @brief Returns whether a ConCommandBase is a command or not.
		 *
		 * @param pCommand		ConCommandBase pointer.
		 * @return				True if a command, false otherwise.
		 */
		virtual bool IsConCommandBaseACommand(ConCommandBase *pCommand) =0;

		/**
		 * @brief Returns the number of user messages in the GameDLL.
		 *
		 * @return				Number of user messages, or -1 if SourceMM has 
		 *						failed to get user message list.
		 */
		virtual int GetUserMessageCount() =0;

		/**
		 * @brief Returns the index of the specified user message.
		 *
		 * @param name			User message name.
		 * @param size			Optional pointer to store size of user message.
		 * @return				Message index, or -1 on failure.
		 */
		virtual int FindUserMessage(const char *name, int *size=NULL) =0;

		/**
		 * @brief Returns the name of the specified user message.
		 *
		 * @param index			User message index.
		 * @param size			Optional pointer to store size of user message.
		 * @return				Message name, or NULL on failure.
		 */
		virtual const char *GetUserMessage(int index, int *size=NULL) =0;

		/**
		 * @brief Returns the Source Engine build.
		 *
		 * @return				SOURCE_ENGINE constant.
		 */
		virtual int DetermineSourceEngine() =0;

		/**
		 * @brief Processes a VDF plugin file.
		 *
		 */
		virtual bool ProcessVDF(const char *file, char path[], size_t path_len, char alias[], size_t alias_len) =0;
		
		/**
		 * @brief				Returns string that describes engine version.
		 *
		 * @return				Description.
		 */
		virtual const char *GetEngineDescription() const =0;
	};
};

extern PluginId g_PLID;
extern SourceHook::ISourceHook *g_SHPtr;
extern SourceMM::IMetamodSourceProvider *provider;
extern SourceMM::ISmmAPI *g_pMetamod;

#endif //_INCLUDE_METAMOD_SOURCE_SUPPORT_H_

