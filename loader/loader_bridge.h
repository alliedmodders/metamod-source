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

#ifndef _INCLUDE_METAMOD_SOURCE_LOADER_BRIDGE_H_
#define _INCLUDE_METAMOD_SOURCE_LOADER_BRIDGE_H_

typedef void* (*QueryValveInterface)(const char *pName, int *pReturnCode);
class IServerPluginCallbacks;

struct vsp_bridge_info
{
	QueryValveInterface 		engineFactory;
	QueryValveInterface 		gsFactory;
	IServerPluginCallbacks *	vsp_callbacks;
	unsigned int				vsp_version;
};

class IVspBridge
{
public:
	virtual bool Load(const vsp_bridge_info *info, char *buffer, size_t maxlength) = 0;
	virtual void Unload() = 0;
	virtual const char *GetDescription() = 0;
};

struct gamedll_bridge_info
{
	QueryValveInterface engineFactory;
	QueryValveInterface fsFactory;
	QueryValveInterface physicsFactory;
	QueryValveInterface	gsFactory;
	void *				pGlobals;
	unsigned int		dllVersion;
	void *				isgd;
	const char *		vsp_listener_path;
};

class IGameDllBridge
{
public:
	virtual bool DLLInit_Pre(const gamedll_bridge_info *info, char *buffer, size_t maxlength) = 0;
	virtual void DLLInit_Post(int *isgdUnload) = 0;
	virtual void *QueryInterface(const char *name, int *ret) = 0;
	virtual void Unload() = 0;
};

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_BRIDGE_H_ */

