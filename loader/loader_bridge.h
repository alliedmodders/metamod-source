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
};

class IGameDllBridge
{
public:
	virtual bool DLLInit_Pre(const gamedll_bridge_info *info, char *buffer, size_t maxlength) = 0;
	virtual void DLLInit_Post() = 0;
	virtual void *QueryInterface(const char *name, int *ret) = 0;
	virtual void Unload() = 0;
};

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_BRIDGE_H_ */

