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
	virtual bool Load(const vsp_bridge_info *info) = 0;
	virtual void Unload() = 0;
	virtual const char *GetDescription() = 0;
};

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_BRIDGE_H_ */

