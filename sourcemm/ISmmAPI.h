/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_ISMM_API_H
#define _INCLUDE_ISMM_API_H

/**
 * @brief External API interface
 * @file ISmmAPI.h
 */

#include <interface.h>
#include <eiface.h>
#include <convar.h>
#include <sourcehook/sourcehook.h>
#include "IPluginManager.h"

#if defined __GNUC__
#if ((__GNUC__ == 3) && (__GNUC_MINOR__ < 4)) || (__GNUC__ < 3)
#error "You must compile with at least GCC 3.4! If you know what you are doing, you can remove this message."
#endif //version check
#endif //__GNUC__

class IMetamodListener;
class ISmmPluginManager;
class ISmmPlugin;

#define	MMIFACE_SOURCEHOOK		"ISourceHook"
#define	MMIFACE_PLMANAGER		"IPluginManager"	

class ISmmAPI
{
public:
	virtual void LogMsg(ISmmPlugin *pl, const char *msg, ...) =0;
public:
	virtual CreateInterfaceFn engineFactory(bool syn=true) =0;
	virtual CreateInterfaceFn physicsFactory(bool syn=true) =0;
	virtual CreateInterfaceFn fileSystemFactory(bool syn=true) =0;
	virtual CreateInterfaceFn serverFactory(bool syn=true) =0;
	virtual CGlobalVars *pGlobals() =0;
	virtual void SetLastMetaReturn(META_RES res) =0;
	virtual META_RES GetLastMetaReturn() =0;
public:		//Added in 1.00-RC2 (0:0)
	//solves concommand problems by keeping track for loading/unloading
	virtual IConCommandBaseAccessor *GetCvarBaseAccessor() =0;
	virtual bool RegisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand) =0;
	virtual void UnregisterConCmdBase(ISmmPlugin *plugin, ConCommandBase *pCommand) =0;
	//attempt fix at valve not exporting rcon printing
	//these do not add newlines
	virtual void ConPrint(const char *fmt) =0;
	virtual void ConPrintf(const char *fmt, ...) =0;
public:		//Added in 1.10 (1:0)
	//added by request.  Checks if ConPrint/ConPrintf will mirror to rcon.
	virtual bool RemotePrintingAvailable() =0;
	//Returns the Metamod Version numbers as major version and minor (API) version.
	//changes to minor version are guaranteed to be backwards compatible.
	//changes to major version are not.
	//Also returns current plugin version and minimum plugin version
	virtual void GetApiVersions(int &major, int &minor, int &plvers, int &plmin) =0;
	//Returns sourcehook API version and implementation version
	virtual void GetShVersions(int &shvers, int &shimpl) =0;
	//Binds an event listener to your plugin
	virtual void AddListener(ISmmPlugin *plugin, IMetamodListener *pListener) =0;
	/**
	  * @brief Queries the metamod factory
	  *
	  * @param iface	String containing interface name
	  * @param ret		Optional pointer to store return status
	  * @param id		Optional pointer to store id of plugin that overrode interface, 0 if none
	  * @return			Returned pointer
	  */
	virtual void *MetaFactory(const char *iface, int *ret, PluginId *id) =0;
};


/** Version history
 * 1.10 bumped API to 1:0.  The breaking changes occured in sourcehook and the plugin API.
 */

#endif //_INCLUDE_ISMM_API_H
