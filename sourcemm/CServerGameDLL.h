/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_CSERVER_GAMEDLL_H
#define _INCLUDE_CSERVER_GAMEDLL_H

/**
 * @brief Defines wrapper class for gamedll interfaces
 * @file CServerGameDLL.h
 */

#include <eiface.h>

/**
 * @brief Empty class.  We only care about DLLInit.
 */
class CServerGameDLL : public IServerGameDLL
{
public:
	virtual bool DLLInit(	CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn fileSystemFactory, CGlobalVars *pGlobals);
	virtual bool GameInit( void ) { return false; }
	virtual bool LevelInit( char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background )  { return false; }
	virtual void ServerActivate( edict_t *pEdictList, int edictCount, int clientMax ) { }
	virtual void GameFrame( bool simulating ) { }
	virtual void PreClientUpdate( bool simulating ) { }
	virtual void LevelShutdown( void ) { }
	virtual void GameShutdown( void ) { }
	virtual void DLLShutdown( void ) { }
	virtual float GetTickInterval( void ) const { return 0.0f; }
	virtual ServerClass *GetAllServerClasses( void ) { return NULL; }
	virtual const char *GetGameDescription( void ) { return NULL; }
	virtual void CreateNetworkStringTables( void ) { }
	virtual CSaveRestoreData  *SaveInit( int size ) { return NULL; }
	virtual void SaveWriteFields( CSaveRestoreData *, const char *, void *, datamap_t *, typedescription_t *, int ) { }
	virtual void SaveReadFields( CSaveRestoreData *, const char *, void *, datamap_t *, typedescription_t *, int ) { }
	virtual void SaveGlobalState( CSaveRestoreData * ) { }
	virtual void RestoreGlobalState( CSaveRestoreData * ) { }
	virtual void PreSave( CSaveRestoreData * ) { }
	virtual void Save( CSaveRestoreData * ) { }
	virtual void GetSaveComment( char *comment, int maxlength ) { }
	virtual void WriteSaveHeaders( CSaveRestoreData * ) { }
	virtual void ReadRestoreHeaders( CSaveRestoreData * ) { }
	virtual void Restore( CSaveRestoreData *, bool ) { }
	virtual bool IsRestoring() { return false; }
	virtual int CreateEntityTransitionList( CSaveRestoreData *, int ) { return 0; }
	virtual void BuildAdjacentMapList( void ) { }
	virtual bool GetUserMessageInfo( int msg_type, char *name, int maxnamelength, int& size ) { return false; }
	virtual CStandardSendProxies* GetStandardSendProxies() { return NULL; }
};

#endif //_INCLUDE_CSERVER_GAMEDLL_H
