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
	IServerGameDLL *m_pOrig;
	char m_GameDescBuffer[256];
public:
	CServerGameDLL() : m_pOrig(0)
	{
		strcpy(m_GameDescBuffer, "Metamod:Source");
	}
	void SetOrig(IServerGameDLL *pOrig)
	{
		m_pOrig = pOrig;
		snprintf(m_GameDescBuffer, 255, "%s", pOrig->GetGameDescription());
	}

	virtual bool DLLInit(	CreateInterfaceFn engineFactory, CreateInterfaceFn physicsFactory, CreateInterfaceFn fileSystemFactory, CGlobalVars *pGlobals);
	virtual bool GameInit( void )
	{ return m_pOrig->GameInit(); }
	virtual bool LevelInit( char const *pMapName, char const *pMapEntities, char const *pOldLevel, char const *pLandmarkName, bool loadGame, bool background )
	{ return m_pOrig->LevelInit(pMapName, pMapEntities, pOldLevel, pLandmarkName, loadGame, background); }
	virtual void ServerActivate( edict_t *pEdictList, int edictCount, int clientMax )
	{ m_pOrig->ServerActivate(pEdictList, edictCount, clientMax); }
	virtual void GameFrame( bool simulating )
	{ m_pOrig->GameFrame(simulating); }
	virtual void PreClientUpdate( bool simulating )
	{ m_pOrig->PreClientUpdate(simulating); }
	virtual void LevelShutdown( void )
	{ m_pOrig->LevelShutdown(); }
	virtual void GameShutdown( void )
	{ m_pOrig->GameShutdown(); }
	virtual void DLLShutdown( void );
	virtual float GetTickInterval( void ) const
	{ return m_pOrig->GetTickInterval(); }
	virtual ServerClass *GetAllServerClasses( void )
	{ return m_pOrig->GetAllServerClasses(); }
	virtual const char *GetGameDescription( void )
	{ return m_GameDescBuffer; }
	virtual void CreateNetworkStringTables( void )
	{ m_pOrig->CreateNetworkStringTables(); }
	virtual CSaveRestoreData  *SaveInit( int size )
	{ return m_pOrig->SaveInit(size); }
	virtual void SaveWriteFields( CSaveRestoreData *a1, const char *a2, void *a3, datamap_t *a4, typedescription_t *a5, int a6)
	{ m_pOrig->SaveWriteFields(a1, a2, a3, a4, a5, a6); }
	virtual void SaveReadFields( CSaveRestoreData *a1, const char *a2, void *a3, datamap_t *a4, typedescription_t *a5, int a6)
	{ m_pOrig->SaveReadFields(a1, a2, a3, a4, a5, a6); }
	virtual void SaveGlobalState( CSaveRestoreData *a1)
	{ m_pOrig->SaveGlobalState(a1); }
	virtual void RestoreGlobalState( CSaveRestoreData *a1)
	{ m_pOrig->RestoreGlobalState(a1); }
	virtual void PreSave( CSaveRestoreData *a1)
	{ m_pOrig->PreSave(a1); }
	virtual void Save( CSaveRestoreData *a1)
	{ m_pOrig->Save(a1); }
	virtual void GetSaveComment( char *comment, int maxlength )
	{ m_pOrig->GetSaveComment(comment, maxlength); }
	virtual void WriteSaveHeaders( CSaveRestoreData *a1)
	{ m_pOrig->WriteSaveHeaders(a1); }
	virtual void ReadRestoreHeaders( CSaveRestoreData *a1)
	{ m_pOrig->ReadRestoreHeaders(a1); }
	virtual void Restore( CSaveRestoreData *a1, bool a2)
	{ m_pOrig->Restore(a1, a2); }
	virtual bool IsRestoring()
	{ return m_pOrig->IsRestoring(); }
	virtual int CreateEntityTransitionList( CSaveRestoreData *a1, int a2)
	{ return m_pOrig->CreateEntityTransitionList(a1, a2); }
	virtual void BuildAdjacentMapList( void )
	{ m_pOrig->BuildAdjacentMapList(); }
	virtual bool GetUserMessageInfo( int msg_type, char *name, int maxnamelength, int& size )
	{ return m_pOrig->GetUserMessageInfo(msg_type, name, maxnamelength, size); }
	virtual CStandardSendProxies* GetStandardSendProxies()
	{ return m_pOrig->GetStandardSendProxies(); }
};

#endif //_INCLUDE_CSERVER_GAMEDLL_H
