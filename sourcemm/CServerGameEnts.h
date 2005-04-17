/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_CSERVER_GAMEENTS_H
#define _INCLUDE_CSERVER_GAMEENTS_H

/**
 * @brief Dummy class for IServerGameEnts
 * @file CServerGameEnts.h
 */

#include <eiface.h>

class CServerGameEnts : public IServerGameEnts
{
	IServerGameEnts *m_pOrig;
public:
	CServerGameEnts() : m_pOrig(0) {}
	void SetOrig(IServerGameEnts *pOrig)
	{
		m_pOrig = pOrig;
	}

	virtual void SetDebugEdictBase(edict_t *base)
	{ m_pOrig->SetDebugEdictBase(base); }
	virtual void MarkEntitiesAsTouching(edict_t *e1, edict_t *e2)
	{ m_pOrig->MarkEntitiesAsTouching(e1, e2); }
	virtual void FreeContainingEntity(edict_t *e)
	{ m_pOrig->FreeContainingEntity(e); }
	virtual edict_t *BaseEntityToEdict(CBaseEntity *pEnt)
	{ return m_pOrig->BaseEntityToEdict(pEnt); }
	virtual CBaseEntity *EdictToBaseEntity(edict_t *pEdict)
	{ return m_pOrig->EdictToBaseEntity(pEdict); }
	virtual void CheckTransmit(CCheckTransmitInfo *pInfo, const unsigned short *pEdictIndices, int nEdicts)
	{ m_pOrig->CheckTransmit(pInfo, pEdictIndices, nEdicts); }
};

#endif //_INCLUDE_CSERVER_GAMEENTS_H
