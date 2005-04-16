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
public:
	virtual void SetDebugEdictBase(edict_t *base) { }
	virtual void MarkEntitiesAsTouching(edict_t *e1, edict_t *e2) { }
	virtual void FreeContainingEntity(edict_t *e) { }
	virtual edict_t *BaseEntityToEdict(CBaseEntity *pEnt) { return NULL; }
	virtual CBaseEntity *EdictToBaseEntity(edict_t *pEdict) { return NULL; }
	virtual void CheckTransmit(CCheckTransmitInfo *pInfo, const unsigned short *pEdictIndices, int nEdicts) { }
};

#endif //_INCLUDE_CSERVER_GAMEENTS_H
