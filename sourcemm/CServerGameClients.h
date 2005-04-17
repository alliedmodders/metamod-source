/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_CSERVERGAMECLIENTS_H
#define _INCLUDE_CSERVERGAMECLIENTS_H

/**
 * @brief Dummy class for IServerGameClients
 * @file CServerGameClients.h
 */

#include <eiface.h>

class CServerGameClients : public IServerGameClients
{
	IServerGameClients *m_pOrig;
public:
	CServerGameClients() : m_pOrig(0) {}
	void SetOrig(IServerGameClients *pOrig)
	{
		m_pOrig = pOrig;
	}

	virtual void GetPlayerLimits(int& minplayers, int& maxplayers, int &defaultMaxPlayers) const
	{ m_pOrig->GetPlayerLimits(minplayers, maxplayers, defaultMaxPlayers); }

	virtual bool ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char *reject, int maxrejectlen)
	{ return m_pOrig->ClientConnect(pEntity, pszName, pszAddress, reject, maxrejectlen); }

	virtual void ClientActive(edict_t *pEntity, bool bLoadGame)
	{ m_pOrig->ClientActive(pEntity, bLoadGame); }

	virtual void ClientDisconnect(edict_t *pEntity)
	{ m_pOrig->ClientDisconnect(pEntity); }

	virtual void ClientPutInServer(edict_t *pEntity, char const *playername)
	{ m_pOrig->ClientPutInServer(pEntity, playername); }

	virtual void ClientCommand(edict_t *pEntity)
	{ m_pOrig->ClientCommand(pEntity); }

	virtual void SetCommandClient(int index)
	{ m_pOrig->SetCommandClient(index); }

	virtual void ClientSettingsChanged(edict_t *pEdict)
	{ m_pOrig->ClientSettingsChanged(pEdict); }

	virtual void ClientSetupVisibility(edict_t *pViewEntity, edict_t *pClient, unsigned char *pvs, int pvssize)
	{ m_pOrig->ClientSetupVisibility(pViewEntity, pClient, pvs, pvssize); }

	virtual float ProcessUsercmds(edict_t *player, bf_read *buf, int numcmds, int totalcmds, int dropped_packets, bool ignore, bool paused)
	{ return m_pOrig->ProcessUsercmds(player, buf, numcmds, totalcmds, dropped_packets, ignore, paused); }

	virtual void PostClientMessagesSent()
	{ m_pOrig->PostClientMessagesSent(); }

	virtual CPlayerState *GetPlayerState(edict_t *player)
	{ return m_pOrig->GetPlayerState(player); }

	virtual void ClientEarPosition(edict_t *pEntity, Vector *pEarOrigin)
	{ m_pOrig->ClientEarPosition(pEntity, pEarOrigin); }

	virtual int GetReplayDelay(edict_t *player)
	{ return m_pOrig->GetReplayDelay(player); }

	virtual void GetBugReportInfo(char *buf, int buflen)
	{ m_pOrig->GetBugReportInfo(buf, buflen); }
};

#endif //_INCLUDE_CSERVERGAMECLIENTS_H
