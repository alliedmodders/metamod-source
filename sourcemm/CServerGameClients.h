/* ======== SourceMM ========
* Copyright (C) 2004-2005 SourceMM Development Team
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
public:
	virtual void GetPlayerLimits(int& minplayers, int& maxplayers, int &defaultMaxPlayers) const { }
	virtual bool ClientConnect(edict_t *pEntity, const char *pszName, const char *pszAddress, char *reject, int maxrejectlen) { return false; }
	virtual void ClientActive(edict_t *pEntity, bool bLoadGame) { }
	virtual void ClientDisconnect(edict_t *pEntity) { }
	virtual void ClientPutInServer(edict_t *pEntity, char const *playername) { }
	virtual void ClientCommand(edict_t *pEntity) { }
	virtual void SetCommandClient(int index) { }
	virtual void ClientSettingsChanged(edict_t *pEdict) { }
	virtual void ClientSetupVisibility(edict_t *pViewEntity, edict_t *pClient, unsigned char *pvs, int pvssize) { }
	virtual float ProcessUsercmds(edict_t *player, bf_read *buf, int numcmds, int totalcmds, int dropped_packets, bool ignore, bool paused) { return 0.0f; }
	virtual void PostClientMessagesSent() { }
	virtual CPlayerState *GetPlayerState(edict_t *player) { return NULL; }
	virtual void ClientEarPosition(edict_t *pEntity, Vector *pEarOrigin) { }
	virtual int GetReplayDelay(edict_t *player) { return 0; }
	virtual void GetBugReportInfo(char *buf, int buflen) { }
};

#endif //_INCLUDE_CSERVERGAMECLIENTS_H
