#ifndef _INCLUDE_METAMOD_SOURCE_GAMEDLLS_H_
#define _INCLUDE_METAMOD_SOURCE_GAMEDLLS_H_

#include "loader_bridge.h"

extern void *
mm_GameDllRequest(const char *name, int *ret);

extern IGameDllBridge* gamedll_bridge;

#endif /* _INCLUDE_METAMOD_SOURCE_GAMEDLLS_H_ */

