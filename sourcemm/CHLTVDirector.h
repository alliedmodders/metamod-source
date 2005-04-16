/* ======== SourceMM ========
* Copyright (C) 2004-2005 SourceMM Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_HLTVDIRECTOR_H
#define _INCLUDE_HLTVDIRECTOR_H

/**
 * @brief Dummy class for IHLTVDirector
 * @file CHLTVDirector.
 */

#include <eiface.h>
#include <ihltvdirector.h>

class CHLTVDirector : IHLTVDirector
{
public:
	virtual bool IsActive() { return false; }
	virtual void SetHLTVServer(IHLTVServer *hltv) { }
	virtual IHLTVServer	*GetHLTVServer() { return NULL; }
	virtual int	GetDirectorTick() { return 0; }
	virtual int	GetPVSEntity() { return 0; }
	virtual Vector GetPVSOrigin() { return Vector(0, 0, 0); }
	virtual float GetDelay() { return 0.0f; }
	virtual const char**GetModEvents() { return NULL; }
};

#endif //_INCLUDE_HLTVDIRECTOR_H
