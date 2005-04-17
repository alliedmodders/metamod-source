/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
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
	IHLTVDirector *m_pOrig;
public:
	CHLTVDirector() : m_pOrig(0) {}
	void SetOrig(IHLTVDirector *pOrig)
	{
		m_pOrig = pOrig;
	}
	virtual bool IsActive() { return m_pOrig->IsActive(); }
	virtual void SetHLTVServer(IHLTVServer *hltv) { m_pOrig->SetHLTVServer(hltv); }
	virtual IHLTVServer	*GetHLTVServer() { return m_pOrig->GetHLTVServer(); }
	virtual int	GetDirectorTick() { return m_pOrig->GetDirectorTick(); }
	virtual int	GetPVSEntity() { return m_pOrig->GetPVSEntity(); }
	virtual Vector GetPVSOrigin() { return m_pOrig->GetPVSOrigin(); }
	virtual float GetDelay() { return m_pOrig->GetDelay(); }
	virtual const char**GetModEvents() { return m_pOrig->GetModEvents(); }
};

#endif //_INCLUDE_HLTVDIRECTOR_H
