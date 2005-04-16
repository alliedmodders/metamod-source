/* ======== SourceMM ========
* Copyright (C) 2004-2005 Metamod:Source Development Team
* No warranties of any kind
*
* License: zlib/libpng
*
* Author(s): David "BAILOPAN" Anderson
* ============================
*/

#ifndef _INCLUDE_UTIL_H
#define _INCLUDE_UTIL_H

/**
 * @brief Utility functons
 * @file util.h
 */

const char *UTIL_GetExtension(const char *file);

#define META_INTERFACE_MACRO(type, final) \
	PluginIter i; \
	void *mret=NULL, *d; \
	META_RES mres, high=MRES_IGNORED; \
	g_SmmAPI.SetLastMetaReturn(MRES_IGNORED); \
	for (i=g_PluginMngr._begin(); i!=g_PluginMngr._end(); i++) \
	{ \
		if ( (*i) && (*i)->fac_list.##type ) \
		{ \
			d = ((*i)->fac_list.##type)(name, ret); \
			mres = g_SmmAPI.GetLastMetaReturn(); \
			if (mres > high) \
				high = mres; \
			if (mres >= MRES_OVERRIDE) \
				mret = d; \
		} \
	} \
	if (high == MRES_OVERRIDE) \
	{ \
		(final)(name, ret); \
		return mret; \
	} else if (high == MRES_SUPERCEDE) { \
		return mret; \
	} else { \
		return (final)(name, ret); \
	} 

#endif //_INCLUDE_UTIL_H
