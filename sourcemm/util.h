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
void UTIL_TrimComments(char *buffer);
void UTIL_TrimLeft(char *buffer);
void UTIL_TrimRight(char *buffer);
void UTIL_KeySplit(const char *str, char *buf1, size_t len1, char *buf2, size_t len2);
void UTIL_PathFmt(char *buffer, size_t len, const char *fmt, ...);
bool UTIL_PathCmp(const char *path1, const char *path2);

#define META_INTERFACE_MACRO(type, final) \
	PluginIter i; \
	void *mret=NULL, *d; \
	META_RES mres, high=MRES_IGNORED; \
	g_SmmAPI.SetLastMetaReturn(MRES_IGNORED); \
	for (i=g_PluginMngr._begin(); i!=g_PluginMngr._end(); i++) \
	{ \
		if ( (*i) && (*i)->fac_list. type ) \
		{ \
			d = ((*i)->fac_list. type)(name, ret); \
			mres = g_SmmAPI.GetLastMetaReturn(); \
			if (mres > high) \
				high = mres; \
			if (mres >= MRES_OVERRIDE) \
				mret = d; \
		} \
	} \
	if (high == MRES_OVERRIDE) \
	{ \
		if (final) \
			(final)(name, ret); \
		return mret; \
	} else if (high == MRES_SUPERCEDE) { \
		return mret; \
	} else { \
		if (final) \
			return (final)(name, ret); \
		return NULL; \
	} 

#endif //_INCLUDE_UTIL_H
