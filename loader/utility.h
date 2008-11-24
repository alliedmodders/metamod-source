#ifndef _INCLUDE_METAMOD_SOURCE_LOADER_UTILITY_H_
#define _INCLUDE_METAMOD_SOURCE_LOADER_UTILITY_H_

#include <stddef.h>

extern size_t
mm_Format(char *buffer, size_t maxlength, const char *fmt, ...);

extern void *
mm_LoadLibrary(const char *path, char *buffer, size_t maxlength);

extern void *
mm_GetLibAddress(void *lib, const char *name);

extern void
mm_UnloadLibrary(void *lib);

extern bool
mm_ResolvePath(const char *path, char *buffer, size_t maxlength);

extern size_t
mm_PathFormat(char *buffer, size_t len, const char *fmt, ...);

extern void
mm_TrimLeft(char *buffer);

extern void
mm_TrimRight(char *buffer);

extern void
mm_TrimComments(char *buffer);

extern void
mm_KeySplit(const char *str, char *buf1, size_t len1, char *buf2, size_t len2);

extern bool
mm_PathCmp(const char *path1, const char *path2);

extern bool
mm_GetFileOfAddress(void *pAddr, char *buffer, size_t maxlength);

#endif /* _INCLUDE_METAMOD_SOURCE_LOADER_UTILITY_H_ */

