#ifndef _METAMOD_SOURCE_PROVIDER_UTIL_H_
#define _METAMOD_SOURCE_PROVIDER_UTIL_H_

#include <stdio.h>
#include <string.h>
#include <stdarg.h>

size_t UTIL_FormatArgs(char *buffer, size_t maxlength, const char *fmt, va_list params);

#endif //_METAMOD_SOURCE_PROVIDER_UTIL_H_

