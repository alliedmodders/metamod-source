#include "provider_util.h"

size_t UTIL_FormatArgs(char *buffer, size_t maxlength, const char *fmt, va_list params)
{
	size_t len = vsnprintf(buffer, maxlength, fmt, params);

	if (len >= maxlength)
	{
		len = maxlength - 1;
		buffer[len] = '\0';
	}

	return len;
}

