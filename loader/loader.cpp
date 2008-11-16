#include <time.h>
#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include "loader.h"
#include "serverplugin.h"

static HMODULE mm_library = NULL;
static char mm_fatal_logfile[PLATFORM_MAX_PATH] = "metamod-fatal.log";

extern void
mm_LogFatal(const char *message, ...)
{
	FILE *fp;
	time_t t;
	va_list ap;
	char header[256];

	fp = fopen(mm_fatal_logfile, "at");
	if (!fp && (fp = fopen("metamod-fatal.log", "at")) == NULL)
		return;

	t = time(NULL);
	strftime(header, sizeof(header), "%d/%d/%Y - %H:%M:%S", localtime(&t));
	fprintf(fp, "L %s: ", header);
	
	va_start(ap, message);
	vfprintf(fp, message, ap);
	va_end(ap);

	fprintf(fp, "\n");

	fclose(fp);	
}

static size_t
mm_FormatArgs(char *buffer, size_t maxlength, const char *fmt, va_list params)
{
	size_t len = vsnprintf(buffer, maxlength, fmt, params);

	if (len >= maxlength)
	{
		len = maxlength - 1;
		buffer[len] = '\0';
	}

	return len;
}

static size_t
mm_Format(char *buffer, size_t maxlength, const char *fmt, ...)
{
	size_t len;
	va_list ap;

	va_start(ap, fmt);
	len = mm_FormatArgs(buffer, maxlength, fmt, ap);
	va_end(ap);

	return len;
}

static bool
mm_GetFileOfAddress(void *pAddr, char *buffer, size_t maxlength)
{
#if defined _WIN32
	MEMORY_BASIC_INFORMATION mem;
	if (!VirtualQuery(pAddr, &mem, sizeof(mem)))
		return false;
	if (mem.AllocationBase == NULL)
		return false;
	HMODULE dll = (HMODULE)mem.AllocationBase;
	GetModuleFileName(dll, (LPTSTR)buffer, maxlength);
#else
	Dl_info info;
	if (!dladdr(pAddr, &info))
		return false;
	if (!info.dli_fbase || !info.dli_fname)
		return false;
	const char *dllpath = info.dli_fname;
	snprintf(buffer, maxlength, "%s", dllpath);
#endif
	return true;
}

static const char *backend_names[3] =
{
	"1.ep1",
	"2.ep2",
	"2.l4d"
};

#if defined _WIN32
#define LIBRARY_EXT		".dll"
#define LIBRARY_MINEXT	".dll"
#elif defined __linux__
#define LIBRARY_EXT		"_i486.so"
#define LIBRARY_MINEXT	".so"
#endif

#if defined _WIN32
static void
mm_GetPlatformError(char *buffer, size_t maxlength)
{
	DWORD dw = GetLastError();
	FormatMessageA(
		FORMAT_MESSAGE_FROM_SYSTEM|FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		dw,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPSTR)buffer,
		maxlength,
		NULL);
}
#endif

bool
mm_LoadMetamodLibrary(MetamodBackend backend, char *buffer, size_t maxlength)
{
	size_t len, temp_len;
	char mm_path[PLATFORM_MAX_PATH * 2];

	/* Get our path */
	if (!mm_GetFileOfAddress((void*)mm_GetFileOfAddress, mm_path, sizeof(mm_path)))
		return false;

	len = strlen(mm_path);
	temp_len = strlen("server" LIBRARY_EXT);
	if (len < temp_len)
		return false;

	/* Build log file name */
	mm_path[len - temp_len] = '\0';
	mm_Format(mm_fatal_logfile,
			  sizeof(mm_fatal_logfile),
			  "%smetamod-fatal.log",
			  mm_path);

	/* Replace server.dll with the new binary we want */
	mm_Format(&mm_path[len - temp_len],
			  sizeof(mm_path) - (len - temp_len),
			  "metamod.%s" LIBRARY_MINEXT,
			  backend_names[backend]);

#if defined _WIN32
	mm_library = LoadLibrary(mm_path);

	if (mm_library == NULL)
	{
		mm_GetPlatformError(buffer, maxlength);
		return false;
	}
#elif defined __linux__
	mm_library = dlopen(mm_path, RTLD_NOW);

	if (mm_library == NULL)
	{
		mm_Format(buffer, maxlength, "%s", dlerror());
		return false;
	}
#endif

	return true;
}

void
mm_UnloadMetamodLibrary()
{
#if defined _WIN32
	FreeLibrary(mm_library);
#else
	dlclose(mm_library);
#endif
}

#if defined _WIN32
#define EXPORT extern "C" __declspec(dllexport)
#elif defined __GNUC__
#if __GNUC__ == 4
#define EXPORT extern "C" __attribute__ ((visibility("default")))
#else
#define EXPORT extern "C"
#endif
#endif

EXPORT void *
CreateInterface(const char *name, int *ret)
{
	if (mm_library != NULL)
	{
		if (ret != NULL)
			*ret = 1;
		return NULL;
	}

	void *ptr;
	if (strncmp(name, "ISERVERPLUGINCALLBACKS", 22) == 0)
	{
		ptr = mm_GetVspCallbacks(atoi(&name[22]));
		if (ret != NULL)
			*ret = ptr != NULL ? 0 : 1;
		return ptr;
	}

	if (ret != NULL)
		*ret = 1;

	return NULL;
}

extern void *
mm_GetProcAddress(const char *name)
{
#if defined _WIN32
	return GetProcAddress(mm_library, name);
#elif defined __linux__
	return dlsym(mm_library, name);
#endif
}

