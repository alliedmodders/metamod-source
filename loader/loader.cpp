#include <time.h>
#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "loader.h"
#include "serverplugin.h"
#include "gamedll.h"
#include "utility.h"

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

	mm_library = (HMODULE)mm_LoadLibrary(mm_path, buffer, maxlength);

	return (mm_library != NULL);
}

void
mm_UnloadMetamodLibrary()
{
	mm_UnloadLibrary(mm_library);
	mm_library = NULL;
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
	/* If we've got a VSP bridge, do nothing. */
	if (vsp_bridge != NULL)
	{
		if (ret != NULL)
			*ret = 1;
		return NULL;
	}

	/* If we've got a gamedll bridge, forward the request. */
	if (gamedll_bridge != NULL)
		return gamedll_bridge->QueryInterface(name, ret);

	/* Otherwise, we're probably trying to load Metamod. */
	void *ptr;
	if (strncmp(name, "ISERVERPLUGINCALLBACKS", 22) == 0)
		ptr = mm_GetVspCallbacks(atoi(&name[22]));
	else
		ptr = mm_GameDllRequest(name, ret);

	if (ret != NULL)
		*ret = (ptr != NULL) ? 0 : 1;

	return ptr;
}

void *
mm_GetProcAddress(const char *name)
{
	return mm_GetLibAddress(mm_library, name);
}

MetamodBackend
mm_DetermineBackend(QueryValveInterface engineFactory)
{
	/* Check for L4D */
	if (engineFactory("VEngineServer022", NULL) != NULL &&
		engineFactory("VEngineCvar007", NULL) != NULL)
	{
		return MMBackend_Left4Dead;
	}
	else if (engineFactory("VEngineServer021", NULL) != NULL)
	{
		/* Check for OB */
		if (engineFactory("VEngineCvar004", NULL) != NULL &&
			engineFactory("VModelInfoServer002", NULL) != NULL)
		{
			return MMBackend_Episode2;
		}
		/* Check for EP1 */
		else if (engineFactory("VModelInfoServer001", NULL) != NULL &&
				 (engineFactory("VEngineCvar003", NULL) != NULL ||
				  engineFactory("VEngineCvar002", NULL) != NULL))
		{
			return MMBackend_Episode1;
		}
	}

	return MMBackend_UNKNOWN;
}

