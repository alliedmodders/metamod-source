#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <ctype.h>

extern "C" void GetGameDir(char *buffer, int maxlength);
extern "C" void *GetThisPointer();

#if defined _MSC_VER
#define SEPCHAR			"\\"
#define MMPATH			"addons\\metamod\\bin"
#define WINDOWS_LEAN_AND_MEAN
#include <windows.h>
#elif defined __linux__
#define SEPCHAR			"/"
#define MMPATH			"addons/metamod/bin"
#include <unistd.h>
#endif

size_t UTIL_Format(char *buffer, size_t maxlength, const char *fmt, ...);
bool s_isspace(char c);
bool RenameFile(const char *old, const char *newf);

/* This will be called by the thunk */
#if defined _MSC_VER
extern "C" void LoadFunction()
#elif defined __linux__
extern "C" void _LoadFunction()
#endif
{
	char gamedir[260];
	char mmpath[260];

	GetGameDir(gamedir, sizeof(gamedir));

	/* Defaults */
	UTIL_Format(mmpath, sizeof(mmpath), "|gameinfo_path|%s", MMPATH);

	/* Read config */
	char config[260];
	UTIL_Format(config, sizeof(config), "%s" SEPCHAR "sourcemm_updater.conf", gamedir);
	FILE *fpCfg = fopen(config, "rt");
	if (fpCfg)
	{
		char cfgLine[512];
		while (!feof(fpCfg) && fgets(cfgLine, sizeof(cfgLine), fpCfg) != NULL)
		{
			char key[255];

			size_t keyLen = 0;

			/* Strip whitespace */
			char *input = cfgLine;
			while (*input != '\0' && s_isspace(*input))
			{
				input++;
			}

			/* Strip ending whitespace */
			size_t len = strlen(input);
			for (size_t i = len - 1;
				 i >= 0 && i < len;
				 i--)
			{
				if (s_isspace(input[i]))
				{
					input[i] = '\0';
					len--;
				} else {
					break;
				}
			}
			
			/* Eat stuff until we find a key */
			while (*input != '\0' && !s_isspace(*input))
			{
				if (keyLen < sizeof(key))
				{
					key[keyLen++] = *input;
				}
				input++;
			}
			key[keyLen] = '\0';

			/* Eat spaces until we hit an = sign */
			while (*input != '\0' && *input != '=')
			{
				input++;
			}

			if (*input == '=')
			{
				input++;
			}

			/* Eat spaces again */
			while (*input != '\0' && s_isspace(*input))
			{
				input++;
			}

			/* The rest is our key */
			if (strcmp(key, "mmpath") == 0)
			{
				UTIL_Format(mmpath, sizeof(mmpath), "%s", input);
			}
		}
		fclose(fpCfg);
	}

	char old_path[260];
	char new_path[260];

	UTIL_Format(old_path, sizeof(old_path), "%s" SEPCHAR "gameinfo.txt", gamedir);
	UTIL_Format(new_path, sizeof(new_path), "%s" SEPCHAR "gameinfo.new.txt", gamedir);

	FILE *fp = fopen(old_path, "rt");
	
	if (!fp)
	{
		return;
	}

	FILE *op = fopen(new_path, "wt");
	if (!op)
	{
		fclose(fp);
		return;
	}

	enum ParseState
	{
		Parse_None,
		Parse_Root,
		Parse_GameInfo,
		Parse_FileSystem,
		Parse_SearchPaths,
	};

	ParseState ps = Parse_Root;

	char input[1024];
	char backup[1024];
	
	while (!feof(fp) && fgets(input, sizeof(input), fp) != NULL)
	{
		UTIL_Format(backup, sizeof(backup), "%s", input);

		if (ps != Parse_None)
		{
			char *inbuf = input;
			
			/* Strip beginning whitespace */
			while (*inbuf != '\0' && s_isspace(*inbuf))
			{
				inbuf++;
			}

			/* Strip ending whitespace */
			size_t len = strlen(inbuf);
			for (size_t i = len - 1;
				 i >= 0 && i < len;
				 i--)
			{
				if (s_isspace(inbuf[i]))
				{
					inbuf[i] = '\0';
					len--;
				} else {
					break;
				}
			}

			/* Strip quotation marks */
			if (inbuf[0] == '"'
				&& inbuf[len-1] == '"')
			{
				inbuf[len - 1] = '\0';
				inbuf = &inbuf[1];
				len -= 2;
			}

			/* Do tests */
			if (ps == Parse_Root && strcmp(inbuf, "GameInfo") == 0)
			{
				ps = Parse_GameInfo;
			} else if (ps == Parse_GameInfo && strcmp(inbuf, "FileSystem") == 0) {
				ps = Parse_FileSystem;
			} else if (ps == Parse_FileSystem && strcmp(inbuf, "SearchPaths") == 0) {
				ps = Parse_SearchPaths;
			} else if (ps == Parse_SearchPaths) {
				const char *game = strstr(inbuf, "Game");
				if (game)
				{
					if (strstr(game, "GameBin") != NULL
						&& strstr(game, mmpath) != NULL)
					{
						fclose(op);
						op = NULL;
						break;			/* Nothing more to do! */
					} else {
						fputs("\t\t\tGameBin\t\t\t|gameinfo_path|", op);
						fputs(mmpath, op);
						fputs("\n", op);
						ps = Parse_None;
					}
				}
			}
		}

		fputs(backup, op);
	}

	if (!op)
	{
		/* Well, we can't really do anything else.  Give up. */
		fclose(fp);
		return;
	}

	/* Close all streams */
	fclose(op);
	fclose(fp);

	/* Move the old file to a backup name */
	char backup_name[260];
	UTIL_Format(backup_name, sizeof(backup_name), "%s" SEPCHAR "gameinfo.backup.txt", gamedir);

	if (!RenameFile(old_path, backup_name))
	{
		/* If we can't rename, just bail out.
		 * We don't want to overwrite the client's default
		 * without backing it up first!
		 */
		return;
	}
	if (!RenameFile(new_path, old_path))
	{
		/* Since this failed, we really have no choice.
		 * Try and rename the old back.
		 */
		RenameFile(backup_name, old_path);
		return;
	}
#if defined _MSC_VER
	_unlink(new_path);
#else
	unlink(new_path);
#endif
}

bool s_isspace(char c)
{
	if ((unsigned)c & 0x80)
	{
		return false;
	} else {
		return isspace(c) ? true : false;
	}
}

size_t UTIL_Format(char *buffer, size_t maxlength, const char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	size_t len = vsnprintf(buffer, maxlength, fmt, ap);
	va_end(ap);

	if (len >= maxlength)
	{
		len = maxlength - 1;
		buffer[len] = '\0';
	}

	return len;
}

bool RenameFile(const char *old, const char *newf)
{
#if defined __linux__
	return (rename(old, newf) == 0);
#elif defined WIN32
	return (MoveFileA(old, newf) != 0);
#endif
}

#if defined _MSC_VER
extern "C" __declspec(dllexport) void *CreateInterface(const char *iface, int *ret)
#elif defined __linux__
extern "C" __attribute__((visibility("default"))) void *CreateInterface(const char *iface, int *ret)
#endif
{
	if (strcmp(iface, "ISERVERPLUGINCALLBACKS001") == 0)
	{
		if (ret)
		{
			*ret = 0;
		}
		return GetThisPointer();
	}

	if (ret)
	{
		*ret = 1;
	}
	return NULL;
}
