/* ======== SourceMM ========
 * Copyright (C) 2004-2008 Metamod:Source Development Team
 * No warranties of any kind
 *
 * License: zlib/libpng
 *
 * Author(s): David "BAILOPAN" Anderson
 * ============================
 */

#ifndef _INCLUDE_UTIL_H
#define _INCLUDE_UTIL_H

#include <stdarg.h>
#include "sourcehook/sourcehook.h"

/**
 * @brief Utility functions
 * @file util.h
 */

#define IA32_JMP_IMM32 '\xE9'

/**
 * @brief Returns a pointer to the extension in a file name.
 */
const char *UTIL_GetExtension(const char *file);

/**
 * @brief Removes whitespace characters from left side of string.
 */
void UTIL_TrimLeft(char *buffer);

/**
 * @brief Removes whitespace characters from right side of string.
 */
void UTIL_TrimRight(char *buffer);

/**
 * @brief Compares two file paths.
 */
bool UTIL_PathCmp(const char *path1, const char *path2);

/**
 * @brief Same as snprintf except that it ensures the string buffer is null terminated.
 */
size_t UTIL_Format(char *buffer, size_t maxlength, const char *fmt, ...);

/**
 * @brief Same as vsnprintf except that it ensures the string buffer is null terminated.
 */
size_t UTIL_FormatArgs(char *buffer, size_t maxlength, const char *fmt, va_list params);

/**
 * @brief Forms a relative path given two absolute paths.
 *
 * @param buffer		Buffer to store relative path in.
 * @param maxlength		Maximum length of the output buffer.
 * @param relTo			Destination folder to use as a working directory.
 *						Final folder name should not be pathchar-terminated.
 * @param relFrom		Source file or folder to use as a target.
 * @return				True on success, false on failure.
 */
bool UTIL_Relatize(char buffer[],
				   size_t maxlength,
				   const char *relTo,
				   const char *relFrom);

/**
 * @brief Compares memory address against a signature.
 *
 * @param addr			Memory address to check.
 * @param sig			Signature used to check against memory address. Accept 0x2A as wildcard.
 * @param len			Length of signature.
 * @return				True if signature was verified, false otherwise.
 */
bool UTIL_VerifySignature(const void *addr, const char *sig, size_t len);

/**
 * @brief Returns the original function address of a given virtual function.
 *
 * @param mfp			Member function pointer to virtual function.
 * @param ptr			Pointer to interface in which the virtual function belongs.
 * @return				Address of function originally pointed to by the virtual function.
 */
template <class MFP, class Iface>
char *UTIL_GetOrigFunction(MFP vfunc, Iface *ptr)
{
	SourceHook::MemFuncInfo info = {true, -1, 0, 0};
	SourceHook::GetFuncInfo(vfunc, info);

	/* Get address of original GetUserMessageInfo() */
	char *func = reinterpret_cast<char *>(SH_GET_ORIG_VFNPTR_ENTRY(ptr, vfunc));

	/* Check for relative jumps */
	if (func[0] == IA32_JMP_IMM32)
	{
		/* Get address from displacement...
		 *
		 * Add 5 because it's relative to next instruction:
		 * Opcode <1 byte> + 32-bit displacement <4 bytes> 
		 */
		func += *reinterpret_cast<unsigned long *>(func + 1) + 5;
	}

	return func;
}

#endif //_INCLUDE_UTIL_H
