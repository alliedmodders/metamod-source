/**
 * vim: set ts=4 sw=4 tw=99 noet :
 * ======================================================
 * Metamod:Source
 * Copyright (C) 2004-2010 AlliedModders LLC and authors.
 * All rights reserved.
 * ======================================================
 *
 * This software is provided 'as-is', without any express or implied warranty.
 * In no event will the authors be held liable for any damages arising from 
 * the use of this software.
 * 
 * Permission is granted to anyone to use this software for any purpose, 
 * including commercial applications, and to alter it and redistribute it 
 * freely, subject to the following restrictions:
 *
 * 1. The origin of this software must not be misrepresented; you must not 
 * claim that you wrote the original software. If you use this software in a 
 * product, an acknowledgment in the product documentation would be 
 * appreciated but is not required.
 * 2. Altered source versions must be plainly marked as such, and must not be
 * misrepresented as being the original software.
 * 3. This notice may not be removed or altered from any source distribution.
 */

#ifndef _INCLUDE_METAMOD_LOADER_PUBLIC_H_
#define _INCLUDE_METAMOD_LOADER_PUBLIC_H_

enum MetamodBackend
{
	MMBackend_Episode1 = 0,
	MMBackend_DarkMessiah,
	MMBackend_Episode2,
	MMBackend_BloodyGoodTime,
	MMBackend_EYE,
	MMBackend_CSS,
	MMBackend_Episode2Valve_OBSOLETE,
	MMBackend_Left4Dead,
	MMBackend_Left4Dead2,
	MMBackend_AlienSwarm,
	MMBackend_Portal2,
	MMBackend_CSGO,
	MMBackend_DOTA,
	MMBackend_HL2DM,
	MMBackend_DODS,
	MMBackend_TF2,
	MMBackend_NuclearDawn,
	MMBackend_SDK2013,
	MMBackend_Blade,
	MMBackend_Insurgency,
	MMBackend_Contagion,
	MMBackend_BMS,
	MMBackend_Source2,

	MMBackend_UNKNOWN
};

#endif // _INCLUDE_METAMOD_LOADER_PUBLIC_H_