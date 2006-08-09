//========= Copyright © 1996-2005, Valve Corporation, All rights reserved. ============//
//
// Purpose: 
//
// $NoKeywords: $
//
//=============================================================================//

#ifdef _XBOX
#include "xbox/xbox_platform.h"
#include "xbox/xbox_win32stubs.h"
#include "xbox/xbox_core.h"
#endif
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "basetypes.h"
#include "convar.h"
#include "vstdlib/strtools.h"
#include "icvar.h"
#include "tier0/dbg.h"
#ifdef _XBOX
#include "vstdlib/ICommandLine.h"
#endif
#include "tier0/memdbgon.h"

ConCommandBase			*ConCommandBase::s_pConCommandBases = NULL;
IConCommandBaseAccessor	*ConCommandBase::s_pAccessor = NULL;

#ifdef _XBOX
static char const* GetCommandLineValue( char const *pVariableName )
{
	int nLen = Q_strlen(pVariableName);
	char *pSearch = (char*)stackalloc(nLen + 2);
	pSearch[0] = '+';
	memcpy(&pSearch[1], pVariableName, nLen + 1);
	return CommandLine()->ParmValue(pSearch);
}
#endif

#if defined( _XBOX ) && !defined( _RETAIL )
void ConCommandBaseMgr::PublishCommands( bool bForce )
{
	ConCommandBase	*pCur;
	const char		*commands[2048];
	const char		*helptext[2048];
	int				numCommands = 0;

	if ( bForce )
	{
		for ( pCur=ConCommandBase::s_pConCommandBases; pCur; pCur=pCur->m_pNext )
		{
			pCur->m_bRegistered = false;
		}
	}

	// iterate and publish commands to the remote console
	for ( pCur=ConCommandBase::s_pConCommandBases; pCur; pCur=pCur->m_pNext )
	{
		if ( !pCur->m_bRegistered )
		{
			// add unregistered commands to list
			if ( numCommands < sizeof(commands)/sizeof(commands[0]) )
			{
				commands[numCommands] = pCur->m_pszName;
				helptext[numCommands] = pCur->m_pszHelpString;
				numCommands++;
			}

			// mark as registered
			pCur->m_bRegistered = true;
		}
	}
	if ( numCommands )
	{
		XBX_rAddCommands( numCommands, commands, helptext );
	}
}
#endif

#ifdef _XBOX
bool ConCommandBaseMgr::Fixup(ConCommandBase* pConCommand)
{
	ConCommandBase	*pCur;
	ConCommandBase	*pPrev2;
	ConCommandBase	*pCur2;
	ConCommandBase	*pNext2;
	const char		*name;
	static int		initCount = 0;

	// xboxissue - cvars and its class hierarchy could not be made to instance per subsystem
	// without massive mangling and re-arranging, instead...
	// there is only a single chain and therefore single /init/fixup
	// missing: need to identify which subsystem
	// could pass as part of declaration in constructor, but how to hide parameter for pc
	// the accessors (aka callbacks to subsystems) to register with engine 
	// cannot be invoked as their unlink logic expect private lists
	// so this just mimics the expected end result
	// must handle early and late constructors
	// late constructors are usually function scoped static
	if (!pConCommand)
	{
		// the caller is one-time-init 
		if (++initCount > 1)
		{
			// the list has already been fixed
			return true;
		}
	}
	else
	{
		// the caller is a console command constructor
		if (!initCount)
		{
			// the list has not been fixed yet 
			// no special behavior
			return false;
		}
		else
		{
			// the list has already been fixed
			// the console command is a late constructor
			// add in to fixed list 
			bool hasParent = false;
			if (!pConCommand->IsCommand())
			{
				pCur  = ConCommandBase::s_pConCommandBases;
				while (pCur)
				{
					if (pCur->IsCommand() && !stricmp(pCur->m_pszName, pConCommand->m_pszName))
					{
						// set its parent
						((ConVar*)pConCommand)->m_pParent = ((ConVar*)pCur)->m_pParent;
						hasParent = true;
						break;
					}
					pCur = pCur->m_pNext;
				}
			}
			if (!hasParent)
			{
				// add to head of list
				pConCommand->m_pNext = ConCommandBase::s_pConCommandBases;
				ConCommandBase::s_pConCommandBases = pConCommand;
			}
			else
				return true;
		}
	}

	if (initCount == 1)
	{
		// iterate the cvars and set their possible proxy parents
		// skip over registered (fixed) entries
		pCur = ConCommandBase::s_pConCommandBases;
		while (pCur)
		{
			if (!pCur->IsCommand() && !pCur->m_bRegistered)
			{
				// iterate from the next node until end of list
				name   = pCur->m_pszName; 
				pPrev2 = pCur;		
				pCur2  = pCur->m_pNext;
				while (pCur2)
				{
					pNext2 = pCur2->m_pNext;
					if (!pCur2->IsCommand() && !stricmp(pCur2->m_pszName, name))
					{
						// found duplicate
						// unlink and fixup
						pCur2->m_pNext  = NULL;
						pPrev2->m_pNext = pNext2;

						// set its parent
						((ConVar*)pCur2)->m_pParent = ((ConVar*)pCur)->m_pParent;
					}
					else
					{
						// no unlink, advance to next node
						pPrev2 = pCur2;
					}

					pCur2 = pNext2;
				}

				char const *pValue = GetCommandLineValue(name);
				if (pValue)
					((ConVar*)pCur)->SetValue(pValue);
			}
			pCur = pCur->m_pNext;
		}
	}

#if !defined( _RETAIL )
	XBX_rTimeStampLog( Plat_FloatTime(), "xbx PublishCommands:Start" );

	PublishCommands( false );

	XBX_rTimeStampLog( Plat_FloatTime(), "xbx PublishCommands:Done" );
#endif

	// fixup has been performed
	return true;
}
#endif

// ----------------------------------------------------------------------------- //
// ConCommandBaseMgr.
// ----------------------------------------------------------------------------- //
void ConCommandBaseMgr::OneTimeInit( IConCommandBaseAccessor *pAccessor )
{
#ifdef _XBOX
	// fixup the list
	ConCommandBaseMgr::Fixup(NULL);
#else
	ConCommandBase *pCur, *pNext;

	ConCommandBase::s_pAccessor = pAccessor;
	pCur = ConCommandBase::s_pConCommandBases;
	while ( pCur )
	{
		pNext = pCur->m_pNext;
		pCur->Init();
		pCur = pNext;
	}
#endif
}

//-----------------------------------------------------------------------------
// Purpose: Default constructor
//-----------------------------------------------------------------------------
ConCommandBase::ConCommandBase( void )
{
	m_bRegistered   = false;
	m_pszName       = NULL;
	m_pszHelpString = NULL;

	m_nFlags = 0;
	m_pNext  = NULL;
}

//-----------------------------------------------------------------------------
// Purpose: The base console invoked command/cvar interface
// Input  : *pName - name of variable/command
//			*pHelpString - help text
//			flags - flags
//-----------------------------------------------------------------------------
ConCommandBase::ConCommandBase( char const *pName, char const *pHelpString /*=0*/, int flags /*= 0*/ )
{
	Create( pName, pHelpString, flags );
}

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
ConCommandBase::~ConCommandBase( void )
{
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool ConCommandBase::IsCommand( void ) const
{ 
//	Assert( 0 ); This can't assert. . causes a recursive assert in Sys_Printf, etc.
	return true;
}
	
//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *pName - 
//			callback - 
//			*pHelpString - 
//			flags - 
//-----------------------------------------------------------------------------
void ConCommandBase::Create( char const *pName, char const *pHelpString /*= 0*/, int flags /*= 0*/ )
{
	static char *empty_string = "";

	m_bRegistered = false;

	// Name should be static data
	Assert( pName );
	m_pszName = pName;
	m_pszHelpString = pHelpString ? pHelpString : empty_string;

	m_nFlags = flags;

#ifdef _XBOX
	if (ConCommandBaseMgr::Fixup(this))
		return;
#endif

	if ( !( m_nFlags & FCVAR_UNREGISTERED ) )
	{
#ifndef _XBOX
		m_pNext = s_pConCommandBases;
		s_pConCommandBases = this;
#else
		// xboxissue - engine cvars should be at head of list to
		// ensure they are set as the cvar master/parent during fixup
		if (!s_pConCommandBases || !(flags & FCVAR_NON_ENGINE))
		{
			// engine cvars, place at head of list
			m_pNext = s_pConCommandBases;
			s_pConCommandBases = this;
		}
		else
		{
			// non-engine cvars, place at end of list
			ConCommandBase *cur = s_pConCommandBases;
			while (1)
			{
				if (!cur->m_pNext)
				{
					cur->m_pNext = this;
					m_pNext      = NULL;
					break;
				}
				cur = cur->m_pNext;
			}
		}
#endif
	}
	else
	{
		// It's unregistered
		m_pNext = NULL;
	}

	// If s_pAccessor is already set (this ConVar is not a global variable),
	//  register it.
	if ( s_pAccessor )
	{
		Init();
	}
}

//-----------------------------------------------------------------------------
// Purpose: Used internally by OneTimeInit to initialize.
//-----------------------------------------------------------------------------
void ConCommandBase::Init()
{
	if ( !s_pAccessor )
		return;
	
	if ( m_bRegistered )
		return;

	s_pAccessor->RegisterConCommandBase( this );

	m_bRegistered = true;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *name - 
// Output : ConCommandBase
//-----------------------------------------------------------------------------
ConCommandBase const *ConCommandBase::FindCommand( char const *name )
{
	ConCommandBase const *cmd = GetCommands();
	for ( ; cmd; cmd = cmd->GetNext() )
	{
		if ( !stricmp( name, cmd->GetName() ) )
			return cmd;
	}
	return NULL;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : ConCommandBase
//-----------------------------------------------------------------------------
const ConCommandBase *ConCommandBase::GetCommands( void )
{
	return s_pConCommandBases;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *var - 
// Output : static
//-----------------------------------------------------------------------------
void ConCommandBase::AddToList( ConCommandBase *var )
{
	// This routine is only valid on root ConCommandBases
	var->m_pNext = s_pConCommandBases;
	s_pConCommandBases = var;
}

//-----------------------------------------------------------------------------
// Purpose: Return name of the command/var
// Output : char const
//-----------------------------------------------------------------------------
char const *ConCommandBase::GetName( void ) const
{
	return m_pszName;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : flag - 
//-----------------------------------------------------------------------------
void ConCommandBase::RemoveFlaggedCommands( int flag )
{
	ConCommandBase	*pNewList;
	ConCommandBase  *pCommand, *pNext;

	pNewList = NULL;

	pCommand = s_pConCommandBases;
	while ( pCommand )
	{
		pNext = pCommand->m_pNext;
		if ( !( pCommand->m_nFlags & flag ) )
		{
			pCommand->m_pNext = pNewList;
			pNewList = pCommand;
		}
		else
		{
			// Unlink
			pCommand->m_pNext = NULL;
		}

		pCommand = pNext;
	}
	
	s_pConCommandBases = pNewList;
}

void ConCommandBase::RevertFlaggedCvars( int flag )
{
	for (const ConCommandBase *var= GetCommands() ; var ; var=var->GetNext())
	{
		if ( var->IsCommand() )
			continue;

		ConVar *cvar = ( ConVar * )var;

		if ( !cvar->IsBitSet( flag ) )
			continue;

		// It's == to the default value, don't count
		if ( !Q_strcasecmp( cvar->GetDefault(), cvar->GetString() ) )
			continue;

		cvar->Revert();

		// DevMsg( "%s = \"%s\" (reverted)\n", cvar->GetName(), cvar->GetString() );
	}
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : flag - 
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool ConCommandBase::IsBitSet( int flag ) const
{
	return ( flag & m_nFlags ) ? true : false;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : flags - 
//-----------------------------------------------------------------------------
void ConCommandBase::AddFlags( int flags )
{
	m_nFlags |= flags;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : const ConCommandBase
//-----------------------------------------------------------------------------
const ConCommandBase *ConCommandBase::GetNext( void ) const
{
	return m_pNext;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *next - 
//-----------------------------------------------------------------------------
void ConCommandBase::SetNext( ConCommandBase *next )
{
	m_pNext = next;
}

//-----------------------------------------------------------------------------
// Purpose: Copies string using local new/delete operators
// Input  : *from - 
// Output : char
//-----------------------------------------------------------------------------
char *ConCommandBase::CopyString( char const *from )
{
	int		len;
	char	*to;

	len = strlen( from );
	if ( len <= 0 )
	{
		to = new char[1];
		to[0] = 0;
	}
	else
	{
		to = new char[len+1];
		Q_strncpy( to, from, len+1 );
	}
	return to;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : char const
//-----------------------------------------------------------------------------
char const *ConCommandBase::GetHelpText( void ) const
{
	return m_pszHelpString;
}

//-----------------------------------------------------------------------------
// Purpose: Has this cvar been registered
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool ConCommandBase::IsRegistered( void ) const
{
	return m_bRegistered;
}

//-----------------------------------------------------------------------------
// Purpose: Constructs a console command
// Input  : *pName - name of command
//			callback - function to call upon execution
//			*pHelpString - help text for command
//			flags - command flags, if any
//-----------------------------------------------------------------------------
ConCommand::ConCommand( char const *pName, FnCommandCallback callback, char const *pHelpString /*= 0*/, int flags /*= 0*/, FnCommandCompletionCallback completionFunc /*= 0*/ )
{
	Create( pName, callback, pHelpString, flags, completionFunc );
}

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
ConCommand::~ConCommand( void )
{
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool ConCommand::IsCommand( void ) const
{ 
	return true;
}

//-----------------------------------------------------------------------------
// Purpose: Invoke the function if there is one
//-----------------------------------------------------------------------------
void ConCommand::Dispatch( void )
{
	if ( m_fnCommandCallback )
	{
		( *m_fnCommandCallback )();
	}
	else
	{
		// Command without callback!!!
		Assert( 0 );
	}
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *partial - 
//			context - 
//			longest - 
//			maxcommands - 
//			**commands - 
// Output : int
//-----------------------------------------------------------------------------
int DefaultCompletionFunc( char const *partial, char commands[ COMMAND_COMPLETION_MAXITEMS ][ COMMAND_COMPLETION_ITEM_LENGTH ] )
{
	return 0;
}

//-----------------------------------------------------------------------------
// Purpose: Create the named command
// Input  : *pName - 
//			callback - 
//			*pHelpString - 
//			flags - 
//-----------------------------------------------------------------------------
void ConCommand::Create( char const *pName, FnCommandCallback callback, char const *pHelpString /*= 0*/, int flags /*= 0*/, FnCommandCompletionCallback completionFunc /*=0*/ )
{
	// Set the callback
	m_fnCommandCallback = callback;

	m_fnCompletionCallback = completionFunc ? completionFunc : DefaultCompletionFunc;
	m_bHasCompletionCallback = completionFunc != 0 ? true : false;

	// Setup the rest
	BaseClass::Create( pName, pHelpString, flags );
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *partial - 
//			context - 
//			longest - 
//			maxcommands - 
//			**commands - 
//-----------------------------------------------------------------------------
int	ConCommand::AutoCompleteSuggest( char const *partial, char commands[ COMMAND_COMPLETION_MAXITEMS ][ COMMAND_COMPLETION_ITEM_LENGTH ] )
{
	Assert( m_fnCompletionCallback );
	if ( !m_fnCompletionCallback )
		return 0;

	return ( m_fnCompletionCallback )( partial, commands );
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool ConCommand::CanAutoComplete( void )
{
	return m_bHasCompletionCallback;
}

// ----------------------------------------------------------------------------- //
// ConVar.
// ----------------------------------------------------------------------------- //
ConVar::ConVar( char const *pName, char const *pDefaultValue, int flags /* = 0 */ )
{
	Create( pName, pDefaultValue, flags );
}

// ----------------------------------------------------------------------------- //
// ConVar.
// ----------------------------------------------------------------------------- //
ConVar::ConVar( char const *pName, char const *pDefaultValue, int flags, char const *pHelpString )
{
	Create( pName, pDefaultValue, flags, pHelpString );
}

// ----------------------------------------------------------------------------- //
// ConVar.
// ----------------------------------------------------------------------------- //
ConVar::ConVar( char const *pName, char const *pDefaultValue, int flags, char const *pHelpString, bool bMin, float fMin, bool bMax, float fMax )
{
	Create( pName, pDefaultValue, flags, pHelpString, bMin, fMin, bMax, fMax );
}

// ----------------------------------------------------------------------------- //
// ConVar.
// ----------------------------------------------------------------------------- //
ConVar::ConVar( char const *pName, char const *pDefaultValue, int flags, char const *pHelpString, FnChangeCallback callback )
{
	Create( pName, pDefaultValue, flags, pHelpString, false, 0.0, false, 0.0, callback );
}

// ----------------------------------------------------------------------------- //
// ConVar.
// ----------------------------------------------------------------------------- //
ConVar::ConVar( char const *pName, char const *pDefaultValue, int flags, char const *pHelpString, bool bMin, float fMin, bool bMax, float fMax, FnChangeCallback callback )
{
	Create( pName, pDefaultValue, flags, pHelpString, bMin, fMin, bMax, fMax, callback );
}

//-----------------------------------------------------------------------------
// Purpose: Free dynamic memory
//-----------------------------------------------------------------------------
ConVar::~ConVar( void )
{
	if ( m_pszString )
	{
		delete[] m_pszString;
		m_pszString = NULL;
	}
}


//-----------------------------------------------------------------------------
// Install a change callback (there shouldn't already be one....)
//-----------------------------------------------------------------------------

void ConVar::InstallChangeCallback( FnChangeCallback callback )
{
	Assert( !m_fnChangeCallback || !callback );
	m_fnChangeCallback = callback;

	if (m_fnChangeCallback)
	{
		// Call it immediately to set the initial value...
		m_fnChangeCallback( this, m_pszString );
	}
}

bool ConVar::IsBitSet( int flag ) const
{
	return ( flag & m_pParent->m_nFlags ) ? true : false;
}

char const *ConVar::GetHelpText( void ) const
{
	return m_pParent->m_pszHelpString;
}

void ConVar::AddFlags( int flags )
{
	m_pParent->m_nFlags |= flags;
}

bool ConVar::IsRegistered( void ) const
{
	return m_pParent->m_bRegistered;
}

char const *ConVar::GetName( void ) const
{
	return m_pParent->m_pszName;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : Returns true on success, false on failure.
//-----------------------------------------------------------------------------
bool ConVar::IsCommand( void ) const
{ 
	return false;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : 
//-----------------------------------------------------------------------------
void ConVar::Init()
{
	BaseClass::Init();
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *value - 
//-----------------------------------------------------------------------------
void ConVar::InternalSetValue( char const *value )
{
	float fNewValue;
	char  tempVal[ 32 ];
	char  *val;

	Assert(m_pParent == this); // Only valid for root convars.

	val = (char *)value;
	fNewValue = ( float )atof( value );

	if ( ClampValue( fNewValue ) )
	{
		Q_snprintf( tempVal,sizeof(tempVal), "%f", fNewValue );
		val = tempVal;
	}
	
	// Redetermine value
	m_fValue		= fNewValue;
	m_nValue		= ( int )( m_fValue );

	if ( !(m_nFlags & FCVAR_NEVER_AS_STRING) )
		ChangeStringValue( val );
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *tempVal - 
//-----------------------------------------------------------------------------
void ConVar::ChangeStringValue( char const *tempVal )
{
	Assert( !( m_nFlags & FCVAR_NEVER_AS_STRING ) );

 	char* pszOldValue = (char*)stackalloc( m_StringLength );
	memcpy( pszOldValue, m_pszString, m_StringLength );
	
	int len = Q_strlen(tempVal) + 1;

	if ( len > m_StringLength)
	{
		if (m_pszString)
			delete[] m_pszString;

		m_pszString	= new char[len];
		m_StringLength = len;
	}

	memcpy( m_pszString, tempVal, len );

	// Invoke any necessary callback function
	if ( m_fnChangeCallback )
	{
		m_fnChangeCallback( this, pszOldValue );
	}

	GetCVarIF()->CallGlobalChangeCallback( this, pszOldValue );

	stackfree( pszOldValue );
}

//-----------------------------------------------------------------------------
// Purpose: Check whether to clamp and then perform clamp
// Input  : value - 
// Output : Returns true if value changed
//-----------------------------------------------------------------------------
bool ConVar::ClampValue( float& value )
{
	if ( m_bHasMin && ( value < m_fMinVal ) )
	{
		value = m_fMinVal;
		return true;
	}
	
	if ( m_bHasMax && ( value > m_fMaxVal ) )
	{
		value = m_fMaxVal;
		return true;
	}

	return false;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *value - 
//-----------------------------------------------------------------------------
void ConVar::InternalSetFloatValue( float fNewValue )
{
	if ( fNewValue == m_fValue )
		return;

	Assert( m_pParent == this ); // Only valid for root convars.

	// Check bounds
	ClampValue( fNewValue );

	// Redetermine value
	m_fValue		= fNewValue;
	m_nValue		= ( int )m_fValue;

	if ( !( m_nFlags & FCVAR_NEVER_AS_STRING ) )
	{
		char tempVal[ 32 ];
		Q_snprintf( tempVal, sizeof( tempVal), "%f", m_fValue );
		ChangeStringValue( tempVal );
	}
	else
	{
		Assert( !m_fnChangeCallback );
	}
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *value - 
//-----------------------------------------------------------------------------
void ConVar::InternalSetIntValue( int nValue )
{
	if ( nValue == m_nValue )
		return;

	Assert( m_pParent == this ); // Only valid for root convars.

	float fValue = (float)nValue;
	if ( ClampValue( fValue ) )
	{
		nValue = ( int )( fValue );
	}

	// Redetermine value
	m_fValue		= fValue;
	m_nValue		= nValue;

	if ( !( m_nFlags & FCVAR_NEVER_AS_STRING ) )
	{
		char tempVal[ 32 ];
		Q_snprintf( tempVal, sizeof( tempVal ), "%d", m_nValue );
		ChangeStringValue( tempVal );
	}
	else
	{
		Assert( !m_fnChangeCallback );
	}
}

//-----------------------------------------------------------------------------
// Purpose: Private creation
//-----------------------------------------------------------------------------
void ConVar::Create( char const *pName, char const *pDefaultValue, int flags /*= 0*/,
	char const *pHelpString /*= NULL*/, bool bMin /*= false*/, float fMin /*= 0.0*/,
	bool bMax /*= false*/, float fMax /*= false*/, FnChangeCallback callback /*= NULL*/ )
{
	static char *empty_string = "";

	m_pParent = this;

	// Name should be static data
	m_pszDefaultValue	= pDefaultValue ? pDefaultValue : empty_string;
	Assert( pDefaultValue );

	m_StringLength = strlen( m_pszDefaultValue ) + 1;
	m_pszString = new char[m_StringLength];
	memcpy( m_pszString, m_pszDefaultValue, m_StringLength );
	
	m_bHasMin = bMin;
	m_fMinVal = fMin;
	m_bHasMax = bMax;
	m_fMaxVal = fMax;
	
	m_fnChangeCallback = callback;

	m_fValue = ( float )atof( m_pszString );

	// Bounds Check, should never happen, if it does, no big deal
	if ( m_bHasMin && ( m_fValue < m_fMinVal ) )
	{
		Assert( 0 );
	}

	if ( m_bHasMax && ( m_fValue > m_fMaxVal ) )
	{
		Assert( 0 );
	}

	m_nValue = ( int )m_fValue;

	BaseClass::Create( pName, pHelpString, flags );
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : *value - 
//-----------------------------------------------------------------------------
void ConVar::SetValue(char const *value)
{
	ConVar *var = ( ConVar * )m_pParent;
	var->InternalSetValue( value );
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : value - 
//-----------------------------------------------------------------------------
void ConVar::SetValue( float value )
{
	ConVar *var = ( ConVar * )m_pParent;
	var->InternalSetFloatValue( value );
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : value - 
//-----------------------------------------------------------------------------
void ConVar::SetValue( int value )
{
	ConVar *var = ( ConVar * )m_pParent;
	var->InternalSetIntValue( value );
}

//-----------------------------------------------------------------------------
// Purpose: Reset to default value
//-----------------------------------------------------------------------------
void ConVar::Revert( void )
{
	// Force default value again
	ConVar *var = ( ConVar * )m_pParent;
	var->SetValue( var->m_pszDefaultValue );
}

//-----------------------------------------------------------------------------
// Purpose: 
//-----------------------------------------------------------------------------
void ConVar::RevertAll( void )
{
	ConCommandBase *p = s_pConCommandBases;
	while ( p  )
	{
		if ( !p->IsCommand() )
		{
			ConVar *var = ( ConVar * )p;
			var->Revert();
		}
		p = p->m_pNext;
	}
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : minVal - 
// Output : true if there is a min set
//-----------------------------------------------------------------------------
bool ConVar::GetMin( float& minVal ) const
{
	minVal = m_pParent->m_fMinVal;
	return m_pParent->m_bHasMin;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Input  : maxVal - 
//-----------------------------------------------------------------------------
bool ConVar::GetMax( float& maxVal ) const
{
	maxVal = m_pParent->m_fMaxVal;
	return m_pParent->m_bHasMax;
}

//-----------------------------------------------------------------------------
// Purpose: 
// Output : char const
//-----------------------------------------------------------------------------
char const *ConVar::GetDefault( void ) const
{
	return m_pParent->m_pszDefaultValue;
}

