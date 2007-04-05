;;;;
;; (C)2005-2007 AlliedModders LLC
;; By the Metamod:Source Development Team
;; This software is licensed under the zlib/libpng free software license.
;;
;; This assembly file is a short thunk wrapper to let us load as a VSP and exit quietly,
;; without any overhead of the rest of the interface, and also to prevent linking against
;; tierX or vstdlib
;;;;

;;Exports:
;; void GetBaseDir(char *buffer, maxlength);
;; void *GetThisPointer();
;;Imports:
;; void LoadFunction();

section .text

global GetThisPointer, GetGameDir, ServerCommand
global _GetThisPointer, _GetGameDir, _ServerCommand
global _GetICvar
extern _LoadFunction

_GetICvar
	mov 	eax, [icvar]
	ret

GetThisPointer:
_GetThisPointer:
	mov		eax, GLOBAL_POINTER
	ret
	
GetGameDir:
_GetGameDir:
	push	ebp
	mov		ebp, esp			
	
	mov		ecx, [engine]		;get this pointer
	mov		edx, [ecx]			;get the vtable
	push	dword [ebp+12]		;push maxlenth
	push	dword [ebp+8]		;push buffer
	%ifdef LINUX
	push	ecx					;push this pointer
	%endif
	call	dword [edx+216]		;call IVEngineServer::GetGameDir
	%ifdef LINUX
	add		esp, 12				;correct stack
	%endif
	
	pop 	ebp
	ret
	
ServerCommand
_ServerCommand:
	push	ebp
	mov		ebp, esp
	
	mov		ecx, [engine]		;get this pointer
	mov		edx, [ecx]			;get the vtable
	push	dword [ebp+8]		;push string
	%ifdef LINUX
	push	ecx					;push this pointer
	%endif
	call	dword [edx+144]		;call IVEngineServer::ServerCommand
	%ifdef LINUX
	add		esp, 8				;correct stack
	%endif
	
	pop		ebp
	ret
	
thisLoadFunction:
	push	ebp
	mov		ebp, esp
	
	push	edi
	
	;get factory
	%ifdef LINUX
	mov		edi, [ebp+12]
	%else
	mov		edi, [ebp+8]
	%endif
	
	push	dword 0				;NULL
	push	dword VENGINESERVER	;iface name
	call	edi					;call factory
	add		esp, 8				;correct stack
	
	test	eax, eax			;do we have a valid pointer?
	jz		.end				;no, bail out
	
	mov		[engine], eax		;store the engine pointer
	
	push	dword 0				;NULL
	push	dword VENGINECVAR	;iface name
	call	edi					;call factory
	add		esp, 8				;correct stack
	
	test	eax, eax			;do we have a valid pointer?
	jz		.end				;no, bail out
	
	mov		[icvar], eax		;store the icvar pointer
	
	call	_LoadFunction
	
.end:
	;We never load, never ever ever!
	xor		eax, eax
	
	pop		edi
	
	pop		ebp
	%ifdef LINUX
	ret
	%else
	retn	8
	%endif
	
thisUnloadFunction:
	ret
	
section .data
	INTERFACE_NAME		DB		"ISERVERPLUGINCALLBACKS001", 0
	VENGINESERVER		DB		"VEngineServer021", 0
	VENGINECVAR			DB		"VEngineCvar003", 0
	
	VIRTUAL_TABLE		DD		thisLoadFunction
						DD		thisUnloadFunction
						;We don't need any more of the vtable here
								
	GLOBAL_POINTER		DD		VIRTUAL_TABLE

	temp_ret			DD		0
	temp_ptr			DD		temp_ret
	engine				DD		0
	icvar				DD		0