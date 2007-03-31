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

global GetThisPointer, GetGameDir
global _GetThisPointer, _GetGameDir
extern _LoadFunction

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
	%ifdef LINUX
	push	ecx					;push this pointer
	%endif
	push	dword [ebp+12]		;push maxlenth
	push	dword [ebp+8]		;push buffer
	call	dword [edx+216]		;call IVEngineServer::GetGameDir
	%ifdef LINUX
	add		esp, 8				;correct stack
	%endif
	
	pop ebp
	ret

thisLoadFunction:
	push	ebp
	mov		ebp, esp
	
	;get factory
	mov		eax, [ebp+8]
	
	push	dword 0				;NULL
	push	dword VENGINESERVER	;iface name
	call	eax					;call factory
	add		esp, 8				;correct stack
	
	test	eax, eax			;do we have a valid pointer?
	jz		.end				;no, bail out
	
	mov		[engine], eax		;store the engine pointer
	
	call	_LoadFunction
	
.end:
	;We never load, never ever ever!
	xor		eax, eax
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
	
	VIRTUAL_TABLE		DD		thisLoadFunction
						DD		thisUnloadFunction
						;We don't need any more of the vtable here
								
	GLOBAL_POINTER		DD		VIRTUAL_TABLE

	temp_ret			DD		0
	temp_ptr			DD		temp_ret
	engine				DD		0
