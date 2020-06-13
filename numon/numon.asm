;=============================================================================
;FILE:	MAIN.XXX
;
;DESC:	Template for MAIN program module
;
;NOTES:	Use this template file when creating the main module for a new
;	program. Use MODULE.XXX when creating additional modules.
;=============================================================================


include		MODEL.INC


.codeseg	lib
.extrn		start:auto, exit:auto, exit_ok:auto
;----------------------------------------------------------
; Declare external library functions here...
;----------------------------------------------------------
.ends


.dataseg
;----------------------------------------------------------
; Declare external library and program data here...
;----------------------------------------------------------
;----------------------------------------------------------
; Declare local data here...
;----------------------------------------------------------
.ends


IF NOT __TINY__
.stackseg
.public		stack_start		
.label		stack_start	word
		db	1024 dup(?)	;define a 1024 byte stack
.ends
ENDIF


;=============================================================================
;FUNC:	MAIN
;
;DESC:	Main body of program.
;
;IN:	DX		segment address of PSP
;
;ASUMS:	DS,ES		@DATASEG  (same as @CODESEG in TINY model)
;	SS		@STACKSEG (same as @CODESEG in TINY model)
;=============================================================================

.codeseg

IF __TINY__
assume		cs:@codeseg, ds:@dataseg, es:@dataseg, ss:@dataseg
ELSE
assume		cs:@codeseg, ds:@dataseg, es:@dataseg, ss:@stackseg
ENDIF

.public		main
.proc		main		auto
;----------------------------------------------------------
; Code for MAIN goes here...
;----------------------------------------------------------
                mov             ax,40h
                mov             ds,ax
                mov             ax,ds:[17h]
                or              ax,32d
                mov             ds:[17h],ax
		ret			;exit to DOS with ERRORLEVEL=0
.endp		main

;----------------------------------------------------------
; Declare additional functions here...
;----------------------------------------------------------

.ends







;=============================================================================
; Stack normalization and memory management initialization labels
;
; NOTE: These declarations must remain after the declaration of the stack
; and anything in the stack segment. These labels define the end of the
; stack and the program, which is where the near and far heaps are placed
; by default. These declarations do not affect the size of the program and
; may be left here even if the stack is not normalized and the heaps are
; not used.
;=============================================================================

.public		nheap_default, fheap_default
IF NOT __TINY__
.stackseg
  IF __SMALL__ OR __MEDIUM__
.public		stack_end		;used by START to normalize stack
.label		stack_end	word	;must be defined past entire stack
  ENDIF
.label		nheap_default	word	;used by the near heap
.label		fheap_default	word	;used by the far heap
.ends
ELSE
_BSSEND		segment	byte public 'STACK'
.label		nheap_default	word	;used by the near heap
.label		fheap_default	word	;used by the far heap
_BSSEND		ends
% @codeseg	group	_BSSEND
ENDIF


		end	start		;specify START as starting address
