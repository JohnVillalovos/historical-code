;=============================================================================
;FILE:  nodeid.asm
;
;DESC:	Template for MAIN program module
;
;NOTES:	Use this template file when creating the main module for a new
;	program. Use MODULE.XXX when creating additional modules.
;=============================================================================


include		MODEL.INC
include         console.inc


.codeseg	lib
.extrn		start:auto, exit:auto, exit_ok:auto
;----------------------------------------------------------
; Declare external library functions here...
;----------------------------------------------------------
.extrn  byte_to_hexe:auto,get_version:auto,get_rootenvseg:auto,set_env:auto
.extrn  get_version:auto,adapter_type:auto,console_init:auto
.extrn  cput_str:auto,arg_next:auto,cput_chr:auto,str_upr:auto

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



IF NOT __TINY__
        mov     ax,@dataseg
        mov     ds,ax
ENDIF
        push    ds                      ; put data segment
        pop     es                      ; into es
        call    console_init            ; initialize console


        call    get_version             ; check DOS Version
        .IF     ah < 2h                 ; Check if DOS 2.0 or greater
                jmp     Bad_Dos         ; If less then DOS 2.0 then jump
        .ENDIF
Command_Loop:
        mov     si,offset Command_Buffer        ; point to buffer for command
        call    arg_next                        ; line and get next argument
        jc      No_arguments                    ; jump if no arguments
        call    str_upr                         ; convert command line to U/C
        cmp     byte ptr [si],'/'               ; see if command option
        jnz     Bad_CommandLine                 ; jump if no option
        inc     si                              ; bump pointer
        cmp     byte ptr [si],'N'               ; see if No NETX error message
        jnz     @F                              ; jump if not
        mov     al,1                            ; Set no NetX error message
        mov     NoNetXError,al                  ; store value
        jmp     Command_Loop
@@:
        cmp     byte ptr [si],'Q'               ; see if quiet mode?
        jnz     Bad_CommandLine                 ; If not then bad command line
        mov     al,1                            ; Say quiet mode
        mov     Quiet,al                        ; store value
        jmp     Command_Loop
No_arguments:
        mov     al,Quiet
        cmp     al,0
        jnz     No_Arg1
        mov     si,offset @dataseg:Copyright
        call    cput_str
No_Arg1:
        xor     ax,ax                   ; zero ax
        mov     al,byte ptr @dataseg:adapter_type        ; get adapter type
        cmp     al,4h                   ; see if adapter type is known
        ja      Unknown_adapter         ; If don't know it then jump
        mov     si,offset @dataseg:VideoTypes   ; point to VideoTypes
        mov     cl,2                    ; put shift count into cl
        shl     al,cl                   ; multiply al*4
        add     ax,si                   ; add offset to ax
        mov     si,ax                   ; put into si
        mov     di,offset @dataseg:VideoTypeA    ; point to Video Types
        mov     cx,4                    ; loop 4 times
        cld                             ; clear direction flag
        rep     movsb
Unknown_adapter:
        mov     ax,7A00h                ; Check for IPX
        int     2fh
        cmp     al,0FFh                 ; Is it loaded?
        jne     No_IPX                  ; If not then jump
        mov     Word PTR EnterIPX,di    ; else set up for calling IPX/SPX
        mov     ax,es                   ; by loading in values
        mov     Word PTR EnterIPX+2,AX
        mov     bx,0010h                ; Prepare for SPX Initialize
        mov     al,00h
        call    dword ptr EnterIPX
        mov     ax,0F002h               ; See if NetX is loaded
        int     21h
        cmp     al,00h                  ; Is NetX loaded?
        jnz     NetX_Available          ; If so then jump
        mov     al,NoNetXError          ; Check if we display error message
        cmp     al,0
        jnz     NetX_Available
        mov     si,offset @dataseg:NoNetXMesg
        call    cput_str                ; Print a warning message
NetX_Available:
        push    ds                      ; put segment
        pop     es                      ; into es
        mov     si, offset @dataseg:Reply        ; point to reply buffer ES:SI
        mov     bx,09h                  ; call IPX Get Internetwork Address
        call    dword ptr EnterIPX
        mov     di,offset @dataseg:Reply
        mov     si,offset @dataseg:NetNumberA
        mov     cx,4
        call    Looper
        mov     di,offset @dataseg:Reply+4
        mov     si,offset @dataseg:Node1A
        mov     cx,2
        call    Looper
        mov     di,offset @dataseg:Reply+6
        mov     si,offset @dataseg:Node2A
        mov     cx,4
        call    Looper
        call    get_version
        call    get_rootenvseg
        mov     al,byte ptr @dataseg:adapter_type
        cmp     al,4
        ja      VidDoNotKnow
        mov     si,offset @dataseg:VideoType
        call    set_env
        jc      No_Env
        mov     al,Quiet
        cmp     al,0
        jnz     VidDoNotKnow
        mov     si,offset @dataseg:VideoMesg
        call    cput_str
        mov     si,offset @dataseg:VideoType
        call    cput_str
        call    CRLF
VidDoNotKnow:
        mov     si,offset @dataseg:NetNumber
        call    set_env
        jc      No_Env
        mov     al,Quiet
        cmp     al,0
        jnz     @F
        mov     si,offset @dataseg:NetNumberMesg
        call    cput_str
        mov     si,offset @dataseg:NetNumber
        call    cput_str
        call    CRLF
@@:
        mov     si,offset @dataseg:Node1
        call    set_env
        jc      No_Env
        mov     al,Quiet
        cmp     al,0
        jnz     @F
        mov     si,offset @dataseg:Node1Mesg
        call    cput_str
        mov     si,offset @dataseg:Node1
        call    cput_str
        call    CRLF
@@:
        mov     si,offset @dataseg:Node2
        call    set_env
        jc      No_env
        mov     al,Quiet
        cmp     al,0
        jnz     @F
        mov     si,offset @dataseg:Node2Mesg
        call    cput_str
        mov     si,offset @dataseg:Node2
        call    cput_str
        call    CRLF
@@:
        mov     ax,0F002h               ; See if NetX is loaded
        int     21h
        cmp     al,00h                  ; Is NetX loaded?
        jz      No_NetX                 ; If so then jump
        mov     al,Quiet
        cmp     al,0
        jnz     @F
        mov     si,offset @dataseg:SuccessMesg
        call    cput_str
@@:
        ret                             ;exit to DOS


CRLF:
        mov     si,offset @dataseg:LineFeed
        jmp     cput_str
Looper:
        mov     ax,[di]
        call    byte_to_hexe
        inc     di
        loop    Looper
        ret
No_NetX:
        mov     al,01h
        jmp     exit
No_IPX:
        mov     al,03h
        mov     si, offset @dataseg:No_IPX_Message
        jmp     PrintExit
No_Env:
        mov     al,02h
        mov     si, offset @dataseg:No_Env_Message
        jmp     PrintExit
Too_Long2:
        mov     al,06h
        jmp     PrintExit
Bad_CommandLine:
        mov     al,Quiet
        cmp     al,0
        jnz     Bad_CommandLine1
        mov     si,offset @dataseg:Copyright
        call    cput_str
Bad_CommandLine1:
        mov     si, offset @dataseg:Bad_CommandLine_Mesg
        jmp     Too_Long2
Bad_DOS:
        mov     al,0ffh
        mov     si,offset @dataseg:Bad_DOS_Message
PrintExit:
        push    ax
        push    si
        call    PrintCopyright
        mov     al,07h                  ; beep
        call    cput_chr
        pop     si
        pop     ax
        call    cput_str
        jmp     exit
PrintCopyright:
        mov     al,Quiet
        cmp     al,0
        jz      PrintCopyright1
        mov     si,offset @dataseg:Copyright
        call    cput_str
PrintCopyright1:
        ret



.dataseg

CopyRight       db      'NodeId 1.04 is Copyright 1993 by John L. Villalovos',0dh,0ah
                db      'of Seriously Sound.  All Rights Reserved.  This version of the',0Dh,0Ah
                db      'program and previous versions are freeware.  Future versions may',0dh,0ah
                db      'become shareware. ',0dh,0ah
                db      'Phone USA: (503) 753-7883.  Internet mail: villalj@xanth.cs.orst.edu',0dh,0ah
                db      0ah,'Seriously Sound',0dh,0ah,'563 S.W. Adams',0dh,0ah
                db      'Corvallis, OR  97333',0dh,0ah
                db      0dh,0ah,0h
NoNetXMesg      db      'NetX is not loaded.  The NETNUM environment variable may be wrong if',0dh,0ah
                db      'the station has not yet received a server advertisement.  This will',0dh,0ah
                db      'be evident by a NETNUM=00000000.  If you do not use NetX then ignore',0dh,0ah
                db      'this message.',0dh,0ah,0
VideoMesg       db      'VIDEO environment variable set to current video type...',0dh,0ah,0
NetNumberMesg   db      'NETNUM environment variable set to network address/number...',0dh,0ah,0
Node1Mesg       db      'NODE1 environment variable set to first 4 bytes of node address...',0dh,0ah,0
Node2Mesg       db      'NODE2 environment variable set to last 8 bytes of node address...',0dh,0ah,0
SuccessMesg     db      'Program successful',0dh,0ah,0
No_IPX_Message  db      'IPX Must be loaded for this to work.',0Dh,0Ah,0
No_Env_Message  db      'Not enough environment space for all environment'
                db      ' variables.'
LineFeed        db      0Dh,0Ah,0
Bad_DOS_Message db      'This program requires DOS 2.0 or greater.',0dh,0ah,0,'$'
EnterIPX        dd      0
Handle          dw      0000h
NetNumber       db      'NETNUM='
NetNumberA      db      8 dup (0)
                db      0,0dh,0ah,'$'
Node1           db      'NODE1='
Node1A          db      4 dup (0)
                db      0,0dh,0ah,'$'
Node2           db      'NODE2='
Node2A          db      8 dup (0)
                db      0,0dh,0ah,'$'
VideoType       db      'VIDEO='
VideoTypeA      db      4 dup (0),0,0dh,0ah,'$'
VideoTypes      db      'MDA',0
                db      'CGA',0
                db      'EGA',0
                db      'MCGA'
                db      'VGA',0
Reply           dw      0h
                db      8 dup (?)
                db      '1234567890'
CommandBuffer   db      128 dup (?)
scrbuf_addr	dw	?
Quiet           db      00h
NoNetXError     db      00h
Bad_CommandLine_Mesg:
                db      'Invalid command line parameter.',0dh,0ah
                db      'NODEID [/Q] [/N]',0dh,0ah
                db      '/Q Quiet mode.  If no errors then no output is '
                db      'displayed.',0dh,0ah
                db      '/N No NetX missing message will be displayed'
                db      0dh,0ah,0
Command_Buffer  db      128 dup (0)
.ends

.endp           main

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
