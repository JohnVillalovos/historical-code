;=============================================================================
;FILE:  LoggedIn.asm
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
.extrn  get_version:auto,get_rootenvseg:auto,set_env:auto,cput_chr:auto
.extrn  get_version:auto,arg_next:auto,str_upr:auto,str_cpye:auto
.extrn  cput_str:auto,str_len:auto,console_init:auto
.extrn  buf_cpy:auto

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
                db      2048 dup(?)     ;define a 1024 byte stack
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
        call    console_init
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
        mov     ax,7A00h                ; Check for IPX
        int     2fh
        .IF     al != 0FFh              ; Is it loaded?
                jmp     No_IPX          ; If not then jump
        .ENDIF
        mov     Word PTR EnterIPX,di    ; else set up for calling IPX/SPX
        mov     ax,es                   ; by loading in values
        mov     Word PTR EnterIPX+2,AX
        mov     bx,0010h                ; Prepare for SPX Initialize
        mov     al,00h
        call    dword ptr EnterIPX
        .IF     al != 0FFh              ; Is SPX loaded?
                jmp     No_IPX          ; If not then jump
        .ENDIF
        mov     ax,0F002h               ; See if NetX is loaded
        int     21h
        .IF     al == 0h                ; Is NetX loaded?
                jmp     No_NetX         ; If not then jump
        .ENDIF
        push    ds                      ; put segment
        pop     es                      ; into es
        mov     ah,0DCh                 ; Get Connection Number
        int     21h
        mov     Request2.Get_Conn.ConnectionNum,al      ; Store connection
                                                        ; Number
        mov     al,16h                  ; Get Connection Information call
        mov     Request2.Get_Conn.Function, al
        mov     ax,2h                   ; Buffer Length
        mov     Request2.Get_Conn.RequestLen, ax
        mov     ax,62d
        mov     Reply2.Get_Conn.ReplyLen , ax

        mov     si, offset Request2     ; Point to Request Buffer
        mov     di, offset Reply2       ; Point to Reply Buffer
        mov     ah, 0E3h
        int     21h                     ; Make Get Connection Info call

        .IF     al != 00h
                jmp     NotLogged           ; If error then jump.  Assume
                                            ; not logged in
        .ENDIF
        mov     dx, word ptr Reply2.Get_Conn.ObjectID
        cmp     dx,0h
        jnz     LoggedIn
        mov     dx, word ptr Reply2.Get_Conn.ObjectID+2
        jz      NotLogged
LoggedIn:
        mov     al,Quiet
        cmp     al,0
        jnz     Good1
        mov     si,offset @dataseg:LoggedInMesg
        call    cput_str
Good1:
        jmp     exit_ok         ; exit program with ok value
No_IPX:
        mov     al,01h
        mov     si, offset @dataseg:No_IPX_Message
        jmp     PrintExit
No_NetX:
        mov     al,02h
        mov     si,offset @dataseg:NoNetXMesg
        jmp     PrintExit
Error:
        mov     al,03h
        mov     si, offset @dataseg:Unknown_Error
        jmp     PrintExit
Bad_CommandLine:
        mov     al,Quiet
        cmp     al,0
        jnz     Bad_CommandLine1
        mov     si,offset @dataseg:Copyright
        call    cput_str
Bad_CommandLine1:
        mov     si, offset @dataseg:Bad_CommandLine_Mesg
        mov     al,04h
        jmp     PrintExit
NotLogged:
        mov     al,05h
        mov     si,offset @dataseg:NotLoggedMesg
        jmp     PrintExit
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

CopyRight       db      'LoggedIn 1.00 beta is Copyright 1993 by John L. Villalovos',0dh,0ah
                db      'of Seriously Sound.  All Rights Reserved.  This version of the',0Dh,0Ah
                db      'program and previous versions are freeware.  Future versions may',0dh,0ah
                db      'become shareware. ',0dh,0ah
                db      'Phone USA: (503) 753-7883.  Internet mail: villalj@xanth.cs.orst.edu',0dh,0ah
                db      0ah,'Seriously Sound',0dh,0ah,'563 S.W. Adams',0dh,0ah
                db      'Corvallis, OR  97333',0dh,0ah
                db      0dh,0ah,0h

NotLoggedMesg   db      'You are not logged in.',0dh,0ah,0
LoggedInMesg    db      'You are logged in.',0dh,0ah,0h

NoNetXMesg      db      'NetX must be loaded for this to work.',0dh,0ah
                db      0

Unknown_Error   db      'An unknown error occured',0dh,0ah,0

No_IPX_Message  db      'IPX and SPX Must be loaded for this to work.',0Dh,0Ah,0
Bad_CommandLine_Mesg:
                db      'Invalid command line parameter.',0dh,0ah
                db      'LOGGEDIN [/Q]',0dh,0ah
                db      '/Q Quiet mode.  If no errors then no output is '
                db      'displayed.',0dh,0ah,0
LineFeed        db      0Dh,0Ah,0

Bad_DOS_Message db      'This program requires DOS 2.0 or greater.',0dh,0ah,0,'$'

EnterIPX        dd      0
Quiet           db      00h

LoginTime_Struc         STRUC
        Year            DB      0
        Month           DB      0
        Day             DB      0
        Hour            DB      0
        Minute          DB      0
        Second          DB      0
        DayNum          DB      0
LoginTime_Struc ENDS

Req_ReadProp    STRUC
        RequestLen      DW      70d
        Function        DB      3Dh
        ObjectType      DW      0
        ObjectNameLen   DB      0
        ObjectName      DB      48 dup (' ')
        SegmentNumber   DB      0
        PropertyNameLen DB      0
        PropertyName    DB      15 dup (' ')
Req_ReadProp    ENDS

Rep_ReadProp    STRUC
        ReplyLen        DW      130d
        PropertyValue   DB      128 dup (0)
        MoreSegments    DB      0
        PropertyFlags   DB      0
Rep_ReadProp    ENDS

Req_Get_Conn_Struc      STRUC
        RequestLen      DW      2h
        Function        DB      16h
        ConnectionNum   DB      0
Req_Get_Conn_Struc      ENDS

Rep_Get_Conn_Struc      STRUC
        ReplyLen        DW      0
        ObjectID        DD      0
        ObjectType      DW      0
        ObjectName      DB      48 dup (' ')
        LoginTime       LoginTime_Struc {}
Rep_Get_Conn_Struc      ENDS

RequestTwo      UNION
        Get_Conn        Req_Get_Conn_Struc {}
        ReadProp        Req_ReadProp {}
RequestTwo      ENDS

ReplyTwo        UNION
        Get_Conn        Rep_Get_Conn_Struc  {}
        ReadProp        Rep_ReadProp {}
ReplyTwo        ENDS

Request2        RequestTwo {}
Reply2          ReplyTwo {}

Command_Buffer  db      128 dup (0)
scrbuf_addr	dw	?
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
