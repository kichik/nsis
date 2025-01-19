;# 
;# This file is a part of NSIS.
;#
;# Copyright (C) 2014-2025 Anders Kjersem
;#
;# Licensed under the zlib/libpng license (the "License");
;# you may not use this file except in compliance with the License.
;#
;# Licence details can be found in the file COPYING.
;#
;# This software is provided 'as-is', without any express or implied
;# warranty.
;# 
;#
;# MASM:
;# ml64.exe /c Call-amd64.S
;#
;# Notes:
;# * MASM does not accept 0x* constants and GAS does not accept *h constants in Intel mode, must use decimal!
;#

; .if 0
;# MASM

SECTION_DATA equ .data
SECTION_CODE equ .code

FUNC_DECL MACRO name
name PROC
ENDM
FUNC_END MACRO name
name ENDP
ENDM

;# ~MASM
IF 0
; .else
;# GNU
.intel_syntax noprefix
.set __GNU__,1

#define IFDEF .ifdef
#define ELSE  .else
#define ENDIF .endif

#define SECTION_DATA .data
#define SECTION_CODE .text

#define END          .end
#define EXTERN       .extern

.macro FUNC_DECL name
.global \name
.func   \name
\name:
.endm
.macro FUNC_END name
.endfunc
.endm

;# ~GNU
ENDIF


IFDEF SYSTEM_LOG_DEBUG
  EXTERN __imp_IsDebuggerPresent : PROC
ENDIF

EXTERN GetProcOffset       : PROC
EXTERN GetParamsOffset     : PROC
EXTERN GetSizeOfProcParam  : PROC
EXTERN GetValueOffsetParam : PROC
EXTERN SetCallProcResultValues   : PROC


SECTION_CODE


FUNC_DECL CallProc2 ;# rcx=SystemProc* edx=ParamCount
  mov [rsp+8], r12
  mov [rsp+16], r13
  mov [rsp+24], r14
  ;#mov [rsp+32], r15

  ;# The stack is unaligned on function entry. We have to calculate the required 
  ;# stack size for our parameters + maybe 8 padding bytes to end up 16 byte aligned.
IFDEF __GNU__
#define pSystemProc r14
ELSE
  pSystemProc equ r14
ENDIF
  mov pSystemProc, rcx ;# Save SystemProc*
  ;# Not required since we zero-extend eax: xor rax, rax
  mov r13d, edx ;# Save ParamCount
  imul eax, edx, 8
  and edx, 1
  jnz noparamalignpadding
  lea eax, [eax+8] ;# sizeof(params) + 8 will make us 16 byte aligned
noparamalignpadding:
  cmp eax, 40 ;# The ABI guarantees shadow space for the 4 register parameters
  ja computedstacksize
  mov eax, 40 ;# Minimum (4*8) + 8 to align
computedstacksize:
  mov r12d, eax ;# Save stack size (Zero-extended mov)
  sub rsp, r12

IFDEF SYSTEM_LOG_DEBUG
  ;# BUGBUG: Remove this
  call qword ptr [__imp_IsDebuggerPresent]
  test eax, eax
  jz nodbgbrk
  int 3
  nodbgbrk:
ENDIF

  ;# We are going to set all stack parameters including the first 4, 
  ;# it does not hurt to do that and it allows us to copy them to 
  ;# their registers without reading pSystemProc->Params[1..3] again
  call GetSizeOfProcParam
  mov r9, rax ;# Store sizeof(ProcParameter)
  call GetValueOffsetParam
  mov r8, rax ;# Store FIELD_OFFSET(ProcParameter,Value)
  call GetParamsOffset
  lea r10, [pSystemProc+rax] ;# Store pSystemProc+FIELD_OFFSET(SystemProc,Params)
  mov ecx, r13d ;# Zero-extended mov
  test rcx, rcx
  jz callthefunc
setparameter:
  mov rax, r9
  mul rcx ;# rax = sizeof(ProcParameter) * paramidx (paramidx is 1 based because the return value is stored in Params[0])
  add rax, r10 ;# rax += pSystemProc->Params
  mov rax, qword ptr [rax+r8] ;# rax = pSystemProc->Params[paramidx].Value
  dec rcx
  mov [rsp+(8*rcx)], rax
  inc rcx
  loop setparameter
  ;# The 4 parameter registers are all volatile so we might as well assign all of them:
  ;# setparam4:
  ;#   cmp r13d, 4
  ;#   jb setparam3
  mov r9, [rsp+(8*3)]
  ;# setparam3:
  ;#   cmp r13d, 3
  ;#   jb setparam2
  mov r8, [rsp+(8*2)]
  ;# setparam2:
  ;#   cmp r13d, 2
  ;#   jb setparam1
  mov rdx, [rsp+(8*1)]
  ;# setparam1:
  ;#   cmp r13d, 1
  ;#   jb callthefunc
  mov rcx, [rsp+(8*0)]

callthefunc:
  call GetProcOffset
  mov r10, qword ptr [pSystemProc+rax]
  xor rax, rax ;# Fix bug #1535007
  call r10
  mov rcx, pSystemProc
  mov rdx, rax ;# Return value
  call SetCallProcResultValues ;# Store GetLastError() and return value
  ;# mov rax, pSystemProc has been performed by SetCallProcResultValues

  ;# Epilog:
  ;# http://msdn.microsoft.com/en-us/library/tawsa7cb claims that only 
  ;# add/lea rsp and pop is valid in the epilog. Unwind might fail on our version?
  add rsp, r12 ;# Restore stack
  ;# Restore nonvolatile registers:
  mov r12, [rsp+8]
  mov r13, [rsp+16]
  mov r14, [rsp+24]
  ;#mov r15, [rsp+32]
  ret
FUNC_END CallProc2


END
