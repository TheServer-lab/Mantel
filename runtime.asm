; =========================
; Mantel Runtime Library v0.4.1
; FIXED: read_int now works correctly
; Windows x64 version
; =========================

bits 64
default rel

extern ExitProcess
extern GetStdHandle
extern WriteConsoleA
extern ReadConsoleA

; Windows constants
STD_INPUT_HANDLE  equ -10
STD_OUTPUT_HANDLE equ -11

section .data
    ; Format strings
    fmt_int:        db "%lld", 0
    fmt_newline:    db 10, 0
    
    ; Buffers
    int_buffer:     times 32 db 0
    temp_buffer:    times 256 db 0

section .bss
    bytes_read:     resq 1

section .text

; ========================
; Global exports
; ========================
global print_int
global print_float
global print_string
global print_char
global print_bool
global println
global read_int
global read_string
global str_length
global str_concat

; ========================
; Print Integer
; ========================
print_int:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    ; Save the value
    mov [rsp+32], rcx
    
    ; Convert to string manually
    mov rax, rcx
    lea rdi, [int_buffer]
    
    ; Handle negative
    test rax, rax
    jns .positive
    neg rax
    mov byte [rdi], '-'
    inc rdi
    
.positive:
    ; Convert number to string
    mov rbx, rdi
    mov rcx, 10
    
.convert:
    xor rdx, rdx
    div rcx
    add dl, '0'
    mov [rdi], dl
    inc rdi
    test rax, rax
    jnz .convert
    
    ; Reverse the string
    dec rdi
    mov rsi, rbx
.reverse:
    cmp rsi, rdi
    jge .done_reverse
    mov al, [rsi]
    mov bl, [rdi]
    mov [rdi], al
    mov [rsi], bl
    inc rsi
    dec rdi
    jmp .reverse
    
.done_reverse:
    ; Print the string
    lea rcx, [int_buffer]
    call print_string
    
    add rsp, 64
    pop rbp
    ret

; ========================
; Print String
; ========================
print_string:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    ; Save string pointer
    mov [rsp+32], rcx
    
    ; Get string length
    push rcx
    call str_length
    pop rcx
    mov r9, rax  ; length
    
    ; Get stdout handle
    push rcx
    push r9
    mov rcx, STD_OUTPUT_HANDLE
    call GetStdHandle
    pop r9
    pop rcx
    
    ; Write to console
    mov r8, r9              ; nNumberOfCharsToWrite
    mov rdx, [rsp+32]       ; lpBuffer
    mov rcx, rax            ; hConsoleOutput
    lea r9, [bytes_read]    ; lpNumberOfCharsWritten
    push 0                  ; lpReserved
    call WriteConsoleA
    add rsp, 8
    
    add rsp, 64
    pop rbp
    ret

; ========================
; Print Character
; ========================
print_char:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    ; Store character in buffer
    mov [temp_buffer], cl
    mov byte [temp_buffer+1], 0
    
    ; Print as string
    lea rcx, [temp_buffer]
    call print_string
    
    add rsp, 64
    pop rbp
    ret

; ========================
; Print Boolean
; ========================
print_bool:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    test rcx, rcx
    jz .false
    
    ; Print "true"
    mov byte [temp_buffer], 't'
    mov byte [temp_buffer+1], 'r'
    mov byte [temp_buffer+2], 'u'
    mov byte [temp_buffer+3], 'e'
    mov byte [temp_buffer+4], 0
    jmp .print
    
.false:
    ; Print "false"
    mov byte [temp_buffer], 'f'
    mov byte [temp_buffer+1], 'a'
    mov byte [temp_buffer+2], 'l'
    mov byte [temp_buffer+3], 's'
    mov byte [temp_buffer+4], 'e'
    mov byte [temp_buffer+5], 0
    
.print:
    lea rcx, [temp_buffer]
    call print_string
    
    add rsp, 64
    pop rbp
    ret

; ========================
; Print Float (simplified)
; ========================
print_float:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    ; Convert float to int and print
    cvttsd2si rcx, xmm0
    call print_int
    
    add rsp, 64
    pop rbp
    ret

; ========================
; Print Newline
; ========================
println:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    mov byte [temp_buffer], 13  ; CR
    mov byte [temp_buffer+1], 10  ; LF
    mov byte [temp_buffer+2], 0
    
    lea rcx, [temp_buffer]
    call print_string
    
    add rsp, 64
    pop rbp
    ret

; ========================
; Read String
; ========================
read_string:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    ; Save parameters
    mov [rsp+32], rcx   ; buffer
    mov [rsp+40], rdx   ; max length
    
    ; Get stdin handle
    mov rcx, STD_INPUT_HANDLE
    call GetStdHandle
    
    ; Read from console
    mov rcx, rax            ; hConsoleInput
    mov rdx, [rsp+32]       ; lpBuffer
    mov r8, [rsp+40]        ; nNumberOfCharsToRead
    lea r9, [bytes_read]    ; lpNumberOfCharsRead
    push 0                  ; lpReserved
    call ReadConsoleA
    add rsp, 8
    
    ; Null-terminate and remove CR/LF
    mov rax, [bytes_read]
    mov rdx, [rsp+32]
    
    ; Check for CR/LF at end
    cmp rax, 2
    jl .no_crlf
    
    ; Remove CR/LF
    mov byte [rdx+rax-2], 0
    jmp .done
    
.no_crlf:
    mov byte [rdx+rax], 0
    
.done:
    mov rax, [rsp+32]  ; Return buffer pointer
    add rsp, 64
    pop rbp
    ret

; ========================
; Read Integer - FIXED VERSION
; ========================
read_int:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    ; Read string into buffer
    lea rcx, [int_buffer]
    mov rdx, 32
    call read_string
    
    ; Convert string to integer manually
    lea rsi, [int_buffer]
    xor rax, rax        ; result = 0
    xor rcx, rcx        ; sign = 0
    
    ; Check for negative sign
    movzx rdx, byte [rsi]
    cmp dl, '-'
    jne .parse
    inc rsi
    mov rcx, 1          ; negative
    
.parse:
    movzx rdx, byte [rsi]
    test dl, dl
    jz .done
    
    ; Check if digit
    cmp dl, '0'
    jb .done
    cmp dl, '9'
    ja .done
    
    ; Convert digit
    sub dl, '0'
    imul rax, 10
    add rax, rdx
    inc rsi
    jmp .parse
    
.done:
    ; Apply sign
    test rcx, rcx
    jz .positive
    neg rax
    
.positive:
    add rsp, 64
    pop rbp
    ret

; ========================
; String Length
; ========================
str_length:
    push rbp
    mov rbp, rsp
    
    xor rax, rax
.loop:
    cmp byte [rcx+rax], 0
    je .done
    inc rax
    jmp .loop
    
.done:
    pop rbp
    ret

; ========================
; String Concatenation
; ========================
str_concat:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    ; Save parameters
    push rcx    ; str1
    push rdx    ; str2
    
    ; Get length of str1
    call str_length
    mov rbx, rax
    
    ; Get length of str2
    mov rcx, [rsp]
    call str_length
    add rbx, rax
    inc rbx     ; +1 for null
    
    ; Allocate memory (simplified - using temp buffer)
    lea rdi, [temp_buffer]
    
    ; Copy str1
    pop rdx     ; str2
    pop rcx     ; str1
    
.copy1:
    mov al, [rcx]
    test al, al
    jz .copy2
    mov [rdi], al
    inc rcx
    inc rdi
    jmp .copy1
    
.copy2:
    mov al, [rdx]
    test al, al
    jz .done
    mov [rdi], al
    inc rdx
    inc rdi
    jmp .copy2
    
.done:
    mov byte [rdi], 0
    lea rax, [temp_buffer]
    
    add rsp, 64
    pop rbp
    ret

; ========================
; Input (alias for read_string with default buffer)
; ========================
input:
    push rbp
    mov rbp, rsp
    sub rsp, 64
    
    lea rcx, [temp_buffer]
    mov rdx, 256
    call read_string
    
    add rsp, 64
    pop rbp
    ret
