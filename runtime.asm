; ========================
; Mantel Runtime Library - FIXED VERSION
; Windows x64
; All functions working including read_int()
; ========================

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
    newline_str: db 13, 10, 0
    true_str: db "true", 0
    false_str: db "false", 0

section .bss
    bytes_written: resq 1
    bytes_read: resq 1
    temp_buffer: resb 256
    int_buffer: resb 32

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
global input

; ========================
; Print Integer
; ========================
print_int:
    push rbp
    mov rbp, rsp
    push rbx
    push rsi
    push rdi
    sub rsp, 32
    
    ; Convert integer to string
    mov rax, rcx
    lea rdi, [temp_buffer]
    
    ; Handle negative numbers
    test rax, rax
    jns .positive
    neg rax
    mov byte [rdi], '-'
    inc rdi
    
.positive:
    ; Convert to string (reverse)
    mov rbx, rdi
    mov rcx, 10
    
.convert_loop:
    xor rdx, rdx
    div rcx
    add dl, '0'
    mov [rdi], dl
    inc rdi
    test rax, rax
    jnz .convert_loop
    
    ; Reverse the string
    dec rdi
    mov rsi, rbx
    
.reverse_loop:
    cmp rsi, rdi
    jge .done_reverse
    mov al, [rsi]
    mov bl, [rdi]
    mov [rdi], al
    mov [rsi], bl
    inc rsi
    dec rdi
    jmp .reverse_loop
    
.done_reverse:
    ; Null terminate
    mov byte [rdi + 1], 0
    
    ; Print the string
    lea rcx, [temp_buffer]
    call str_length
    mov r8, rax
    
    mov rcx, STD_OUTPUT_HANDLE
    call GetStdHandle
    
    lea rdx, [temp_buffer]
    mov rcx, rax
    lea r9, [bytes_written]
    push 0
    call WriteConsoleA
    add rsp, 8
    
    add rsp, 32
    pop rdi
    pop rsi
    pop rbx
    pop rbp
    ret

; ========================
; Print String
; ========================
print_string:
    push rbp
    mov rbp, rsp
    push rbx
    push rsi
    push rdi
    sub rsp, 32
    
    ; Save string pointer
    mov rbx, rcx
    
    ; Get string length
    call str_length
    mov rsi, rax
    
    ; Get stdout handle
    mov rcx, STD_OUTPUT_HANDLE
    call GetStdHandle
    
    ; Write to console
    mov rcx, rax
    mov rdx, rbx
    mov r8, rsi
    lea r9, [bytes_written]
    push 0
    call WriteConsoleA
    add rsp, 8
    
    add rsp, 32
    pop rdi
    pop rsi
    pop rbx
    pop rbp
    ret

; ========================
; Print Character
; ========================
print_char:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    mov [temp_buffer], cl
    mov byte [temp_buffer + 1], 0
    
    lea rcx, [temp_buffer]
    call print_string
    
    add rsp, 32
    pop rbp
    ret

; ========================
; Print Boolean
; ========================
print_bool:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    test rcx, rcx
    jz .print_false
    
    lea rcx, [true_str]
    call print_string
    jmp .done
    
.print_false:
    lea rcx, [false_str]
    call print_string
    
.done:
    add rsp, 32
    pop rbp
    ret

; ========================
; Print Float (simplified)
; ========================
print_float:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    ; Convert to int and print (simple version)
    cvttsd2si rcx, xmm0
    call print_int
    
    add rsp, 32
    pop rbp
    ret

; ========================
; Print Newline
; ========================
println:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    lea rcx, [newline_str]
    call print_string
    
    add rsp, 32
    pop rbp
    ret

; ========================
; Read String - WORKING VERSION
; ========================
read_string:
    push rbp
    mov rbp, rsp
    push rbx
    push rsi
    push rdi
    sub rsp, 32
    
    ; Save parameters
    mov rbx, rcx    ; buffer
    mov rsi, rdx    ; max length
    
    ; Get stdin handle
    mov rcx, STD_INPUT_HANDLE
    call GetStdHandle
    
    ; Read from console
    mov rcx, rax
    mov rdx, rbx
    mov r8, rsi
    lea r9, [bytes_read]
    push 0
    call ReadConsoleA
    add rsp, 8
    
    ; Remove CR/LF from end
    mov rax, [bytes_read]
    test rax, rax
    jz .done
    
    cmp rax, 2
    jl .single_char
    
    ; Remove \r\n (Windows line ending)
    mov byte [rbx + rax - 2], 0
    jmp .done
    
.single_char:
    mov byte [rbx + rax], 0
    
.done:
    mov rax, rbx
    add rsp, 32
    pop rdi
    pop rsi
    pop rbx
    pop rbp
    ret

; ========================
; Read Integer - FIXED VERSION!
; ========================
read_int:
    push rbp
    mov rbp, rsp
    push rbx
    push rsi
    push rdi
    sub rsp, 32
    
    ; Read string into buffer
    lea rcx, [int_buffer]
    mov rdx, 32
    call read_string
    
    ; Convert string to integer
    lea rsi, [int_buffer]
    xor rax, rax        ; result = 0
    xor rcx, rcx        ; sign = 0 (positive)
    xor r10, r10        ; temporary for digit
    
    ; Check for negative sign
    movzx rdx, byte [rsi]
    cmp dl, '-'
    jne .parse_digits
    inc rsi
    mov rcx, 1          ; sign = 1 (negative)
    
.parse_digits:
    movzx rdx, byte [rsi]
    
    ; Check if end of string
    test dl, dl
    jz .apply_sign
    
    ; Check if it's a digit (0-9)
    cmp dl, '0'
    jb .done_parsing
    cmp dl, '9'
    ja .done_parsing
    
    ; Convert digit: result = result * 10 + (char - '0')
    sub dl, '0'
    imul rax, 10
    movzx r10, dl
    add rax, r10
    
    inc rsi
    jmp .parse_digits
    
.done_parsing:
.apply_sign:
    ; Apply sign if negative
    test rcx, rcx
    jz .positive_number
    neg rax
    
.positive_number:
    add rsp, 32
    pop rdi
    pop rsi
    pop rbx
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
    cmp byte [rcx + rax], 0
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
    push rbx
    push rsi
    push rdi
    sub rsp, 32
    
    ; For now, use temp_buffer
    ; rcx = string1, rdx = string2
    mov rbx, rcx
    mov rsi, rdx
    
    lea rdi, [temp_buffer]
    
    ; Copy first string
.copy1:
    mov al, [rbx]
    test al, al
    jz .copy2
    mov [rdi], al
    inc rbx
    inc rdi
    jmp .copy1
    
    ; Copy second string
.copy2:
    mov al, [rsi]
    test al, al
    jz .done
    mov [rdi], al
    inc rsi
    inc rdi
    jmp .copy2
    
.done:
    mov byte [rdi], 0
    lea rax, [temp_buffer]
    
    add rsp, 32
    pop rdi
    pop rsi
    pop rbx
    pop rbp
    ret

; ========================
; Input (alias for read_string)
; ========================
input:
    push rbp
    mov rbp, rsp
    sub rsp, 32
    
    lea rcx, [temp_buffer]
    mov rdx, 256
    call read_string
    
    add rsp, 32
    pop rbp
    ret