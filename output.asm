bits 64
default rel

section .data
str_0: db "Hello from Mantel", 0

section .text
mantel_main:
    push rbp
    mov rbp, rsp
    push rbx
    push rdi
    push rsi
    sub rsp, 32
    mov qword [rbp-8], 0
    lea rax, [str_0]
    mov rcx, rax
    call print_string
.mantel_main_epilogue:
    add rsp, 32
    pop rsi
    pop rdi
    pop rbx
    pop rbp
    ret
global main
extern ExitProcess
extern print_int
extern print_float
extern print_string
extern print_char
extern print_bool
extern println
extern str_length
extern read_string
extern read_int

main:
    sub rsp, 40
    call mantel_main
    add rsp, 40
    mov rcx, rax
    call ExitProcess
