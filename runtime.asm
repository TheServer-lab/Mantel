; =========================
; Mantel Runtime Library v0.3
; Supports Windows x64 and Linux x86_64
; =========================

%ifidn __OUTPUT_FORMAT__, win64
    bits 64
    default rel
    
    ; Windows imports
    extern ExitProcess
    extern GetStdHandle
    extern WriteConsoleA
    extern ReadConsoleA
    extern malloc
    extern free
    extern printf
    extern sprintf
    
    ; Windows constants
    STD_INPUT_HANDLE  equ -10
    STD_OUTPUT_HANDLE equ -11
%else
    bits 64
    default rel
    
    ; Linux syscalls
    SYS_READ    equ 0
    SYS_WRITE   equ 1
    SYS_EXIT    equ 60
    SYS_BRK     equ 12
    STDIN       equ 0
    STDOUT      equ 1
%endif

section .data
    ; Format strings for printf/sprintf
    fmt_int:        db "%lld", 0
    fmt_float:      db "%g", 0
    fmt_string:     db "%s", 0
    fmt_char:       db "%c", 0
    fmt_bool_true:  db "true", 0
    fmt_bool_false: db "false", 0
    fmt_newline:    db 10, 0
    
    ; Buffer for conversions
    int_buffer:     times 32 db 0
    float_buffer:   times 32 db 0
    
section .text

; ========================
; Global exports
; ========================
global exit
global print_int
global print_float
global print_string
global print_char
global print_bool
global println
global read_int
global read_string
global str_concat
global str_length
global array_create
global array_length

; ========================
; Exit with code
; ========================
exit:
%ifidn __OUTPUT_FORMAT__, win64
    mov rcx, rax    ; Exit code
    sub rsp, 40     ; Shadow space
    call ExitProcess
    add rsp, 40
%else
    mov rdi, rax    ; Exit code
    mov rax, 60     ; sys_exit
    syscall
%endif
    ret

; ========================
; Print Functions
; ========================

print_int:
%ifidn __OUTPUT_FORMAT__, win64
    ; rcx = value
    mov rdx, rcx        ; Value
    lea rcx, [fmt_int]
    sub rsp, 40
    call printf
    add rsp, 40
%else
    ; rdi = value
    push rdi
    push rsi
    push rdx
    
    ; Convert integer to string
    mov rax, rdi
    lea rdi, [int_buffer]
    call int_to_str
    
    ; Print the string
    mov rsi, int_buffer
    call print_raw_string
    
    pop rdx
    pop rsi
    pop rdi
%endif
    ret

print_float:
%ifidn __OUTPUT_FORMAT__, win64
    ; xmm0 = value
    sub rsp, 40
    lea rcx, [fmt_float]
    call printf
    add rsp, 40
%else
    ; xmm0 = value
    push rdi
    push rsi
    push rdx
    
    ; Convert float to string (simplified - just print as integer)
    cvttsd2si rax, xmm0
    mov rdi, rax
    call print_int
    
    pop rdx
    pop rsi
    pop rdi
%endif
    ret

print_string:
%ifidn __OUTPUT_FORMAT__, win64
    ; rcx = string pointer
    mov rdx, rcx
    lea rcx, [fmt_string]
    sub rsp, 40
    call printf
    add rsp, 40
%else
    ; rdi = string pointer
    push rsi
    push rdx
    
    mov rsi, rdi
    call print_raw_string
    
    pop rdx
    pop rsi
%endif
    ret

print_char:
%ifidn __OUTPUT_FORMAT__, win64
    ; rcx = character
    mov rdx, rcx
    lea rcx, [fmt_char]
    sub rsp, 40
    call printf
    add rsp, 40
%else
    ; rdi = character
    push rdi
    push rsi
    push rdx
    
    ; Store character in buffer
    mov [int_buffer], dil
    mov byte [int_buffer+1], 0
    
    ; Print it
    mov rsi, int_buffer
    call print_raw_string
    
    pop rdx
    pop rsi
    pop rdi
%endif
    ret

print_bool:
%ifidn __OUTPUT_FORMAT__, win64
    ; rcx = boolean (0 = false, 1 = true)
    cmp rcx, 0
    je .false
    lea rcx, [fmt_bool_true]
    jmp .print
.false:
    lea rcx, [fmt_bool_false]
.print:
    sub rsp, 40
    call printf
    add rsp, 40
%else
    ; rdi = boolean
    push rsi
    push rdx
    
    cmp rdi, 0
    je .false
    mov rsi, fmt_bool_true
    jmp .print
.false:
    mov rsi, fmt_bool_false
.print:
    call print_raw_string
    
    pop rdx
    pop rsi
%endif
    ret

println:
%ifidn __OUTPUT_FORMAT__, win64
    lea rcx, [fmt_newline]
    sub rsp, 40
    call printf
    add rsp, 40
%else
    push rsi
    push rdx
    
    mov rsi, fmt_newline
    call print_raw_string
    
    pop rdx
    pop rsi
%endif
    ret

; ========================
; Input Functions (Windows only for now)
; ========================

read_int:
%ifidn __OUTPUT_FORMAT__, win64
    sub rsp, 40
    lea rcx, [int_buffer]
    mov rdx, 32
    call read_string
    mov rcx, rax
    call atoi
    add rsp, 40
%else
    ; Linux implementation
    push rdi
    push rsi
    push rdx
    
    lea rdi, [int_buffer]
    mov rsi, 32
    call read_raw_string
    
    lea rdi, [int_buffer]
    call atoi
    
    pop rdx
    pop rsi
    pop rdi
%endif
    ret

read_string:
%ifidn __OUTPUT_FORMAT__, win64
    ; rcx = buffer, rdx = max length
    push rbx
    push rsi
    push rdi
    
    mov rsi, rcx        ; Buffer
    mov rdi, rdx        ; Max length
    
    ; Get stdin handle
    mov rcx, STD_INPUT_HANDLE
    sub rsp, 40
    call GetStdHandle
    add rsp, 40
    mov rbx, rax        ; Handle
    
    ; Read from console
    mov rcx, rbx        ; Handle
    mov rdx, rsi        ; Buffer
    mov r8, rdi         ; Max chars
    lea r9, [rsp+56]    ; Bytes read (dummy)
    push 0
    sub rsp, 40
    call ReadConsoleA
    add rsp, 40
    
    ; Null-terminate (remove CRLF)
    mov rax, [rsp+56]
    cmp rax, 2
    jl .no_crlf
    mov byte [rsi+rax-2], 0
    jmp .done
.no_crlf:
    mov byte [rsi+rax], 0
.done:
    mov rax, rsi        ; Return buffer pointer
    
    add rsp, 8
    pop rdi
    pop rsi
    pop rbx
%else
    ; Linux implementation
    push rdi
    push rsi
    push rdx
    
    mov rsi, rdi        ; Buffer
    mov rdx, rsi        ; Max length
    
    ; Read from stdin
    mov rax, SYS_READ
    mov rdi, STDIN
    syscall
    
    ; Null-terminate
    mov byte [rsi+rax-1], 0
    mov rax, rsi        ; Return buffer pointer
    
    pop rdx
    pop rsi
    pop rdi
%endif
    ret

; ========================
; String Operations
; ========================

str_concat:
    ; rdi = str1, rsi = str2 (Linux) or rcx = str1, rdx = str2 (Windows)
%ifidn __OUTPUT_FORMAT__, win64
    push rbx
    push rsi
    push rdi
    
    mov rsi, rcx        ; str1
    mov rdi, rdx        ; str2
%endif
    push rsi
    push rdi
    
    ; Calculate total length
    mov rdi, rsi
    call str_length
    mov rbx, rax        ; len1
    
    mov rdi, [rsp]      ; str2
    call str_length
    add rbx, rax        ; total length
    inc rbx             ; +1 for null
    
    ; Allocate memory
    mov rdi, rbx
%ifidn __OUTPUT_FORMAT__, win64
    sub rsp, 40
    call malloc
    add rsp, 40
%else
    call malloc
%endif
    mov rdx, rax        ; new string
    
    ; Copy str1
    mov rdi, rdx
    pop rsi             ; str2
    pop rcx             ; str1
    
.copy1:
    mov al, [rcx]
    test al, al
    jz .copy2
    mov [rdi], al
    inc rcx
    inc rdi
    jmp .copy1
    
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
    mov rax, rdx        ; Return new string
    
%ifidn __OUTPUT_FORMAT__, win64
    pop rdi
    pop rsi
    pop rbx
%endif
    ret

str_length:
    ; rdi = string (Linux) or rcx = string (Windows)
%ifidn __OUTPUT_FORMAT__, win64
    mov rdi, rcx
%endif
    xor rax, rax
.count:
    cmp byte [rdi+rax], 0
    je .done
    inc rax
    jmp .count
.done:
    ret

; ========================
; Array Operations
; ========================

array_create:
    ; rdi = length, rsi = element size (Linux)
    ; rcx = length, rdx = element size (Windows)
%ifidn __OUTPUT_FORMAT__, win64
    mov rdi, rcx
    mov rsi, rdx
%endif
    push rdi
    push rsi
    
    ; total_size = len * elem_size + 8
    mov rax, rdi
    imul rax, rsi
    add rax, 8
    
    ; malloc(total_size)
    mov rdi, rax
%ifidn __OUTPUT_FORMAT__, win64
    sub rsp, 40
    call malloc
    add rsp, 40
%else
    call malloc
%endif
    test rax, rax
    je .fail
    
    ; Store length in header
    pop rsi
    pop rdi
    mov [rax], rdi
    add rax, 8
    ret
    
.fail:
    add rsp, 16
    xor rax, rax
    ret

array_length:
    ; rdi = array pointer (Linux) or rcx = array pointer (Windows)
%ifidn __OUTPUT_FORMAT__, win64
    mov rdi, rcx
%endif
    mov rax, [rdi-8]    ; Length stored before array data
    ret

; ========================
; Helper Functions
; ========================

atoi:
    ; rdi = string
    xor rax, rax
    xor rcx, rcx
    mov rsi, rdi
    
    ; Check for sign
    movzx r8, byte [rsi]
    cmp r8, '-'
    jne .positive
    inc rsi
    mov rcx, 1          ; Negative flag
    
.positive:
.loop:
    movzx rdx, byte [rsi]
    test rdx, rdx
    jz .done
    sub rdx, '0'
    imul rax, 10
    add rax, rdx
    inc rsi
    jmp .loop
    
.done:
    test rcx, rcx
    jz .return
    neg rax
.return:
    ret

int_to_str:
    ; rax = number, rdi = buffer
    push rbx
    push rdx
    
    test rax, rax
    jns .positive
    neg rax
    mov byte [rdi], '-'
    inc rdi
    
.positive:
    mov rbx, rdi
    
    ; Special case for zero
    test rax, rax
    jnz .convert
    mov byte [rdi], '0'
    inc rdi
    jmp .done
    
.convert:
    xor rcx, rcx
.divide:
    xor rdx, rdx
    mov r8, 10
    div r8
    add dl, '0'
    push rdx
    inc rcx
    test rax, rax
    jnz .divide
    
.reverse:
    pop rax
    mov [rdi], al
    inc rdi
    loop .reverse
    
.done:
    mov byte [rdi], 0
    mov rax, rdi        ; Return end of string
    
    pop rdx
    pop rbx
    ret

; ========================
; Linux-specific helpers
; ========================
%ifnidn __OUTPUT_FORMAT__, win64

print_raw_string:
    ; rsi = string
    push rdi
    push rdx
    push rax
    
    ; Calculate length
    mov rdi, rsi
    call str_length
    mov rdx, rax        ; Length
    
    ; Write to stdout
    mov rax, SYS_WRITE
    mov rdi, STDOUT
    syscall
    
    pop rax
    pop rdx
    pop rdi
    ret

read_raw_string:
    ; rdi = buffer, rsi = max length
    push rdx
    push rax
    
    mov rax, SYS_READ
    mov rdx, rsi        ; Max length
    mov rsi, rdi        ; Buffer
    mov rdi, STDIN
    syscall
    
    ; Null-terminate
    mov byte [rsi+rax-1], 0
    
    pop rax
    pop rdx
    ret

%endif

%ifidn __OUTPUT_FORMAT__, win64
section .idata
%endif