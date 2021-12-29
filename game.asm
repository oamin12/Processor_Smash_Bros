include DrawRec.inc
include DrawDS.inc
include Drawbtn.inc
include P1regs.inc
include P2regs.inc

.model small
.data
xr dw ?
yr dw ?

x db ?
y db ?

;command
btn_num dw ?

;Registers labels--------------------
Lax      db "AX$"
Lbx      db "BX$"
Lcx      db "CX$"
Ldx      db "DX$"
Lsi      db "SI$"
Ldi      db "DI$"
Lsp      db "SP$"
Lbp      db "BP$"
Lah      db "AH$"
Lal      db "AL$"
Lbh      db "BH$"
Lbl      db "BL$"
Lch      db "CH$"
Lcl      db "CL$"
Ldh      db "DH$"
Ldl      db "DL$"
Limd_adr db "VAL$"
Ldir_adr db "[VL]$"
Lind_adr db "[BX]$"
Lbas_adr db "[BX+V]$"

db "$$$"
trycatch db "0000$"

;command buttons lables------------------
Ladd  db "ADD$"
Ladc  db "ADC$"
Lsub  db "SUB$"
Lsbb  db "SBB$"
Ldiv  db "DIV$"
Lmul  db "MUL$"
Lxor  db "XOR$"
Land  db "AND$"
Lor   db "OR$"
Lnop  db "NOP$";---------new_row----------
Lshr  db "SHR$"
Lshl  db "SHL$"
Lsar  db "SAR$"
Linc  db "INC$"
Ldec  db "DEC$"
Lclc  db "CLC$"
Lidiv db "IDIV$"
Limul db "IMUL$"
Lror  db "ROR$"
Lrol  db "ROL$"

.code

main proc far
    
    mov ax,@data
    mov ds,ax
     
    mov ah,0
    mov al,10h  ;;10h 640x350
    int 10h

    mov al,03h
    mov ah,0ch

    mov cx,320
    mov dx,0
    loop1: ;split screen into 2 players' screen
    int 10h
    inc dx
    cmp dx,230 ;joe
    jnz loop1

    mov cx,0  ;split for bottons bel 3ard
    mov dx,230  ;joe
    loop2:
    int 10h
    inc cx
    cmp cx,640
    jnz loop2

    mov cx,0  ;Draw Chat Bar
    mov dx,300
    loop33:
    int 10h
    inc cx
    cmp cx,640
    jnz loop33


    ;Drawing Commands buttons
    DrawCommandRow
    DrawAddressingRow

    ;Drawing Registers
    P1regs
    P2regs
    DrawDS

    ;showing mouse
    mov ax,1
    int 33h

    call Getbtnclicked   

    ;hlt
main endp

Getbtnclicked proc near

    noleftclick:
            mov ax,0003h
            int 33h ;CX(X), DX(Y)
            test bx,1
            jz noleftclick ;break if user clicked the left click

    mov bx,dx
    mov dx,0
    mov ax,cx
    mov cx,64 ;58 is the btn width
    div cx    ;integer division

    cmp bx,230
    ja under

    mov ax,0ffffh
    jmp exit

    under:
        cmp bx,265
        jb row_0
        
        cmp bx,300
        jb row_10

        mov ax,0ffffh
        jmp exit

        row_0:
            add ax,0
            jmp exit

        row_10:
            add ax,10d


    exit:

    mov btn_num,ax
    ret
Getbtnclicked endp

end main

