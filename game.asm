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

;Registers labels
Lax db "AX$"
Lbx db "BX$"
Lcx db "CX$"
Ldx db "DX$"
Lsi db "SI$"
Ldi db "DI$"
Lsp db "SP$"
Lbp db "BP$"
Lah db "AH$"
Lal db "AL$"
Lbh db "BH$"
Lbl db "BL$"
db "$$$"
trycatch db "0000$"

;command buttons lables
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
   
   

main endp



end main

