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

Lax db "AX$"
Lbx db "BX$"
Lcx db "CX$"
Ldx db "DX$"
Lsi db "SI$"
Ldi db "DI$"
Lsp db "SP$"
Lbp db "BP$"
db "$$$"
trycatch db "0000$"



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
    cmp dx,200
    jnz loop1

    mov cx,0  ;split for bottons bel 3ard
    mov dx,200
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


    ;Drawing Data Segment
    DrawCommandRow

    ;Drawing Registers
    P1regs
    P2regs
    DrawDS
   
   

main endp



end main

