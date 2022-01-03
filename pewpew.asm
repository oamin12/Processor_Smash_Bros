include DrawRec.inc
include DrawCir.inc 
include DrawDS.inc
include Drawbtn.inc
include P1regs.inc
include P2regs.inc
include DrawTri.inc
include DrawCir.inc
.model small
.386
.stack 64
.data
xr dw ?
yr dw ?
x db ?
y db ?
.data
xcm dw ?
ycm dw ?
xc dw 0
yc dw 0
r dw ?
p dw 0
clr db ?
;triangle coordinates
x1 dw 50
y1 dw 50
x2 dw 148
y2 dw 174
.code
main proc far

mov ax,@data
mov DS,ax
mov ah,0
mov al,10h
int 10h
    
loop1: 
    DrawTri x1,y1,4

    mov ah,1
    int 16h     ;getting key pressed from the buffer

    cmp Ah,77  ;checking if user pressed 'right'
    jz loop5
    cmp ah,75  ;checking if user pressed 'left'
    jz loop2
    cmp ah,72  ;checking if user pressed 'up'
    jz loop4
    cmp ah,80 ;checking if user pressed 'down'
    jz loop3
    cmp al,32 ;checking if user pressed 'spacebar'
    jz loop6
    mov ah,0ch
    mov al,0
    int 21h    ;flushing keyboard buffer
    jmp loop1    

loop2:
DrawTri x1,y1,0
sub x1,4
DrawTri x1,y1,4
mov ah,0ch
mov al,0
int 21h

hlt
jmp loop1

loop3:
DrawTri x1,y1,0
add y1,3
DrawTri x1,y1,4
mov ah,0ch
    mov al,0
    int 21h

hlt
jmp loop1

loop4:
DrawTri x1,y1,0
sub y1,3
DrawTri x1,y1,4
mov ah,0ch
    mov al,0
    int 21h

hlt
jmp loop1

loop5: 
DrawTri x1,y1,0
add x1,4
DrawTri x1,y1,4
mov ah,0ch
mov al,0
int 21h 

hlt
jmp loop1


loop6: ;for projectile

mov cx,x1
mov dx,y1

mov xcm,cx 
mov ycm,dx
mov r,3
mov clr,0Ah

repeat:

DrawCir xcm,ycm,xc,yc,p,r,00 
sub ycm,3
DrawCir xcm,ycm,xc,yc,p,r,0Ah 

cmp ycm,5 ;;check the position of the projectile "must be edited for targets"

hlt  
jnz repeat 
DrawCir xcm,ycm,xc,yc,p,r,00
mov ah,0ch
mov al,0
int 21h
hlt
jmp stopgun ;if projectile was fired the gun stops


stopgun:

main endp
end main
