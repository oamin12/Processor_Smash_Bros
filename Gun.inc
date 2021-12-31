Gun MACRO x1,y1
    local addpos,loop1,loop2,loop3,loop4,loop6,loop5,repeat,Exit,yellow,red,green,purble,skipclr,skipaddpos,cmpcolumn,cmpcolumn2,Exit1,incyellow,incred,incgreen,incpurble


    MOV AH, 00h  ; interrupts to get system time
    int 1AH      ; CX:DX now hold number of clock ticks since midnight
    mov  ax, dx
    xor  dx, dx
    mov  cx, 3d
    div  cx       ;here dx contains the remainder of the division - from 0 to 3
    cmp dx,0
    je yellow
    cmp dx,1
    je red
    cmp dx,2
    je green
    cmp dx,3
    je purble

    yellow:
    mov clro,0Eh
    jmp skipclr
    red:
    mov clro,04h
    jmp skipclr
    green:
    mov clro,0Ah
    jmp skipclr
    purble:
    mov clro,0Dh
    jmp skipclr

    skipclr:


    MOV AH, 00h  ; interrupts to get system time
    int 1AH      ; CX:DX now hold number of clock ticks since midnight
    mov  ax, dx
    xor  dx, dx
    mov  cx, 250d
    div  cx       ;here dx contains the remainder of the division - from 0 to 250
    cmp dx,10
    jb addpos
    jmp skipaddpos
    addpos:
    add dx,10
    skipaddpos:

    
    mov xo,dx ; random column
    ;mov xo,50
    mov yo,10
    mov ro,10
    ;mov clro,4h
    DrawCir xo,yo,xc,yc,p,ro,clro 

    loop1: 
    DrawTri x1,y1,4 ;;GUN POSITION
    DrawCir xo,yo,xc,yc,p,ro,00
    inc yo
    cmp yo,220
    je Exit
    DrawCir xo,yo,xc,yc,p,ro,clro
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
    int 21h
    hlt    ;flushing keyboard buffer
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
dec ycm

DrawCir xcm,ycm,xc,yc,p,r,0Ah 
    
    
DrawCir xo,yo,xc,yc,p,ro,00
inc yo

DrawCir xo,yo,xc,yc,p,ro,clro 
mov ax,yo
cmp ycm,ax
jbe cmpcolumn
cmp ycm,5 ;;check the position of the projectile "must be edited for targets" ;; The Following 3 lines might be removed
hlt
jnz repeat

jmp Exit 

cmpcolumn:
mov ax,xcm ;;projectile
mov bx,xo ;;object
add bx,13

cmp ax,bx
jb cmpcolumn2
cmp ycm,5 ;;check the position of the projectile "must be edited for targets"
jz Exit
cmp yo,220
jz Exit
hlt
jmp repeat
cmpcolumn2:
sub bx,26
cmp ax,bx
ja Exit1
cmp ycm,5 ;;check the position of the projectile "must be edited for targets"
jz Exit
cmp yo,220
jz Exit
hlt
jmp repeat

Exit1:;; add points to user IMP
cmp clro,0Eh
je incyellow
cmp clro,04h
je incred
cmp clro,0Ah
je incgreen
cmp clro,0Dh
je incpurble

incyellow:
mov Ah,LFallObjY1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjY1,ah
jmp Exit
incred:
mov Ah,LFallObjR1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjR1,ah
jmp Exit
incgreen:
mov Ah,LFallObjG1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjG1,ah
jmp Exit
incpurble:
mov Ah,LFallObjP1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjP1,ah
jmp Exit


Exit:

DrawCir xcm,ycm,xc,yc,p,r,00
DrawCir xo,yo,xc,yc,p,ro,00
DrawTri x1,y1,00

mov ah,0ch
mov al,0
int 21h

    
ENDM Gun