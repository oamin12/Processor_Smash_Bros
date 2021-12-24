include DrawRec.inc
.model small
.data
xr dw ?
yr dw ?
x db ?
y db ?
texxt db "MOSMOS$"
Lax db "AX$"
Lbx db "BX$"
Lcx db "CX$"
Ldx db "DX$"
Lsi db "SI$"
Ldi db "DI$"
Lsp db "SP$"
Lbp db "BP$"
; trycatch db "0000$"



.code

main proc far
    
    mov ax,@data
    mov ds,ax
     
    mov ah,0
    mov al,13h  ;; 640x320
    int 10h

    mov al,0fh
    mov ah,0ch

    mov cx,160
    mov dx,0
    loop1: ;split screen into 2 players' screen
    int 10h
    inc dx
    cmp dx,115
    jnz loop1

    mov cx,0  ;split for bottons bel 3ard
    mov dx,115
    loop2:
    int 10h
    inc cx
    cmp cx,320
    jnz loop2

    ;kol el arqam el hatetbe3 gowa el box ba3d makan esmo be +3    

    ; mov ah,2
    ; mov dl,24
    ; mov dh,2
    ; int 10h      
    ; mov ah,9
    ; mov dx,offset trycatch
    ; int 21h

    ;Drawing rectangles
    call drawRegsP1
    call drawRegsP2
   

main endp

drawRegsP1 proc NEAR
    ;Drawing Player 1 Boxes
    mov xr,30
    mov yr,10
    DrawRec xr,yr
    
    add yr,21
    DrawRec xr,yr

    add yr,21
    DrawRec xr,yr
    
    add yr,21
    DrawRec xr,yr

    mov xr,70
    mov yr,10
    DrawRec xr,yr
    
    add yr,21
    DrawRec xr,yr
    
    add yr,21
    DrawRec xr,yr
    
    add yr,21
    DrawRec xr,yr

    mov ah,2
    mov dl,1
    mov dh,2
    int 10h      
    mov ah,9
    mov dx,offset Lax
    int 21h

    mov ah,2
    mov dl,1
    mov dh,5
    int 10h  
    mov ah,9
    mov dx,offset Lbx
    int 21h

    mov ah,2
    mov dl,1
    mov dh,8
    int 10h  
    mov ah,9
    mov dx,offset Lcx
    int 21h


    mov ah,2
    mov dl,1
    mov dh,11
    int 10h  
    mov ah,9
    mov dx,offset Ldx
    int 21h

    mov ah,2
    mov dl,15
    mov dh,2
    int 10h  
    mov ah,9
    mov dx,offset Lsi
    int 21h

    mov ah,2
    mov dl,15
    mov dh,5
    int 10h  
    mov ah,9
    mov dx,offset Ldi
    int 21h
    
    mov ah,2
    mov dl,15
    mov dh,8
    int 10h  
    mov ah,9
    mov dx,offset Lsp
    int 21h

    mov ah,2
    mov dl,15
    mov dh,11
    int 10h  
    mov ah,9
    mov dx,offset Lbp
    int 21h
drawRegsP1 endp

drawRegsP2 proc NEAR

    mov xr,190
    mov yr,10
    
    DrawRec xr,yr
    
    add yr,21
    DrawRec xr,yr
    
    add yr,21
    DrawRec xr,yr
    add yr,21
    DrawRec xr,yr

    mov xr,230
    mov yr,10
    DrawRec xr,yr
    add yr,21
    DrawRec xr,yr
    add yr,21
    DrawRec xr,yr
    add yr,21
    DrawRec xr,yr

    mov ah,2
    mov dl,21
    mov dh,2
    int 10h      
    mov ah,9
    mov dx,offset Lax
    int 21h

    mov ah,2
    mov dl,21
    mov dh,5
    int 10h  
    mov ah,9
    mov dx,offset Lbx
    int 21h

    mov ah,2
    mov dl,21
    mov dh,8
    int 10h  
    mov ah,9
    mov dx,offset Lcx
    int 21h


    mov ah,2
    mov dl,21
    mov dh,11
    int 10h  
    mov ah,9
    mov dx,offset Ldx
    int 21h

    mov ah,2
    mov dl,35
    mov dh,2
    int 10h  
    mov ah,9
    mov dx,offset Lsi
    int 21h

    mov ah,2
    mov dl,35
    mov dh,5
    int 10h  
    mov ah,9
    mov dx,offset Ldi
    int 21h
    
    mov ah,2
    mov dl,35
    mov dh,8
    int 10h  
    mov ah,9
    mov dx,offset Lsp
    int 21h

    mov ah,2
    mov dl,35
    mov dh,11
    int 10h  
    mov ah,9
    mov dx,offset Lbp
    int 21h
drawRegsP2 endp


end main

