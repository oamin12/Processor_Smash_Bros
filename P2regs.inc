P2regs MACRO
    
    mov xr,380
    mov yr,10
    
    DrawRec xr,yr
    
    add yr,40
    DrawRec xr,yr
    
    add yr,40
    DrawRec xr,yr
    add yr,40
    DrawRec xr,yr

    add xr,60
    mov yr,10
    DrawRec xr,yr
    add yr,40
    DrawRec xr,yr
    add yr,40
    DrawRec xr,yr
    add yr,40
    DrawRec xr,yr

    mov ah,2
    mov dl,44
    mov dh,2
    int 10h      
    mov ah,9
    mov dx,offset Lax
    int 21h

    mov ah,2
    mov dl,44
    mov dh,5
    int 10h  
    mov ah,9
    mov dx,offset Lbx
    int 21h

    mov ah,2
    mov dl,44
    mov dh,8
    int 10h  
    mov ah,9
    mov dx,offset Lcx
    int 21h


    mov ah,2
    mov dl,44
    mov dh,11
    int 10h  
    mov ah,9
    mov dx,offset Ldx
    int 21h

    mov ah,2
    mov dl,64
    mov dh,2
    int 10h  
    mov ah,9
    mov dx,offset Lsi
    int 21h

    mov ah,2
    mov dl,64
    mov dh,5
    int 10h  
    mov ah,9
    mov dx,offset Ldi
    int 21h
    
    mov ah,2
    mov dl,64
    mov dh,8
    int 10h  
    mov ah,9
    mov dx,offset Lsp
    int 21h

    mov ah,2
    mov dl,64
    mov dh,11
    int 10h  
    mov ah,9
    mov dx,offset Lbp
    int 21h
ENDM P2regs