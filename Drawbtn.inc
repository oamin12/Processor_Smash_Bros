Drawbtncommand MACRO x,y ;x y top left corner of the drawn rectangle
    local loop1,loop2
     
  
  mov cx,x
  mov dx,y
  
  
  mov al,0Fh
  mov ah,0ch
  mov bx,28
  
  
  loop1:
  int 10h
  add cx,58
  int 10h
  sub cx,58
  inc dx
  dec bx
  cmp bx,0
  jnz loop1
  
    
  mov cx,x
  mov dx,y
  mov bx,58
  
  
  loop2:
  int 10h
  add dx,28
  int 10h
  sub dx,28
  inc cx
  dec bx
  cmp bx,0
  jnz loop2  

    
ENDM Drawbtn

DrawCommandRow MACRO  ;x y top left corner of the drawn rectangle

    ;clearing buttons area
    mov ax,0600h
    mov bh,0
    mov cl,0  ;x1
    mov ch,17 ;y1
    mov dl,80 ;x2
    mov dh,20 ;y2
    int 10h

    mov xr,5d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,69d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,133d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,197d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,261d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,325d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,389d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,453d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,517d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,581d
    mov yr,235
    Drawbtncommand xr,yr
;-------------------------------------------------------------

   mov xr,5d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,69d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,133d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,197d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,261d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,325d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,389d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,453d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,517d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,581d
    mov yr,265
    Drawbtncommand xr,yr
    ;--------------------------------------
    ;Labels
    mov ah,2
    mov dl,3
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Ladd
    int 21h

    mov ah,2
    mov dl,11
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Ladc
    int 21h

    mov ah,2
    mov dl,19
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lsub
    int 21h
    
    mov ah,2
    mov dl,27
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lsbb
    int 21h

    mov ah,2
    mov dl,35
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Ldiv
    int 21h

    mov ah,2
    mov dl,43
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lmul
    int 21h

    mov ah,2
    mov dl,51
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lxor
    int 21h

    mov ah,2
    mov dl,59
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Land
    int 21h

    mov ah,2
    mov dl,67
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lor
    int 21h

    mov ah,2
    mov dl,75
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lnop
    int 21h
    ;--------------new_row---------------
    mov ah,2
    mov dl,3
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lshr
    int 21h

    mov ah,2
    mov dl,11
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lshl
    int 21h

    mov ah,2
    mov dl,19
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lsar
    int 21h

    mov ah,2
    mov dl,27
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Linc
    int 21h

    mov ah,2
    mov dl,35
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Ldec
    int 21h

    mov ah,2
    mov dl,43
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lclc
    int 21h

    mov ah,2
    mov dl,51
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lidiv
    int 21h

    mov ah,2
    mov dl,59
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Limul
    int 21h

    mov ah,2
    mov dl,67
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lror
    int 21h

    mov ah,2
    mov dl,75
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lrol
    int 21h
    
ENDM DrawCommandRow




 DrawAddressingRow MACRO 

  
    ;clearing buttons area
    mov ax,0600h
    mov bh,0
    mov cl,0  ;x1
    mov ch,17 ;y1
    mov dl,80 ;x2
    mov dh,20 ;y2
    int 10h

    mov xr,5d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,69d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,133d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,197d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,261d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,325d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,389d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,453d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,517d
    mov yr,235
    Drawbtncommand xr,yr

    mov xr,581d
    mov yr,235
    Drawbtncommand xr,yr
;-------------------------------------------------------------

   mov xr,5d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,69d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,133d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,197d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,261d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,325d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,389d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,453d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,517d
    mov yr,265
    Drawbtncommand xr,yr

    mov xr,581d
    mov yr,265
    Drawbtncommand xr,yr



  ;Labels
    mov ah,2
    mov dl,3
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lax
    int 21h

    mov ah,2
    mov dl,11
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lbx
    int 21h

    mov ah,2
    mov dl,19
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lcx
    int 21h
    
    mov ah,2
    mov dl,27
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Ldx
    int 21h

    mov ah,2
    mov dl,35
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lsi
    int 21h

    mov ah,2
    mov dl,43
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Ldi
    int 21h

    mov ah,2
    mov dl,51
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lsp
    int 21h

    mov ah,2
    mov dl,59
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lbp
    int 21h

    mov ah,2
    mov dl,67
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lah
    int 21h

    mov ah,2
    mov dl,75
    mov dh,17
    int 10h  
    mov ah,9
    mov dx,offset Lal
    int 21h

    ;--------------new_row---------------

    mov ah,2
    mov dl,3
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lbh
    int 21h

    mov ah,2
    mov dl,11
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lbl
    int 21h

    mov ah,2
    mov dl,19
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lch
    int 21h

    mov ah,2
    mov dl,27
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lcl
    int 21h

    mov ah,2
    mov dl,35
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Ldh
    int 21h

    mov ah,2
    mov dl,43
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Ldl
    int 21h

    mov ah,2
    mov dl,50
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Limd_adr
    int 21h

    mov ah,2
    mov dl,59
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Ldir_adr
    int 21h

    mov ah,2
    mov dl,66
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lind_adr
    int 21h

    mov ah,2
    mov dl,73
    mov dh,19
    int 10h  
    mov ah,9
    mov dx,offset Lbas_adr
    int 21h
ENDM DrawAddressingRow

