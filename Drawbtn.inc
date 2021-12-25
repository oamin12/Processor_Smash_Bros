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
     
    mov xr,5d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,69d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,133d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,197d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,261d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,325d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,389d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,453d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,517d
    mov yr,205
    Drawbtncommand xr,yr

    mov xr,581d
    mov yr,205
    Drawbtncommand xr,yr
;-------------------------------------------------------------

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

    
ENDM DrawCommandRow



