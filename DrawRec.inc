DrawRec MACRO x,y ;x y top left corner of the drawn rectangle
    local loop1,loop2
     
  
  mov cx,x
  mov dx,y
  
  
  mov al,0Fh
  mov ah,0ch
  mov bx,20
  
  
  loop1:
  int 10h
  add cx,40
  int 10h
  sub cx,40
  inc dx
  dec bx
  cmp bx,0
  jnz loop1
  
    
  mov cx,x
  mov dx,y
  mov bx,40
  
  
  loop2:
  int 10h
  add dx,20
  int 10h
  sub dx,20
  inc cx
  dec bx
  cmp bx,0
  jnz loop2  
           
  
    


    
ENDM DisplayString