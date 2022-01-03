include DrawRec.inc ;x y top left corner of the drawn rectangle

model small
.stack 64

.Data
j dw 50
k dw 50
main proc far
    mov ax,@data
    mov ds,ax
    DrawRec j,k    
      
  
main endp
end main