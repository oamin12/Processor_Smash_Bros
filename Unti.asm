include DrawRec.inc 

.model small
.data
x dw 50
y dw 50
.code

main proc far
    
mov ax,@data
mov ds,ax
DrawRec x,y     
 
  
main endp
end main