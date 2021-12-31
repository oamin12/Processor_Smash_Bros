BtnAct MACRO  
    local exit


cmp ax, 0
jz addb
; cmp ax, 1
; jz adcb    
; cmp ax,2
; jz subb
; cmp ax, 3
; jz sbbb
; cmp ax, 4
; jz divb
; cmp ax, 5
; jz mulb    
; cmp ax,6
; jz xorb
; cmp ax,7
; jz andb
; cmp ax,8
; jz orb    
; cmp ax,9
; jz nopb
; cmp ax, 10
; jz shrb
; cmp ax, 11
; jz shlb    
; cmp ax,12
; jz sarb
; cmp ax, 13
; jz incb    
; cmp ax,14
; jz decb
; cmp ax, 15
; jz clcb
; cmp ax, 16
; jz idivb    
; cmp ax,17
; jz imulb
; cmp ax, 18
; jz rorb
; cmp ax, 19
; jz rolb    
  
 addb: 
 DrawAddressingRow
 AddOp
 
 




;  adcb:
; Getbtnclicked
;  subb:
; Getbtnclicked
;  sbbb:
; Getbtnclicked
;  divb:
; Getbtnclicked
;  mulb:
; Getbtnclicked
;  xorb:
; Getbtnclicked
;  andb:
;  Getbtnclicked
;  orb:
;  Getbtnclicked
;  nopb:
;  Getbtnclicked
;  shrb:
;  Getbtnclicked
;  shlb:
;  Getbtnclicked
;  sarb:
;  Getbtnclicked
;  incb:
;  Getbtnclicked
;  decb:
;  Getbtnclicked
;  clcb:
;  Getbtnclicked
;  idivb:
;  Getbtnclicked
;  imulb:
;  Getbtnclicked
;  rorb:
;  Getbtnclicked
;  rolb:   
;  Getbtnclicked
    
   
   
   exit: 
    ENDM BtnAct