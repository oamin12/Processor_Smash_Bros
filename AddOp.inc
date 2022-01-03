AddOp MACRO btn_num
Local firstReg,GetSecOp0,GetSecOp1,GetSecOp2      
firstReg:
;showing mouse
;     mov ax,1
;     int 33h
;hlt
mov ah,0ch
    mov al,0
    int 21h
mov ah,0
     int 16h
Call Getbtnclicked  
; mov xcm,50  ;tagroba
; mov ycm,50
; mov r,20
; mov clr,1
; DrawCir xcm,ycm,xc,yc,p,r,clr
cmp ax,0ffffh
jz firstReg

cmp ax, 0 
jz axb
    
cmp ax, 1 
jz bxb
    
; cmp ax, 2 
; jz cxb

; cmp ax, 3 
; jz dxb


; cmp ax, 4 
; jz sib


; cmp ax, 5 
; jz dib


; cmp ax, 6 
; jz spb


; cmp ax, 7 
; jz bpb


; cmp ax, 8 
; jz ahb


; cmp ax, 9 
; jz alb


; cmp ax, 10
; jz bhb


; cmp ax, 11
; jz blb


; cmp ax, 12 
; jz chb


; cmp ax, 13
; jz clb


; cmp ax, 14
; jz dhb


; cmp ax, 15
; jz dlb


; cmp ax, 16
; jz immediateaddb

; cmp ax, 17 
; jz directaddb

; cmp ax, 18
; jz indirectregb

; cmp ax, 19
; jz basedaddb

axb:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_AX
convrt_string_hex P1_regs,mode
mov operand1,CX
jmp GetSecOp0
p2_AX:
convrt_string_hex P2_regs,mode
mov operand1,CX
GetSecOp0:

mov ah,0ch
    mov al,0
    int 21h
mov ah,0
     int 16h
        Call Getbtnclicked
        cmp ax,0ffffh
        jz GetSecOp0
        cmp ax, 0 
        jz axReg_axReg  
        cmp ax, 1 
        jz axReg_bxReg
        cmp ax, 2 
        jz axReg_cxReg
        cmp ax, 3 
        jz axReg_dxReg
        cmp ax, 4 
        jz axReg_siReg
        cmp ax, 5 
        jz axReg_diReg
        cmp ax, 6 
        jz axReg_spReg
        cmp ax, 7 
        jz axReg_bpReg
        cmp ax, 8 
        jz axReg_ahReg
        cmp ax, 9 
        jz axReg_alReg
        cmp ax, 10
        jz axReg_bhReg
        cmp ax, 11
        jz axReg_blReg
        cmp ax, 12 
        jz axReg_chReg
        cmp ax, 13
        jz axReg_clReg
        cmp ax, 14
        jz axReg_dhReg
        cmp ax, 15
        jz axReg_dlReg
        ; cmp ax, 16
        ; jz axReg_immediate
        ; cmp ax, 17 
        ; jz axReg_direct
        ; cmp ax, 18
        ; jz axReg_indirect
        ; cmp ax, 19
        ; jz axReg_based



axReg_axReg:
cmp Player_turn,2 
jz p1aal
jmp p2aal

p1aal:
convrt_string_hex P1_regs,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 cmp btn_num,0
 je adding 
 cmp btn_num,1
 je adding_c
cmp btn_num,2
je subtract
 adding:
 add ax, bx
 adding_c:
 add ax, bx
 ;add ax,carryflag
 subtract:
 ;sub ax,bv
mov RegToBeUpdated,0h
mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2aal:
convrt_string_hex P2_regs,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1

axReg_bxReg:

cmp Player_turn,2 
jz p1abl
jmp p2abl

p1abl:
convrt_string_hex P1_regs+5,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2abl:
convrt_string_hex P2_regs+5,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1

axReg_cxReg:

cmp Player_turn,2 
jz p1acl
jmp p2acl

p1acl:
convrt_string_hex P1_regs+10,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2acl:
convrt_string_hex P2_regs+10,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1

axReg_dxReg:

cmp Player_turn,2 
jz p1adl
jmp p2adl

p1adl:
convrt_string_hex P1_regs+15,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2adl:
convrt_string_hex P2_regs+15,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1

axReg_siReg:

cmp Player_turn,2 
jz p1asil
jmp p2asil

p1asil:
convrt_string_hex P1_regs+20,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2asil:
convrt_string_hex P2_regs+20,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1

axReg_diReg:

cmp Player_turn,2 
jz p1adil
jmp p2adil

p1adil:
convrt_string_hex P1_regs+25,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2adil:
convrt_string_hex P2_regs+25,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1

axReg_spReg:

cmp Player_turn,2 
jz p1aspl
jmp p2aspl

p1aspl:
convrt_string_hex P1_regs+30,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2aspl:
convrt_string_hex P2_regs+30,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1



axReg_bpReg:

cmp Player_turn,2 
jz p1abpl
jmp p2abpl

p1abpl:
convrt_string_hex P1_regs+35,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
 jmp Exit1

p2abpl:
convrt_string_hex P2_regs+35,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov RegToBeUpdated,0h
    mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1


axReg_ahReg:
cmp Player_turn,0 
jz p1aahl
jmp p2aahl
p1aahl:
;Decrease points
jmp Exit1

p2aahl:
;Decrease points
jmp Exit1

axReg_alReg:
cmp Player_turn,0 
jz p1aall
jmp p2aall
p1aall:
;Decrease points
jmp Exit1

p2aall:
;Decrease points
jmp Exit1

axReg_bhReg:
cmp Player_turn,0 
jz p1abhl
jmp p2abhl
p1abhl:
;Decrease points
jmp Exit1

p2abhl:
;Decrease points
jmp Exit1


axReg_blReg:
cmp Player_turn,0 
jz p1abll
jmp p2abll
p1abll:
;Decrease points
jmp Exit1

p2abll:
;Decrease points
jmp Exit1

axReg_chReg:
cmp Player_turn,0 
jz p1achl
jmp p2achl
p1achl:
;Decrease points
jmp Exit1

p2achl:
;Decrease points
jmp Exit1


axReg_clReg:
cmp Player_turn,0 
jz p1acll
jmp p2acll
p1acll:
;Decrease points
jmp Exit1

p2acll:
;Decrease points
jmp Exit1


axReg_dhReg:
cmp Player_turn,0 
jz p1adhl
jmp p2adhl
p1adhl:
;Decrease points
jmp Exit1

p2adhl:
;Decrease points
jmp Exit1


axReg_dlReg:
cmp Player_turn,0 
jz p1adll
jmp p2adll
p1adll:
;Decrease points
jmp Exit1

p2adll:
;Decrease points
jmp Exit1


; axReg_immediate:
; cmp Player_turn,0 
; jz p1aimml
; jmp p2aimml

; axReg_direct:
; cmp Player_turn,0 
; jz p1aDirectl
; jmp p2aDirectl

; axReg_indirect:
; cmp Player_turn,0 
; jz p1aIndirectl
; jmp p2aIndirectl

; axReg_based:
; cmp Player_turn,0 
; jz p1aBasedl
; jmp p2aBasedl


bxb:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BX
convrt_string_hex P1_regs+5,mode
mov operand1,CX
jmp GetSecOp1
p2_BX:
convrt_string_hex P2_regs+5,mode
mov operand1,CX
GetSecOp1:
        Call Getbtnclicked
        cmp ax,0ffffh
        jz GetSecOp1
        cmp ax, 0 
        jz bxReg_axReg  
        cmp ax, 1 
        jz bxReg_bxReg
        cmp ax, 2 
        jz bxReg_cxReg
        cmp ax, 3 
        jz bxReg_dxReg
        cmp ax, 4 
        jz bxReg_siReg
        cmp ax, 5 
        jz bxReg_diReg
        cmp ax, 6 
        jz bxReg_spReg
        cmp ax, 7 
        jz bxReg_bpReg
        cmp ax, 8 
        jz bxReg_ahReg
        cmp ax, 9 
        jz bxReg_alReg
        cmp ax, 10
        jz bxReg_bhReg
        cmp ax, 11
        jz bxReg_blReg
        cmp ax, 12 
        jz bxReg_chReg
        cmp ax, 13
        jz bxReg_clReg
        cmp ax, 14
        jz bxReg_dhReg
        cmp ax, 15
        jz bxReg_dlReg
        ; cmp ax, 16
        ; jz bxReg_immediate
        ; cmp ax, 17 
        ; jz bxReg_direct
        ; cmp ax, 18
        ; jz bxReg_indirect
        ; cmp ax, 19
        ; jz bxReg_based



bxReg_axReg:
cmp Player_turn,0 
jz p1bal
jmp p2bal

p1bal:
convrt_string_hex P2_regs,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
 mov operand1, ax
 jmp Exit1

p2bal:
convrt_string_hex P2_regs,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

bxReg_bxReg:

cmp Player_turn,0 
jz p1bbl
jmp p2bbl
p1bbl:
convrt_string_hex P1_regs+5,mode
mov operand2,CX
 mov ax,operand1
 mov bx, operand2
 add ax, bx 
mov operand1, ax
jmp Exit1

p2bbl:
convrt_string_hex P2_regs+5,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

bxReg_cxReg:

cmp Player_turn,0 
jz p1bcl
jmp p2bcl

p1bcl:
convrt_string_hex P1_regs+10,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

p2bcl:
convrt_string_hex P2_regs+10,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

bxReg_dxReg:

cmp Player_turn,0 
jz p1bdl
jmp p2bdl
p1bdl:
convrt_string_hex P1_regs+15,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

p2bdl:
convrt_string_hex P2_regs+15,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

bxReg_siReg:

cmp Player_turn,0 
jz p1bsil
jmp p2bsil
p1bsil:
convrt_string_hex P1_regs+20,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

p2bsil:
convrt_string_hex P2_regs+20,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

bxReg_diReg:

cmp Player_turn,0 
jz p1bdil
jmp p2bdil
p1bdil:
convrt_string_hex P1_regs+25,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

p2bdil:
convrt_string_hex P2_regs+25,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

bxReg_spReg:

cmp Player_turn,0 
jz p1bspl
jmp p2bspl
p1bspl:
convrt_string_hex P1_regs+25,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

p2bspl:
convrt_string_hex P2_regs+25,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1



bxReg_bpReg:

cmp Player_turn,0 
jz p1bbpl
jmp p2bbpl
p1bbpl:
convrt_string_hex P1_regs+25,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1

p2bbpl:
convrt_string_hex P2_regs+25,mode
mov operand2,CX
 mov ax,operand1
mov bx, operand2
 add ax, bx 
mov operand1, ax 
jmp Exit1


bxReg_ahReg:
cmp Player_turn,0 
jz p1bahl
jmp p2bahl
p1bahl:
;Decrease points
jmp Exit1

p2bahl:
;Decrease points
jmp Exit1

bxReg_alReg:
cmp Player_turn,0 
jz p1ball
jmp p2ball
p1ball:
;Decrease points
jmp Exit1

p2ball:
;Decrease points
jmp Exit1

bxReg_bhReg:
cmp Player_turn,0 
jz p1bbhl
jmp p2bbhl
p1bbhl:
;Decrease points
jmp Exit1

p2bbhl:
;Decrease points
jmp Exit1


bxReg_blReg:
cmp Player_turn,0 
jz p1bbll
jmp p2bbll
p1bbll:
;Decrease points
jmp Exit1

p2bbll:
;Decrease points
jmp Exit1

bxReg_chReg:
cmp Player_turn,0 
jz p1bchl
jmp p2bchl
p1bchl:
;Decrease points
jmp Exit1

p2bchl:
;Decrease points
jmp Exit1


bxReg_clReg:
cmp Player_turn,0 
jz p1bcll
jmp p2bcll
p1bcll:
;Decrease points
jmp Exit1

p2bcll:
;Decrease points
jmp Exit1


bxReg_dhReg:
cmp Player_turn,0 
jz p1bdhl
jmp p2bdhl
p1bdhl:
;Decrease points
jmp Exit1

p2bdhl:
;Decrease points
jmp Exit1


bxReg_dlReg:
cmp Player_turn,0 
jz p1bdll
jmp p2bdll
p1bdll:
;Decrease points
jmp Exit1

p2bdll:
;Decrease points
jmp Exit1


; bxReg_immediate:
; cmp Player_turn,0 
; jz p1bimml
; jmp p2bimml

; bxReg_direct:
; cmp Player_turn,0 
; jz p1bDirectl
; jmp p2bDirectl

; bxReg_indirect:
; cmp Player_turn,0 
; jz p1bIndirectl
; jmp p2bIndirectl

; bxReg_based:
; cmp Player_turn,0 
; jz p1bBasedl
; jmp p2bBasedl



; cxb:
; cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
; jz p2_CX
; convrt_string_hex P1_regs+10,mode
; mov operand1,CX
; jmp GetSecOp2
; p2_CX:
; convrt_string_hex P2_regs+10,mode
; mov operand1,CX
; GetSecOp2:
;         Call Getbtnclicked
;         cmp ax,0ffffh
;         jz GetSecOp2
;         cmp ax, 0 
;         jz bxReg_axReg  
;         cmp ax, 1 
;         jz bxReg_bxReg
;         cmp ax, 2 
;         jz bxReg_cxReg
;         cmp ax, 3 
;         jz bxReg_dxReg
;         cmp ax, 4 
;         jz bxReg_siReg
;         cmp ax, 5 
;         jz bxReg_diReg
;         cmp ax, 6 
;         jz bxReg_spReg
;         cmp ax, 7 
;         jz bxReg_bpReg
;         cmp ax, 8 
;         jz bxReg_ahReg
;         cmp ax, 9 
;         jz bxReg_alReg
;         cmp ax, 10
;         jz bxReg_bhReg
;         cmp ax, 11
;         jz bxReg_blReg
;         cmp ax, 12 
;         jz bxReg_chReg
;         cmp ax, 13
;         jz bxReg_clReg
;         cmp ax, 14
;         jz bxReg_dhReg
;         cmp ax, 15
;         jz bxReg_dlReg
;         cmp ax, 16
;         jz bxReg_immediate
;         cmp ax, 17 
;         jz bxReg_direct
;         cmp ax, 18
;         jz bxReg_indirect
;         cmp ax, 19
;         jz bxReg_based



; cxReg_axReg:
; cmp Player_turn,0 
; jz p1cal
; jmp p2cal

; p1cal:
; convrt_string_hex P2_regs,mode
; mov operand2,CX
;  mov ax,operand1
;  mov bx, operand2
;  add ax, bx 
;  mov operand1, ax
;  jmp Exit1

; p2cal:
; convrt_string_hex P2_regs,mode
; mov operand2,CX
;  mov ax,operand1
;  mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; cxReg_bxReg:

; cmp Player_turn,0 
; jz p1cbl
; jmp p2cbl
; p1cbl:
; convrt_string_hex P1_regs+5,mode
; mov operand2,CX
;  mov ax,operand1
;  mov bx, operand2
;  add ax, bx 
; mov operand1, ax
; jmp Exit1

; p2cbl:
; convrt_string_hex P2_regs+5,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; cxReg_cxReg:

; cmp Player_turn,0 
; jz p1bcl
; jmp p2bcl

; p1ccl:
; convrt_string_hex P1_regs+10,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; p2ccl:
; convrt_string_hex P2_regs+10,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; cxReg_dxReg:

; cmp Player_turn,0 
; jz p1cdl
; jmp p2cdl
; p1cdl:
; convrt_string_hex P1_regs+15,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; p2cdl:
; convrt_string_hex P2_regs+15,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; cxReg_siReg:

; cmp Player_turn,0 
; jz p1csil
; jmp p2csil
; p1bsil:
; convrt_string_hex P1_regs+20,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; p2csil:
; convrt_string_hex P2_regs+20,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; cxReg_diReg:

; cmp Player_turn,0 
; jz p1cdil
; jmp p2cdil
; p1cdil:
; convrt_string_hex P1_regs+25,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; p2cdil:
; convrt_string_hex P2_regs+25,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; cxReg_spReg:

; cmp Player_turn,0 
; jz p1cspl
; jmp p2cspl
; p1cspl:
; convrt_string_hex P1_regs+25,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; p2cspl:
; convrt_string_hex P2_regs+25,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1



; cxReg_bpReg:

; cmp Player_turn,0 
; jz p1cbpl
; jmp p2cbpl
; p1cbpl:
; convrt_string_hex P1_regs+25,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1

; p2bbpl:
; convrt_string_hex P2_regs+25,mode
; mov operand2,CX
;  mov ax,operand1
; mov bx, operand2
;  add ax, bx 
; mov operand1, ax 
; jmp Exit1


; bxReg_ahReg:
; cmp Player_turn,0 
; jz p1bahl
; jmp p2bahl
; p1bahl:
; ;Decrease points
; jmp Exit1

; p2bahl:
; ;Decrease points
; jmp Exit1

; bxReg_alReg:
; cmp Player_turn,0 
; jz p1ball
; jmp p2ball
; p1ball:
; ;Decrease points
; jmp Exit1

; p2ball:
; ;Decrease points
; jmp Exit1

; bxReg_bhReg:
; cmp Player_turn,0 
; jz p1bbhl
; jmp p2bbhl
; p1bbhl:
; ;Decrease points
; jmp Exit1

; p2bbhl:
; ;Decrease points
; jmp Exit1


; bxReg_blReg:
; cmp Player_turn,0 
; jz p1bbll
; jmp p2bbll
; p1bbll:
; ;Decrease points
; jmp Exit1

; p2bbll:
; ;Decrease points
; jmp Exit1

; bxReg_chReg:
; cmp Player_turn,0 
; jz p1bchl
; jmp p2bchl
; p1bchl:
; ;Decrease points
; jmp Exit1

; p2bchl:
; ;Decrease points
; jmp Exit1


; bxReg_clReg:
; cmp Player_turn,0 
; jz p1bcll
; jmp p2bcll
; p1bcll:
; ;Decrease points
; jmp Exit1

; p2bcll:
; ;Decrease points
; jmp Exit1


; bxReg_dhReg:
; cmp Player_turn,0 
; jz p1bdhl
; jmp p2bdhl
; p1bdhl:
; ;Decrease points
; jmp Exit1

; p2bdhl:
; ;Decrease points
; jmp Exit1


; bxReg_dlReg:
; cmp Player_turn,0 
; jz p1bdll
; jmp p2bdll
; p1bdll:
; ;Decrease points
; jmp Exit1

; p2bdll:
; ;Decrease points
; jmp Exit1


; bxReg_immediate:
; cmp Player_turn,0 
; jz p1bimml
; jmp p2bimml

; bxReg_direct:
; cmp Player_turn,0 
; jz p1bDirectl
; jmp p2bDirectl

; bxReg_indirect:
; cmp Player_turn,0 
; jz p1bIndirectl
; jmp p2bIndirectl

; bxReg_based:
; cmp Player_turn,0 
; jz p1bBasedl
; jmp p2bBasedl
































Exit1:
; mov ax,Player_turn
; cmp ax,0
; jz T1
; T2:
; dec ax
; mov Player_turn,ax
; T1:
; inc ax
; mov Player_turn,ax

ENDM AddOp