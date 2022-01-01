Ops MACRO btn_num
Local firstReg,axb1,bxb1,cxb1,dxb1,sib1,dib1,spb1,bpb1,ahb1,alb1,bhb1,blb1,chb1,clb1,dhb1,dlb1,p2_AX,p2_BX,p2_CX,p2_DX,p2_SI,p2_SP,p2_DI,p2_BP,p2_AH,p2_AL,p2_BH,p2_BL,p2_CH,p2_CL,p2_DH,p2_DL,getop2,axb2,bxb2,cxb2,dxb2,sib2,dib2,spb2,bpb2,ahb2,alb2,bhb2,blb2,chb2,clb2,dhb2,dlb2,immediateaddb2,p2_AX2,p2_BX2,p2_CX2,p2_DX2,p2_SI2,p2_DI2,p2_SP2,p2_BP2,p2_AH2,p2_AL2,p2_BH2,p2_BL2,p2_CH2,p2_CL2,p2_DH2,p2_DL2,p2_imm,changein2,changein1,Addition,Exit1,skipp,chk_not_8bits,chk_not_16bits,Errors,skipE,check_halfReg,chk_not_8bits1,chk_not_16bits1,check_halfReg1,skipE1,changein1_1,changein2_1,AddwithCarry
   
firstReg:
mov ah,0ch
mov al,0
int 21h
mov ah,0
int 16h
Call Getbtnclicked 
mov operand1_btn,al
cmp ax,0ffffh
jz firstReg 
cmp ax, 0 
jz axb1
cmp ax, 1 
jz bxb1   
cmp ax, 2 
jz cxb1
cmp ax, 3 
jz dxb1
cmp ax, 4 
jz sib1
cmp ax, 5 
jz dib1
cmp ax, 6 
jz spb1
cmp ax, 7 
jz bpb1
cmp ax, 8 
jz ahb1
cmp ax, 9 
jz alb1
cmp ax, 10
jz bhb1
cmp ax, 11
jz blb1
cmp ax, 12 
jz chb1
cmp ax, 13
jz clb1
cmp ax, 14
jz dhb1
cmp ax, 15
jz dlb1
cmp ax, 16
jz Errors

; cmp ax, 17 
; jz directaddb

; cmp ax, 18
; jz indirectregb

; cmp ax, 19
; jz basedaddb

axb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_AX
convrt_string_hex P1_regs,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_AX:
convrt_string_hex P2_regs,mode
mov operand1,CX
jmp getop2

bxb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BX
convrt_string_hex P1_regs+5,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_BX:
convrt_string_hex P2_regs+5,mode
mov operand1,CX
jmp getop2

cxb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_CX
convrt_string_hex P1_regs+10,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_CX:
convrt_string_hex P2_regs+10,mode
mov operand1,CX
jmp getop2

dxb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DX
convrt_string_hex P1_regs+15,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_DX:
convrt_string_hex P2_regs+15,mode
mov operand1,CX
jmp getop2
;;;;;;bxb1,cxc1.....

sib1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_SI
convrt_string_hex P1_regs+20,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_SI:
convrt_string_hex P2_regs+20,mode
mov operand1,CX
jmp getop2

dib1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DI
convrt_string_hex P1_regs+25,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_DI:
convrt_string_hex P2_regs+25,mode
mov operand1,CX
jmp getop2

spb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_SP
convrt_string_hex P1_regs+30,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_SP:
convrt_string_hex P2_regs+30,mode
mov operand1,CX
jmp getop2

bpb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BP
convrt_string_hex P1_regs+35,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_BP:
convrt_string_hex P2_regs+35,mode
mov operand1,CX
jmp getop2

ahb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_AH
mov mode,2
convrt_string_hex P1_regs-2,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_AH:
mov mode,2
convrt_string_hex P2_regs-2,mode
mov operand1,CX
jmp getop2

alb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_AL
mov mode,2
convrt_string_hex P1_regs,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_AL:
mov mode,2
convrt_string_hex P2_regs,mode
mov operand1,CX
jmp getop2

bhb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BH
mov mode,2
convrt_string_hex P1_regs+3,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_BH:
mov mode,2
convrt_string_hex P2_regs+3,mode
mov operand1,CX
jmp getop2

blb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BL
mov mode,2
convrt_string_hex P1_regs+5,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_BL:
mov mode,2
convrt_string_hex P2_regs+5,mode
mov operand1,CX
jmp getop2

chb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_CH
mov mode,2
convrt_string_hex P1_regs+8,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_CH:
mov mode,2
convrt_string_hex P2_regs+8,mode
mov operand1,CX
jmp getop2

clb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_CL
mov mode,2
convrt_string_hex P1_regs+10,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_CL:
mov mode,2
convrt_string_hex P2_regs+10,mode
mov operand1,CX
jmp getop2

dhb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DH
mov mode,2
convrt_string_hex P1_regs+13,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_DH:
mov mode,2
convrt_string_hex P2_regs+13,mode
mov operand1,CX
jmp getop2

dlb1:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DL
mov mode,2
convrt_string_hex P1_regs+15,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand1,CX
jmp getop2
p2_DL:
mov mode,2
convrt_string_hex P2_regs+15,mode
mov operand1,CX
jmp getop2

getop2:
        mov ah,0ch
        mov al,0
        int 21h
        mov ah,0
        int 16h
        Call Getbtnclicked
        mov operand2_btn,al
        cmp ax,0ffffh
        jz getop2
        cmp ax, 0 
        jz axb2
        cmp ax, 1 
        jz bxb2
        cmp ax, 2 
        jz cxb2
        cmp ax, 3 
        jz dxb2
        cmp ax, 4 
        jz sib2
        cmp ax, 5 
        jz dib2
        cmp ax, 6 
        jz spb2
        cmp ax, 7 
        jz bpb2
        cmp ax, 8 
        jz ahb2
        cmp ax, 9 
        jz alb2
        cmp ax, 10
        jz bhb2
        cmp ax, 11
        jz blb2
        cmp ax, 12 
        jz chb2
        cmp ax, 13
        jz clb2
        cmp ax, 14
        jz dhb2
        cmp ax, 15
        jz dlb2
        cmp ax, 16
        jz immediateaddb2
        ; cmp ax, 17  
        ; jz directaddb2
        ; cmp ax, 18
        ; jz indirectregb2
        ; cmp ax, 19
        ; jz basedaddb2
axb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_AX2
convrt_string_hex P1_regs,mode
mov operand2,CX
jmp skipp
p2_AX2:
convrt_string_hex P2_regs,mode
mov operand2,CX
jmp skipp

bxb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BX2
convrt_string_hex P1_regs+5,mode
mov operand2,CX
jmp skipp
p2_BX2:
convrt_string_hex P2_regs+5,mode
mov operand2,CX
jmp skipp

cxb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_CX2
convrt_string_hex P1_regs+10,mode
mov operand2,CX
jmp skipp
p2_CX2:
convrt_string_hex P2_regs+10,mode
mov operand2,CX
jmp skipp

dxb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DX2
convrt_string_hex P1_regs+15,mode
mov operand2,CX
jmp skipp
p2_DX2:
convrt_string_hex P2_regs+15,mode
mov operand2,CX
jmp skipp

sib2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_SI2
convrt_string_hex P1_regs+20,mode
mov operand2,CX
jmp skipp
p2_SI2:
convrt_string_hex P2_regs+20,mode
mov operand2,CX
jmp skipp

dib2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DI2
convrt_string_hex P1_regs+25,mode
mov operand2,CX
jmp skipp
p2_DI2:
convrt_string_hex P2_regs+25,mode
mov operand2,CX
jmp skipp

spb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_SP2
convrt_string_hex P1_regs+30,mode
mov operand2,CX
jmp skipp
p2_SP2:
convrt_string_hex P2_regs+30,mode
mov operand2,CX
jmp skipp

bpb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BP2
convrt_string_hex P1_regs+30,mode
mov operand2,CX
jmp skipp
p2_BP2:
convrt_string_hex P2_regs+30,mode
mov operand2,CX
jmp skipp

ahb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_AH2
mov mode,2
convrt_string_hex P1_regs-2,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX
jmp skipp
p2_AH2:
mov mode,2
convrt_string_hex P2_regs-2,mode
mov operand2,CX
jmp skipp

alb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_AL2
mov mode,2
convrt_string_hex P1_regs,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX ; '0000'
jmp skipp
p2_AL2:
mov mode,2
convrt_string_hex P2_regs,mode
mov operand2,CX
jmp skipp

bhb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BH2
mov mode,2
convrt_string_hex P1_regs+3,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX
jmp skipp
p2_BH2:
mov mode,2
convrt_string_hex P2_regs+3,mode
mov operand2,CX
jmp skipp

blb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_BL2
mov mode,2
convrt_string_hex P1_regs+5,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX
jmp skipp
p2_BL2:
mov mode,2
convrt_string_hex P2_regs+5,mode
mov operand2,CX
jmp skipp

chb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_CH2
mov mode,2
convrt_string_hex P1_regs+8,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX
jmp skipp
p2_CH2:
mov mode,2
convrt_string_hex P2_regs+8,mode
mov operand2,CX
jmp skipp

clb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_CL2
mov mode,2
convrt_string_hex P1_regs+10,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX
jmp skipp
p2_CL2:
mov mode,2
convrt_string_hex P2_regs+10,mode
mov operand2,CX
jmp skipp

dhb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DH2
mov mode,2
convrt_string_hex P1_regs+13,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX
jmp skipp
p2_DH2:
mov mode,2
convrt_string_hex P2_regs+13,mode
mov operand2,CX
jmp skipp

dlb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_DL2
mov mode,2
convrt_string_hex P1_regs+15,mode ;; me7tageen ne8ayar P1_regs 3ala 7asab meen first operand P1_regs+5,10,15
mov operand2,CX
jmp skipp
p2_DL2:
mov mode,2
convrt_string_hex P2_regs+15,mode
mov operand2,CX
jmp skipp

immediateaddb2:
cmp Player_turn,1   ;Player_turn = 0 if it's P1's Player_turn , Player_turn =1 if it's P2's Player_turn
jz p2_imm
call GetNumFromUser ; Value returns in CX ALways 'Must be edited'
mov operand2,cx
; mov bh,operand1_btn
; mov RegToBeUpdated,bh
; mov Player_num,1h
; UpdateRegValue Player_num, RegToBeUpdated
jmp skipp
p2_imm:
call GetNumFromUser ; Value returns in CX ALways 'Must be edited'
mov operand2,cx
; mov bh,operand1_btn
; mov RegToBeUpdated,bh
; mov Player_num,2h
; UpdateRegValue Player_num, RegToBeUpdated
jmp skipp

;;bxb2,cxb2.....

skipp:
;;:OPS;;;;
cmp OpBtn,0
jz Addition
cmp OpBtn,1
jz AddwithCarry


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Addition:
;Addition Validations ;; RegIndirect, direct addressing should be handled as well !!
cmp operand1_btn,7
jbe chk_not_8bits
jmp chk_not_16bits

chk_not_8bits:
cmp operand2_btn,7
ja check_halfReg
jmp skipE
check_halfReg:
cmp operand2_btn,15
jbe Errors

chk_not_16bits:
cmp operand2_btn,7
jbe Errors

skipE:
mov ax,operand1
mov bx,operand2
add ax,bx
cmp Player_turn,1
je changein2
jmp changein1
changein2:
mov bh,operand1_btn
mov RegToBeUpdated,bh
mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1
changein1:
mov bh,operand1_btn
mov RegToBeUpdated,bh
mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
AddwithCarry:
;Addition Validations ;; RegIndirect, direct addressing should be handled as well !!
cmp operand1_btn,7
jbe chk_not_8bits1
jmp chk_not_16bits1

chk_not_8bits1:
cmp operand2_btn,7
ja check_halfReg1
jmp skipE1
check_halfReg1:
cmp operand2_btn,15
jbe Errors

chk_not_16bits1:
cmp operand2_btn,7
jbe Errors

skipE1:
mov ax,operand1
mov bx,operand2
adc ax,bx
cmp Player_turn,1
je changein2_1
jmp changein1_1
changein2_1:
mov bh,operand1_btn
mov RegToBeUpdated,bh
mov Player_num,2h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1
changein1_1:
mov bh,operand1_btn
mov RegToBeUpdated,bh
mov Player_num,1h
UpdateRegValue Player_num,RegToBeUpdated
jmp Exit1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

Errors:


Exit1:
ENDM Ops