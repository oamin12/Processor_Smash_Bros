Convrt_Hex_String MACRO 
local myloop,continue_convrt,exit_convrt,letter,continue,bit_0,bit_1,bit_2,bit_3
;output string will be in  num_placeholder variable
mov cl,0d
mov bx, 10h
myloop:
	mov dx,0
    div bx
	cmp dx,9h
	ja letter
	
    add dx,30h;if number add 30h
    inc cl
	jmp continue
	
	letter:;if letter add 37h
	add dx,37h
    inc cl
	jmp continue

    add ax,0   
jnz myloop
jmp exit_convrt

continue:
    cmp cl,1
    jz bit_0

    cmp cl,2
    jz bit_1

    cmp cl,3
    jz bit_2

    cmp cl,4
    jz bit_3

    jmp exit_convrt

    bit_0:
        mov num_placeholder+3,dl
        jmp myloop 

    bit_1:
        mov num_placeholder+2,dl
        jmp myloop 

    bit_2:
        mov num_placeholder+1,dl
        jmp myloop 

    bit_3:
        mov num_placeholder+0,dl
        jmp myloop    


exit_convrt:

ENDM Convrt_Hex_String     


ResetPlaceholder MACRO

;ALWAYS RESET THE PLACEHOLDER
mov si,offset zeros_placeholder
mov di,offset num_placeholder

mov cx,4
repe movsb

ENDM ResetPlaceholder


convrt_string_hex MACRO STR,mode 
    local exit,letter1,letter2,letter3,letter4,continue1,continue2,continue3,continue4
    ;Starting from least to most significant digits
    ;mn el yemeen L el shemal ya fala7een ya3ny
    
    ;INSTRUCTIONS: 
    ;RESULT-->CX
    ;if you want to use it for AL
    ;Then when call mode=2, STR=offset AX 3ady gedan
    ;----------------
    ;BUT for AH
    ;Then when call mode=2, !!!STR=offset AX-2!!!
    ;minus 2 lama te3melaha call tab3an msh hena ya alby              
    
    ;First digit
    
    mov cx,0
    mov di,offset STR
    mov cl,[di+3]
    cmp cl,41h
    
    jae letter1
    
    sub cl,30h
    jmp continue1
    
    letter1: 
    sub cl,37h
    
    continue1:

    cmp mode,3
    jz exit
    ;------------------
    ;Second digit
    mov al,[di+2]
    mov ah,0
    cmp al,41h
    jae letter2 
    
    sub al,30h
    jmp continue2
    
    letter2:
    sub al,37h
    
    
    continue2:
    
    mov bl,10h
    mul bx 
    
    add al,cl
    mov cl,al 
    
    
    cmp mode,2
    jz exit
    
    ;-----------------
    ;Third digit
    mov al,[di+1]
    mov ah,0
   
    cmp al,41h
    jae letter3 
    
    sub al,30h
    jmp continue3
    
    letter3:
    sub al,37h
    
    
    continue3:
    
    
    mov dx,0
    mov bx,100h
    mul bx 
    
    add ax,cx
    mov cx,ax
    
    ;---------------- 
    ;Fourth digit
    mov al,[di]
    mov ah,0
    cmp al,41h
    
    jae letter4 
    
    sub al,30h
    jmp continue4
    letter4:
    sub al,37h
    
    
    continue4:
     
    
    mov dx,0
    mov bx,1000h
    mul bx
    
    add ax,cx
    mov cx,ax
    
    exit:
    mov ax,0
    
    
ENDM convrt_string_hex


UpdateRegValue MACRO p_num, reg_num
    Local reg_AX_p1, reg_BX_p1,reg_CX_p1,reg_DX_p1,reg_SI_p1,reg_DI_p1,reg_SP_p1,reg_BP_p1,Reg_byte_player1,reg_AH_p1,reg_AL_p1,reg_BH_p1,reg_BL_p1,reg_CH_p1,reg_CL_p1,reg_DH_p1,reg_DL_p1,PLAYER_2_REGS,reg_AX_p2,reg_BX_p2,reg_CX_p2,reg_DX_p2,reg_SI_p2,reg_DI_p2,reg_SP_p2,reg_BP_p2,Reg_byte_player2,reg_AH_p2,reg_AL_p2,reg_BH_p2,reg_BL_p2,reg_CH_p2,reg_CL_p2,reg_DH_p2,reg_DL_p2,exit_updt
   
    Convrt_Hex_String ;takes the new value from AX and converts it to string in num_placeholder variable
    
    mov si,offset num_placeholder

    cmp p_num,2
    jz PLAYER_2_REGS

    ;PLAYER-1 REGISTERS
    cmp reg_num,7
    ja Reg_byte_player1

    ;16-bit registers--------------------------------------------
    mov cx,4
    cmp reg_num,0
    jz reg_AX_p1

    cmp reg_num,1
    jz reg_BX_p1

    cmp reg_num,2
    jz reg_CX_p1

    cmp reg_num,3
    jz reg_DX_p1

    cmp reg_num,4
    jz reg_SI_p1

    cmp reg_num,5
    jz reg_DI_p1

    cmp reg_num,6
    jz reg_SP_p1

    cmp reg_num,7
    jz reg_BP_p1

    reg_AX_p1:
    mov di,offset P1_regs
    rep movsb   
    jmp exit_updt

    reg_BX_p1:
    mov di,offset P1_regs+5
    rep movsb   
    jmp exit_updt

    reg_CX_p1:
    mov di,offset P1_regs+10
    rep movsb   
    jmp exit_updt

    reg_DX_p1:
    mov di,offset P1_regs+15
    rep movsb   
    jmp exit_updt

    reg_SI_p1:
    mov di,offset P1_regs+20
    rep movsb   
    jmp exit_updt

    reg_DI_p1:
    mov di,offset P1_regs+25
    rep movsb   
    jmp exit_updt

    reg_SP_p1:
    mov di,offset P1_regs+30
    rep movsb   
    jmp exit_updt

    reg_BP_p1:
    mov di,offset P1_regs+35
    rep movsb   
    jmp exit_updt

    ;8-bit registers-----------------------------------------------

    Reg_byte_player1:
    add si,2

    mov cx,2
    cmp reg_num,8d
    jz reg_AH_p1

    cmp reg_num,9d
    jz reg_AL_p1

    cmp reg_num,10d
    jz reg_BH_p1

    cmp reg_num,11d
    jz reg_BL_p1

    cmp reg_num,12d
    jz reg_CH_p1

    cmp reg_num,13d
    jz reg_CL_p1

    cmp reg_num,14d
    jz reg_DH_p1

    cmp reg_num,15d
    jz reg_DL_p1

    reg_AH_p1:
    mov di,offset P1_regs
    rep movsb   
    jmp exit_updt

    reg_AL_p1:
    mov di,offset P1_regs+2
    rep movsb   
    jmp exit_updt

    reg_BH_p1:
    mov di,offset P1_regs+5
    rep movsb   
    jmp exit_updt

    reg_BL_p1:
    mov di,offset P1_regs+7
    rep movsb   
    jmp exit_updt

    reg_CH_p1:
    mov di,offset P1_regs+10
    rep movsb   
    jmp exit_updt

    reg_CL_p1:
    mov di,offset P1_regs+12
    rep movsb   
    jmp exit_updt

    reg_DH_p1:
    mov di,offset P1_regs+15
    rep movsb   
    jmp exit_updt

    reg_DL_p1:
    mov di,offset P1_regs+17
    rep movsb   
    jmp exit_updt

    ; ;PLAYER-2 -----------------------------------------------------
    PLAYER_2_REGS:

    cmp reg_num,7
    ja Reg_byte_player2

    ;16-bit registers--------------------------------------------
    mov cx,4
    cmp reg_num,0
    jz reg_AX_p2

    cmp reg_num,1
    jz reg_BX_p2

    cmp reg_num,2
    jz reg_CX_p2

    cmp reg_num,3
    jz reg_DX_p2

    cmp reg_num,4
    jz reg_SI_p2

    cmp reg_num,5
    jz reg_DI_p2

    cmp reg_num,6
    jz reg_SP_p2

    cmp reg_num,7
    jz reg_BP_p2

    reg_AX_p2:
    mov di,offset P2_regs
    rep movsb   
    jmp exit_updt

    reg_BX_p2:
    mov di,offset P2_regs+5
    rep movsb   
    jmp exit_updt

    reg_CX_p2:
    mov di,offset P2_regs+10
    rep movsb   
    jmp exit_updt

    reg_DX_p2:
    mov di,offset P2_regs+15
    rep movsb   
    jmp exit_updt

    reg_SI_p2:
    mov di,offset P2_regs+20
    rep movsb   
    jmp exit_updt

    reg_DI_p2:
    mov di,offset P2_regs+25
    rep movsb   
    jmp exit_updt

    reg_SP_p2:
    mov di,offset P2_regs+30
    rep movsb   
    jmp exit_updt

    reg_BP_p2:
    mov di,offset P2_regs+35
    rep movsb   
    jmp exit_updt

    ;8-bit registers-----------------------------------------------

    Reg_byte_player2:
    add si,2

    mov cx,2
    cmp reg_num,8d
    jz reg_AH_p2

    cmp reg_num,9d
    jz reg_AL_P2

    cmp reg_num,10d
    jz reg_BH_p2

    cmp reg_num,11d
    jz reg_BL_p2

    cmp reg_num,12d
    jz reg_CH_p2

    cmp reg_num,13d
    jz reg_CL_p2

    cmp reg_num,14d
    jz reg_DH_p2

    cmp reg_num,15d
    jz reg_DL_p2

    reg_AH_p2:
    mov di,offset P2_regs
    rep movsb   
    jmp exit_updt

    reg_AL_p2:
    mov di,offset P2_regs+2
    rep movsb   
    jmp exit_updt

    reg_BH_p2:
    mov di,offset P2_regs+5
    rep movsb   
    jmp exit_updt

    reg_BL_p2:
    mov di,offset P2_regs+7
    rep movsb   
    jmp exit_updt

    reg_CH_p2:
    mov di,offset P2_regs+10
    rep movsb   
    jmp exit_updt

    reg_CL_p2:
    mov di,offset P2_regs+12
    rep movsb   
    jmp exit_updt

    reg_DH_p2:
    mov di,offset P2_regs+15
    rep movsb   
    jmp exit_updt

    reg_DL_p2:
    mov di,offset P2_regs+17
    rep movsb   
    jmp exit_updt

    exit_updt:
    

    ResetPlaceholder

ENDM UpdateRegValue


UpdateDataSegmentValue MACRO p_num, address, mode ;player number, adress 0->8, 1->16bit, 2->update8bits
    local updt_oneSegment_p1,updt_oneSegment_p2,player2_data,exit_updtdatasegment,data_0_p1,data_1_p1,data_2_p1,data_3_p1,data_4_p1,data_5_p1,data_6_p1,data_7_p1,data_8_p1,data_0_p2,data_1_p2,data_2_p2,data_3_p2,data_4_p2,data_5_p2,data_6_p2,data_7_p2,data_8_p2,data_0_p11,data_1_p11,data_2_p11,data_3_p11,data_4_p11,data_5_p11,data_6_p11,data_7_p11,data_8_p11,data_0_p22,data_1_p22,data_2_p22,data_3_p22,data_4_p22,data_5_p22,data_6_p22,data_7_p22,data_8_p22

    Convrt_Hex_String ;takes the new value from AX and converts it to string in num_placeholder variable
    mov si,offset num_placeholder

    cmp p_num,2
    jz player2_data


    cmp mode,2
    jz updt_oneSegment_p1

    mov cx,2

    cmp address,0
    jz data_0_p11

    cmp address,1
    jz data_1_p11

    cmp address,2
    jz data_2_p11

    cmp address,3
    jz data_3_p11

    cmp address,4
    jz data_4_p11

    cmp address,5
    jz data_5_p11

    cmp address,6
    jz data_6_p11

    cmp address,7
    jz data_7_p11

    cmp address,8
    jz data_8_p11

    ;------------------------------------------
    data_0_p11:
    mov di,offset P1_ds[3]
    rep movsb
    mov cx,2
    mov di,offset P1_ds
    rep movsb   
    jmp exit_updtdatasegment

    data_1_p11:
    mov di,offset P1_ds[6]
    rep movsb
    mov cx,2
    mov di,offset P1_ds[3]
    rep movsb   
    jmp exit_updtdatasegment

    data_2_p11:
    mov di,offset P1_ds[9]
    rep movsb
    mov cx,2
    mov di,offset P1_ds[6]
    rep movsb   
    jmp exit_updtdatasegment

    data_3_p11:
    mov di,offset P1_ds[12]
    rep movsb
    mov cx,2
    mov di,offset P1_ds[9]
    rep movsb   
    jmp exit_updtdatasegment

    data_4_p11:
    mov di,offset P1_ds[15]
    rep movsb
    mov cx,2
    mov di,offset P1_ds[12]
    rep movsb   
    jmp exit_updtdatasegment

    data_5_p11:
    mov di,offset P1_ds[18]
    rep movsb
    mov cx,2
    mov di,offset P1_ds[15]
    rep movsb   
    jmp exit_updtdatasegment

    data_6_p11:
    mov di,offset P1_ds[21]
    rep movsb
    mov cx,2
    mov di,offset P1_ds[18]
    rep movsb   
    jmp exit_updtdatasegment

    data_7_p11:
    mov di,offset P1_ds[24]
    rep movsb
    mov cx,2
    mov di,offset P1_ds[21]
    rep movsb   
    jmp exit_updtdatasegment

    data_8_p11:
    mov di,offset P1_ds
    rep movsb
    mov cx,2
    mov di,offset P1_ds[24]
    rep movsb   
    jmp exit_updtdatasegment



    jmp exit_updtdatasegment
    ;-----------------------------------
    ;-----------------------------------
    updt_oneSegment_p1:
    add si,2
    mov cx,2

    cmp address,0
    jz data_0_p1

    cmp address,1
    jz data_1_p1

    cmp address,2
    jz data_2_p1

    cmp address,3
    jz data_3_p1

    cmp address,4
    jz data_4_p1

    cmp address,5
    jz data_5_p1

    cmp address,6
    jz data_6_p1

    cmp address,7
    jz data_7_p1

    cmp address,8
    jz data_8_p1



    data_0_p1:
    mov di,offset P1_ds
    
    rep movsb   
    jmp exit_updtdatasegment

    data_1_p1:
    mov di,offset P1_ds[3]
    rep movsb   
    jmp exit_updtdatasegment

    data_2_p1:
    mov di,offset P1_ds[6]
    rep movsb   
    jmp exit_updtdatasegment

    data_3_p1:
    mov di,offset P1_ds[9]
    rep movsb   
    jmp exit_updtdatasegment

    data_4_p1:
    mov di,offset P1_ds[12]
    rep movsb   
    jmp exit_updtdatasegment

    data_5_p1:
    mov di,offset P1_ds[15]
    rep movsb   
    jmp exit_updtdatasegment

    data_6_p1:
    mov di,offset P1_ds[18]
    rep movsb   
    jmp exit_updtdatasegment

    data_7_p1:
    mov di,offset P1_ds[21]
    rep movsb   
    jmp exit_updtdatasegment

    data_8_p1:
    mov di,offset P1_ds[24]
    rep movsb   
    jmp exit_updtdatasegment
    

    ;-----------------------------------------
    player2_data:

    cmp mode,2
    jz updt_oneSegment_p2

    mov cx,2

    cmp address,0
    jz data_0_p22

    cmp address,1
    jz data_1_p22

    cmp address,2
    jz data_2_p22

    cmp address,3
    jz data_3_p22

    cmp address,4
    jz data_4_p22

    cmp address,5
    jz data_5_p22

    cmp address,6
    jz data_6_p22

    cmp address,7
    jz data_7_p22

    cmp address,8
    jz data_8_p22

    ;------------------------------------------
    data_0_p22:
    mov di,offset P2_ds[3]
    rep movsb
    mov cx,2
    mov di,offset P2_ds
    rep movsb   
    jmp exit_updtdatasegment

    data_1_p22:
    mov di,offset P2_ds[6]
    rep movsb
    mov cx,2
    mov di,offset P2_ds[3]
    rep movsb   
    jmp exit_updtdatasegment

    data_2_p22:
    mov di,offset P2_ds[9]
    rep movsb
    mov cx,2
    mov di,offset P2_ds[6]
    rep movsb   
    jmp exit_updtdatasegment

    data_3_p22:
    mov di,offset P2_ds[12]
    rep movsb
    mov cx,2
    mov di,offset P2_ds[9]
    rep movsb   
    jmp exit_updtdatasegment

    data_4_p22:
    mov di,offset P2_ds[15]
    rep movsb
    mov cx,2
    mov di,offset P2_ds[12]
    rep movsb   
    jmp exit_updtdatasegment

    data_5_p22:
    mov di,offset P2_ds[18]
    rep movsb
    mov cx,2
    mov di,offset P2_ds[15]
    rep movsb   
    jmp exit_updtdatasegment

    data_6_p22:
    mov di,offset P2_ds[21]
    rep movsb
    mov cx,2
    mov di,offset P2_ds[18]
    rep movsb   
    jmp exit_updtdatasegment

    data_7_p22:
    mov di,offset P2_ds[24]
    rep movsb
    mov cx,2
    mov di,offset P2_ds[21]
    rep movsb   
    jmp exit_updtdatasegment

    data_8_p22:
    mov di,offset P2_ds
    rep movsb
    mov cx,2
    mov di,offset P2_ds[24]
    rep movsb   
    jmp exit_updtdatasegment


    jmp exit_updtdatasegment
    ;------------------------------------------
    updt_oneSegment_p2:
    add si,2
    mov cx,2

    cmp address,0
    jz data_0_p2

    cmp address,1
    jz data_1_p2

    cmp address,2
    jz data_2_p2

    cmp address,3
    jz data_3_p2

    cmp address,4
    jz data_4_p2

    cmp address,5
    jz data_5_p2

    cmp address,6
    jz data_6_p2

    cmp address,7
    jz data_7_p2

    cmp address,8
    jz data_8_p2



    data_0_p2:
    add si,2
    mov di,offset P2_ds
    
    rep movsb   
    jmp exit_updtdatasegment

    data_1_p2:
    mov di,offset P2_ds[3]
    rep movsb   
    jmp exit_updtdatasegment

    data_2_p2:
    mov di,offset P2_ds[6]
    rep movsb   
    jmp exit_updtdatasegment

    data_3_p2:
    mov di,offset P2_ds[9]
    rep movsb   
    jmp exit_updtdatasegment

    data_4_p2:
    mov di,offset P2_ds[12]
    rep movsb   
    jmp exit_updtdatasegment

    data_5_p2:
    mov di,offset P2_ds[15]
    rep movsb   
    jmp exit_updtdatasegment

    data_6_p2:
    mov di,offset P2_ds[18]
    rep movsb   
    jmp exit_updtdatasegment

    data_7_p2:
    mov di,offset P2_ds[21]
    rep movsb   
    jmp exit_updtdatasegment

    data_8_p2:
    mov di,offset P2_ds[24]
    rep movsb   
    jmp exit_updtdatasegment


    ;ERROR_updtdatasegment:
    ;mov DataSegment_error,1


    exit_updtdatasegment:

    ResetPlaceholder

ENDM UpdateDataSegmentValue




GetDataSegmentValue MACRO pnum,address ;little indian lw 3ayez 1 dakhalo 0 w khod ch
    local exit_getdatasegmentvalue,player2,data_01_p1,data_23_p1,data_45_p1,data_67_p1,data_80_p1,data_01_p2,data_23_p2,data_45_p2,data_67_p2,data_80_p2
    ;convrt_string_hex MACRO STR,mode 
    ;This macro always returns 2 data slots in CH:CL respectively
    ;e.g: address =1 , therefore CX= addr[2]:addr[1] both of them, if u need the exact address u sent (8bits slot) use CH
    ;NOTE: address values: 0, 2, 4, 6, 8

    cmp pnum,2
    jz player2

    mov get_datasegment_mode,2

    cmp address,0
    jz data_01_p1

    cmp address,2
    jz data_23_p1

    cmp address,4
    jz data_45_p1

    cmp address,6
    jz data_67_p1

    cmp address,8
    jz data_80_p1
    

    
    data_01_p1:
    convrt_string_hex P1_ds-2,get_datasegment_mode
    mov temp1,cl
    convrt_string_hex P1_ds+1,get_datasegment_mode
    mov ch,temp1
    xchg ch,cl
    jmp exit_getdatasegmentvalue


    data_23_p1:
    convrt_string_hex P1_ds+4,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P1_ds+7,get_datasegment_mode
    jmp exit_getdatasegmentvalue


    data_45_p1:
    convrt_string_hex P1_ds+10,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P1_ds+13,get_datasegment_mode
    jmp exit_getdatasegmentvalue


    data_67_p1:
    convrt_string_hex P1_ds+16,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P1_ds+19,get_datasegment_mode
    jmp exit_getdatasegmentvalue


    data_80_p1:
    convrt_string_hex P1_ds+22,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P1_ds-2,get_datasegment_mode
    jmp exit_getdatasegmentvalue


    ;-----------------------------------
    player2:

    cmp address,0
    jz data_01_p2

    cmp address,2
    jz data_23_p2

    cmp address,4
    jz data_45_p2

    cmp address,6
    jz data_67_p2

    cmp address,8
    jz data_80_p2
    

    
    data_01_p2:
    convrt_string_hex P2_ds-2,get_datasegment_mode
    mov temp1,cl
    convrt_string_hex P2_ds+1,get_datasegment_mode
    mov ch,temp1
    xchg ch,cl
    jmp exit_getdatasegmentvalue


    data_23_p2:
    convrt_string_hex P2_ds+4,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P2_ds+7,get_datasegment_mode
    jmp exit_getdatasegmentvalue


    data_45_p2:
    convrt_string_hex P2_ds+10,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P2_ds+13,get_datasegment_mode
    jmp exit_getdatasegmentvalue


    data_67_p2:
    convrt_string_hex P2_ds+16,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P2_ds+19,get_datasegment_mode
    jmp exit_getdatasegmentvalue


    data_80_p2:
    convrt_string_hex P2_ds+22,get_datasegment_mode
    xchg ch,cl
    convrt_string_hex P2_ds-2,get_datasegment_mode
    jmp exit_getdatasegmentvalue
    


    exit_getdatasegmentvalue:
ENDM GetDataSegmentValue    