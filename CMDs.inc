Convrt_Hex_String MACRO 
local myloop,continue_convrt,exit_convrt,letter
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
    local exit,letter1,letter2,letter3,letter4,continue,continue2,continue3,continue4
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
    jz reg_CX_P1

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

    reg_AX_P1:
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