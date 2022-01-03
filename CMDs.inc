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