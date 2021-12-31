include Drawgrd.inc
include DrawRec.inc
include DrawCir.inc 
include DrawDS.inc
include Drawbtn.inc
include P1regs.inc
include P2regs.inc
include CMDs.inc
include Valid.inc


.model small
.386
.stack 64
.data
xr dw ?
yr dw ?
 
x db ?
y db ?
;;circle related variables
xcm dw ? ;circle midpoint
ycm dw ? ;circle midpoint
xc dw 0
yc dw 0
r dw ?
p dw 0
clr db ?
;command

btn_num           dw ?
num_placeholder   db "0000$"
zeros_placeholder db "0000$"
Player_turn       db 1
Player_num        db ? ; used to know which player register will change
RegToBeUpdated    db ? ; register number that will be updated for the player

;commands operands
operand1 db ?
operand2 db ?


 

;Registers labels--------------------
Lax      db "AX$"
Lbx      db "BX$"
Lcx      db "CX$"
Ldx      db "DX$"
Lsi      db "SI$"
Ldi      db "DI$"
Lsp      db "SP$"
Lbp      db "BP$"
Lah      db "AH$"
Lal      db "AL$"
Lbh      db "BH$"
Lbl      db "BL$"
Lch      db "CH$"
Lcl      db "CL$"
Ldh      db "DH$"
Ldl      db "DL$"
Limd_adr db "VAL$"
Ldir_adr db "[VL]$"
Lind_adr db "[BX]$"
Lbas_adr db "[BX+V]$"


;Registers values---------------------
;           AX[0]   BX[5]   CX[10]  DX[15]  SI[20]  DI[25]  SP[30]  BP[35]
P1_regs db "0000$","0000$","0000$","0000$","0000$","0000$","0000$","0000$"
P2_regs db "0000$","0000$","0000$","0000$","0000$","0000$","0000$","0000$"



db "$$$"
trycatch db "0000$"
 
;command buttons lables------------------
Ladd  db "ADD$"
Ladc  db "ADC$"
Lsub  db "SUB$"
Lsbb  db "SBB$"
Ldiv  db "DIV$"
Lmul  db "MUL$"
Lxor  db "XOR$"
Land  db "AND$"
Lor   db "OR$"
Lnop  db "NOP$";---------new_row----------
Lshr  db "SHR$"
Lshl  db "SHL$"
Lsar  db "SAR$"
Linc  db "INC$"
Ldec  db "DEC$"
Lclc  db "CLC$"
Lidiv db "IDIV$"
Limul db "IMUL$"
Lror  db "ROR$"
Lrol  db "ROL$"
tes  db "testbtn$"


;User string-------------------
ReadUserSTR                db 5,?,5 dup('0'),'0'
ReadUserSTR_type           db ?
ReadUserSTR_syntaxErroFlag db 0  ;0 for no error, 1 for error, RESET FOR USER AFTER HIS TURN HAS ENDED
Valid_input                db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'


.code

main proc far
 
    mov ax,@data
    mov ds,ax
    mov es,ax ;to be able to use string operations
     
    mov ah,0
    mov al,10h  ;;10h 640x350
    int 10h
    

    ;Drawing lines(grid)
    Drawgrd


    ;Drawing Commands buttons
    DrawCommandRow
    ;DrawAddressingRow

    ;Drawing Registers
    
    P1regs
    P2regs
    DrawDS


    mov xcm,90
    mov ycm,170
    mov r,10
    mov clr,0fh
    DrawCir xcm,ycm,xc,yc,p,r,clr
     add xcm,40
    
     mov clr,4
     DrawCir xcm,ycm,xc,yc,p,r,clr
     add xcm,40
     mov clr,0ah
     DrawCir xcm,ycm,xc,yc,p,r,clr
    add xcm,40
    tryfill:
     DrawCir xcm,ycm,xc,yc,p,r,04h
    dec r
    cmp r,0
    jnz tryfill
    ;showing mouse
    mov ax,1
    int 33h


    GameLoop:;============================================
   

    call Getbtnclicked
   
    call GetNumFromUser
     
    mov ax,cx
    mov RegToBeUpdated,2h
    mov Player_num,1h
    UpdateRegValue Player_num, RegToBeUpdated ;new value have to be in AX if 16 bits
                                              ;new value have to be in AH if  8 bits
    DrawAddressingRow
                                           
    P1regs
    P2regs


    mov ReadUserSTR_syntaxErroFlag,0
    jmp GameLoop;======================================   

    ;call Getbtnclicked
     ;cmp ax,0ffffh
     ;jne alo
     ;jmp meshalo
     ;alo:
     ;DrawAddressingRow
     ;meshalo:
     ;mov ah,0
     ;int 16h





    hlt
main endp
 

Getbtnclicked proc near
 
    noleftclick:
            mov ax,0003h
            int 33h ;CX(X), DX(Y)
            test bx,1
            jz noleftclick ;break if user clicked the left click
 
    mov bx,dx
    mov dx,0
    mov ax,cx
    mov cx,64 ;58 is the btn width
    div cx    ;integer division
 
    cmp bx,230
    ja under
 
    mov ax,0ffffh
    jmp exit
 
    under:
        cmp bx,265
        jb row_0
 
        cmp bx,300
        jb row_10
 
        mov ax,0ffffh
        jmp exit
 
        row_0:
            add ax,0
            jmp exit
 
        row_10:
            add ax,10d
 
 
    exit:
 
    mov btn_num,ax
    ret
Getbtnclicked endp

;-----------------------------------------------------

GetNumFromUser proc near

    ;clearing buttons area---
    mov ax,0600h
    mov bh,0
    mov cl,0  ;x1
    mov ch,17 ;y1
    mov dl,80 ;x2
    mov dh,20 ;y2
    int 10h

    ;setting cursor position-----
    mov ah,2
    mov dl,3
    mov dh,17
    int 10h 

    ;Reading string-----
    mov ah,0ah
    mov dx,offset ReadUserSTR
    int 21h
    
    ;Capitalize every letter entered (if exist)
    ;and check for bad input, if syntax error ---> jmp exit_user
    mov dl,[ReadUserSTR+1]
    Valid_user_syntaxError ReadUserSTR+2,dl ;sending the string, and its actual size
    
    cmp ReadUserSTR_syntaxErroFlag,1
    jz exit_user

    ;Checking how many digits the user entered
    cmp ReadUserSTR+1,4
    jz user_16bit_4digit
    cmp ReadUserSTR+1,3
    jz user_16bit_3digit
    cmp ReadUserSTR+1,2
    jz user_8bit_2digit
    cmp ReadUserSTR+1,1
    jz user_8bit_1digit


    ;----------------------
    user_16bit_4digit:
    mov ReadUserSTR_type,1
    convrt_string_hex ReadUserSTR+2,ReadUserSTR_type
    jmp exit_user

    ;----------------------
    user_16bit_3digit:
    mov ReadUserSTR_type,1
    mov ReadUserSTR+1,0 ;moving zero to the actual size slot
    convrt_string_hex ReadUserSTR+1,ReadUserSTR_type
    jmp exit_user

    ;----------------------
    user_8bit_2digit:
    mov ReadUserSTR_type,2
    convrt_string_hex ReadUserSTR,ReadUserSTR_type
    jmp exit_user

    ;----------------------
    user_8bit_1digit:
    mov ReadUserSTR_type,2
    mov ReadUserSTR+1,0 ;moving zero to the actual size slot
    convrt_string_hex ReadUserSTR-1,ReadUserSTR_type
    
    
    exit_user:
    push cx

    ;clearing buttons area---
    mov ax,0600h
    mov bh,0
    mov cl,0  ;x1
    mov ch,17 ;y1
    mov dl,80 ;x2
    mov dh,20 ;y2
    int 10h

    pop cx
    
    ret
GetNumFromUser endp

end main


