include DrawRec.inc
include DrawCir.inc
include DrawFcr.inc 
include DrawDS.inc
include Drawbtn.inc
include P1regs.inc
include P2regs.inc
include Drawgrd.inc
include Gun.inc
include Gun2.inc
include DrawTri.inc
include DrawObj.inc
include CMDs.inc
include BtnAct.inc
include Valid.inc
include Ops.inc
include Pts.inc
include Print.inc
include P1ds.inc
include p2ds.inc

.model small
.386
.stack 64
.data

;---------------MAIN MENU DATA---------------------------------------------
;X & Y positions
point_x                     db ?
point_y                     db ?
 
;user info
user_name                   db 17,?, 17 dup('$')
P1_init_points              db 3,?,2  dup('0'),'$','$'
P2_init_points              db 3,?,2  dup('0'),'$','$'
points_inc_value            dw ? ;points incrementation value
points_inc_index            db ? ;Player1-->1 && Player2-->2


;Messages strings
user_name_message           db 'Enter User Name:',10,13,'$'
user_init_points_message    db 10,13,'Enter Initial Points:',10,13,'$'
main_menu_continue_message  db 10,13,'Press Enter to contiune$'
press_F1_message            db 'Press [F1] to start chatting mode $'
press_F2_message            db 'Press [F2] to game mode $'
press_ESC_message           db 'Press [ESC] to exit program $'
;----------------------------------------------------------------



xr dw ?
yr dw ?
 
x db ?
y db ?
;;circle related variables
xcm dw ? ;circle midpoint
ycm dw ? ;circle midpoint
xc dw 0
yc dw 0
r dw 5
p dw 0
clr db ?
;triangle coordinates
x1 dw ?
y1 dw ?
;Object Coordinates
xo dw ?
yo dw ?
ro dw 10
clro db ?

;command

btn_num           dw ?
num_placeholder   db "0000$"
zeros_placeholder db "0000$"
Player_turn       dw 2
Player_num        db ? ; used to know which player register will change
RegToBeUpdated    db ? ; register number that will be updated for the player
AddrToBeUpdated   db ? ; data segment address number that will be updated for the player
OpBtn             dw ?
;commands operands
operand1 dw 0000
operand2 dw 0000
operand1_btn db ?
operand2_btn db ?
Temp_Mul dw ?
mode db 1 
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
Lind_adr db "[Reg]$"
Lbas_adr db "[Reg+V]$"

;;Falling Objects Data
LFallObjY1 db '0$'
LFallObjR1 db '0$'
LFallObjG1 db '0$'
LFallObjP1 db '0$'
LFallObjY2 db '0$'
LFallObjR2 db '0$'
LFallObjG2 db '0$'
LFallObjP2 db '0$'
NumPos db ?

;Registers and Data Segment values---------------------
;           AX[0]   BX[5]   CX[10]  DX[15]  SI[20]  DI[25]  SP[30]  BP[35]
P1_regs db "0000$","0000$","0000$","0000$","0000$","0000$","0000$","0000$"
;           [0]   [3]   [6]   [9]   [12]  [15]  [18]  [21]  [24]
P1_ds   db "00$","00$","00$","00$","00$","00$","00$","00$","00$"


;           AX[0]   BX[5]   CX[10]  DX[15]  SI[20]  DI[25]  SP[30]  BP[35]
P2_regs db "0000$","0000$","0000$","0000$","0000$","0000$","0000$","0000$"
;           [0]   [3]   [6]   [9]   [12]  [15]  [18]  [21]  [24]
P2_ds   db "00$","00$","00$","00$","00$","00$","00$","00$","00$"


;-----------------------------
DS_labels db "0$","1$","2$","3$","4$","5$","6$","7$","8$"
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
LMov  db "MOV$"
Linc  db "INC$"
Ldec  db "DEC$"
Lclc  db "CLC$"
Lidiv db "IDIV$"
Limul db "IMUL$"
Lror  db "ROR$"
Lrol  db "ROL$"
tes   db "testbtn$"
;User string-------------------
ReadUserSTR                db 5,?,5 dup('0'),'0'
ReadUserSTR_type           db ?
ReadUserSTR_syntaxErroFlag db 0  ;0 for no error, 1 for error, RESET FOR USER AFTER HIS TURN HAS ENDED
Valid_input                db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'
;temps
get_datasegment_address   dw ?
get_datasegment_mode      db ?
temp1                     db ? ; used in get value from data segment

.code

MainMenu proc far

mov ax,@data
mov ds,ax
mov es,ax 

mov ah,0
mov al,3h
int 10h   ;opening text mode

;Taking Input----------------------------------------
ShowMessage user_name_message ;displaying user_name_message 

mov ah,0Ah
mov dx,offset user_name
int 21h   ;taking user name


ShowMessage user_init_points_message  ;displaying user_init_points_message 


mov ah,0Ah
mov dx,offset P1_init_points
int 21h   ;taking user intitial points

ShowMessage user_init_points_message  ;displaying user_init_points_message 


mov ah,0Ah
mov dx,offset P2_init_points
int 21h   ;taking user intitial points


ShowMessage main_menu_continue_message ;displaying main_menu_continue_message
;------------------------------------------------
call AssignSmallestPts ;choosing the lowest points

Enter_Loop:
    mov ah,0
    int 16h     ;getting key pressed from the buffer
    cmp AL,13d  ;checking if user pressed 'Enter'
    jz contiune
    mov ah,0ch
    mov al,0
    int 21h     ;flushing keyboard buffer
    jmp Enter_Loop


contiune: ;continue program
;Main Menu
mov ax,0600h
mov bh,07
mov cx,0000
mov dx,184FH
int 10h  ;clearing whole screen


call CreateMainMenu

;--------------------------------------------


mov ah,0
int 16h

cmp ah,3bh ;if the user pressed F1
jz RunChat

cmp ah,3ch ;if the user pressed F2
jz RunGame

RunChat:
;call chat

jmp exit_main

RunGame:
call PlayGame


exit_main:

MainMenu endp

PlayGame proc near
    mov ax,@data
    mov ds,ax
    mov es,ax 
    mov ah,0
    mov al,10h  ;;10h 640x350
    int 10h
    
    ;Drawing lines(grid)
    Drawgrd
    ;Drawing Commands buttons
    DrawCommandRow
    ;DrawAddressingRow

    ;Drawing Registers
    Update_P1ds
    Update_P2ds
    P1regs
    P2regs
    DrawDS
    UpdatePoints
    

    mov x1,120
    mov y1,180
    Gun x1, y1
    mov x1,440
    mov y1,180
   ; Gun2 x1, y1
    Start_Again:

    
    Drawgrd
    P1regs
    P2regs
    DrawCommandRow
    mov xcm,60
    mov NumPos,7
    DrawObj1 xcm,NumPos
    mov xcm,380
    mov NumPos,47
    DrawObj2 xcm,NumPos
   
    P1regs
    P2regs
    mov ax,1
    int 33h
    GameLoop:;============================================
   
    UpdatePoints
    ; ;;;
    ; mov AddrToBeUpdated,0h
    ; mov Player_num,1h
    ; mov dl,1h
    ; UpdateDataSegmentValue Player_num, AddrToBeUpdated,dl  ;mode 1 for 16bits, mode 2 for 8bits

    ; mov dl,1
    ; mov dh,0
    ; GetDataSegmentValue dl,dh

    ; mov ax,CX
    ; mov AddrToBeUpdated,1h
    ; mov Player_num,2h
    ; mov dl,1h
    ; UpdateDataSegmentValue Player_num, AddrToBeUpdated,dl  ;mode 1 for 16bits, mode 2 for 8bits

    ; DrawAddressingRow
                 
    call Getbtnclicked
   
    cmp ax, 0ffffh
    jnz RegAddMenu 
    jz GameLoop
    RegAddMenu:

    BtnAct  
    P1regs
    
    mov ReadUserSTR_syntaxErroFlag,0
    ;; here check if player points reaches ZERO
    cmp Player_turn,2
    je chng_turn
    jmp chng_turn2
    chng_turn:
    mov Player_turn,1
    jmp Start_Again
    chng_turn2:
    mov Player_turn,2
    jmp Start_Again
    ; call GetNumFromUser ; Value returns in CX ALways 'Must be edited'
     
    ; mov ax,cx
    ; mov RegToBeUpdated,2h
    ; mov Player_num,1h
    ; UpdateRegValue Player_num, RegToBeUpdated ;new value have to be in AX if 16 bits
    ;                                           ;new value have to be in AH if  8 bits
    ; DrawAddressingRow
                                           
    


    ;GameLoop: 
    
    ;showing mouse
    ;mov ax,1
    ;int 33h
    ;call Getbtnclicked
    ;cmp ax, 0ffffh
    ;jnz RegAddMenu 
    ;jz GameLoop
    ;RegAddMenu:

    ;BtnAct

    ; call Getbtnclicked
    ;  cmp ax,0ffffh
    ;  jne alo
    ;  jmp meshalo
    ;  alo:
    ;  DrawAddressingRow
    ;  meshalo:
    ;  mov ah,0
    ;  int 16h


    hlt
    ret
playGame endp

;-----------MAIN MENU PROCEDURES----------------

CreateMainMenu PROC NEAR

mov point_x,32d
mov point_y,10d

SetCursor point_x,point_y

ShowMessage press_F1_message
;----------------------------------------
mov point_x,32d
mov point_y,12d

SetCursor point_x,point_y

ShowMessage press_F2_message
;----------------------------------------
mov point_x,32d
mov point_y,14d

SetCursor point_x,point_y

ShowMessage press_ESC_message
;----------------------------------------

ret
CreateMainMenu ENDP


;Taking lowest points and assigning it to both users
AssignSmallestPts PROC NEAR

cmp P1_init_points+1,2
jnz assign_1num1
mov dl,2
convrt_string_hex P1_init_points,dl
jmp skip_assign1

assign_1num1:
mov dl,3
convrt_string_hex P1_init_points-1,dl
skip_assign1:

mov ax,cx

;--------------------------------------------------
push ax
cmp P2_init_points+1,2
jnz assign_1num2
mov dl,2
convrt_string_hex P2_init_points,dl
jmp skip_assign2

assign_1num2:
mov dl,3
convrt_string_hex P2_init_points-1,dl
skip_assign2:

pop ax
mov bx,cx

mov cx,2
;ax-> Pts1, bx->Pts2
cmp ax,bx
ja assign_pts2

mov si,offset P1_init_points+2
mov di,offset P2_init_points+2
repe movsb

jmp exit_assign

assign_pts2:;player2 has the lowest points

mov si,offset P2_init_points+2
mov di,offset P1_init_points+2
repe movsb

exit_assign:
ret
AssignSmallestPts ENDP

;---------------------------------------------
;---------------------------------------------

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
 
end MainMenu