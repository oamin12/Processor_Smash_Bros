include DrawRec.inc
include DrawCir.inc
include DrawFcr.inc
include DrawDS.inc
include Drawbtn.inc
include Drawgrd.inc
include DrawTri.inc
include DrawObj.inc
include CMDs.inc
include BtnAct.inc
include Valid.inc
include Ops.inc
include Pts.inc
include Print.inc
;include Pwrups.inc
.model small
.stack 64
.386
.data

;---------------MAIN MENU DATA---------------------------------------------
;X & Y positions
point_x                     db ?
point_y                     db ?

;user info
user_name                  db 17,?, 17 dup('$')
user2_name                  db 17,?, 17 dup('$')

P1_init_points              db 3,?,2  dup('0'),'$','$'
P2_init_points              db 3,?,2  dup('0'),'$','$'
P1_FrbdnChar                db 2,?,2  dup('?'),'$','$'
P2_FrbdnChar                db 2,?,2  dup('?'),'$','$'
frbdncharflg                db 0

points_inc_value            dw ? ;points incrementation value
points_inc_index            db ? ;Player1-->1 && Player2-->2

;Messages strings
user_name_message           db 'Enter User 1 Name:',10,13,'$'
user2_name_message          db 10,13,'Enter User 2 Name:',10,13,'$'
user1_FrbdnChar             db 10,13,'Enter Player 1 forbidden Character:',10,13,'$'
user2_FrbdnChar             db 10,13,'Enter Player 2 forbidden Character:',10,13,'$'
user_init_points_message    db 10,13,'Enter Initial Points:',10,13,'$'
main_menu_continue_message  db 10,13,'Press Enter to contiune$'
press_F1_message            db 'Press [F1] to start chatting mode $'
press_F2_message            db 'Press [F2] to game mode $'
press_ESC_message           db 'Press [ESC] to exit program $'
;----------------------------------------------------------------

commandString               db 10  dup('$')
resetcmdStr               db 10  dup('$')
cmndStringLength            dw 0

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
Lind_adr db "[BX]$"
Lbas_adr db "[BX+V]$"

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
Lmov  db "MOV$"
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
ReadUserSTR_frbdn          db 8 dup('$'),'$','$'
ReadUserSTR_frbdn_Size     dw ?
ReadUserSTR_type           db ?
ReadUserSTR_syntaxErroFlag db 0  ;0 for no error, 1 for error, RESET FOR USER AFTER HIS TURN HAS ENDED
Valid_input                db '0','1','2','3','4','5','6','7','8','9','A','B','C','D','E','F'

;power ups---------------------
flagpower1_own             db 0
flagpower2_ownandopponent  db 0

flagpower3_changefbdn_p1   db 0 ;can only be used once by a player
flagpower3_changefbdn_p2   db 0 ;can only be used once by a player

flagpower4_stuckbit        db 0 ;value to be used is in CX
power4_stuckbitvalue       dw ?
power4_stuckbitindex       dw ?

flagpower5_regclr_p1       db 0
flagpower5_regclr_p2       db 0

powerup_error              db 0

;-----------------------------------------

;temps
get_datasegment_address   dw ?
get_datasegment_mode      db ?
temp1                     db ? ; used in get value from data segment
player_points             dw ? ;used in power ups
temp_reg_num              db ? ;used in power ups
tempFrbd                  dw ?
p_num                     db ?
reg_num                   db ?
address                   db ?
.code

Main proc far
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

ShowMessage user2_name_message ;displaying user_name_message

mov ah,0Ah
mov dx,offset user2_name
int 21h   ;taking user2 name

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
mov ax,0600h
mov bh,07
mov cx,0000
mov dx,184FH
int 10h  ;clearing whole screen
call ChooseFrbdn
call PlayGame


exit_main:

Main endp

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
    call Update_P1ds
    call Update_P2ds
    call P1regs
    call P2regs
    DrawDS
    UpdatePoints

    call Gun

    Start_Again:
    ;Pwrups Player_turn
    Drawgrd
    call P1regs
    call P2regs
    DrawCommandRow
    mov xcm,60
    mov NumPos,7
    DrawObj1 xcm,NumPos
    mov xcm,380
    mov NumPos,47
    DrawObj2 xcm,NumPos

    mov ax,1
    int 33h
    GameLoop:;============================================

    UpdatePoints

    call Getbtnclicked

    cmp ax, 0ffffh
    jnz RegAddMenu
    jz GameLoop
    RegAddMenu:

    BtnAct
    call P1regs

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
jnz assign_1num111
mov dl,2
convrt_string_hex P1_init_points,dl
jmp skip_assign1

assign_1num111:
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
    push cx
    mov SI,offset ReadUserSTR+2
    mov DI,offset ReadUserSTR_frbdn
    mov ch,0
    mov cl,ReadUserSTR+1
    rep MOVSB
    mov Cl,ReadUserSTR+1
    mov ReadUserSTR_frbdn_Size,Cx
    pop cx
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

ChooseFrbdn proc near

mov point_x,0d
mov point_y,0d

SetCursor point_x,point_y
ShowMessage user1_FrbdnChar  ;displaying user_init_points_message


mov ah,0Ah
mov dx,offset P1_FrbdnChar
int 21h   ;taking user intitial points
cmp P1_FrbdnChar+2,61h
jb skipcapital1
sub P1_FrbdnChar+2,20h
skipcapital1:

ShowMessage user2_FrbdnChar  ;displaying user_init_points_message


mov ah,0Ah
mov dx,offset P2_FrbdnChar
int 21h   ;taking user intitial points
cmp P2_FrbdnChar+2,61h
jb skipcapital2
sub P2_FrbdnChar+2,20h
skipcapital2:

ret
ChooseFrbdn endp

Gun proc near
    ;local addpos,loop1,loop2,loop3,loop4,loop6,loop5,repeat,Exit,yellow,red,green,purble,skipclr,skipaddpos,cmpcolumn,cmpcolumn2,Exit1,incyellow,incred,incgreen,incpurble
    cmp Player_turn,2
    jz gunp1
    jmp gunp2

    gunp1:
    mov x1,120
    mov y1,180
    jmp skipgunn
    gunp2:
    mov x1,440
    mov y1,180
    skipgunn:

    MOV AH, 00h  ; interrupts to get system time
    int 1AH      ; CX:DX now hold number of clock ticks since midnight
    mov  ax, dx
    xor  dx, dx
    mov  cx, 3d
    div  cx       ;here dx contains the remainder of the division - from 0 to 3
    cmp dx,0
    je yellow
    cmp dx,1
    je red
    cmp dx,2
    je green
    cmp dx,3
    je purble

    yellow:
    mov clro,0Eh
    jmp skipclr
    red:
    mov clro,04h
    jmp skipclr
    green:
    mov clro,0Ah
    jmp skipclr
    purble:
    mov clro,0Dh
    jmp skipclr
    skipclr:

    cmp Player_turn,2
    jz objpos1
    jmp objpos2

    objpos1:
    MOV AH, 00h  ; interrupts to get system time
    int 1AH      ; CX:DX now hold number of clock ticks since midnight
    mov  ax, dx
    xor  dx, dx
    mov  cx, 250d
    div  cx       ;here dx contains the remainder of the division - from 0 to 250
    cmp dx,10
    jb addpos
    jmp skipaddpos
    addpos:
    add dx,10
    skipaddpos:
    jmp skipobjpos

    objpos2:
    MOV AH, 00h  ; interrupts to get system time
    int 1AH      ; CX:DX now hold number of clock ticks since midnight
    mov  ax, dx
    xor  dx, dx
    mov  cx, 250d
    div  cx       ;here dx contains the remainder of the division - from 0 to 250
    cmp dx,10
    jb addpos1
    jmp skipaddpos1
    addpos1:
    add dx,10
    skipaddpos1:
    add dx,320

    skipobjpos:

    mov xo,dx ; random column
    ;mov xo,50
    mov yo,10
    mov ro,10
    ;mov clro,4h
    DrawCir xo,yo,xc,yc,p,ro,clro 

    loop1G: 
    DrawTri x1,y1,4 ;;GUN POSITION
    DrawCir xo,yo,xc,yc,p,ro,00
    inc yo
    cmp yo,220
    je Exit
    DrawCir xo,yo,xc,yc,p,ro,clro
    mov ah,1
    int 16h     ;getting key pressed from the buffer

    cmp Ah,77  ;checking if user pressed 'right'
    jz loop5G
    cmp ah,75  ;checking if user pressed 'left'
    jz loop2G
    cmp ah,72  ;checking if user pressed 'up'
    jz loop4G
    cmp ah,80 ;checking if user pressed 'down'
    jz loop3G
    cmp al,32 ;checking if user pressed 'spacebar'
    jz loop6G
    mov ah,0ch
    mov al,0
    int 21h
    hlt    ;flushing keyboard buffer
    jmp loop1G    

loop2G:
DrawTri x1,y1,0
sub x1,4
DrawTri x1,y1,4
mov ah,0ch
mov al,0
int 21h

hlt
jmp loop1G

loop3G:
DrawTri x1,y1,0
add y1,3
DrawTri x1,y1,4
mov ah,0ch
    mov al,0
    int 21h

hlt
jmp loop1G

loop4G:
DrawTri x1,y1,0
sub y1,3
DrawTri x1,y1,4
mov ah,0ch
    mov al,0
    int 21h

hlt
jmp loop1G

loop5G: 
DrawTri x1,y1,0
add x1,4
DrawTri x1,y1,4
mov ah,0ch
mov al,0
int 21h 

hlt
jmp loop1G


loop6G: ;for projectile

mov cx,x1
mov dx,y1

mov xcm,cx 
mov ycm,dx
mov r,3
mov clr,0Ah

repeatG:

DrawCir xcm,ycm,xc,yc,p,r,00 
dec ycm

DrawCir xcm,ycm,xc,yc,p,r,0Ah 
    
    
DrawCir xo,yo,xc,yc,p,ro,00
inc yo

DrawCir xo,yo,xc,yc,p,ro,clro 
mov ax,yo
cmp ycm,ax
jbe cmpcolumn
cmp ycm,5 ;;check the position of the projectile "must be edited for targets" ;; The Following 3 lines might be removed
hlt
jnz repeatG

jmp ExitG 

cmpcolumn:
mov ax,xcm ;;projectile
mov bx,xo ;;object
add bx,13

cmp ax,bx
jb cmpcolumn2
cmp ycm,5 ;;check the position of the projectile "must be edited for targets"
jz ExitG
cmp yo,220
jz Exit
hlt
jmp repeatG
cmpcolumn2:
sub bx,26
cmp ax,bx
ja Exit1G
cmp ycm,5 ;;check the position of the projectile "must be edited for targets"
jz ExitG
cmp yo,220
jz ExitG
hlt
jmp repeatG

Exit1G:;; add points to user IMP

cmp Player_turn,1
jz addptsp1
jmp addptsp2
addptsp1:
mov points_inc_index,2
cmp clro,0Eh
je incyellow
cmp clro,04h
je incred
cmp clro,0Ah
je incgreen
cmp clro,0Dh
je incpurble

incyellow:
mov points_inc_value,1
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjY2
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjY2,ah
jmp ExitG
incred:
mov points_inc_value,2
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjR2
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjR2,ah
jmp ExitG
incgreen:
mov points_inc_value,4
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjG2
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjG2,ah
jmp ExitG
incpurble:
mov points_inc_value,8
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjP2
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjP2,ah
jmp ExitG

jmp skippaddpts

addptsp2:
mov points_inc_index,1
skippaddpts:

cmp clro,0Eh
je incyellow1
cmp clro,04h
je incred1
cmp clro,0Ah
je incgreen1
cmp clro,0Dh
je incpurble1

incyellow1:
mov points_inc_value,1
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjY1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjY1,ah
jmp ExitG
incred1:
mov points_inc_value,2
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjR1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjR1,ah
jmp ExitG
incgreen1:
mov points_inc_value,4
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjG1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjG1,ah
jmp ExitG
incpurble1:
mov points_inc_value,8
IncrementPoints points_inc_index,points_inc_value
mov Ah,LFallObjP1
add Ah,30h
inc AH
sub Ah,30h
mov LFallObjP1,ah
jmp ExitG

ExitG:

DrawCir xcm,ycm,xc,yc,p,r,00
DrawCir xo,yo,xc,yc,p,ro,00
DrawTri x1,y1,00

mov ah,0ch
mov al,0
int 21h

ret    
Gun endp

P1regs proc near 
    
    mov ah,2
    mov dl,1   ;X-position
    mov dh,0   ;Y-position
    int 10h
    mov ah,9
    mov dx,offset user_name+2
    int 21h

    mov ah,2
    mov dl,1   ;X-position
    mov dh,4   ;Y-position
    int 10h
    mov ah,9
    mov dx,offset P2_FrbdnChar+2
    int 21h
    
    mov xr,60

    mov yr,25  ;joe old:10
  
    DrawRec xr,yr
  
    add yr,28  ;joe old:40
    DrawRec xr,yr

    add yr,28  ;joe old:40

    DrawRec xr,yr
    
    add yr,28  ;joe old:40
    DrawRec xr,yr

    mov xr,125 ;joe old:120
    mov yr,25  ;joe old:10
    DrawRec xr,yr
    
    add yr,28  ;joe old:40
    DrawRec xr,yr
    
    add yr,28  ;joe old:40
    DrawRec xr,yr
    
    add yr,28  ;joe old:40
    DrawRec xr,yr

     mov ah,2
     mov dl,5   ;X-position
     mov dh,2   ;Y-position
     int 10h      
     mov ah,9
     mov dx,offset Lax
     int 21h

    mov ah,2
    mov dl,5    ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lbx
    int 21h

    mov ah,2
    mov dl,5    ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lcx
    int 21h


    mov ah,2
    mov dl,5    ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Ldx
    int 21h

    mov ah,2
    mov dl,24   ;X-position
    mov dh,2    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lsi
    int 21h

    mov ah,2
    mov dl,24   ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Ldi
    int 21h
    
    mov ah,2
    mov dl,24   ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lsp
    int 21h

    mov ah,2
    mov dl,24   ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lbp
    int 21h

    ;Resgisters values-----------------
    mov ah,2
    mov dl,9   ;X-position
    mov dh,2   ;Y-position
    int 10h      
    mov ah,9
    mov dx,offset P1_regs[0]
    int 21h

    mov ah,2
    mov dl,9    ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_regs[5]
    int 21h

    mov ah,2
    mov dl,9    ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_regs[10]
    int 21h


    mov ah,2
    mov dl,9    ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_regs[15]
    int 21h

    mov ah,2
    mov dl,17   ;X-position
    mov dh,2    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_regs[20]
    int 21h

    mov ah,2
    mov dl,17   ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_regs[25]
    int 21h
    
    mov ah,2
    mov dl,17   ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_regs[30]
    int 21h

    mov ah,2
    mov dl,17   ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_regs[35]
    int 21h
ret
P1regs endp

P2regs proc near
    
    mov ah,2
    mov dl,42   ;X-position
    mov dh,0   ;Y-position
    int 10h
    mov ah,9
    mov dx,offset user2_name+2
    int 21h

    mov ah,2
    mov dl,42   ;X-position
    mov dh,4   ;Y-position
    int 10h
    mov ah,9
    mov dx,offset P1_FrbdnChar+2
    int 21h

    mov xr,380
    mov yr,25  ;joe old:10
    
    DrawRec xr,yr
    
    add yr,28  ;joe old:40
    DrawRec xr,yr
    
    add yr,28  ;joe old:40
    DrawRec xr,yr
    add yr,28  ;joe old:40
    DrawRec xr,yr

    add xr,65
     mov yr,25  ;joe old:10
    DrawRec xr,yr
    add yr,28  ;joe old:40
    DrawRec xr,yr
    add yr,28  ;joe old:40
    DrawRec xr,yr
    add yr,28  ;joe old:40
    DrawRec xr,yr

    mov ah,2
    mov dl,44   ;X-position
    mov dh,2    ;Y-position
    int 10h      
    mov ah,9
    mov dx,offset Lax
    int 21h

    mov ah,2
    mov dl,44   ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lbx
    int 21h

    mov ah,2
    mov dl,44   ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lcx
    int 21h


    mov ah,2
    mov dl,44   ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Ldx
    int 21h

    mov ah,2
    mov dl,64   ;X-position
    mov dh,2    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lsi
    int 21h

    mov ah,2
    mov dl,64   ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Ldi
    int 21h
    
    mov ah,2
    mov dl,64   ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lsp
    int 21h

    mov ah,2
    mov dl,64   ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset Lbp
    int 21h

    ;Resgisters values-----------------

    mov ah,2
    mov dl,49   ;X-position
    mov dh,2    ;Y-position
    int 10h      
    mov ah,9
    mov dx,offset P2_regs[0]
    int 21h

    mov ah,2
    mov dl,49   ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_regs[5]
    int 21h

    mov ah,2
    mov dl,49   ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_regs[10]
    int 21h


    mov ah,2
    mov dl,49   ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_regs[15]
    int 21h

    mov ah,2
    mov dl,57   ;X-position
    mov dh,2    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_regs[20]
    int 21h

    mov ah,2
    mov dl,57   ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_regs[25]
    int 21h
    
    mov ah,2
    mov dl,57   ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_regs[30]
    int 21h

    mov ah,2
    mov dl,57   ;X-position
    mov dh,8    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_regs[35]
    int 21h
ret
P2regs endp

Update_P1ds proc near
    
    ;Data Segment values-----------------
    mov ah,2
    mov dl,34   ;X-position
    mov dh,0   ;Y-position
    int 10h      
    mov ah,9
    mov dx,offset P1_ds[0]
    int 21h

    mov ah,2
    mov dl,34   ;X-position
    mov dh,2   ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[3]
    int 21h

    mov ah,2
    mov dl,34    ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[6]
    int 21h


    mov ah,2
    mov dl,34    ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[9]
    int 21h

    mov ah,2
    mov dl,34   ;X-position
    mov dh,8d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[12]
    int 21h

    mov ah,2
    mov dl,34   ;X-position
    mov dh,10d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[15]
    int 21h
    
    mov ah,2
    mov dl,34   ;X-position
    mov dh,12d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[18]
    int 21h

    mov ah,2
    mov dl,34   ;X-position
    mov dh,13d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[21]
    int 21h

    mov ah,2
    mov dl,34   ;X-position
    mov dh,15d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P1_ds[24]
    int 21h



    ;Data Segment Lables-----------------
    mov ah,2
    mov dl,38   ;X-position
    mov dh,0   ;Y-position
    int 10h      
    mov ah,9
    mov dx,offset DS_labels[0]
    int 21h

    mov ah,2
    mov dl,38   ;X-position
    mov dh,2   ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[2]
    int 21h

    mov ah,2
    mov dl,38    ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[4]
    int 21h


    mov ah,2
    mov dl,38    ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[6]
    int 21h

    mov ah,2
    mov dl,38   ;X-position
    mov dh,8d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[8]
    int 21h

    mov ah,2
    mov dl,38   ;X-position
    mov dh,10d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[10]
    int 21h
    
    mov ah,2
    mov dl,38   ;X-position
    mov dh,12d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[12]
    int 21h

    mov ah,2
    mov dl,38   ;X-position
    mov dh,13d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[14]
    int 21h

    mov ah,2
    mov dl,38   ;X-position
    mov dh,15d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[16]
    int 21h

ret
Update_P1ds endp

Update_P2ds proc near
    
    ;Data Segment values-----------------
    mov ah,2
    mov dl,74   ;X-position
    mov dh,0   ;Y-position
    int 10h      
    mov ah,9
    mov dx,offset P2_ds[0]
    int 21h

    mov ah,2
    mov dl,74   ;X-position
    mov dh,2   ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[3]
    int 21h

    mov ah,2
    mov dl,74    ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[6]
    int 21h


    mov ah,2
    mov dl,74    ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[9]
    int 21h

    mov ah,2
    mov dl,74   ;X-position
    mov dh,8d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[12]
    int 21h

    mov ah,2
    mov dl,74   ;X-position
    mov dh,10d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[15]
    int 21h
    
    mov ah,2
    mov dl,74   ;X-position
    mov dh,12d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[18]
    int 21h

    mov ah,2
    mov dl,74   ;X-position
    mov dh,13d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[21]
    int 21h

    mov ah,2
    mov dl,74   ;X-position
    mov dh,15d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset P2_ds[24]
    int 21h



    ;Data Segment Lables-----------------
    mov ah,2
    mov dl,78   ;X-position
    mov dh,0   ;Y-position
    int 10h      
    mov ah,9
    mov dx,offset DS_labels[0]
    int 21h

    mov ah,2
    mov dl,78   ;X-position
    mov dh,2   ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[2]
    int 21h

    mov ah,2
    mov dl,78    ;X-position
    mov dh,4    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[4]
    int 21h


    mov ah,2
    mov dl,78    ;X-position
    mov dh,6    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[6]
    int 21h

    mov ah,2
    mov dl,78   ;X-position
    mov dh,8d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[8]
    int 21h

    mov ah,2
    mov dl,78   ;X-position
    mov dh,10d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[10]
    int 21h
    
    mov ah,2
    mov dl,78   ;X-position
    mov dh,12d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[12]
    int 21h

    mov ah,2
    mov dl,78   ;X-position
    mov dh,13d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[14]
    int 21h

    mov ah,2
    mov dl,78   ;X-position
    mov dh,15d    ;Y-position
    int 10h  
    mov ah,9
    mov dx,offset DS_labels[16]
    int 21h

ret
Update_P2ds endp

; Pwrups proc near
    
;     push di
;     push bx
;     mov bh,0
;     mov bx,Player_turn

;     call GetNumFromUser

;     push cx ;user chosen option

;     cmp Player_turn,2
;     jz player_2

;     cmp P1_init_points+1,2
;     jnz assign_1num1
;     mov dl,2
;     convrt_string_hex P1_init_points,dl
;     jmp skip_p2

;     assign_1num1:
;     mov dl,3
;     convrt_string_hex P1_init_points-1,dl

;     jmp skip_p2
;     ;----------------------------------------
;     player_2:

;     cmp P2_init_points+1,2
;     jnz assign_1num222
;     mov dl,2
;     convrt_string_hex P2_init_points,dl
;     jmp skip_assign2

;     assign_1num222:
;     mov dl,3
;     convrt_string_hex P2_init_points-1,dl

;     skip_p2:
;     ;-------------------------------

;     mov player_points,cx
;     pop cx

;     ;!!!players points in temp2, chosen power up in cx!!!

;     cmp cx,1
;     jz powerup_1

;     cmp cx,2
;     jz powerup_2

;     cmp cx,3
;     jz powerup_3

;     cmp cx,4
;     jz powerup_4

;     cmp cx,5
;     jz powerup_5


;     jmp exit_pwrups
;     ;------------------------------------------
;     powerup_1:

;     cmp player_points,5
;     jb exit_pwrups

;     mov flagpower1_own,1

;     mov points_inc_index,bl
;     mov points_inc_value,5

;     DecrementPoints points_inc_index,points_inc_value

;     jmp exit_pwrups



;     ;-----------------------
;     powerup_2:

;     cmp player_points,3
;     jb exit_pwrups

;     mov points_inc_index,bl
;     mov points_inc_value,3

;     DecrementPoints points_inc_index,points_inc_value

;     mov flagpower2_ownandopponent,1

;     jmp exit_pwrups

;     ;-----------------------
    
;     powerup_3:
    
;     cmp player_points,8
;     jb exit_pwrups


;     cmp Player_turn,2
;     jz player2_check3

;     cmp flagpower3_changefbdn_p1,1

   
;     jz exit_pwrups

;     ;PUT YOUR CODE HERE AMINOZ------------------------


;     ;-------------------------------------------------

;     mov points_inc_index,bl
;     mov points_inc_value,8

;     DecrementPoints points_inc_index,points_inc_value

;     mov flagpower3_changefbdn_p1,1
;     jmp exit_pwrups


;     player2_check3:

;     ;PUT YOUR CODE HERE AMINOZ------------------------


;     ;-------------------------------------------------
;     cmp flagpower3_changefbdn_p2,1

;     mov points_inc_index,bl
;     mov points_inc_value,8

;     DecrementPoints points_inc_index,points_inc_value

;     mov flagpower3_changefbdn_p2,1
;     jz exit_pwrups


;     ;-----------------------
;     powerup_4: ;VALUE IS RETURNED IN CX, bit stuck type in power4_stuckbitvalue

;     ;NOTE: if bit type = 0, yeb2a (cx AND value) to force the bit to be zero
;     ;      if bit type = 1, yeb2a (cx OR value)  to force the bit to be one


;     cmp player_points,2
;     jb exit_pwrups

;     call GetNumFromUser ;taking stuck bit value (0 or 1)
;     mov power4_stuckbitvalue,cx
;     call GetNumFromUser ;taking bit index (0-->15)
;     mov power4_stuckbitindex,cx

;     cmp power4_stuckbitvalue,1
;     ja exit_pwrups

;     cmp power4_stuckbitindex,15
;     ja exit_pwrups

;     mov dx,1
;     push cx
;     mov ch,0
;     mov cx,power4_stuckbitindex
;     shl dx,cl

;     pop cx



;     mov points_inc_index,bl
;     mov points_inc_value,2

;     DecrementPoints points_inc_index,points_inc_value



;     cmp power4_stuckbitvalue,1
;     jz exit_pwrups

;     not cx

;     jmp exit_pwrups

    
;     ;-------------------------------------


;     powerup_5:

;     cmp player_points,30
;     jb exit_pwrups

;     cmp Player_turn,2
;     jz player2_check5

;     cmp flagpower5_regclr_p1,1
;     jz exit_pwrups

;     push ax
;     mov ax,0

;     mov temp_reg_num,0
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,1
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,2
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,3
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,4
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,5
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,6
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,7
;     UpdateRegValue Player_turn,temp_reg_num


;     mov points_inc_index,bl
;     mov points_inc_value,30d

;     DecrementPoints points_inc_index,points_inc_value

;     mov flagpower3_changefbdn_p1,1

;     pop ax
;     jmp exit_pwrups


;     player2_check5:

;     cmp flagpower5_regclr_p2,1
;     jz exit_pwrups


;     mov temp_reg_num,0
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,1
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,2
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,3
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,4
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,5
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,6
;     UpdateRegValue Player_turn,temp_reg_num

;     mov temp_reg_num,7
;     UpdateRegValue Player_turn,temp_reg_num


;     mov points_inc_index,bl
;     mov points_inc_value,30
    

;     DecrementPoints points_inc_index,points_inc_value

;     mov flagpower5_regclr_p2,1
    
;     pop ax
;     jmp exit_pwrups



;     exit_pwrups_error:
;     mov powerup_error,1

;     exit_pwrups:

;     pop bx
;     pop di
; ret    
; Pwrups endp

Pwrups_resetflag proc near 
    
    mov flagpower1_own,0
    mov flagpower2_ownandopponent,0
    mov flagpower4_stuckbit,0
ret
Pwrups_resetflag endp

Convrt_Hex_String proc near 
;local myloop,continue_convrt,exit_convrt,letter,continue,bit_0,bit_1,bit_2,bit_3
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
ret
Convrt_Hex_String endp

UpdateRegValue proc near    
    call Convrt_Hex_String ;takes the new value from AX and converts it to string in num_placeholder variable
    
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
ret
UpdateRegValue endp

UpdateDataSegmentValue  proc near
    ;local updt_oneSegment_p1,updt_oneSegment_p2,player2_data,exit_updtdatasegment,data_0_p1,data_1_p1,data_2_p1,data_3_p1,data_4_p1,data_5_p1,data_6_p1,data_7_p1,data_8_p1,data_0_p2,data_1_p2,data_2_p2,data_3_p2,data_4_p2,data_5_p2,data_6_p2,data_7_p2,data_8_p2,data_0_p11,data_1_p11,data_2_p11,data_3_p11,data_4_p11,data_5_p11,data_6_p11,data_7_p11,data_8_p11,data_0_p22,data_1_p22,data_2_p22,data_3_p22,data_4_p22,data_5_p22,data_6_p22,data_7_p22,data_8_p22

    call Convrt_Hex_String ;takes the new value from AX and converts it to string in num_placeholder variable
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
ret
UpdateDataSegmentValue endp


end Main