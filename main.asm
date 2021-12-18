;Setting cursor position
SetCursor MACRO x,y

    mov bh,0
    mov ah,2
    mov dl,x
    mov dh,y
    int 10h

ENDM SetCursor

;Displaying messages
ShowMessage MACRO MyMessage
    
    mov ah,9h
    mov dx,offset MyMessage
    int 21h

ENDM ShowMessage


.model small
.stack 64

.Data

;X & Y positions
point_x                     db ?
point_y                     db ?
 
;user info
user_name                   db 17,?, 17 dup('$')
user_init_points            db 10,?,10  dup('$')


;Messages strings
user_name_message           db 'Enter User Name:',10,13,'$'
user_init_points_message    db 10,13,'Enter Initial Points:',10,13,'$'
main_menu_continue_message  db 10,13,'Press Enter to contiune$'
press_F1_message            db 'Press [F1] to start chatting mode $'
press_F2_message            db 'Press [F2] to game mode $'
press_ESC_message           db 'Press [ESC] to exit program $'

.code 

MAIN proc FAR

MOV AX, @DATA
MOV DS, AX

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
mov dx,offset user_init_points
int 21h   ;taking user intitial points


ShowMessage main_menu_continue_message ;displaying main_menu_continue_message
;------------------------------------------------

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


MAIN ENDP


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


CreateMainMenu ENDP

END MAIN
