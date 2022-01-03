;;Time Delay 1 tick delay (18.2/sec)
    mov bp, 43690
    mov si, 43690
    delay2:
    dec bp
    nop
    jnz delay2
    delay1:
    dec si
    cmp si,0
    jnz delay1
    ; end delay