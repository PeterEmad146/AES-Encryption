; Computer Organization and System Programming Project
; Milestone 2
; Team: Peter Emad, Mark Fahim, Youssef Ismail, Abdullah Tamer

;====================================================================================================================
SubBytes MACRO message,SBOX
    local subBytes

    mov si,0
    subBytes:  
        mov al,message[si]  
        mov bx,0                ; Clear BX to ensure proper indexing
        mov bl, al              ; Move AL (index) into BL
        mov bl, SBOX[bx]        ; Perform substitution using BX as index
        mov message[si], bl 
        inc si   
        cmp si,16
        jnz  subBytes
          
        mov ax, 0
        mov bx, 0
        mov cx, 0
        mov dx, 0
        mov di, 0
        mov si, 0  
      
    ENDM
;====================================================================================================================

                            
;====================================================================================================================                            
ShiftRows MACRO message, SBOX,tempRow
    local ShiftRowsLoop,return,rotate,rotateLoop,equal,Terminate

    ; Setup result and tempRow Pointers
    mov si, 0       ; Load the address of the result array into SI register
    mov di, 0       ; Load the address of tempRow array into DI register
    mov cx, 4       ; Loop counter for 4 rows
    mov bx, 0       ; BX register used to track the number of shifts (0 shifts for row0)
    
    ShiftRowsLoop: 
        ; Load the 4 bytes of the current row to AX and DX registers
        mov ah, message[si]
        mov al, message[si+1]
        mov dh, message[si+2]
        mov dl, message[si+3]
        
        cmp bx, 0   ; Compare the BX register with 0
        jg rotate   ; Go to 'rotate' if a shift is needed [BX>0]
        cmp bx, 0   ; Compare the BX register with 0
        je equal    ; Go to 'equal' if no shift is needed [BX=0]
        
    return: 
        ; Return the 4 shifted bytes to the result again
        mov message[si], ah
        mov message[si+1],al
        mov message[si+2],dh
        mov message[si+3],dl
        
        inc bx              ; Increment the BX register by 1
        add si, 4           ; Increment the index to the next row
        dec cx              ; Decrement the CX register by 1
        cmp cx, 0           ; Compare the CX register with 0
        jg ShiftRowsLoop    ; Jump to ShiftRowsLoop if CX is greater than 0 (more rows to be manipulated)
        jmp Terminate       ; Terminate the program (no more rows to be shifted)
    
    rotate:
        push cx         ; Save the value of cx to be used in the rotate loop with another value
        mov cx, bx      ; Set the rotation loop counter (cx = bx)
        
    rotateLoop:
        ; Rotate 1 item from left to right each time
        ; The rotation is done by the help of the tempRow variable
        mov tempRow[di],  al
        mov tempRow[di+1], dh
        mov tempRow[di+2], dl
        mov tempRow[di+3], ah
        mov ah, tempRow[di]
        mov al, tempRow[di+1]
        mov dh, tempRow[di+2]
        mov dl, tempRow[di+3]
        
        dec cx              ; Decrement The CX Register by 1
        cmp cx, 0           ; Compare CX with 0
        jg rotateLoop       ; Jump to rotateLoop if there are more cells to be shifted    
    
        pop cx          ; Restore the previous value of cx
        jmp return      ; Jump to the return block
        
    equal:
        ; Set the tempRow values for row0 case
        mov tempRow[di], ah
        mov tempRow[di+1], al
        mov tempRow[di+2], dh
        mov tempRow[di+3], dl
        jmp return
        
    Terminate:

        mov ax, 0
        mov bx, 0
        mov cx, 0
        mov dx, 0
        mov di, 0
        mov si, 0  
        ENDM 
;====================================================================================================================
      
      
;====================================================================================================================
MixColumns MACRO message, Rijindaels, tempRes, tempMix  
    local mult_two, zero_bit, mult_three, return_three, zero_bit_thre, continue, set, check, restore

    mov si, 0   ; index for message
    mov di, 0   ; index for rijndaels
    mov cx, 0   ; index to store the result in message
    
    ; reset the values of the other registers
    mov ax, 0
    mov bx, 0               
    mov dx, 0
    
    L1: 
        mov al, message[si]         ; put the element in the message in AL register
        mov bl, Rijindaels[di]      ; put the element in the Rijndaels matrix in the BL register
        
        ; Comparing the value of the Rijndaels matrix whether it is 01 or 02 or 03 to decide which label to be accessed
        cmp bl, 01
        jz continue     ; If there Rijindaels[di] = 01
        cmp bl, 02
        jz mult_two     ; If there Rijindaels[di] = 02
        jmp mult_three  ; If there Rijindaels[di] = 03
               
    mult_two:
        test al, 80h    ; check if the most siginificant bit is 1
        jz zero_bit
        ; MSB is 1, XOR is needed
        shl al, 1       ; Shifting the message to the left by 1
        xor al, 1Bh     ; xor the value with 1B
        jmp continue
        
    zero_bit:
        ; MSB is 0, no XOR is needed
        shl al, 1
        jmp continue    
    
    mult_three:
        mov dl, al              ; Making a copy of the message to be XORed later (result of the 01 operation is stored in DL register)
        test al, 80h            ; check if the most significant bit is 1
        jz zero_bit_three
        ; MSB is 1, XOR is needed
        shl al, 1               ; Shifting the message to the left by 1
        xor al, 1Bh             ; xor the value with 1B
        
    return_three:    
        xor al, dl              ; XORing the results of both results from 01 and 02 operations
        jmp continue
        
    zero_bit_three:
        shl al, 1               ; Shifting the message to the left by 1
        jmp return_three    
    
    continue:
        cmp cl, 0           ; Check if this is the first result from the column, if so, we need to set the value in the tempRes
        jz set
        xor al, tempRes     ; if the tempRes has previous value, we need to xor the current result with the previous result
        mov tempRes, al     ; Storing the XOR result in the tempRes
        inc cl              ; incrementing the CL to keep track if we still need further operations in this column 
        jmp check
        
    set:
        mov tempRes, al     ; Storing the value in AL register to the tempRes
        inc cl              ; incrementing the CL to keep track if we still need further operations in this column  
        jmp check    
    
    check:
        add si, 4           ; adding si by 4 to shift the index to the next element in the same column
        inc di              ; adding di by 1 to shift the index to the next element in the same row
        cmp cl, 4           ; Check if CL is not equal 4, we need to do further operations to do further operations on the column
        jnz L1
        
        ; CL is equal to 4, so we need to save the value in the tempRes in the tempMix matrix. (We got the needed result)
        ; The index of the element we need to store is calculated by this formula (DI - 4 + CH), by subtracting 4 from the DI we reached the position of the row, and CH is used to track we are in which column, so by adding the CH we reached the desired position (row & Column)
        mov ax, 0           ; reset the AX register
        ; Getting the index of the result
        mov ax, di          ; Moving the DI to AX
        sub al, 4           ; Subtracting 4 from AL (DI - 4)
        add al, ch          ; Adding CH to AL (DI - 4 + CH)
        mov di, ax          ; Returning back the value to DI
        mov al, tempRes     ; Moving the tempRes value to AL register 
        mov tempMix[di], al ; Storing the AL to the tempMix matrix 
        ; Reversing the operations done on DI to get back the DI value (DI + 4 - CH)
        mov ax, di
        add al, 4
        sub al, ch
        mov di, ax
        cmp di, 16          ; If DI = 16, then we reached the end of the Rijndaels matrix, no further operations needed to be done on this column, we need to shift to the next column
        jz new_column
        ; If DI < 16, the next value in the column will be manipulated
        mov dl, ch          ; Setting the index of the message to the first element in the column using DX register and then storing DX to SI, The index of the first element is determined by the value of CH (Index tracking the columns) 
        mov dh, 0
        mov si, dx
        mov cl, 0           ; Setting the CL to 0 as we are will now get the first results from the 4 results that need to be XORed
        jmp L1
        
        
    new_column:
        inc ch              ; Incrementing the index tracking the column [CH]
        mov di, 0           ; Moving the DI (Rijndaels index) to the beginning the start the operations for the next column
        cmp ch, 4           ; If CH = 4, we finished all the operations on the message, if not, there still column(s) that need to be manipulated
        jz Terminate
        mov al, ch          ; Setting the index of the message to the first element in the column using AL register and then storing AL to SI, The index of the first element is determined by the value of CH (Index tracking the columns)
        mov ah, 0 
        mov si, ax
        mov cl, 0
        jmp L1
        
                        
    Terminate:
        mov si, 0
        mov cx, 16
        
        ; Storing the values of the tempMix (result) to the message 
        r: 
          mov al, tempMix[si]
          mov message[si], al 
          mov tempMix[si], 0
          inc si
          loop r  
          
        mov ax, 0
        mov bx, 0
        mov cx, 0
        mov dx, 0
        mov di, 0
        mov si, 0  
        ENDM    
;====================================================================================================================
     

;====================================================================================================================
AddRoundKey MACRO message
    local L1
    
    mov cx, 16
    mov si, 0
    
    L1: 
        mov al, message[si]
        xor al, 0FFh
        mov message[si], al
        inc si
        loop L1
        mov ax, 0
        mov bx, 0
        mov cx, 0
        mov dx, 0
        mov di, 0
        mov si, 0
    ENDM
;====================================================================================================================

.MODEL SMALL
.STACK 100H
                         
.data segment

    ; The SBOX byte substitution table    
    SBOX DB 63H,7CH,77H,7BH,0F2H,6BH,6FH,0C5H,30H,01H,67H,2BH,0FEH,0D7H,0ABH,76H
         DB 0CAH,82H,0C9H,7DH,0FAH,59H,47H,0F0H,0ADH,0D4H,0A2H,0AFH,9CH,0A4H,72H,0C0H
         DB 0B7H,0FDH,93H,26H,36H,3FH,0F7H,0CCH,34H,0A5H,0E5H,0F1H,71H,0D8H,31H,15H
         DB 04H,0C7H,23H,0C3H,18H,96H,05H,9AH,07H,12H,80H,0E2H,0EBH,27H,0B2H,75H
         DB 09H,83H,2CH,1AH,1BH,6EH,5AH,0A0H,52H,3BH,0D6H,0B3H,29H,0E3H,2FH,84H
         DB 53H,0D1H,00H,0EDH,20H,0FCH,0B1H,5BH,6AH,0CBH,0BEH,39H,4AH,4CH,58H,0CFH
         DB 0D0H,0EFH,0AAH,0FBH,43H,4DH,33H,85H,45H,0F9H,02H,7FH,50H,3CH,9FH,0A8H
         DB 51H,0A3H,40H,8FH,92H,9DH,38H,0F5H,0BCH,0B6H,0DAH,21H,10H,0FFH,0F3H,0D2H
         DB 0CDH,0CH,13H,0ECH,5FH,97H,44H,17H,0C4H,0A7H,7EH,3DH,64H,5DH,19H,73H
         DB 60H,81H,4FH,0DCH,22H,2AH,90H,88H,46H,0EEH,0B8H,14H,0DEH,5EH,0BH,0DBH
         DB 0E0H,32H,3AH,0AH,49H,06H,24H,5CH,0C2H,0D3H,0ACH,62H,91H,95H,0E4H,79H
         DB 0E7H,0C8H,37H,6DH,8DH,0D5H,4EH,0A9H,6CH,56H,0F4H,0EAH,65H,7AH,0AEH,08H
         DB 0BAH,78H,25H,2EH,1CH,0A6H,0B4H,0C6H,0E8H,0DDH,74H,1FH,4BH,0BDH,8BH,8AH
         DB 70H,3EH,0B5H,66H,48H,03H,0F6H,0EH,61H,35H,57H,0B9H,86H,0C1H,1DH,9EH
         DB 0E1H,0F8H,98H,11H,69H,0D9H,8EH,94H,9BH,1EH,87H,0E9H,0CEH,55H,28H,0DFH
         DB 8CH,0A1H,89H,0DH,0BFH,0E6H,42H,68H,41H,99H,2DH,0FH,0B0H,54H,0BBH,16H   
         
    
    tempRow DB 4 DUP(?)  ; Temporary Row that is used in the row shifting block   
    Rijindaels DB 02h,03h,01h,01h
               DB 01h,02h,03h,01h
               DB 01h,01h,02h,03h
               DB 03h,01h,01h,02h
    tempMix DB 16 DUP(?) ; Temporary matrix to store the results from the mixColumns
    tempRes DB 0         ; Temporary variable to store the result of each element in the mixColumns then add it to tempMix
    rounds DW 0009h
    
    ; Message to be encrypted
    msg DB 16 DUP('?') ; Reserve space for 16 bytes (128 bits)
    
    inputPrompt DB "Enter 16 hexadecimal bytes separated by spaces: $"
    outputPrompt DB "The Encrypted message: $"
    space DB ' '
    newline DB 0DH, 0AH, '$'
    
.code segment    

MAIN PROC
    MOV AX, @DATA
    MOV DS, AX        ; Initialize data segment

    ; Prompt user for input
    LEA DX, inputPrompt
    MOV AH, 09H
    INT 21H

    ; Call procedure to read input
    CALL read_input

    ; Print newline
    LEA DX, newline
    MOV AH, 09H
    INT 21H 
    
    AddRoundKey msg
    LLoop:
        SubBytes msg, SBOX
        ShiftRows msg, SBOX, tempRow
        MixColumns msg, Rijindaels, tempRes, tempMix
        AddRoundKey msg
        dec rounds
        mov cx, rounds
        cmp cx, 0
        jnz LLoop
    
    SubBytes msg, SBOX
    ShiftRows msg, SBOX, tempRow
    AddRoundKey msg
           

    ; Display the result
    LEA DX, outputPrompt
    MOV AH, 09H
    INT 21H

    ; Call procedure to print result
    CALL print_result

    ; Print newline
    LEA DX, newline
    MOV AH, 09H
    INT 21H

    ; Exit program
    MOV AH, 4CH
    INT 21H
MAIN ENDP

; Procedure to read 16 bytes (128 bits) of input
read_input PROC
    LEA SI, msg       ; Pointer to msg
    MOV CX, 16        ; Number of bytes to read
    MOV AH, 01H       ; Function to read a character

READ_LOOP:
    ; Read higher nibble
    INT 21H           ; Read a character
    CMP AL, 20H       ; Check if it's a space
    JE INVALID_INPUT  ; Spaces not allowed here
    CALL convert_hex  ; Convert ASCII to hex
    SHL AL, 4         ; Shift higher nibble to the left
    MOV BL, AL        ; Store in BL

    ; Read lower nibble
    INT 21H           ; Read a character
    CALL convert_hex  ; Convert ASCII to hex
    OR BL, AL         ; Combine with higher nibble

    ; Read the space character
    INT 21H           ; Read the space
    CMP AL, 20H       ; Check if it's a space
    JNE INVALID_INPUT ; If not a space, invalid input

    ; Store the byte
    MOV [SI], BL      ; Store byte in msg
    INC SI            ; Increment pointer
    LOOP READ_LOOP    ; Repeat for all 16 bytes
    RET

INVALID_INPUT:
    JMP READ_LOOP     ; Restart loop to read next hexadecimal (space is entered)

read_input ENDP

; Procedure to print 16 bytes (128 bits) from msg
print_result PROC
    LEA SI, msg       ; Pointer to msg
    MOV CX, 16        ; Number of bytes to print
    MOV AH, 02H       ; Function to print a character

PRINT_LOOP:
    MOV AL, [SI]      ; Get the byte
    MOV BL, AL
    SHR AL, 4         ; Extract higher nibble
    CALL print_hex
    MOV AL, BL
    AND AL, 0FH       ; Extract lower nibble
    CALL print_hex
    MOV DL, space
    INT 21H           ; Print space
    INC SI            ; Increment pointer
    LOOP PRINT_LOOP   ; Repeat for all 16 bytes
    RET

print_result ENDP

; Procedure to convert ASCII to hex
convert_hex PROC
    CMP AL, '0'
    JB INVALID_INPUT  ; If below '0', invalid input
    CMP AL, '9'
    JBE NUMERIC_DIGIT ; If '0' <= AL <= '9', it's a numeric digit
    CMP AL, 'A'
    JB INVALID_INPUT  ; If below 'A', invalid input
    CMP AL, 'F'
    JBE UPPERCASE_HEX ; If 'A' <= AL <= 'F', it's a valid uppercase hex
    CMP AL, 'a'
    JB INVALID_INPUT  ; If below 'a', invalid input
    CMP AL, 'f'
    JBE LOWERCASE_HEX ; If 'a' <= AL <= 'f', it's a valid lowercase hex
    JMP INVALID_INPUT ; Otherwise, invalid input

NUMERIC_DIGIT:
    SUB AL, '0'       ; Convert ASCII '0'–'9' to hex 0–9
    RET

UPPERCASE_HEX:
    SUB AL, 'A'       ; Convert ASCII 'A'–'F' to hex 10–15
    ADD AL, 10
    RET

LOWERCASE_HEX:
    SUB AL, 'a'       ; Convert ASCII 'a'–'f' to hex 10–15
    ADD AL, 10
    RET

print_hex PROC
    ADD AL, '0'       ; Convert to ASCII
    CMP AL, '9'
    JBE PRINT_CHAR
    ADD AL, 7         ; Adjust for hex A–F
PRINT_CHAR:
    MOV DL, AL
    INT 21H           ; Print character
    RET
print_hex ENDP

END MAIN