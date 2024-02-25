.setcpu "65C02"
.segment "lcdkb"
; 1-16-2024
; was: LCDps2SerialIRQ.asm

; Run under WOZMON

;12-6-23 1952
;PS2 keyboard IRQ now on VIA2 CA1

; 11-19-23 1829
; Add delay_50ms AND LCD Reset by instruction "lcdinitinstr:"

; REQUIRES wozmon in ROM to specify IRQ address $3A00

; Now: LCDps2SerialIRQ.asm
; was: printnum_4001_IRQ2-6551.asm 11-11-23
; On keyboard keypress press:
;	print decimal of 8-bit keypress
;	print SPACE
; 	print keyboard Character
;	print SPACE
;	print HIGH and LOW bit as decimal, separated by hyphen
;
;	3 button presses:
; VIA1 PORTA
	; $02 - quit
	; $08 - toggle $80 LED with H/W timer
			; During keypress loop, toggle VIA2 PORTB $01
	; CA1 - IRQ enabled
; VIA2 port	
	; CA1 - IRQ enabled

;;DUNNO ABOUT NEEDING THE ISR COMPILED HERE.
;; BECAUSE THE ADDRESSES ARE THE SAME.
; REQUIRES the ISR (interrupt service routine) to be 
; included in the compilation.



; 11-11-2023 1135 AM Update
; Add Keyboard buffer logic, and read from keyboard buffer
; instead of directly from, and comparing against, the
; Shift Registers output value.

; 2042 Code for the keyboard must be added to the interrupt
; service routine.


; 11-5-2023 - Adding VIA2 IRQ on CA1 button press.
; This code needs IRQv2-3A00.asm to service the IRQs.
; Which come from VIA1 and VIA2's CA1 pin.

; 11-2-2023 - Adding VIA1 IRQ on CA1 with button press.
; Needs IFR1 and IER1

; 10-24-2023 EPIPHANY!!!
; 19FF + 1 equals 1A00, NOT 2000 you dumbass! :)

; printnum_4001_5.asm 10-24-2023 1918
; --10-15-23 1021AM
; --Add hyphen between MSB and LSB
; --THIS WORKS!!!
; --EXAMPLE FOR KEYPRESS 'a': 28 a 1-12


; 1103am - remove btn $40 check, replace with btn $80 using timer
; 	to blink VIA1 PORTA1 LED $$08
; this WORKS 10-4-2023 
;     I had to remove the LED blinks from inside of print_char.
;     Now I just toggle LED 4 at end of print_char before rts
;     On keypress, it toggles on to print 1st #, toggle off for 2nd #
;     Also add a SPACE and then print the Character by keycode


; 10-3-2023 PROBLEM IS IN print_char 20:00

; 10-2-2023 trying to add button Debounce
; so that pressing buttons doesn't stay in a loop
; and the cpu can't do anything until the button is depressed

; SAVE This works! 9-28-2023
; With delay subroutine that blinks BLUE LED on PORTB2
; while button $80 is pressed. Button press also
; toggles blue LED on PORTA1 ($08).
; simple: print a binary number as decimal on lcdbusy

; Reads value $4001, puts in in LOW BYTE of value, not value + 1.
; value is then converted to decimal and printed on LCD
; THEN without a "space" it prints the keyboard character
; by referencing the keymap (not keymap_shifted).7

; 9-25-2023
; use loop until button 1 is pressed ($10) on PORTA1 to exit.
; 9-26-2023
; detect keyboard press by the changing value $4001 and then
; put it on the LCD

; Data Direction Registers
DDRA1 = $6003
DDRA2 = $4003
DDRB1 = $6002
DDRB2 = $4002

; VIA 1 & 2, A and B PORT addresses
PORTA1 = $6001
PORTB1 = $6000
PORTA2 = $4001
PORTB2 = $4000

; VIA 1 & 2 peripheral Control Registers
PCR1 = $600c
PCR2 = $400c

; VIA 1 & 2 INTERRUPT Flag Registers
IFR1 = $600d
IER1 = $600e
IFR2 = $400d
IER2 = $400e

; VIA 1 & 2 Timer Control Latch Low & High
VIA1_T1CL = $6004
VIA1_T1CH = $6005
VIA2_T1CL = $4004
VIA2_T1CH = $4005

; ACIA Data address
ACIA_DATA   = $5000

; For printing decimals
value = $0300 ; 2 bytes
mod10 = $0302 ; 2 bytes
message = $0304 ; 6 bytes
;counter = $0320 ; 2 bytes ; NOT USED here ; only for IRQ video

; keyboard
kb_wptr = $0000	; kb write pointer
kb_rptr = $0001	; kb read pointer
				; if write & reader point are equal,
				; we've read everything in buffer
kb_buffer = $0400	;  256-byte kb buffer ($0400-$04ff)
kb_flags = $0002

RELEASE = %00000001
SHIFT   = %00000010


kb = $030c ; 1 byte to store kb keypress value
; btnstatus really doesn't do what I want
btnstatus=$030e ; 1 byte for button status

E  = %01000000
RW = %00100000
RS = %00010000

  .org $1800

reset:
  ldx #$ff
  txs
  
  ; sei ; set INTERRUPT disable
  cli ; clear interrupt disable bit
  
  ; initialize btnstatus to #$00
  lda #$00
  sta btnstatus 
  
  lda #$01 ; CA1 set to positive active edge (0000 0001)
  sta PCR1 ; set peripheral control register to zero
  lda #$05 ; CA1 & CA2 set to positive active edge (0000 0101)
  sta PCR2 

  ; Initialize kb read and kb write pointers to zero
  sta kb_flags
  sta kb_wptr
  sta kb_rptr

  
  lda #$82
  sta IER1 ; init IER1 - CA1 only
  lda #$83
  sta IER2 ; init IER2 - CA1 & CA2 only

  lda #%11111111 ; Set all pins on port B to output
  sta DDRB1
  lda #%00001111 ; led pins on port A to output
  sta DDRA1  
  lda #$0f ; via2 PORTB1 leds output pins
  sta DDRB2
  
  ;TURN ON LED1
  lda #$01
  ;eor PORTA1
  sta PORTA1  
  
  jsr lcd_init
  ;lda #%00111000 ; Set 8-bit mode; 2-line display; 5x8 font
  ;jsr lcd_instruction
  lda #%00101000 ; Set 4-bit mode; 2-line display; 5x8 font
  jsr lcd_instruction

  lda #%00001110 ; Display on; cursor on; blink off
  jsr lcd_instruction

  lda #%00000110 ; entry mode set: Increment and shift cursor to right; don't shift display-no scrolling;
  jsr lcd_instruction
  lda #$00000001 ; Clear display
  jsr lcd_instruction
    
  lda PORTA2
  sta kb ; initialize kb var
  
loop:
  ;toggle ON LED1
  lda #$01
  eor PORTA1
  sta PORTA1  
  ;toggle ON LED2
  lda #$02
  eor PORTA1
  sta PORTA1  
  
kbcheck:
;loop4cont:
  ; check if button PORTA1 $20 button pressed
  lda PORTA1
  and #$20
  bne quit ; quit program

  ; check if button PORTA1 $80 button pressed
  lda PORTA1
  and #$80 ; just the buttons
  bne blueled  

  ; check for button $01 from PORTA1
  lda PORTA1
  and #$10
  bne lcdsoftreset

  sei
  lda kb_rptr
  cmp kb_wptr
  cli
  bne key_pressed  
  jmp kbcheck

key_pressed: 
  ldx kb_rptr
  lda kb_buffer, x
  
  cmp #$0a           ; enter - go to second line
  beq enter_pressed
  cmp #$1b           ; escape - clear display
  beq esc_pressed  
  
  sta kb ; store keypress
  jsr print_char
  
  lda kb ; load keypress to send to serial PORT
  sta ACIA_DATA ; write data to ACIA data  
  
  inc kb_rptr
  jmp loop

enter_pressed:
  lda #%10101000 ; put cursor at position 40
  jsr lcd_instruction
  inc kb_rptr
  jmp loop

esc_pressed:
  lda #%00000001 ; Clear display
  jsr lcd_instruction
  inc kb_rptr
  jmp loop

quit:
  jmp $ff00

btn4press:
  ; toggle LED $04
  lda #$08  
  eor PORTA1
  sta PORTA1 

  jmp loop

blueled:  
  ; TOGGLE LED 8
  lda #$08
  eor PORTA1
  sta PORTA1
  
  ; Debounce button press
blueleddb:
  lda PORTB2
  eor #$01
  sta PORTB2
  
  jsr delay
  
  lda PORTA1
  and #$80
  bne blueleddb
  jmp loop

lcdsoftreset:
  jsr delay_50ms
  jsr delay_50ms
  lda #%00110000 ; Special function set
  jsr lcd_instruction_1
  jsr delay_50ms
  lda #%00110000 ; Special function set
  jsr lcd_instruction_1
  jsr delay_50ms
  lda #%00110000 ; Special function set
  jsr lcd_instruction_1
  jsr delay_50ms
  
  ;lda #%00111000 ; Set 8-bit mode; 2-line display; 5x8 font
  ;jsr lcd_instruction
  lda #%00101000 ; Set 4-bit mode; 2-line display; 5x8 font
  jsr lcd_instruction
  ;lda #%00001110 ; Display on; cursor on; blink off
  ;jsr lcd_instruction
  ;lda #%00000110 ; entry mode set: Increment and shift cursor to right; don't shift display-no scrolling;
  ;jsr lcd_instruction
  lda #$00000001 ; Clear display
  jsr lcd_instruction  
  
  ; toggle VIA1 blue LED $08
  lda PORTA1
  eor #$08
  sta PORTA1
  jmp loop

lcd_instruction_1:  ; Used for setting up the LCD where busy flag can't be checked
  sta PORTB1
  lda #0         ; Clear RS/RW/E bits
  sta PORTB1
  lda #E         ; Set E bit to send instruction
  sta PORTB1
  lda #0         ; Clear RS/RW/E bits
  sta PORTB1
  rts

delay_50ms:       
  ldy #22
outer_loop:
  ldx #250
inner_loop:
  dex
  nop
  nop
  bne inner_loop  ; Branch if x not zero (9 cyclesx250 = 2250 microsecs. Repeat 22 times will give 50 ms)
  dey
  bne outer_loop
  rts
  

delay:
  lda #$50
  sta VIA1_T1CL
  lda #$c3
  sta VIA1_T1CH
delay1:
  bit IFR1
  bvc delay1	; if bit 6 is 0, goto delay1 again
  lda VIA1_T1CL		; read VIA1_T1CL to reset timer1 flag
				; It acks that the timer has expired.
  rts

print_num: ; store the num you want to print in value
			; before calling print_num
			; For 2 byte numbers, add MSB to value + 1 
  lda #0
  sta message	; clear message

divide:
  ; Initialize the remainder to zero
  lda #0
  sta mod10
  sta mod10 + 1
  clc ; clear carry bit
  
  ldx #16 ; store #16 in x register
		;18f7 (ldx) + 18f8 (16 dec=10 hex)
  
divloop:
  ; Rotate quotient and remainder
  rol value;18f9
  rol value + 1
  rol mod10
  rol mod10 + 1

  ; a,y = dividend - divisor
  sec ; set carry bit
  lda mod10
  sbc #10 ; subtract 10
  tay ; xfer a to y register. save low byte
  lda mod10 + 1
  sbc #0 ; subtract zero
  bcc ignore_result ; branch if divident < divisor
					; (carry is Clear or = 0)
  sty mod10 ; store mod10 into y;8c
  sta mod10 + 1 ; store mod10+1 into a

ignore_result:
  dex ; decrement x;ca
  bne divloop ; branch if Zero flag not set
				; branch if x not zero
  rol value ; shift in the last bit of the quotient;
  rol value + 1
  
  ; our answer is in mod10
  lda mod10 ; load remainder (mod 10) into a reg
  clc ; clear carry bit
  adc #0 ; add value of zero to a reg
  ;jsr print_char ; instead of print char, push char onto front of our string
  jsr push_char;20;1928
  

  ; if value != 0, continue dividing
  lda value ; load value into a reg ;
  ora value + 1 ; OR the a reg;
				; test if a reg is all zeroes
  bne divide ; if a reg not equal zero, then branch

  ldx #0 ; Load 0 into X register

print:
  lda message,x  ; Load message char(x) into a
  ;beq loop	; branch if Z=1 (CPU zero flag = 1; a = 0)
  beq done
  jsr print_char
  inx ; increment x
  jmp print
  
done:
  rts

; We have char in A register
; Add the char in the A register to the
; beginning of the null-terminated string
; 'message'.
push_char:
  pha ; push new first char onto stack from a;48
  ldy #0 ; load into y 0, use for counter;a0
  
 char_loop:
  lda message,y ; Get char on string and put into x;b9
  tax ; x reg contains the char;aa
  pla ; pull char off of a reg;68
  sta message,y ;  pull char off stack and add it to the string
		;99
  iny ; incremement y;c8;
  txa ; put x back into a reg;8a
  pha ; push a onto stack. Push char from string onto stack
	;48
  bne char_loop;d0

  pla ;68
  sta message,y ; Pull the null off stack and add to end of string
  
  rts ; return from subroutine

print_kb_char: ; print the actual kb character pressed
		; lookup via keymap
		; add a space here if you can figure it out
  
  ; ldx PORTA2 ; get data from keyboard
  ; lda keytoascii, x
  ; sta ACIA_DATA ; write data to ACIA data
  
  ldx PORTA2
  lda keymap, x
  
  jsr print_char
  rts
  
  ;jmp exit ; no need to jump, it's next up anyways!

print_space: ; print space character
  ldx #$29 ; space char scan code
  ;ldx #$55 ; space char scan code
  lda keymap, x ; get character from keymap
  jsr print_char
  rts
  
print_hyphen:
  ldx #$4e; space char scan code
  lda keymap, x ; get character from keymap
  jsr print_char
  rts

lcd_wait:  
  pha
  lda #%11110000  ; top 4 bits of Port B are input
				; pin PB7 is NC
  sta DDRB1

lcdbusy: 
  lda #RW
  sta PORTB1
  lda #(RW | E)
  sta PORTB1
  lda PORTB1
  pha             ; and put on stack since it has the busy flag
  lda #RW
  sta PORTB1
  lda #(RW | E)
  sta PORTB1
  lda PORTB1     ; Read low nibble
  pla            ; Get high nibble off stack
  and #%00001000
  bne lcdbusy
  
  lda #RW
  sta PORTB1
  lda #%11111111
  sta DDRB1

  pla
  rts

lcd_init:
  lda #%00000010 ; Set 4-bit mode
  sta PORTB1
  ora #E
  sta PORTB1
  and #%00001111
  sta PORTB1
  rts

lcd_instruction: ; 4-bit LCD mode

  jsr lcd_wait
  pha
  lsr
  lsr
  lsr
  lsr            ; Send high 4 bits
  sta PORTB1
  ora #E         ; Set E bit to send instruction
  sta PORTB1
  eor #E         ; Clear E bit
  sta PORTB1
  pla
  and #%00001111 ; Send low 4 bits
  sta PORTB1
  ora #E         ; Set E bit to send instruction
  sta PORTB1
  eor #E         ; Clear E bit
  sta PORTB1

  rts

print_char: ; 4-bit LCD mode
  jsr lcd_wait
  
  pha
  lsr
  lsr
  lsr
  lsr             ; Send high 4 bits

  ora #RS         ; Set RS
  sta PORTB1
  ora #E          ; Set E bit to send instruction
  sta PORTB1
  eor #E          ; Clear E bit
  sta PORTB1

  
  pla
  and #%00001111  ; Send low 4 bits

  ora #RS         ; Set RS
  sta PORTB1

  ora #E          ; Set E bit to send instruction
  sta PORTB1
  
  eor #E          ; Clear E bit
  sta PORTB1

  rts



 .include "keymap.asm"