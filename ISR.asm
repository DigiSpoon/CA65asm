; 1-16-2024
; Was: IRQv3-3A00kbflags.asm

; Interrupt Vector code
; 11-26-23 2132 DD COMOUT call to delay subroutine

; 11-12-23 V3 IRQv3-3A00
; Add keyboard keypress check from VIA 2, then add to kb buffer.
; If SERVICE_VIA2 IFR2 is set, it means VIA2 triggered
; the IRQ. But then, check the IFR2 for the either button
; or keyboard press. Then branch to appropriate code.

; 11-5-2023 Version 2 IRQv2-3A00
; v2: checks VIA1 & VIA2 IFR bit 7 using bit instruction
; 10-30-2023 add delay after LED toggle

PORTA1 = $6001
PORTB2 = $4000
PORTA2 = $4001
IFR1 = $600d
IFR2 = $400d
T1CL1 = $6004
T1CH1 = $6005

; keyboard
kb_wptr = $0000	; kb write pointer
kb_rptr = $0001	; kb read pointer
;kb_buffer = $0200	;  256-byte kb buffer ($0200-$02ff)
;kb_buffer = $0300	;  256-byte kb buffer ($0300-$03ff)
kb_buffer = $0400	;  256-byte kb buffer ($0400-$04ff)

kb_flags = $0002

RELEASE = %00000001
SHIFT   = %00000010

  .org $3A00
  sei ; set interrupt disable bit
  
  ; push a, x, y to stack
  pha
  phx
  phy
  
  ; check VIA1's IFR register without loading it
  bit IFR1
  bmi SERVICE_VIA1
  bit IFR2
  bmi SERVICE_VIA2
  jmp exitirq
  
SERVICE_VIA1:
  lda PORTA1
  eor #$04 ; toggle PORTA1 LED 3 ($04)
  sta PORTA1
  ; jsr delay
  bit PORTA1 ; read porta to clear the Interrupt
  jmp exitirq
  
SERVICE_VIA2:

  ; load IFR2 to find what caused the Interrupt
  lda IFR2
  and #$02 ; #%0000 0010 CA1 is 2nd bit
  bne VIA2_CA1_interrupt
  
  lda IFR2
  and #$01 ; #%0000 0001 CA2 is 1st bit
  bne keyboard_interrupt
  
  
  jmp exitirq ; exit interrupt handler

VIA2_CA1_interrupt:
  lda PORTB2
  eor #$04 ; toggle PORTA2 LED 3 ($04)
  sta PORTB2
  ; jsr delay
  bit PORTA2 ; read B OF VIA 2 port to clear the Interrupt
  
  jmp exitirq
  
delay:
  ldy #$32
  ldx #$32
count:
  dex
  bne count
  dey
  bne count
  rts

keyboard_interrupt:
  ; ldx PORTA2 ; read keyboard port
  ; lda keymap, x
  ; ldx kb_wptr
  ; sta kb_buffer, x ; store the pressed key in kb buffer
  ; inc kb_wptr ; increment kb write pointer
  ; bit PORTB2 ; read PORTB2 to clear the Interrupt

  lda kb_flags
  and #RELEASE   ; check if we're releasing a key
  beq read_key   ; otherwise, read the key

  lda kb_flags
  eor #RELEASE   ; flip the releasing bit
  sta kb_flags
  lda PORTA2      ; read the key value that's being RELEASED
  cmp #$12       ; left shift
  beq shift_up
  cmp #$59       ; right shift
  beq shift_up
  jmp exit_keyboard_interrupt

read_key:
  lda PORTA2
  cmp #$f0        ; if releasing a key
  beq key_release ; set the releasing bit
  cmp #$12        ; left shift
  beq shift_down
  cmp #$59        ; right shift
  beq shift_down

  tax
  lda kb_flags
  and #SHIFT
  bne shifted_key

  lda keymap, x   ; map to character code
  jmp push_key

shifted_key:
  lda keymap_shifted, x   ; map to character code

push_key:
  ldx kb_wptr
  sta kb_buffer, x
  inc kb_wptr
  jmp exit_keyboard_interrupt

shift_up:
  lda kb_flags
  eor #SHIFT  ; flip the shift bit
  sta kb_flags
  jmp exit_keyboard_interrupt
  
shift_down:
  lda kb_flags
  ora #SHIFT
  sta kb_flags
  jmp exit_keyboard_interrupt
 
key_release:
  lda kb_flags
  ora #RELEASE
  sta kb_flags

exit_keyboard_interrupt:
exitirq:
  ; pull y, x, a from stack
  ply
  plx
  pla  

  cli ; clear interrupt disable bit
  rti ; return from interrupt
  
  .org $1B00
keymap:
  .byte "????????????? `?" ; 00-0F
  .byte "?????q1???zsaw2?" ; 10-1F
  .byte "?cxde43?? vftr5?" ; 20-2F
  .byte "?nbhgy6???mju78?" ; 30-3F
  .byte "?,kio09??./l;p-?" ; 40-4F
  .byte "??'?[=????",$0a,"]?\??" ; 50-5F
  .byte "?????????1?47???" ; 60-6F
  .byte "0.2568",$1b,"??+3-*9??" ; 70-7F
  .byte "????????????????" ; 80-8F
  .byte "????????????????" ; 90-9F
  .byte "????????????????" ; A0-AF
  .byte "????????????????" ; B0-BF
  .byte "????????????????" ; C0-CF
  .byte "????????????????" ; D0-DF
  .byte "????????????????" ; E0-EF
  .byte "????????????????" ; F0-FF
  
  .org $1C00
keymap_shifted:
  .byte "????????????? ~?" ; 00-0F
  .byte "?????Q!???ZSAW@?" ; 10-1F
  .byte "?CXDE#$?? VFTR%?" ; 20-2F
  .byte "?NBHGY^???MJU&*?" ; 30-3F
  .byte "?<KIO)(??>?L:P_?" ; 40-4F
  .byte '??"?{+?????}?|??' ; 50-5F
  .byte "?????????1?47???" ; 60-6F
  .byte "0.2568???+3-*9??" ; 70-7F
  .byte "????????????????" ; 80-8F
  .byte "????????????????" ; 90-9F
  .byte "????????????????" ; A0-AF
  .byte "????????????????" ; B0-BF
  .byte "????????????????" ; C0-CF
  .byte "????????????????" ; D0-DF
  .byte "????????????????" ; E0-EF
  .byte "????????????????" ; F0-FF