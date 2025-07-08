; NES ROM Header - tells emulator/hardware about the ROM
.segment "HEADER"
.byte 'N', 'E', 'S', $1a      ; "NES" followed by MS-DOS EOF marker
.byte $02                     ; 2 x 16KB PRG-ROM banks
.byte $01                     ; 1 x 8KB CHR-ROM bank
.byte $00, $00                ; Mapper 0, no special features

; Main program code section
.segment "CODE"

; Interrupt Request Handler - called when IRQ interrupt occurs
.proc irq_handler
  RTI                     ; Return from interrupt (we don't use IRQ)
.endproc

; Non-Maskable Interrupt Handler - called during VBlank
.proc nmi_handler
  RTI                     ; Return from interrupt (not using NMI yet)
.endproc

; Reset Handler - called when system starts up or resets
.proc reset_handler
  ; === CPU Initialization ===
  SEI                     ; Set interrupt disable flag (ignore IRQ)
  CLD                     ; Clear decimal mode flag (NES doesn't support BCD)

  ; === APU Initialization ===
  LDX #$40                ; Load X with $40
  STX $4017               ; Write to APU Frame Counter register
                          ; Disables APU frame IRQ

  ; === Stack Initialization ===
  LDX #$FF                ; Load X with $FF (top of stack page)
  TXS                     ; Transfer X to Stack pointer ($01FF)

  ; === PPU Initialization ===
  INX                     ; Increment X (now $00)
  STX $2000               ; PPUCTRL = 0 (disable NMI, sprites, background)
  STX $2001               ; PPUMASK = 0 (disable rendering)
  STX $4010               ; DMC frequency register = 0 (disable DMC)

  ; === Wait for PPU to be ready ===
  BIT $2002               ; Read PPUSTATUS to clear VBlank flag

  ; First VBlank wait - PPU needs time to stabilize
vblankwait:
  BIT $2002               ; Read PPUSTATUS register
  BPL vblankwait          ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until VBlank flag is set

  ; Second VBlank wait - ensures PPU is fully ready
vblankwait2:
  BIT $2002               ; Read PPUSTATUS register again
  BPL vblankwait2         ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until second VBlank occurs
  JSR init
  JMP main                ; Jump to main program
.endproc

.proc init

  LDA #$00
  STA $20
  STA $21
  STA $22

.endproc

.proc update

  JSR handle_input

.endproc

.proc handle_input

    JSR read_controller  ; Read controller input
    LDA $22 ; store button just pressed in accumulator


    ; Checks each button ID with just pressed and branches to the corresponding label
    CMP #$01
    BEQ a_button_just_pressed

    CMP #$02
    BEQ b_button_just_pressed

    CMP #$03
    BEQ select_button_just_pressed

    CMP #$04
    BEQ start_button_just_pressed

    CMP #$05
    BEQ up_button_just_pressed

    CMP #$06
    BEQ down_button_just_pressed

    CMP #$07
    BEQ left_button_just_pressed

    CMP #$08
    BEQ right_button_just_pressed

    JMP no_input

  a_button_just_pressed:
    ; Handle A button just pressed
    RTS

  b_button_just_pressed:
    RTS

  select_button_just_pressed:
    RTS

  start_button_just_pressed:
    RTS

  right_button_just_pressed:
    RTS

  left_button_just_pressed:
    RTS

  up_button_just_pressed:
    RTS

  down_button_just_pressed:
    RTS

  no_input:
    RTS


.endproc

.proc read_controller

  LDA $20
  STA $21

  ; Read controller state
  ; Controller 1 is at $4016 ( controller 2 at $4017)
  ; Strobe the controller to reset the state
  ; Writing $01 to $4016 seems to reset it to first button (a, b, select, start, up, down, left, right)
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016

  LDX #$01 ; Loop index for button ID

  read_controller_loop:

    LDA $4016 ; Check to see what button is pressed
    AND #$01 ; Mask to get only the first bit (button pressed)
    BEQ next_controller_step        ; Skip if not pressed

    ; Button pressed, store its ID
    STX $20         ; Store current button ID in $20
    CPX $21         ; Compare with previous button state
    BNE new_controller_press         ; If new press, handle it

    ; If same button pressed, just continue
    LDA #$00
    STA $22 ; Clear button just pressed
    RTS

  next_controller_step:

    INX
    CPX #$09
    BNE read_controller_loop ; Continue loop until all buttons checked

     ; Finished loop, no key pressed
    LDA $20 ; Store current button
    STA $23 ; Store button just pressed

    LDA #$00
    STA $20 ; Clear current button
    STA $22 ; Clear button just pressed

    RTS

  new_controller_press:
    STX $22 ; Store button just pressed in $22
    RTS

.endproc

; Main program logic
.proc main

  ; === Set Background Color ===
  LDX $2002               ; Read PPUSTATUS to reset address latch

  ; Set PPU address to palette RAM
  LDX #$3f                ; High byte of palette address ($3F00)
  STX $2006               ; Write to PPUADDR register
  LDX #$00                ; Low byte of palette address ($3F00)
  STX $2006               ; Write to PPUADDR register
                          ; PPU address is now $3F00 (background palette 0)

  ; Write background color
  LDA #$31               ; Load color $31 (light blue) into accumulator
  STA $2007               ; Write to PPUDATA register
                          ; This sets the background color

  ; === Enable Rendering ===
  LDA #%00011110          ; Load rendering flags:
                          ; bit 4 = 1: Show background
                          ; bit 3 = 1: Show sprites
                          ; bit 2 = 1: Show background in leftmost 8 pixels
                          ; bit 1 = 1: Show sprites in leftmost 8 pixels
  STA $2001               ; Write to PPUMASK register (enable rendering)

  ; === Infinite Loop ===
forever:

  INC $01
  LDX $01
  CPX #$FF
  BNE forever

  LDX #$00
  STX $01
  JSR update

  JMP forever             ; Jump to forever (infinite loop)

.endproc

; Interrupt vectors - tells CPU where to jump for each interrupt
.segment "VECTORS"
.addr nmi_handler         ; NMI vector ($FFFA-$FFFB)
.addr reset_handler       ; Reset vector ($FFFC-$FFFD)
.addr irq_handler         ; IRQ vector ($FFFE-$FFFF)

; Character ROM data (graphics patterns)
.segment "CHARS"
.res 8192                 ; Reserve 8KB of space for CHR-ROM data
                          ; (sprite and background tile patterns)

; Startup segment
.segment "STARTUP"
