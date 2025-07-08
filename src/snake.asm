.include "nes.inc"
.include "macros.inc"

;*****************************************************************
; Define NES cartridge Header
;*****************************************************************
; NES ROM Header - tells emulator/hardware about the ROM
.segment "HEADER"
.byte 'N', 'E', 'S', $1a      ; "NES" followed by MS-DOS EOF marker
.byte $02                     ; 2 x 16KB PRG-ROM banks
.byte $01                     ; 1 x 8KB CHR-ROM bank
.byte $00, $00                ; Mapper 0, no special features

;*****************************************************************
; Define NES interrupt vectors
;*****************************************************************
; Interrupt vectors - tells CPU where to jump for each interrupt
.segment "VECTORS"
.addr nmi_handler         ; NMI vector ($FFFA-$FFFB)
.addr reset_handler       ; Reset vector ($FFFC-$FFFD)
.addr irq_handler         ; IRQ vector ($FFFE-$FFFF)

;*****************************************************************
; 6502 Zero Page Memory ($0000–$00FF)
;*****************************************************************
; Fast RAM accessible with 1-byte instructions (faster, smaller)
; Use this for variables accessed frequently (like gamepad, game variables, pointers)
.segment "ZEROPAGE"
gamepad:		.res 1 ; stores the current gamepad values

d_x:			  .res 1 ; x velocity of ball
d_y:			  .res 1 ; y velocity of ball

current_button_state:   .res 1 ; Stores the ID of the currently pressed button (1-8)
previous_button_state:  .res 1 ; Stores the ID of the previously pressed button
button_just_pressed:    .res 1 ; Stores the ID of the button that was just pressed
button_just_released:   .res 1 ; Stores the ID of the button that was just released

p_x: .res 1
p_y: .res 1

snake_direction: .res 1
move_snake_delay: .res 1

snake_length: .res 1
snake_body: .res 32

;*****************************************************************
; OAM (Object Attribute Memory) ($0200–$02FF)
;*****************************************************************
; This 256-byte buffer holds sprite data to be copied to the PPU's
; internal OAM via DMA ($4014). Each sprite uses 4 bytes:
;   Byte 0: Y position
;   Byte 1: Tile index
;   Byte 2: Attributes (palette, flipping, priority)
;   Byte 3: X position
.segment "OAM"
oam: .res 256	; sprite OAM data


.segment "RODATA"
bit_masks:
  .byte %00000001 ; A
  .byte %00000010 ; B
  .byte %00000100 ; Select
  .byte %00001000 ; Start
  .byte %00010000 ; Up
  .byte %00100000 ; Down
  .byte %01000000 ; Left
  .byte %10000000 ; Right

;*****************************************************************
; Code Segment (ROM)
;*****************************************************************
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
  LDA #$00                ; Set A = $00
  STA PPU_CONTROL         ; PPUCTRL = 0 (disable NMI, sprites, background)
  STA PPU_MASK            ; PPUMASK = 0 (disable rendering)
  STA APU_DM_CONTROL      ; disable DMC IRQ

  ; First VBlank wait - PPU needs time to stabilize
:                         ; Anonymous label (used to branch to in BPL command)
  BIT PPU_STATUS          ; Read PPUSTATUS register
  BPL :-                  ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until VBlank flag is set

  clear_oam oam

  ; Second VBlank wait - ensures PPU is fully ready
:                         ; Anonymous label (used to branch to in BPL command)
  BIT PPU_STATUS          ; Read PPUSTATUS register again
  BPL :-                  ; Branch if Plus (bit 7 = 0, no VBlank)
                          ; Loop until second VBlank occurs

  JSR set_palette         ; Set palette colors
  JSR set_nametable       ; Set nametable tiles
  JSR init_sprites        ; Initialize sprites

  JMP main                ; Jump to main program
.endproc

;******************************************************************************
; Procedure: set_palette
;------------------------------------------------------------------------------
; Writes 32 bytes of color data from palette_data into the PPU's palette memory
; at $3F00. This fills all 4 background palettes and all 4 sprite palettes.
;
; Assumes:
;   - palette_data is a 32-byte table in ROM.
;   - Rendering is off or you're in VBlank (writes to $2007 are safe).
;******************************************************************************
.proc set_palette

    vram_set_address PALETTE_ADDRESS  ; Set PPU VRAM pointer to $3F00 (palette memory start)

    LDX #$00                          ; Start index at 0

@loop:
    LDA palette_data, X              ; Load color byte from palette_data table
    STA PPU_VRAM_IO                  ; Write to PPU at $3F00 + X
    INX                              ; Move to next color
    CPX #$20                         ; Have we written all 32 bytes?
    BNE @loop                        ; Loop until done

    RTS                              ; Return from procedure

.endproc

;******************************************************************************
; Procedure: set_nametable
;------------------------------------------------------------------------------
; Transfers 960 bytes of tile data from `nametable_data` to the PPU's nametable 0
; at $2000. This fills the entire 32×30 background tilemap.
;
; Assumes:
;   - PPU is ready (called during or before VBlank)
;   - nametable_data is a 960-byte table in ROM
;   - $00/$01 are available as temporary zero-page pointer
;******************************************************************************
.proc set_nametable

    wait_for_vblank                        ; Wait for VBlank to safely write to PPU

    vram_set_address NAME_TABLE_0_ADDRESS ; Set VRAM address to start of nametable ($2000)

    ; Set up 16-bit pointer to nametable_data
    LDA #<nametable_data
    STA $00                                ; Store low byte of address in $00
    LDA #>nametable_data
    STA $01                                ; Store high byte in $01

    ; Begin loading 960 bytes (32×30 tiles)
    LDY #$00                               ; Offset within current page
    LDX #$03                               ; 3 full 256-byte pages (768 bytes total)

load_page:
    LDA ($00),Y                            ; Load byte from nametable_data + Y
    STA PPU_VRAM_IO                        ; Write to PPU VRAM ($2007)
    INY
    BNE load_page                          ; Loop through 256-byte page

    INC $01                                ; Move to next page (high byte of pointer)
    DEX
    BEQ check_remaining                    ; After 3 pages (768 bytes), handle the remaining 192
    JMP load_page

check_remaining:
    LDY #$00                               ; Reset Y to load remaining 192 bytes
remaining_loop:
    LDA ($00),Y
    STA PPU_VRAM_IO
    INY
    CPY #192                               ; Stop after 192 bytes (960 - 768)
    BNE remaining_loop

    ; Reset scroll registers to 0,0 (needed after VRAM access)
    LDA #$00
    STA PPU_SCROLL                         ; Write horizontal scroll
    STA PPU_SCROLL                         ; Write vertical scroll

    RTS                                    ; Done

.endproc

.proc init_sprites

  set_sprite oam, 0, 10, 0, (SPRITE_PALETTE_0), 10
  set_sprite oam, 1, 10, 1, (SPRITE_PALETTE_2), 10

  LDA #$00

  STA current_button_state
  STA previous_button_state
  STA button_just_pressed
  STA button_just_released

  LDA #$08
  STA p_x
  STA p_y

  LDA #$03
  STA snake_direction

  LDA #$00
  STA move_snake_delay

  ; Initialize snake body
  ; snake_body is an array of 32 bytes, each representing a segment of the snake
  LDA #$03
  STA snake_length

  ; Initialize snake body to empty
  ; Each segment is 2 bytes (x, y) so we need to clear
  LDX #$00
  init_snake_body:

    LDA #$00
    STA snake_body,X
    INX
    STA snake_body,X
    INX
    CPX #$20
    BNE init_snake_body

  ; Set initial snake body segments
  ; The snake starts at (5,5) and extends to the left
  LDX #$05
  LDY #$05
  STX snake_body + 0
  STY snake_body + 1

  DEX
  STX snake_body + 2
  STY snake_body + 3

  DEX
  STX snake_body + 4
  STY snake_body + 5

  RTS
.endproc

;******************************************************************************
; Procedure: update_sprites
;------------------------------------------------------------------------------
; Transfers 256 bytes of sprite data from the OAM buffer in CPU RAM
; to the PPU's internal Object Attribute Memory (OAM) using DMA.
;
; Assumes:
;   - OAM sprite data is stored at a page-aligned label `oam` (e.g., $0200)
;   - This is called during VBlank or with rendering disabled
;******************************************************************************
.proc update_sprites
  ; Update OAM values

  LDA p_x
  STA oam + 3

  LDA p_y
  STA oam

  JSR move_snake


  ; Set OAM address to 0 — required before DMA or manual OAM writes
  LDA #$00
  STA PPU_SPRRAM_ADDRESS    ; $2003 — OAM address register

  ; Start OAM DMA transfer (copies 256 bytes from oam → PPU OAM)
  ; Write the high byte of the source address (e.g., $02 for $0200)
  LDA #>oam
  STA SPRITE_DMA            ; $4014 — triggers OAM DMA (513–514 cycles, CPU stalled)

  RTS

.endproc

.proc move_snake

  LDX snake_length

  clear_snake_body:

    LDY snake_body, X
    DEX
    DEX
    STY snake_body, X
    INX

    LDY snake_body, X
    DEX
    DEX
    STY snake_body, X
    INX

    CMP #$02
    BNE clear_snake_body

  LDA snake_direction
  CMP #$00
  BEQ move_snake_up

  CMP #$01
  BEQ move_snake_down

  CMP #$02
  BEQ move_snake_left

  CMP #$03
  BEQ move_snake_right

  RTS

  move_snake_right:

    CLC
    LDA p_x
    ADC #$08
    STA p_x
    RTS

  move_snake_left:
    CLC
    LDA p_x
    SBC #$07
    STA p_x
    RTS

  move_snake_up:
    CLC
    LDA p_y
    SBC #$07
    STA p_y
    RTS

  move_snake_down:
    CLC
    LDA p_y
    ADC #$08
    STA p_y
    RTS

.endproc

.proc handle_input

  JSR read_controller

  ; Check up button just pressed
  LDA button_just_pressed
  AND #%00010000
  BEQ skip_up_button_just_pressed

  ; Up button pressed
  LDA snake_direction
  CMP #$01
  BEQ skip_up_button_just_pressed

  LDA #$00
  STA snake_direction

  skip_up_button_just_pressed:

    ; Check down button just pressed
    LDA button_just_pressed
    AND #%00100000
    BEQ skip_down_button_just_pressed

    ; Down button pressed
    LDA snake_direction
    CMP #$00
    BEQ skip_down_button_just_pressed

    LDA #$01
    STA snake_direction

  skip_down_button_just_pressed:

    ; Check left button just pressed
    LDA button_just_pressed
    AND #%01000000
    BEQ skip_left_button_just_pressed

    ; Left button pressed
    LDA snake_direction
    CMP #$03
    BEQ skip_left_button_just_pressed

    LDA #$02
    STA snake_direction

  skip_left_button_just_pressed:

    ; Check right button just pressed
    LDA button_just_pressed
    AND #%10000000
    BEQ skip_right_button_just_pressed

    ; Right button pressed
    LDA snake_direction
    CMP #$02
    BEQ skip_right_button_just_pressed

    LDA #$03
    STA snake_direction

  skip_right_button_just_pressed:

    ; Do next checks

  RTS

.endproc

.proc read_controller

  LDA current_button_state
  STA previous_button_state

  ; Read controller state
  ; Controller 1 is at $4016 ( controller 2 at $4017)
  ; Strobe the controller to reset the state
  ; Writing $01 to $4016 seems to reset it to first button (a, b, select, start, up, down, left, right)
  LDA #$01
  STA $4016
  LDA #$00
  STA $4016

  LDX #$00        ; Bit index (0-7)
  STA current_button_state ; Clear previous button state

  read_controller_loop:

    LDA $4016
    AND #$01        ; Mask to get button press
    BEQ no_press

    ; Set the corresponding bit in current_button_state
    LDA current_button_state
    ORA bit_masks, X
    STA current_button_state

  no_press:
    INX
    CPX #$08
    BNE read_controller_loop

  ; Compute button_just_pressed = current & ~previous
  ; (button that is pressed this frame but not last)
  LDA previous_button_state
  EOR #$FF
  AND current_button_state
  STA button_just_pressed

  ; Compute button_just_released = previous & ~current
  ; (button that was pressed last frame but not this)
  LDA current_button_state
  EOR #$FF
  AND previous_button_state
  STA button_just_released

  RTS

.endproc


;******************************************************************************
; Procedure: main
;------------------------------------------------------------------------------
; Main entry point for the game loop.
; Initializes PPU control settings, enables rendering, and enters
; an infinite loop where it waits for VBlank and updates sprite data.
;******************************************************************************
.proc main

    ;--------------------------------------------------------------------------
    ; Configure PPU Control Register ($2000)
    ; - Enable NMI on VBlank (bit 7 = 1)
    ; - Use pattern table 1 ($1000) for background tiles (bit 4 = 1)
    ;--------------------------------------------------------------------------
    LDA #(PPUCTRL_ENABLE_NMI | PPUCTRL_BG_TABLE_1000)
    STA PPU_CONTROL

    ;--------------------------------------------------------------------------
    ; Configure PPU Mask Register ($2001)
    ; - Show background and sprites (bits 3 & 4 = 1)
    ; - Show background and sprites in leftmost 8 pixels (bits 1 & 2 = 1)
    ;--------------------------------------------------------------------------
    LDA #(PPUMASK_SHOW_BG | PPUMASK_SHOW_SPRITES | PPUMASK_SHOW_BG_LEFT | PPUMASK_SHOW_SPRITES_LEFT)
    STA PPU_MASK

forever:
    ; Wait for vertical blank before doing game logic and rendering updates
    wait_for_vblank
    INC move_snake_delay

    ; Update sprite data (DMA transfer to PPU OAM)
    JSR update_sprites

    JSR handle_input

    LDA move_snake_delay
    CMP #$10
    BNE forever

    LDA #$00
    STA move_snake_delay
    JSR move_snake

    ; Infinite loop — keep running frame logic
    JMP forever

.endproc


;*****************************************************************
; Character ROM data (graphics patterns)
;*****************************************************************
.segment "CHARS"
; Load CHR data
  .incbin "assets/tiles.chr"

;*****************************************************************
; Character ROM data (graphics patterns)
;*****************************************************************
.segment "RODATA"
; Load palette data
palette_data:
  .incbin "assets/palette.pal"
; Load nametable data
nametable_data:
  .incbin "assets/screen.nam"

; Startup segment
.segment "STARTUP"
