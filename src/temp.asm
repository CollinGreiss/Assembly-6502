; Include NES hardware definitions and useful macros
.include "nes.inc"    ; NES hardware register definitions
.include "macros.inc" ; Useful assembly macros

; Define sprite memory locations for easy access
SPRITE_FOOD_ADDR = oam + 0
SPRITE_SNAKE_ADDR = oam + $10

;===============================================================================
; NES CARTRIDGE HEADER
;===============================================================================
; This header tells the NES (or emulator) important information about the ROM
.segment "HEADER"
.byte 'N', 'E', 'S', $1a      ; "NES" signature followed by MS-DOS EOF marker
.byte $02                     ; 2 x 16KB PRG-ROM banks (32KB total program space)
.byte $01                     ; 1 x 8KB CHR-ROM bank (graphics data)
.byte $01, $00                ; Mapper 0 (NROM), no special features

;===============================================================================
; INTERRUPT VECTORS
;===============================================================================
; These tell the CPU where to jump when specific events occur
.segment "VECTORS"
.addr nmi_handler         ; NMI (Non-Maskable Interrupt) - called 60 times per second
.addr reset_handler       ; RESET - called when system starts up
.addr irq_handler         ; IRQ (Interrupt Request) - not used in this game

;===============================================================================
; ZERO PAGE MEMORY ($0000–$00FF)
;===============================================================================
; Zero page is the fastest memory on the NES - instructions using it are
; smaller and faster than regular RAM. Use it for frequently accessed variables.
.segment "ZEROPAGE"

;--- General Purpose Variables ($00-$0F) ---
temp_var:               .res 1    ; Temporary variable for calculations
temp_var2:              .res 1    ; Second temporary variable
temp_ptr_low:           .res 1    ; Low byte of 16-bit pointer
temp_ptr_high:          .res 1    ; High byte of 16-bit pointer
random_num:             .res 1    ; Current random number for ball direction
nmi_ready:              .res 1    ; Flag: 1=update PPU, 2=turn off rendering
ppu_ctl0:               .res 1    ; PPU Control Register value
ppu_ctl1:               .res 1    ; PPU Mask Register value
                        .res 8   ; Reserved space

;--- Controller Input ($10-$1F) ---

current_button_state:   .res 1 ; Stores the ID of the currently pressed button (1-8)
previous_button_state:  .res 1 ; Stores the ID of the previously pressed button
button_just_pressed:    .res 1 ; Stores the ID of the button that was just pressed
button_just_released:   .res 1 ; Stores the ID of the button that was just released

                        .res 12    ; Reserved space

;--- Game State Variables ($20-$2F) ---
game_state:             .res 1    ; Current game state (not used in this version)
player_score:           .res 1    ; Player 1's current score
time:                   .res 1    ; Frame counter incremented each NMI
prev_time:              .res 1    ; Previous frame's time value
frame_counter:          .res 1    ; Counts frames for seconds timer
seconds:                .res 1    ; Seconds elapsed (not displayed)
                        .res 10    ; Reserved space

;--- Gameplay Variables ($30-$) ---

snake_direction_queue: .res 1
snake_direction: .res 1
move_snake_delay: .res 1

food_position:          .res 2
snake_length:           .res 1
snake_size:             .res 1
snake_body:             .res 128

;===============================================================================
; SPRITE DATA (OAM - Object Attribute Memory)
;===============================================================================
; This 256-byte buffer holds sprite data that gets copied to the PPU each frame
; Each sprite uses 4 bytes: Y position, Tile index, Attributes, X position
.segment "OAM"
oam: .res 256             ; Buffer for all sprite data (64 sprites max)

;===============================================================================
; RODATA (Read-Only Data)
;===============================================================================
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

;===============================================================================
; MAIN PROGRAM CODE
;===============================================================================
.segment "CODE"

;-------------------------------------------------------------------------------
; IRQ Handler - Interrupt Request Handler
;-------------------------------------------------------------------------------
; Called when an IRQ interrupt occurs (we don't use IRQ interrupts in this game)
.proc irq_handler
  RTI                     ; Return from interrupt immediately
.endproc

;-------------------------------------------------------------------------------
; NMI Handler - Non-Maskable Interrupt Handler
;-------------------------------------------------------------------------------
; This is called automatically 60 times per second during VBlank
; VBlank is the only safe time to update graphics on the NES
.proc nmi_handler
  ; Save all CPU registers on the stack (good practice in interrupt handlers)
  PHA                     ; Push Accumulator onto stack
  TXA                     ; Transfer X to Accumulator
  PHA                     ; Push X (via A) onto stack
  TYA                     ; Transfer Y to Accumulator
  PHA                     ; Push Y (via A) onto stack

  ; Update game timing
  INC time                ; Increment frame counter each NMI (60 Hz)

  ; Update seconds timer (60 frames = 1 second)
  INC frame_counter       ; Increment frame counter
  LDA frame_counter       ; Load current frame count
  CMP #60                 ; Compare with 60 (1 second at 60 FPS)
  BNE skip_second_update  ; If not 60, skip second update
    INC seconds           ; Increment seconds counter
    LDA #0                ; Reset frame counter to 0
    STA frame_counter
  skip_second_update:

  ; Transfer sprite data to PPU using DMA (Direct Memory Access)
  ; This is much faster than copying sprites individually
  BIT PPU_STATUS          ; Read PPU status to reset address latch
  LDA #>oam               ; Load high byte of OAM buffer address
  STA SPRITE_DMA          ; Trigger DMA transfer ($4014)
                          ; Hardware automatically copies 256 bytes from $0200-$02FF

  ; Restore CPU registers from stack (in reverse order)
  PLA                     ; Pull Y from stack
  TAY                     ; Transfer A to Y
  PLA                     ; Pull X from stack
  TAX                     ; Transfer A to X
  PLA                     ; Pull original A from stack

  RTI                     ; Return from interrupt
.endproc

;-------------------------------------------------------------------------------
; Reset Handler - System Initialization
;-------------------------------------------------------------------------------
; This code runs when the NES is turned on or reset
; It initializes all hardware and prepares the system for the game
.proc reset_handler
  ;=== CPU Initialization ===
  SEI                                 ; Set Interrupt Disable flag (ignore IRQ)
  CLD                                 ; Clear Decimal mode (NES doesn't use BCD math)

  ;=== APU (Audio Processing Unit) Initialization ===
  LDX #$40                            ; Load X with $40
  STX $4017                           ; Write to APU Frame Counter register
                                      ; This disables APU frame interrupts

  ;=== Stack Pointer Initialization ===
  LDX #$FF                            ; Load X with $FF (top of stack)
  TXS                                 ; Transfer X to Stack pointer
                                      ; Stack now points to $01FF

  ;=== PPU (Picture Processing Unit) Initialization ===
  LDA #$00                            ; Load A with 0
  STA PPU_CONTROL                     ; PPUCTRL = 0 (disable NMI, rendering off)
  STA PPU_MASK                        ; PPUMASK = 0 (disable all rendering)
  STA APU_DM_CONTROL                  ; Disable DMC (Delta Modulation Channel) IRQ

  ;=== Wait for PPU to stabilize ===
  ; The PPU needs time to warm up after power-on
  ; We must wait for 2 VBlank periods before using it

  ; First VBlank wait
wait_first_vblank:
  BIT PPU_STATUS                      ; Read PPU status register
  BPL wait_first_vblank               ; Branch if Plus (bit 7 = 0, no VBlank yet)
                                      ; Loop until VBlank flag is set

  ;=== Clear RAM ===
  ; Clear the OAM (sprite) buffer to ensure clean startup
  clear_oam oam                       ; Macro to clear sprite memory

  ; Second VBlank wait (ensures PPU is fully ready)
wait_second_vblank:
  BIT PPU_STATUS                      ; Read PPU status register again
  BPL wait_second_vblank              ; Branch if Plus (bit 7 = 0, no VBlank yet)
                                      ; Loop until second VBlank occurs

  ;=== Initialize Game Graphics ===
  JSR set_palette                     ; Load color palette into PPU
  JSR set_nametable                   ; Load background tiles into PPU
  JSR init_sprites                    ; Set up initial sprite positions

  ;=== Start Game ===
  JMP main                            ; Jump to main game loop
.endproc

;-------------------------------------------------------------------------------
; Set Palette - Load 32 color values into PPU palette memory
;-------------------------------------------------------------------------------
; The NES can display 64 different colors, but only 32 at a time
; Palette memory starts at $3F00 and holds 32 bytes
.proc set_palette
    ; Set PPU VRAM address to palette memory start ($3F00)
    vram_set_address PALETTE_ADDRESS  ; Macro to set VRAM address

    LDX #$00                         ; Start with first color (index 0)

palette_loop:
    LDA palette_data, X               ; Load color value from ROM data
    STA PPU_VRAM_IO                   ; Write color to PPU palette memory
    INX                               ; Move to next color
    CPX #$20                          ; Have we written all 32 colors?
    BNE palette_loop                  ; If not, continue loop

    RTS                               ; Return to caller
.endproc

;-------------------------------------------------------------------------------
; Set Nametable - Load background tile data into PPU nametable memory
;-------------------------------------------------------------------------------
; The nametable defines which tiles appear where on the background
; Each screen is 32x30 tiles = 960 bytes total
.proc set_nametable
  wait_for_vblank                     ; Wait for VBlank (safe time to write PPU)

  ; Set PPU VRAM address to nametable 0 start ($2000)
  vram_set_address NAME_TABLE_0_ADDRESS

  ; Set up 16-bit pointer to nametable data in ROM
  LDA #<nametable_data                ; Load low byte of nametable_data address
  STA temp_ptr_low                    ; Store in zero page pointer
  LDA #>nametable_data                ; Load high byte of nametable_data address
  STA temp_ptr_high                   ; Store in zero page pointer

  ; Copy 960 bytes of nametable data to PPU
  ; We need to copy 3 full pages (768 bytes) + 192 remaining bytes
  LDY #$00                            ; Y = offset within current page
  LDX #$03                            ; X = number of full pages to copy

copy_full_pages:
  LDA (temp_ptr_low),Y                ; Load byte from ROM using indirect addressing
  STA PPU_VRAM_IO                     ; Write byte to PPU VRAM
  INY                                 ; Move to next byte in page
  BNE copy_full_pages                 ; Continue until Y wraps to 0 (256 bytes copied)

  INC temp_ptr_high                   ; Move pointer to next page
  DEX                                 ; Decrement page counter
  BNE copy_full_pages                 ; Continue if more pages to copy

  ; Copy remaining 192 bytes (960 - 768 = 192)
  LDY #$00                            ; Reset Y to 0
copy_remaining:
  LDA (temp_ptr_low),Y                ; Load byte from ROM
  STA PPU_VRAM_IO                     ; Write to PPU
  INY                                 ; Next byte
  CPY #192                            ; Have we copied all remaining bytes?
  BNE copy_remaining                  ; If not, continue

  ; Reset PPU scroll to (0,0) - required after VRAM writes
  LDA #$00
  STA PPU_SCROLL                      ; Set horizontal scroll to 0
  STA PPU_SCROLL                      ; Set vertical scroll to 0

  RTS                                 ; Return to caller
.endproc

;-------------------------------------------------------------------------------
; Update Score Display - Draw current scores on screen
;-------------------------------------------------------------------------------
; This writes the score digits directly to the nametable during VBlank
.proc update_score
  ; Draw Player 1 score (left side of screen)
  LDA PPU_STATUS                      ; Reset PPU address latch
  LDA #$20                            ; High byte of nametable address
  STA PPU_ADDRESS                     ; Set high byte
  LDA #$25                            ; Low byte (specific tile position)
  STA PPU_ADDRESS                     ; Set low byte

  ; Convert score number to ASCII character
  LDA #'0'                            ; ASCII value of '0'
  CLC                                 ; Clear carry flag
  ADC player_score                    ; Add player score (0-9)
  STA PPU_VRAM_IO                     ; Write character to screen

  ; Reset scroll after VRAM writes
  LDA #$00
  STA PPU_SCROLL                      ; Horizontal scroll = 0
  STA PPU_SCROLL                      ; Vertical scroll = 0

  RTS                                 ; Return to caller
.endproc

;-------------------------------------------------------------------------------
; Initialize Sprites - Copy initial sprite data to OAM buffer
;-------------------------------------------------------------------------------
; This sets up the starting positions and graphics for all sprites
.proc init_sprites
  LDX #0                              ; Start with first sprite byte

copy_sprite_data:
    LDA sprite_data, X                ; Load byte from ROM sprite data
    STA oam, X                        ; Store in OAM buffer
    INX                               ; Move to next byte
    CPX #$24                          ; Have we copied all sprite data?
                                      ; ($24 = 36 bytes = 9 sprites * 4 bytes each)
    BNE copy_sprite_data              ; If not, continue copying

  RTS                                 ; Return to caller
.endproc

;-------------------------------------------------------------------------------
; Initialize Game - Set starting scores and game state
;-------------------------------------------------------------------------------
.proc init_game

  ; Reset starting scores
  LDA #0
  STA player_score

  ; Set snake move position (left)
  LDA #$03
  STA snake_direction
  STA snake_direction_queue

  ; Set Random Food Position
  JSR update_random
  LDA random_num
  AND #$F8

  STA food_position
  STA SPRITE_FOOD_ADDR + SPRITE_OFFSET_X

  JSR update_random
  LDA random_num
  AND #$F8

  STA food_position + 1
  STA SPRITE_FOOD_ADDR + SPRITE_OFFSET_Y

  ; Initialize snake length and size
  LDA #4
  STA snake_length

  LDA #8
  STA snake_size

  LDA #$40
  LDX #$00
  LDY #$40
  init_snake_body:

    STA snake_body, X
    STY snake_body + 1, X

    INX
    CPX snake_length
    BNE init_snake_body

  LDA #$01
  STA game_state
  RTS                                 ; Return to caller
.endproc

;-------------------------------------------------------------------------------
; Update snake - Handle input and collision
;-------------------------------------------------------------------------------
.proc check_eat

  LDA snake_body
  CMP food_position
  BNE no_food_hit

  LDA snake_body + 1
  CMP food_position + 1
  BNE no_food_hit

  INC snake_length
  INC snake_size
  INC snake_size

  JSR update_random
  LDA random_num
  AND #$F8
  STA food_position
  STA SPRITE_FOOD_ADDR + SPRITE_OFFSET_X

  JSR update_random
  LDA random_num + 1
  AND #$F8
  STA food_position + 1
  STA SPRITE_FOOD_ADDR + SPRITE_OFFSET_Y

  CMP #$F8
  BEQ food_out_of_bounds

  CMP #$F0
  BEQ food_out_of_bounds
  RTS

  no_food_hit:
    RTS

  food_out_of_bounds:

    LDA #$E8
    STA food_position + 1

    RTS

.endproc

.proc check_lost

  LDX #$00

  check_snake_hit:

    INX
    INX

    CPX snake_size
    BEQ no_snake_hit

    LDA snake_body
    CMP snake_body, X
    BNE check_snake_hit

    LDA snake_body + 1
    CMP snake_body + 1, X
    BNE check_snake_hit

    LDA #02
    STA game_state
    RTS

  no_snake_hit:
    RTS


.endproc

.proc move_snake

  LDA snake_direction_queue
  STA snake_direction

  LDX snake_size
  INX
  INX
  move_snake_loop:

    LDA snake_body - 3, X
    STA snake_body - 1, X

    LDA snake_body - 4, X
    STA snake_body - 2, X

    DEX
    DEX

    CPX #$02
    BNE move_snake_loop

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
    LDA snake_body + 0
    ADC #$08
    STA snake_body + 0
    RTS

  move_snake_left:
    CLC
    LDA snake_body + 0
    SBC #$07
    STA snake_body + 0
    RTS

  move_snake_up:
    LDA snake_body + 1
    SEC
    SBC #$08
    STA snake_body + 1

    CMP #$F8
    BNE exit_move_snake

    LDA #$E8
    STA snake_body + 1
    RTS

  move_snake_down:
    LDA snake_body + 1
    CLC
    ADC #$08
    STA snake_body + 1

    CMP #$F0
    BNE exit_move_snake

    LDA #$00
    STA snake_body + 1
    RTS

  exit_move_snake:
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
  STA snake_direction_queue

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
    STA snake_direction_queue

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
    STA snake_direction_queue

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
    STA snake_direction_queue

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

;-------------------------------------------------------------------------------
; Main Game Loop - Initialize game and run main loop
;-------------------------------------------------------------------------------
.proc main
    ; Initialize random number generator with a seed
    LDA #$78                          ; Use $78 as initial random seed
    STA random_num

    ; Configure PPU Control Register ($2000)
    ; Enable NMI interrupt and set background pattern table
    LDA #(PPUCTRL_ENABLE_NMI | PPUCTRL_BG_TABLE_1000)
    STA PPU_CONTROL

    ; Configure PPU Mask Register ($2001)
    ; Enable background and sprite rendering
    LDA #(PPUMASK_SHOW_BG | PPUMASK_SHOW_SPRITES | PPUMASK_SHOW_BG_LEFT | PPUMASK_SHOW_SPRITES_LEFT)
    STA PPU_MASK

    ; Initialize game state
    JSR init_game                     ; Set initial scores

main_game_loop:

  ; Wait for frame to change (synchronize with 60Hz refresh)
  LDA time                            ; Load current frame counter
  CMP prev_time                       ; Compare with previous frame
  BEQ main_game_loop                  ; If same, keep waiting

  ; Update game logic
  JSR handle_input                    ; Process input and update direction

  LDA seconds
  CMP #$01
  BNE continue_game

  LDA #$00
  STA seconds                         ; Reset seconds counter
  JSR move_snake                      ; Update player 1 paddle
  ; JSR update_score                    ; Update score display

  continue_game:
  JMP main_game_loop                  ; Loop forever

.endproc

;-------------------------------------------------------------------------------
; Update Random Number - Generate pseudo-random numbers using LFSR
;-------------------------------------------------------------------------------
; This implements a Linear Feedback Shift Register for generating random numbers
; The algorithm uses bit shifting and XOR operations to create a sequence
; that appears random and has a long period before repeating
.proc update_random
    LDA random_num                    ; Load current random value

    ; Check if we need to apply feedback (based on bit 7)
    ASL                               ; Shift left: bit 7 -> Carry, bit 0 <- 0
    BCC no_feedback                   ; If carry clear (bit 7 was 0), skip XOR

    ; Apply feedback polynomial: XOR with $39 (binary: 00111001)
    ; This represents polynomial taps that ensure good randomness
    EOR #$39                          ; XOR with feedback pattern

no_feedback:
    STA random_num                    ; Store new random value
    RTS                               ; Return with new random number in A
.endproc

;===============================================================================
; GRAPHICS DATA (CHR-ROM)
;===============================================================================
; This section contains the tile graphics stored in Character ROM
.segment "CHARS"
; Load tile graphics from external file
; Each tile is 8x8 pixels with 2 bits per pixel (4 colors per tile)
.incbin "assets/tiles/pong.chr"       ; Binary file containing sprite/tile graphics

;===============================================================================
; GAME DATA (ROM)
;===============================================================================
.segment "RODATA"

;--- Color Palette Data ---
; 32 bytes defining the colors used in the game
; First 16 bytes = background palettes, Last 16 bytes = sprite palettes
palette_data:
  .incbin "assets/palettes/pong.pal"        ; Binary file containing palette colors

;--- Background Screen Data ---
; 960 bytes defining which tiles appear where on the background screen
; Arranged as 32 columns × 30 rows = 960 tiles total
nametable_data:
  .incbin "assets/screens/pong.nam"         ; Binary file containing screen layout

;--- Initial Sprite Data ---
; This defines the starting positions and graphics for all sprites
; Each sprite uses 4 bytes: Y position, Tile index, Attributes, X position
sprite_data:

; Food sprite
.byte 40, 2, 0, 40                  ; Food: Y=40, Tile=2, Attr=0, X=40

