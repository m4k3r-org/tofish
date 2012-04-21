; tofish.asm
;
; A simple language interpreter for Toby's Own Forth-ish
;
; Copyright (c) 2012 by Tobias Schaer ("tschaer")
;
;-------------------------------------------------------------------------------------
; "MIT Open Source Software License":
; Permission is hereby granted, free of charge, to any person obtaining a copy of
; this software and associated documentation files (the "Software"), to deal in the
; Software without restriction, including without limitation the rights to use, copy,
; modify, merge, publish, distribute, sublicense, and/or sell copies of the Software,
; and to permit persons to whom the Software is furnished to do so, subject to
; the following conditions:
;
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
;
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
;-------------------------------------------------------------------------------------
; 21.04.2012 T Schaer
; toby dot schaer at gmail dot com

; assemble & download with
; $ avra -l tofish.lst tofish.asm
; $ avrdude -v -patmega328p -carduino -P/dev/tty.usbmodemfd111 -b115200 -D -Uflash:w:tofish.hex:i

; Assembler-specific Directives
.device   ATmega328P
        
; I/O registers
.equ  DDRB    = 0x04
.equ  PORTB   = 0x05
.equ  SPL     = 0x5D
.equ  SPH     = 0x5E
.equ  UCSR0A  = 0xC0
.equ  UCSR0B  = 0xC1
.equ  UCSR0C  = 0xC2
.equ  UBRR0L  = 0xC4
.equ  UBRR0H  = 0xC5
.equ  UDR0    = 0xC6

; Register Names
.def  IRL   = r10       ; Instruction Register address
.def  IRH   = r11
.def  WL    = r12       ; W = the address of the current word's dictionary entry
.def  WH    = r13
.def  IP0L  = r14       ; IP0 = start of current definition
.def  IP0H  = r15       
.def  IPL   = r24       ; IP = Instruction Pointer: current word in definition
.def  IPH   = r25
.def  XL    = r26
.def  XH    = r27
.def  YL    = r28       ; Y = Data stack pointer
.def  YH    = r29
.def  ZL    = r30
.def  ZH    = r31

; Other
.equ  HERE0 = 0x220
.equ  SP0   = 0x8FF

; define variables
            .dseg
            .org  0x0100
MemMode:    .byte 1                 ; 0 = RAM Definition, 1 = FLASH Definition
WMode:      .byte 1                 ; Memory mode of current dictionary entry
Here:       .byte 2                 ; first free RAM location
DP:         .byte 2                 ; pointer to Name Field of most recent RAM dictionary entry
IR:         .byte 26                
TIB:        .byte 128               ; starts at 0x120

; code
            .cseg
; Interrupt vector table
; 26 x 2 words per vector =  104 bytes
            .org  0x0000
            jmp   init              ; Power-on reset
                                    ; Ends 0x0034       
; program
            .org  0x0034
init:       LDI   r17,0x00          ; 19200 baud   
            STS   UBRR0H, r17
            LDI   r17,0x67
            STS   UBRR0L, r17
            LDI   r17, 0x18         ; RX & TX Enable
            STS   UCSR0B, r17
            LDI   r17, 0x06         ; 8-N-1
            STS   UCSR0C, r17

cleanup:    LDI   r17, HIGH(SP0)    ; Subroutine stack pointer
            STS   0x5E, r17
            LDI   r17, LOW(SP0)
            STS   0x5D, r17

            LDI   YH, HIGH(HERE0)   ; Y = Data stack pointer
            LDI   YL, LOW(HERE0)
            STS   Here+1, YH        ; Dictionary grows upward from here
            STS   Here, YL

            LDI   r17, 0x00         ; Set link field terminator
            LDI   r16, 0x00
            STS   DP+1, r17
            STS   DP, r16
            
            LDI   r17, HIGH(IR)
            LDI   r16, LOW(IR)
            MOVW  IRH:IRL, r17:r16
            
            LDI   r17, 0x01
            STS   WMode, r17
            STS   MemMode, r17
   
            LDI   ZH, HIGH(interName<<1)  ; select INTERACT as the word to start executing
            LDI   ZL, LOW(interName<<1)
            JMP   next_2
            
; Interpreter loop: read-lookup-execute
pushnext:   ST    -Y, r16           ; entry point for words that stack a result
            ST    -Y, r17
next:       CALL  _word             ; Put next word in definition into Instruction Register
next_1:     CALL  _find             ; Look up word in dictionary to get Name field address
next_2:     MOVW  WH:WL, ZH:ZL      ; Save Name field address
            LDS   r17, WMode
            TST   r17
            BREQ  next_r
            CALL  fCFA              ; Get address of CodePtr field
            LPM   XL, Z+            ; Get contents of CodePtr field
            LPM   XH, Z
            RJMP  next_4
next_r:     CALL  rCFA
            LD    XL, Z+
            LD    XH, Z
next_4:     MOVW  ZH:ZL, XH:XL      ; only Z can IJMP
next_3:     IJMP                    ; execute word

; ***** Interpreter code
; _word
; put next word in definition into IR
_word:      MOVW  ZH:ZL, IPH:IPL    ; Source
            MOVW  XH:XL, IRH:IRL    ; Destination
            LDS   r17, MemMode
            TST   r17
            BREQ  rWord
            ; word is in FLASH
fWord:      LPM   r17, Z+           ; eat leading whitespace
            CPI   r17, ' '            
            BREQ  fWord
            CPI   r17, 0x00         ; end of definition?
            BREQ  fw_3
fw_1:       ST    X+, r17
            LPM   r17, Z+
            CPI   r17, ' '          ; word separator
            BREQ  fw_2
            CPI   r17, 0x00         ; end of definition?
            BRNE  fw_1
fw_2:       CLR   r17               ; replace ' ' with 0x00
fw_3:       RJMP  _word_3
            ; word is in RAM
rWord:      LD    r17, Z+           ; eat leading whitespace
            CPI   r17, ' '            
            BREQ  rWord
            CPI   r17, 0x00         ; end of input: dictionary
            BREQ  rw_2
            CPI   r17, 0x0D         ; end of input: command line
            BRNE  rw_1
            LDI   r17, 'e'          ; replace 0x0D with a printable character
            ST    X+, r17             
            RJMP  rw_2              
rw_1:       ST    X+, r17
            LD    r17, Z+
            CPI   r17, ' '          ; word separator
            BREQ  rw_2
            TST   r17
            BREQ  rw_2
            CPI   r17, 0x0D
            BRNE  rw_1
            SBIW  ZH:ZL, 0x01       ; back up, 0x0D is own word
rw_2:       CLR   r17               ; terminate buffer
_word_3:    ST    X+, r17           ; X+ here too to count the terminating null
            SUB   XL, IRL
            SBC   XH, IRH           ; XL contains word length
            MOVW  IPH:IPL, ZH:ZL    ; Update instruction Pointer
            RET

; find
; Put address of Name field in Z
_find:      LDI   ZH, HIGH(DictStart) ; Look first in FLASH dictionary
            LDI   ZL, LOW(DictStart)
            LDI   r20, 0x01         ; Search Mode = FLASH
_find_1:    LSL   ZL                ; Word to byte pointer
            ROL   ZH
            MOVW  r19:r18, ZH:ZL
            CALL  fMatch            
            MOVW  ZH:ZL, r19:r18    ; Restore NAME
            BRCS  _find_5
            CALL  fLFA
            LPM   XL, Z+
            LPM   XH, Z
            MOVW  ZH:ZL, XH:XL
            ADIW  ZH:ZL, 0x00       ; hit end of the dictionary?
            BRNE  _find_1           ; look at next word
            ; search the RAM dictionary
            LDS   ZH, DP+1          
            LDS   ZL, DP
            LDI   r20, 0x00         ; Search Mode = RAM
            ADIW  ZH:ZL, 0x00       
            BREQ  _find_3           ; Bail if DP = 0
_find_2:    MOVW  r19:r18, ZH:ZL
            CALL  rMatch
            MOVW  ZH:ZL, r19:r18
            BRCS  _find_5           ; Word found
            CALL  rLFA
            LD    XL, Z+
            LD    XH, Z
            MOVW  ZH:ZL, XH:XL
            ADIW  ZH:ZL, 0x00
            BRNE  _find_2
_find_3:    LDI   r20, 0x01
            LDI   ZH, HIGH(numbName<<1)
            LDI   ZL, LOW(numbName<<1)
_find_5:    STS   WMode, r20
            RET

; ----- Dictionary
; struct Entry {
;   word NameLength;
;   string Name[];
;   word Link;
;   word CodePtr;
;   word Anything[];
;}

; ***** System Words

; This is the main user interaction loop, believe it or not!
            .dw   interLink-interName 
interName:  .db   "interact", 0x00, 0x00
interLink:  .dw   expectName
            .dw   call
            .db   "hello stake expect exec start", 0x00, 0x00

            .equ  DictStart = interName

            .dw   expectLink-expectName
expectName: .db   "expect", 0x00, 0x00
expectLink: .dw   execName
            .dw   expect
expect:     LDI   r17, '>'
            CALL  outChar
            CALL  _space
            LDI   ZH, HIGH(TIB)
            LDI   ZL, LOW(TIB)
            CLR   r16
expect_1:   CALL  getChar
            CPI   r17, 0x08         ; check for backspace
            BRNE  expect_2
            TST   r16
            BREQ  expect_1          ; ignore if buffer empty
            SBIW  ZH:ZL, 0x01
            RJMP  expect_3
expect_2:   ST    Z+, r17           ; store character
            INC   r16               ; characters so far
            CPI   r17, 0x0D         ; on ENTER quit
            BREQ  expect_4
expect_3:   CALL  outChar           ; echo typed character
            RJMP  expect_1          ; get next character
expect_4:   CALL  _CRLF
            JMP   next

            .dw   execLink-execName
execName:   .db   "exec", 0x00, 0x00
execLink:   .dw   numbName
            .dw   exec
exec:       PUSH  IPL               
            PUSH  IPH
            PUSH  IP0L             
            PUSH  IP0H
            LDS   r17, MemMode      
            PUSH  r17               ; Save MemMode
            LDI   r17, 0x00         ; set phasers for RAM
            STS   MemMode, r17
            LDI   ZH, HIGH(TIB)
            LDI   ZL, LOW(TIB)
            MOVW  IPH:IPL, ZH:ZL
            MOVW  IP0H:IP0L, ZH:ZL
            JMP   next              ; Run subroutine

            .dw   numbLink-numbName 
numbName:   .db   "number", 0x00, 0x00    
numbLink:   .dw   trapName        
            .dw   number                
number:     MOVW  XH:XL, IRH:IRL
            CLR   r16               ; zero
            CLR   r3                ; r3:r2 is result pair
            CLR   r2
            CLR   r5                ; High byte of BASE
            LD    r17, X+
            CPI   r17, '$'          ; decide on radix
            BRNE  n_1
            LDI   ZH, HIGH(isHex)   ; Mode = Hex
            LDI   ZL, LOW(isHex)
            LD    r17, X+           ; eat the $
            LDI   r18, 0x10
            RJMP  n_3
n_1:        CPI   r17, '-'          ; Mode = Decimal
            BRNE  n_2
            LDI   r19, 0x01         ; set the minus flag
            LD    r17, X+           ; eat the minus
n_2:        LDI   ZH, HIGH(isNum)  
            LDI   ZL, LOW(isNum)
            LDI   r18, 0x0A
n_3:        MOV   r4, r18           ; Low byte of BASE
            ICALL
            BRCC  n_7               ; invalid char?
            CALL  a2h
            ADD   r2, r17           ; accumulate first digit
n_4:        LD    r17, X+           ; convert remaining digits
            TST   r17
            BREQ  n_5               ; done?
            ICALL
            BRCC  n_7               ; invalid char?
            MOVW  r7:r6, r3:r2      
            CALL  mul16             ; return result in r3:r2
            CALL  a2h
            ADD   r2, r17           ; accumulate result
            ADC   r3, r16
            RJMP  n_4               ; get next character
n_5:        CPI   r19, 0x01         ; deal with negative
            BRNE  n_6
            COM   r2               
            COM   r3
            ADD   r2, r19
            ADC   r3, r16
n_6:        ST    -Y, r2            ; Happy ending
            ST    -Y, r3
            JMP   next
n_7:        LDI   ZH, HIGH(trap)    ; Sad ending
            LDI   ZL, LOW(trap)
            JMP   next_3

            .dw   trapLink-trapName
trapName:   .db   "trap", 0x00, 0x00
trapLink:   .dw   helloName
            .dw   trap
            .db   "Unknown word ", 0x27, 0x00, 0x00
trap:       LDI   ZH, HIGH((trapLink+2)<<1)
            LDI   ZL, LOW((trapLink+2)<<1)
            CALL  outStr
            MOVW  ZH:ZL, IRH:IRL
            CALL  ramStr
trap_2:     LDI   r17, 0x27   ; '
            CALL  outChar
            CALL  _CRLF
            JMP   semi        ; quit definition

            .dw   helloLink-helloName
helloName:  .db   "hello", 0x00
helloLink:  .dw   eomName
            .dw   hello
            .db   "tofish  >-))*>", 0x00, 0x00
hello:      CALL  _CRLF
            LDI   ZH, HIGH((helloLink+2)<<1)
            LDI   ZL, LOW((helloLink+2)<<1)
            CALL  outStr
            CALL  _CRLF
            CALL  _CRLF
            JMP   next
     
            .dw   eomLink-eomName
eomName:    .db   "e", 0x00
eomLink:    .dw   callName
            .dw   eom
eom:        CALL  _CRLF
            LDI   ZH, HIGH(eomStr<<1)
            LDI   ZL, LOW(eomStr<<1)
            CALL  outStr
            CALL  _CRLF
            JMP   semi              ; return from TIB
eomStr:     .db   "Ok.", 0x00

            .dw   callLink-CallName
callName:   .db   "call", 0x00, 0x00
callLink:   .dw   constName
            .dw   call
call:       PUSH  IPL               ; Save Instruction Pointer
            PUSH  IPH
            PUSH  IP0L               ; Save Code segment
            PUSH  IP0H
            LDS   r17, MemMode      
            PUSH  r17               ; Save MemMode
            MOVW  ZH:ZL, WH:WL
            LDS   r17, WMode        ; MemMode equivalent of W
            STS   MemMode, r17
            TST   r17
            BREQ  call_1            ; RAM
            CALL  fAFA
            RJMP  call_2            ; FLASH
call_1:     CALL  rAFA
call_2:     MOVW  IPH:IPL, ZH:ZL
            MOVW  IP0H:IP0L, IPH:IPL
            JMP   next              ; Run subroutine

            .dw   constLink-constName 
constName:  .db   "const", 0x00
constLink:  .dw   emitName
            .dw   const
const:      MOVW  ZH:ZL, WH:WL
            CALL  fAFA
            LPM   r16, Z+
            LPM   r17, Z
            JMP   PUSHNEXT

; **** User-visible dictionary

; **** Console words

            .dw   emitLink-emitName 
emitName:   .db   "emit", 0x00, 0x00
emitLink:   .dw   sEmitName
            .dw   emit
emit:       LD    r17, Y+
            LD    r16, Y+
            MOV   r17, r16
            CALL  outChar
            JMP   next

            .equ  UserDictStart = emitName

            .dw   sEmitLink-sEmitName 
sEmitName:  .db   "sEmit", 0x00
sEmitLink:  .dw   hexdotName
            .dw   sEmit
sEmit:      LD    ZH, Y+
            LD    ZL, Y+
            CALL  outStr
            JMP   NEXT

            .dw   hexdotLink-hexdotName
hexdotName: .db   ".$", 0x00, 0x00
hexdotLink: .dw   dotName
            .dw   hexdot
hexdot:     LD    r17, Y            ; first digit
            CALL  outHigh
hexdot_2:   LD    r17, Y+           ; second digit
            CALL  outLow
hexdot_4:   LD    r17, Y            ; third digit
            CALL  outHigh
hexdot_6:   LD    r17, Y+           ; fourth digit
            CALL  outLow
            JMP   next

            .dw   dotLink-dotName
dotName:    .db   ".", 0x00
dotLink:    .dw   crName
            .dw   dot
dot:        LD    r19, Y+
            LD    r18, Y+
            TST   r19
            BRPL  dot_1             ; negative number ?
            LDI   r17, '-'          ; print minus sign
            CALL  outChar
            COM   r19               ; take 2's comp
            COM   r18
            LDI   r16, 0x01
            ADD   r18, r16
            CLR   r17
            ADC   r19, r17
dot_1:      LDI   r22, 100          ; first digit
            LDI   r21, HIGH(10000)
            LDI   r20, LOW(10000)
            MOVW  r1:r0, r19:r18
            CALL  _dothigh
dot_2:      LDI   r22, 10           ; second digit
            LDI   r21, HIGH(1000)
            LDI   r20, LOW(1000)
            MOVW  r1:r0, r19:r18
            CALL  _dothigh
dot_3:      LDI   r22, 100          ; third digit
            LDI   r21, HIGH(100)
            LDI   r20, LOW(100)
            MOVW  r1:r0, r19:r18
            CALL  _dotlow
dot_4:      LDI   r22, 10           ; fourth digit
            LDI   r21, HIGH(10)
            LDI   r20, LOW(10)
            MOVW  r1:r0, r19:r18
            CALL  _dotlow
dot_5:      MOV   r17, r18          ; fifth digit
            CALL  h2a
            CALL  outChar
            JMP   next
            
_dothigh:   LDI   r17, 100
            MOV   r3, r17
            CALL  div16
            MOVW  r1:r0, r5:r4
_dotlow:    MOV   r3, r22
            CALL  div16             ; result is in r5:r4
            MOV   r17, r4
            CALL  h2a
            CALL  outChar            
            MOVW  r7:r6, r21:r20
            CALL  mul16             ; result is in r3:r2
            SUB   r18, r2           
            SBC   r19, r3
            RET
            
            .dw   crLink-crName 
crName:     .db   "CR", 0x00, 0x00
crLink:     .dw   spaceName
            .dw   cr
cr:         CALL  _CRLF
            JMP   NEXT

            .dw   spaceLink-spaceName 
spaceName:  .db   "space", 0x00
spaceLink:  .dw   spacesName
            .dw   space
space:      CALL  _space
            JMP   NEXT
            
            .dw   spacesLink-spacesName 
spacesName: .db   "spaces", 0x00, 0x00
spacesLink: .dw   plusName
            .dw   call
            .db   "dup =0 ?true drop ; ?end space dec start", 0x00, 0x00

; **** Arithmetic words

            .dw   plusLink-plusName
plusName:   .db   "+", 0x00
plusLink:   .dw   subName
            .dw   plus
plus:       LD    r19, Y+           ; pop 1st number High byte
            LD    r18, Y+           ; pop 1st number Low byte
            LD    r17, Y+           ; pop 2nd number High byte
            LD    r16, Y+           ; pop 2nd number Low byte
            ADD   r16, r18
            ADC   r17, r19
            JMP   PUSHNEXT

            .dw   subLink-subName
subName:    .db   "-", 0x00
subLink:    .dw   negName
            .dw   sub
sub:        LD    r19, Y+           ; pop 1st number High byte
            LD    r18, Y+           ; pop 1st number Low byte
            LD    r17, Y+           ; pop 2nd number High byte
            LD    r16, Y+           ; pop 2nd number Low byte
            SUB   r16, r18          ; 
            SBC   r17, r19
            JMP   PUSHNEXT

            .dw   negLink-negName
negName:    .db   "negate", 0x00, 0x00 
negLink:    .dw   divName
            .dw   neg
neg:        LD    r17, Y+           ; pop 1st number High byte
            LD    r16, Y+           ; pop 1st number Low byte
            COM   r17               ; invert
            COM   r16
            LDI   r18, 0x01         ; add 1
            CLR   r19
            ADD   r16, r18
            ADC   r17, r19
            JMP   PUSHNEXT

            .dw   divLink-divName
divName:    .db   "/", 0x00
divLink:    .dw   incName
            .dw   divide
divide:     LD    r17, Y+     ; high byte of divisor
            LD    r3, Y+
            LD    r1, Y+      ; high byte of numerator
            LD    r0, Y+
            CALL  div16
            ST    -Y, r4
            ST    -Y, r5
            JMP   NEXT

            .dw   incLink-incName 
incName:    .db   "inc", 0x00
incLink:    .dw   decName
            .dw   inc
inc:        LD    r17, Y+
            LD    r16, Y+
            INC   r16
            BRNE  inc_1
            INC   r17
inc_1:      JMP   PUSHNEXT

            .dw   decLink-decName 
decName:    .db   "dec", 0x00
decLink:    .dw   andName
            .dw   dec
dec:        LD    r17, Y+
            LD    r16, Y+
            LDI   r18, 0x01
            SUB   r16, r18
            CLR   r18
            SBC   r17, r18
            JMP   PUSHNEXT

; ***** Bit manipulation words

            .dw   andLink-andName
andName:    .db   "AND", 0x00
andLink:    .dw   orName
            .dw   and
and:        LD    r19, Y+           ; pop 1st number High byte
            LD    r18, Y+           ; pop 1st number Low byte
            LD    r17, Y+           ; pop 2nd number High byte
            LD    r16, Y+           ; pop 2nd number Low byte
            AND   r16, r18
            AND   r17, r19
            JMP   PUSHNEXT

            .dw   orLink-orName
orName:     .db   "OR", 0x00, 0x00
orLink:     .dw   invName
            .dw   or
or:         LD    r19, Y+           ; pop 1st number High byte
            LD    r18, Y+           ; pop 1st number Low byte
            LD    r17, Y+           ; pop 2nd number High byte
            LD    r16, Y+           ; pop 2nd number Low byte
            OR    r16, r18
            OR    r17, r19
            JMP   PUSHNEXT

            .dw   invLink-invName
invName:    .db   "INV", 0x00
invLink:    .dw   xorName
            .dw   inv
inv:        LD    r17, Y+           ; pop 1st number High byte
            LD    r16, Y+           ; pop 1st number Low byte
            COM   r17
            COM   r16
            JMP   PUSHNEXT

            .dw   xorLink-xorName
xorName:    .db   "XOR", 0x00
xorLink:    .dw   lslName
            .dw   xor
xor:        LD    r19, Y+           ; pop 1st number High byte
            LD    r18, Y+           ; pop 1st number Low byte
            LD    r17, Y+
            LD    r16, Y+
            EOR   r16, r18
            EOR   r17, r19
            JMP   PUSHNEXT

            .dw   lslLink-lslName 
lslName:    .db   "LSL", 0x00
lslLink:    .dw   lsrName
            .dw   lsl
lsl:        LD    r17, Y+
            LD    r16, Y+
            LSL   r16
            ROL   r17
            JMP   PUSHNEXT

            .dw   lsrLink-lsrName 
lsrName:    .db   "LSR", 0x00
lsrLink:    .dw   asrName
            .dw   lsr
lsr:        LD    r17, Y+
            LD    r16, Y+
            LSR   r17
            ROR   r16
            JMP   PUSHNEXT

            .dw   asrLink-asrName 
asrName:    .db   "ASR", 0x00
asrLink:    .dw   rolName
            .dw   asr
asr:        LD    r17, Y+
            LD    r16, Y+
            ASR   r17
            ROR   r16
            JMP   PUSHNEXT

            .dw   rolLink-rolName 
rolName:    .db   "ROL", 0x00
rolLink:    .dw   rorName
            .dw   rol
rol:        LD    r17, Y+
            LD    r16, Y+
            MOV   r19, r17    ; copy upper byte
            LSL   r19         ; move LSB into Carry
            ROL   r16         ; rotate
            ROL   r17   
            JMP   PUSHNEXT

            .dw   rorLink-rorName 
rorName:    .db   "ROR", 0x00
rorLink:    .dw   dupName
            .dw   ror
ror:        LD    r17, Y+
            LD    r16, Y+
            MOV   r18, r16    ; copy lower byte
            LSR   r18         ; move LSB into Carry
            ROR   r17         ; rotate
            ROR   r16   
            JMP   PUSHNEXT

; ***** Data Stack operators

            .dw   dupLink-dupName 
dupName:    .db   "dup", 0x00
dupLink:    .dw   dropName
            .dw   dup
dup:        LD    r17, Y     
            LDD   r16, Y+1
            JMP   PUSHNEXT

            .dw   dropLink-dropName 
dropName:   .db   "drop", 0x00, 0x00
dropLink:   .dw   swapName
            .dw   drop
drop:       ADIW  YH:YL, (1<<1)
            JMP   NEXT

            .dw   swapLink-swapName 
swapName:   .db   "swap", 0x00, 0x00
swapLink:   .dw   overName
            .dw   swap
swap:       LD    r19, Y+     ; value 1
            LD    r18, Y+
            LD    r17, Y+     ; value 2
            LD    r16, Y+
            ST    -Y, r18
            ST    -Y, r19
            JMP   PUSHNEXT

            .dw   overLink-overName 
overName:   .db   "over", 0x00, 0x00
overLink:   .dw   byteswName
            .dw   over
over:       LDD   r17, Y+2
            LDD   r16, Y+3
            JMP   PUSHNEXT

            .dw   byteswLink-byteswName 
byteswName: .db   "byteswap", 0x00, 0x00
byteswLink: .dw   RPushName
            .dw   bytesw
bytesw:     LD   r16, Y+
            LD   r17, Y+
            JMP   PUSHNEXT

; ***** Return Stack operators

           .dw   RPushLink-RPushName 
RPushName:  .db   "Rpush", 0x00
RPushLink:  .dw   RPopName
            .dw   RPush             
RPush:      LD    r17, Y+
            LD    r16, Y+
            PUSH  r16
            PUSH  r17
            JMP   next

            .dw   RPopLink-RPopName 
RPopName:   .db   "Rpop", 0x00, 0x00
RPopLink:   .dw   RPeekName
            .dw   RPop             
RPop:       POP   r17
            POP   r16
            JMP   PUSHNEXT

            .dw   RPeekLink-RPeekName 
RPeekName:  .db   "Rpeek", 0x00
RPeekLink:  .dw   rfpName
            .dw   RPeek             
RPeek:      POP   r17
            POP   r16
            PUSH  r16
            PUSH  r17
            JMP   PUSHNEXT

            .dw   rfpLink-rfpName 
rfpName:    .db   "R@+", 0x00
rfpLink:    .dw   rspName
            .dw   rfp             
rfp:        POP   ZH         ; this is the address
            POP   ZL
            LD    r16, Z+
            LD    r17, Z+
            PUSH  ZL
            PUSH  ZH
            JMP   PUSHNEXT

            .dw   rspLink-rspName 
rspName:    .db   "R!+", 0x00
rspLink:    .dw   eqzName
            .dw   rsp             
rsp:        POP   ZH          ; this is the address
            POP   ZL
            LD    r17, Y+     ; this is the data
            LD    r16, Y+
            ST    Z+, r16
            ST    Z+, r17
            PUSH  ZL
            PUSH  ZH
            JMP   next


; ***** Comparison operators

            .dw   eqzLink-eqzName 
eqzName:    .db   "=0", 0x00, 0x00
eqzLink:    .dw   ltzName
            .dw   eqz
eqz:        LD    r17, Y+     
            LD    r16, Y+
            ;ADD   r17, r16
            TST   r17
            BRNE  eqz_1
            TST   r16
            BRNE  eqz_1
            LDI   r16, 0x01
            RJMP  eqz_2
eqz_1:      CLR   r16
eqz_2:      CLR   r17
            JMP   PUSHNEXT

            .dw   ltzLink-ltzName 
ltzName:    .db   "<0", 0x00, 0x00
ltzLink:    .dw   gtzName
            .dw   ltz
ltz:        LD    r17, Y+     
            LD    r16, Y+
            TST   r17
            BRMI  ltz_1       
            CLR   r16               ; its not negative
            RJMP  ltz_2
ltz_1:      LDI   r16, 0x01         ; its negative
ltz_2:      CLR   r17
            JMP   PUSHNEXT

            .dw   gtzLink-gtzName 
gtzName:    .db   ">0", 0x00, 0x00
gtzLink:    .dw   eqName
            .dw   gtz
gtz:        LD    r17, Y+     
            LD    r16, Y+
            TST   r17
            BRMI  gtz_1             ; negative
            ADD   r17, r16
            BREQ  gtz_1             ; or zero
            LDI   r16, 0x01         ; positive
            RJMP  gtz_2
gtz_1:      CLR   r16
gtz_2:      CLR   r17
            JMP   PUSHNEXT

            .dw   eqLink-eqName 
eqName:     .db   "=", 0x00
eqLink:     .dw   ltName
            .dw   eq
eq:         LD    r17, Y+     ; value 1
            LD    r16, Y+
            LD    r19, Y+     ; value 2
            LD    r18, Y+ 
            CP    r16, r18
            BRNE  eq_1
            CP    r17, r19
            BRNE  eq_1
            LDI   r16, 0x01
            RJMP  eq_2
eq_1:       CLR   r16
eq_2:       CLR   r17
            JMP   PUSHNEXT

            .dw   ltLink-ltName 
ltName:     .db   "<", 0x00
ltLink:     .dw   gtName
            .dw   lt
lt:         LD    r17, Y+     ; value 1
            LD    r16, Y+
            LD    r19, Y+     ; value 2
            LD    r18, Y+ 
            SUB   r18, r16
            SBC   r19, r17
            BRMI  lt_1        ; yes its less
            CLR   r16
            RJMP  lt_2
lt_1:       LDI   r16, 0x01
lt_2:       CLR   r17
            JMP   PUSHNEXT

            .dw   gtLink-gtName 
gtName:     .db   ">", 0x00
gtLink:     .dw   qSkipName
            .dw   gt
gt:         LD    r17, Y+     ; value 1
            LD    r16, Y+
            LD    r19, Y+     ; value 2
            LD    r18, Y+ 
            SUB   r18, r16
            SBC   r19, r17
            BRMI  gt_1        ; Less than
            BREQ  gt_1        ; or equal
            LDI   r16, 0x01
            RJMP  gt_2
gt_1:       CLR   r16
gt_2:       CLR   r17
            JMP   PUSHNEXT

; ***** Flow control words

            .dw   qSkipLink-qSkipName 
qSkipName:  .db   "?skip", 0x00
qSkipLink:  .dw   qTrueName
            .dw   qSkip            
qSkip:      LD    r17, Y+           ; Get truth value
            LD    r16, Y+
            TST   r16
            BREQ  qSkip_1
            CALL  _word
qSkip_1:    JMP   next

            .dw   qTrueLink-qTrueName 
qTrueName:  .db   "?true", 0x00
qTrueLink:  .dw   qEndName
            .dw   qTrue
qTrue:      LD    r17, Y+
            LD    r16, Y+
            TST   r16
            BRNE  qTrue_3           ; Stack = 1, do nothing
            LDI   r18, 0x01         ; one ?TRUE was seen
            LDI   r21, HIGH(qTrueName<<1)
            LDI   r20, LOW(qTrueName<<1)
            LDI   r23, HIGH(qEndName<<1)
            LDI   r22, LOW(qEndName<<1)
qTrue_1:    CALL  _word             
            MOVW  ZH:ZL, r21:r20    ; look for ?TRUE
            CALL  fMatch
            BRCC  qTrue_2           ; it wasn't ?TRUE
            INC   r18
qTrue_2:    MOVW  ZH:ZL, r23:r22    ; look for ?END
            CALL  fMatch
            BRCC  qTrue_1           ; it was neither, next word
            DEC   r18
            BRNE  qTrue_1           ; not the matching ?END
            JMP   next_1            ; go to _find instead of _word
qTrue_3:    JMP   next

            .dw   qEndLink-qEndName 
qEndName:   .db   "?end", 0x00, 0x00
qEndLink:   .dw   startName
            .dw   qEnd
qEnd:       JMP   next

            .dw   startLink-startName 
startName:  .db   "start", 0x00
startLink:  .dw   stakeName
            .dw   start     
start:      MOVW  IPH:IPL, IP0H:IP0L
            JMP   next

            .dw   stakeLink-stakeName 
stakeName:  .db   "stake", 0x00
stakeLink:  .dw   procName
            .dw   stake   
stake:      MOVW  IP0H:IP0L, IPH:IPL
            JMP   next

; ***** Definition words

            .dw   procLink-procName
procName:   .db   ":", 0x00
procLink:   .dw   semiName
            .dw   proc
proc:       CALL  _word         
            LDS   ZH, Here+1
            LDS   ZL, Here
            ST    Z+, XL            ; Put down NameLength
            MOVW  r19:r18, ZH:ZL    ; save Name field address
            MOVW  XH:XL, IRH:IRL
            CALL  ramCopy           ; Put down Namefield
            LDI   r17, 0x00
            ST    Z+, r17           ; terminate Namefield
            LDS   r16, DP           ; Put down Link field
            ST    Z+, r16
            LDS   r17, DP+1
            ST    Z+, r17
            LDI   XH, HIGH(call)    ; Put down CodePtr field
            LDI   XL, LOW(call)
            ST    Z+, XL
            ST    Z+, XH
proc_1:     MOVW  r21:r20, ZH:ZL
            CALL  _word             ; Put down source
            LDI   ZH, HIGH(eomName<<1)
            LDI   ZL, LOW(eomName<<1)
            CALL  fMatch
            BRCS  proc_2
            MOVW  XH:XL, IRH:IRL
            MOVW  ZH:ZL, r21:r20
            CALL  ramCopy
            LDI   r17, 0x20
            ST    Z+, r17
            RJMP  proc_1
proc_2:     MOVW  ZH:ZL, r21:r20    ; Fixup the terminator
            STS   Here+1, ZH
            STS   Here, ZL
            LDI   r17, 0x00
            ST    -Z, r17
            STS   DP+1, r19         ; Update DP
            STS   DP, r18
            JMP   next_1

            .dw   semiLink-semiName
semiName:   .db   ";", 0x00
semiLink:   .dw   rFetchName
            .dw   semi
semi:       POP   r17
            STS   MemMode, r17
            POP   IP0H               
            POP   IP0L
            POP   IPH            
            POP   IPL
            JMP   next

; ***** Load / store words

            .dw   rFetchLink-rFetchName 
rFetchName: .db   "r@", 0x00, 0x00
rFetchLink: .dw   rStoreName
            .dw   rFetch             
rFetch:     LD    ZH, Y+
            LD    ZL, Y+   
            LD    r16, Z+
            LD    r17, Z+
            JMP   PUSHNEXT

            .dw   rStoreLink-rStoreName 
rStoreName: .db   "r!", 0x00, 0x00
rStoreLink: .dw   cFetchName
            .dw   rStore             
rStore:     LD    r17, Y+     ; high byte of data to store
            LD    r16, Y+     ; low byte of data to store
            LD    ZH, Y+
            LD    ZL, Y+   
            ST    Z+, r16
            ST    Z+, r17
            JMP   next

            .dw   cFetchLink-cFetchName 
cFetchName: .db   "c@", 0x00, 0x00
cFetchLink: .dw   cStoreName
            .dw   cFetch             
cFetch:     LD    ZH, Y+
            LD    ZL, Y+   
            LD    r16, Z+
            LDI    r17, 0x00
            JMP   PUSHNEXT

            .dw   cStoreLink-cStoreName 
cStoreName: .db   "c!", 0x00, 0x00
cStoreLink: .dw   fetchName
            .dw   cStore             
cStore:     LD    r17, Y+     ; high byte of data to store
            LD    r16, Y+     ; low byte of data to store
            LD    ZH, Y+
            LD    ZL, Y+
            ST    Z, r16
            JMP   next

            .dw   fetchLink-fetchName 
fetchName:  .db   "@", 0x00
fetchLink:  .dw   dumpName
            .dw   fetch
fetch:      LD    ZH, Y+
            LD    ZL, Y+
            LPM   r16, Z+
            LPM   r17, Z
            JMP   PUSHNEXT
         
; ***** Applications

            .dw   dumpLink-dumpName
dumpName:   .db   "dump", 0x00, 0x00
dumpLink:   .dw   memName
            .dw   dump                
dump:       LD    ZH, Y+            ; User gives word address
            LD    ZL, Y+
            LSL   ZL                ; Make byte address (<<1) 
            ROL   ZH
            LDI   r18, 0x10         ; number of paragraphs
dump_1:     MOVW  XH:XL, ZH:ZL      ; COL 1: Memory address
            LSR   XH                
            ROR   XL                
            MOV   r17, XH
            CALL  outByte
            MOV   r17, XL
            CALL  outByte
            CALL  _space
            LDI   r16, 0x08         ; words in a paragraph
dump_2:     LPM   r17, Z+           ; COL 2: Memory as Hex
            CALL  outByte
            LPM   r17, Z+
            CALL  outByte
            CALL  _space
            DEC   r16
            BRNE  dump_2            ; loop once per word
            CALL  _space
            LDI   r16, 0x10             
            SBIW  ZH:ZL, 0x10       ; rewind to paragraph start
dump_3:     LPM   r17, Z+           ; COL 3: Memory as ASCII
            CPI   r17, 0x20         ; Ignore chars < 32
            BRLO  dump_4
            CPI   r17, 0x80         ; Ignore chars >= 128
            BRSH  dump_4
            RJMP  dump_5
dump_4:     LDI   r17, '.'
dump_5:     CALL  outChar
            DEC   r16
            BRNE  dump_3            ; loop once per byte
            CALL  _CRLF
            DEC   r18
            BRNE  dump_1            ; loop once per paragraph
            JMP   next

            .dw memLink-memName
memName:    .db   "mem", 0x00
memLink:    .dw   helpName
            .dw   mem
mem:        LD    ZH, Y+
            LD    ZL, Y+
            LDI   r18, 0x10         ; number of paragraphs
mem_1:      MOV   r17, ZH           ; COL 1: Memory Address
            CALL  outByte
            MOV   r17, ZL
            CALL  outByte
            CALL  _space
            LDI   r16, 0x10         ; bytes in a paragraph
mem_2:      LD    r17, Z+           ; COL 2: Memory in Hex
            CALL  outByte
            CALL  _space
            DEC   r16
            BRNE  mem_2             ; loop once per byte
            CALL  _space
            LDI   r16, 0x10         ; bytes in a paragraph
            SBIW  ZH:ZL, 0x10       ; rewind to paragraph start
mem_3:      LD    r17, Z+           ; COL 3: Memory in ASCII
            CPI   r17, 0x20         ; Ignore chars < 32
            BRLO  mem_4
            CPI   r17, 0x80         ; Ignore chars > 128
            BRSH  mem_4
            RJMP  mem_5
mem_4:      LDI   r17, '.'
mem_5:      CALL  outChar
            DEC   r16
            BRNE  mem_3             ; loop once per char
            CALL  _CRLF
            DEC   r18
            BRNE  mem_1             ; loop once per paragraph
            JMP   next

            .dw   helpLink-helpName
helpName:   .db   "?", 0x00
helpLink:   .dw   showName
            .dw   help
help:       LDI   ZH, HIGH(UserDictStart)     ; Show only user-visible dictionary
            LDI   ZL, LOW(UserDictStart)
help_1:     LSL   ZL
            ROL   ZH
            MOVW  r19:r18, ZH:ZL
            CALL  outStr
            CALL  _space
            MOVW  ZH:ZL, r19:r18
            CALL  fLFA
            LPM   XL, Z+           
            LPM   XH, Z+
            MOVW  ZH:ZL, XH:XL      ; Go to next dictionary entry
            ADIW  ZH:ZL, 0x00       ; is Link Field 0x0000?
            BRNE  help_1
            CALL  _CRLF
            JMP   next
            
            .dw   showLink-showName
showName:   .db   "show", 0x00, 0x00
showLink:   .dw   listName
            .dw   show
show:       CALL  _word
            LDI   ZH, HIGH(eomName<<1)
            LDI   ZL, LOW(eomName<<1)
            CALL  fMatch
            BRCS  show_3            ; quit if no argument given
            CALL  _find
            LDS   r17, WMode        ; only look at RAM definitions
            TST   r17
            BRNE  show_2
            LDI   r17, ':'
            CALL  outChar
            CALL  _space
            MOVW  r19:r18, ZH:ZL
            CALL  ramStr
            CALL  _space
            MOVW  ZH:ZL, r19:r18
show_1:     CALL  rAFA
            CALL  ramStr
            CALL  _CRLF
show_2:     JMP   next
show_3:     JMP   next_1

            .dw   listLink-listName
listName:   .db   "list", 0x00, 0x00
listLink:   .dw   0x0000
            .dw   list
list:       LDS   ZH, DP+1
            LDS   ZL, DP
            ADIW  ZH:ZL, 0x00       ; empty RAM dictionary?
            BREQ list_2
list_1:     MOVW  r19:r18, ZH:ZL
            CALL  ramStr
            CALL  _space
            MOVW  ZH:ZL, r19:r18
            CALL  rLFA
            LD    XL, Z+
            LD    XH, Z+
            MOVW  ZH:ZL, XH:XL      ; Go to next dictionary entry
            ADIW  ZH:ZL, 0x00       ; is Link Field 0x0000?
            BRNE  list_1
            CALL  _CRLF
list_2:     JMP   next


; ***** Subroutines

; LFA
; Take Name field address, return Link field address
fLFA:       CALL  fSkipName
            RET

rLFA:       CALL  rSkipName
            RET
            
; fCFA
; Take Name field address, return CodePtr field address
fCFA:       CALL  fSkipName
            ADIW  ZH:ZL, (1<<1)
            RET

rCFA:       CALL  rSkipName
            ADIW  ZH:ZL, 0x02
            RET
            
; fAFA
; Take Name field address, return Anything field address
fAFA:       CALL  fSkipName
            ADIW  ZH:ZL, (2<<1)
            RET

rAFA:       CALL  rSkipName
            ADIW  ZH:ZL, 0x04
            RET

; Source address in X
; Destination address in Z
; Stops when a 0x00 is encountered
ramCopy:    LD    r17, X+
            TST   r17
            BREQ  ramCopy_1
            ST    Z+, r17
            RJMP  ramCopy
ramCopy_1:  RET

; outByte
; output byte in r17 as two ASCII chars
outByte:    PUSH  r16
            MOV   r16, r17
            CALL  outHigh
            MOV   r17, r16
            CALL  outLow
            POP   r16
            RET

; Helper functions for outByte            
outHigh:    SWAP  r17
outLow:     ANDI  r17,0x0F
            CALL  h2a
            CALL  outChar
            RET
            
; h2a: hex to ascii
; convert r17 from 4 bit hex to 7-bit ascii
h2a:        SUBI  r17, 0xD0         ; Add 0x30
            CPI   r17, 0x3A
            BRLO  h2a_1
            SUBI  r17, 0xF9         ; Add another 0x07
h2a_1:      RET

; a2h: ascii to hex
; numbers 0-9 (0x30-0x39) A-F (0x41-0x46) to 0x00-0x0F
a2h:        SUBI  r17, 0x30
            CPI   r17, 0x0A
            BRLO  a2h_1
            SUBI  r17, 0x07
a2h_1:      RET
             
; outStr
; Print a string from FLASH
; Z contains string starting address
outStr:     LPM   r17, Z+
            TST   r17
            BREQ  outStr_1
            CALL  outChar
            RJMP  outStr
outStr_1:   RET

; ramStr
; Print a string from RAM
ramStr:     LD   r17, Z+
            TST   r17
            BREQ  ramStr_1  
            CALL  outChar
            RJMP  ramStr
ramStr_1:   RET

; _space
; output a space
_space:     LDI   r17, ' '
            CALL  outChar
            RET
            
; _CRLF
; output 0x0D 0x0A
_CRLF:      LDI   r17, 0x0D
            CALL  outChar
            LDI   r17, 0x0A
            CALL  outChar
            RET
            
; getChar
; get a character in r17
getChar:    LDS   r17, UCSR0A
            SBRS  r17, 7     
            RJMP  getChar  
            LDS   r17, UDR0
            RET

; outChar
; send character in r17
outChar:    PUSH  r16
outChar_1:  LDS   r16, UCSR0A
            SBRS  r16, 5
            RJMP  outChar_1
            STS   UDR0,r17
            POP   r16
            RET

; mul16
; 16x16 multiply with 16-bit result
;  CH:CL  =  AH:AL  *  BH:BL
; r3:r2 = r7:r6 * r5:r4
; Straight from ATMEL Application Note 1631, AVR201.asm
; USES: r7, r6, r5, r4, r3, r2, r1, r0 (8 registers!!)
; Non-preserving, be careful
mul16:      MUL   r6, r4          ; AL*BL
            MOVW  r3:r2, r1:r0
            MUL   r7, r4          ; AH*BL
            ADD   r3, r0
            MUL   r5, r6          ; BH*AL
            ADD   r3, r0
            RET

; 16-by-8 divide.
; Caution; alien code!!
; (c) 2002 by http://www.avr-asm-tutorial.net
; r5:r4 = r1:r0 / r3
div16:      CLR   r2 
            CLR   r5  
            CLR   r4  
            INC   r4  
div_1:	    CLC      
            ROL   r0 
            ROL   r1
            ROL   r2
            BRCS  div_2 
            CP    r2, r3 
            BRCS  div_3  
div_2:	    SUB   r2, r3
            SEC      
            RJMP  div_4
div_3:	    CLC     
div_4:	    ROL   r4  
            ROL   r5
            BRCC  div_1
            RET

; isHex/isNum
; Check if character is 0-9, A-F
isHex:      CPI   r17, 0x47   ; >=47, no
            BRSH  v_no
            CPI   r17, 0x41   ; >= 41 and <47, yes
            BRSH  v_yes
isNum:      CPI   r17, 0x3A   ; >= 3A and <41, no
            BRSH  v_no
            CPI   r17, 0x30   ; >= 30 and <3A, yes
            BRSH  v_yes
v_no:       CLC
            RET
v_yes:      SEC
            RET
            
; skipName
; Put address of a Name Field in Z
; Returns with Z pointing to next field after NAME
fSkipName:  SBIW  ZH:ZL, (1<<1)     ; NameLength = NameField-1
            LPM   r17, Z            ; r17 = length in words
            LSL   r17               ; convert to bytes
            ADIW  ZH:ZL, (1<<1)     ; Skip over NameLength
            RJMP  skipName
rSkipName:  SBIW  ZH:ZL, 0x01
            LD    r17, Z
            ADIW  ZH:ZL, 0x01
skipName:   ADD   ZL, r17
            CLR   r17
            ADC   ZH, r17
            RET
            
; match
; try to match word in Instruction Register
; with a dictionary Name field (given in Z)
fMatch:     MOVW  XH:XL, IRH:IRL
fMatch_0:   LD    r16, X+
            LPM   r17, Z+
            CP    r16, r17
            BRNE  match_1           ; No match
            TST   r16
            BREQ  match_2           ; Match
            RJMP  fMatch_0          ; look at next character
rMatch:     MOVW  XH:XL, IRH:IRL
rmatch_0:   LD    r16, X+
            LD    r17, Z+
            CP    r16, r17
            BRNE  match_1           ; No match
            TST   r16
            BREQ  match_2           ; Match
            RJMP  rmatch_0          ; look at next character
match_1:    CLC
            RET
match_2:    SEC
            RET

