; U8/M - UTF-8 for Microcomputers
; Version 0.8 for Commander X16
; (c) 2019-2023 Rebecca G Bettencourt, Kreative Software
; MIT License

*=$0801
BASIC:      !BYTE   $0B,$08,$01,$00,$9E,$32,$30,$36,$31,$00,$00,$00
            JMP     DEMO

; Zero page location where the address of a bitmap is stored.
BITMAPAL    =       $7C
BITMAPAH    =       $7D
; Zero page location where the address of a string is stored.
STRINGAL    =       $7E
STRINGAH    =       $7F
; VERA address registers.
VERAAL      =       $9F20
VERAAM      =       $9F21
VERAAH      =       $9F22
; VERA data registers.
VERAD0      =       $9F23
VERAD1      =       $9F24
; VERA control register.
VERACTL     =       $9F25
; VERA display composer register.
VERAVID     =       $9F29
; VERA layer 0 registers.
VERAL0CFG   =       $9F2D
VERAL0TB    =       $9F2F
VERAL0HSH   =       $9F31
; VERA layer 1 registers.
VERAL1CFG   =       $9F34
VERAL1TB    =       $9F36
VERAL1HSH   =       $9F38
; Register to set high memory bank.
RAMBANK     =       $00
; KERNAL routines for I/O.
SETLFS      =       $FFBA
SETNAM      =       $FFBD
CHROUT      =       $FFD2
LOAD        =       $FFD5
GETIN       =       $FFE4

; Pointer to configuration variables.
            !WORD   VRAMBASE
; Jump table for exported functions.
            JMP     U8MINIT0
            JMP     U8MCONT0
            JMP     U8MEXIT0
            JMP     U8MINIT1
            JMP     U8MCONT1
            JMP     U8MEXIT1
            JMP     GOTOYA
            JMP     GOTOY
            JMP     GOTOXA
            JMP     GOTOX
            JMP     SETCOLOR
            JMP     CLEARSCR
            JMP     FILLSCR
            JMP     FILLSCRA
            JMP     PLOT
            JMP     CHECKY
            JMP     CHECKX
            JMP     DRAWBMP
            JMP     DRAWBMPZP
            JMP     BANKTOLIN
            JMP     LINTOBANK
            JMP     LOADFONT
            JMP     SETFONT
            JMP     GETMETRICS
            JMP     GETGLYPH
            JMP     DRAWGLYPH
            JMP     DRAWGLYPHA
            JMP     CMAPNAT
            JMP     CMAPUTF8
            JMP     CMAPINDEX
            JMP     GLYPHNAT
            JMP     GLYPHUTF8
            JMP     PRINTLN
            JMP     PRINTNAT
            JMP     PRINTUTF8
            JMP     WIDTHNAT
            JMP     WIDTHUTF8

; Location in VRAM where the bitmap data starts.
VRAMBASE:   !WORD   $0000
VRAMBANK:   !BYTE   $00
; Bytes per row; this version of U8/M assumes bytes per row is of the form
; (5 * 2^ROWADVEXP) to avoid having to do a 16-bit multiply on a 6502. :)
ROWADVEXP:  !BYTE   5
; Location in VRAM where the current row starts;
; this is set by a call to GOTOYA or GOTOY.
ROWBASE:    !WORD   $0000
ROWBANK:    !BYTE   $00
; Mask for which bits of the X coordinate determine which pixel within a byte.
; Values: 8bpp: $00, 4bpp: $01, 2bpp: $03, 1bpp: $07 (theoretical).
BITDXMASK:  !BYTE   $03
; How many bits of the X coordinate determine which pixel within a byte.
; Values: 8bpp: 0, 4bpp: 1, 2bpp: 2, 1bpp: 3 (theoretical).
BITDXEXP:   !BYTE   2
; Masks for each pixel in a byte from left to right.
; Values: 8bpp: $FF; 4bpp: $F0,$0F; 2bpp: $C0,$30,$0C,$03; 1bpp: $80,$40,...
BITDCMASK:  !BYTE   $C0,$30,$0C,$03
; Mask for the pixel at the current X coordinate.
CURCMASK:   !BYTE   $C0
; The current color shifted to the appropriate location within a byte.
CURCMSKD:   !BYTE   $40
; The current color shifted into all possible locations within a byte.
CURCOLOR:   !BYTE   $55
; The current X and Y position.
XCOORD:     !WORD   0
YCOORD:     !WORD   0
; The current window.
XMIN:       !WORD   0
XMAX:       !WORD   639
YMIN:       !WORD   0
YMAX:       !WORD   479
; The current font: the high memory bank ($00-$FF) and page address ($A0-$BF).
FONTBASEB:  !BYTE   $A0
FONTBANKB:  !BYTE   $00
; The current font: the bank and page of a linear address within high memory.
FONTBASEL:  !BYTE   $00
FONTBANKL:  !BYTE   $00
; Saved registers for layer 1.
SAVEL1CFG:  !BYTE   $60
SAVEL1TB:   !BYTE   $F8
SAVEL1HSH:  !BYTE   $00

U8MINIT0:   JSR     CLEARSCR
; Initialize layer 0, 4c bitmap.
            LDA     BITDXEXP
            EOR     #$07
            STA     VERAL0CFG
; Clear palette offset.
            STZ     VERAL0HSH
; TILE_BASE = (VRAMBASE >> 9) | 0x03
            LDA     VRAMBASE+2
            ROR
            LDA     VRAMBASE+1
            ROR
            ORA     #$03
            STA     VERAL0TB
; Enable layer 0.
U8MCONT0:   STZ     VERACTL
            LDA     VERAVID
            ORA     #$10
            STA     VERAVID
            RTS

; Disable layer 0.
U8MEXIT0:   STZ     VERACTL
            LDA     VERAVID
            AND     #$EF
            STA     VERAVID
            RTS

U8MINIT1:   JSR     CLEARSCR
; Initialize layer 1, 4c bitmap.
U8MCONT1:   LDA     VERAL1CFG
            STA     SAVEL1CFG
            LDA     BITDXEXP
            EOR     #$07
            STA     VERAL1CFG
; Clear palette offset.
            LDA     VERAL1HSH
            STA     SAVEL1HSH
            STZ     VERAL1HSH
; TILE_BASE = (VRAMBASE >> 9) | 0x03
            LDA     VERAL1TB
            STA     SAVEL1TB
            LDA     VRAMBASE+2
            ROR
            LDA     VRAMBASE+1
            ROR
            ORA     #$03
            STA     VERAL1TB
            RTS

; Initialize layer 1, text mode.
U8MEXIT1:   LDA     SAVEL1CFG
            STA     VERAL1CFG
            LDA     SAVEL1TB
            STA     VERAL1TB
            LDA     SAVEL1HSH
            STA     VERAL1HSH
            RTS

; Set Y coordinate to ((A << 8) | Y).
GOTOYA:     STY     YCOORD+0
            STA     YCOORD+1
; Set ROWBASE according to the current YCOORD.
; Copy YCOORD into ROWBASE. (ROWBASE = YCOORD)
GOTOY:      LDA     YCOORD+0
            STA     ROWBASE+0
            LDA     YCOORD+1
            STA     ROWBASE+1
            LDA     #$00
            STA     ROWBASE+2
; Shift ROWBASE to the left by 2. (ROWBASE = YCOORD * 4)
            ASL     ROWBASE+0
            ROL     ROWBASE+1
            ROL     ROWBASE+2
            ASL     ROWBASE+0
            ROL     ROWBASE+1
            ROL     ROWBASE+2
; Add YCOORD to ROWBASE. (ROWBASE = YCOORD * 5)
            CLC
            LDA     YCOORD+0
            ADC     ROWBASE+0
            STA     ROWBASE+0
            LDA     YCOORD+1
            ADC     ROWBASE+1
            STA     ROWBASE+1
            LDA     #$00
            ADC     ROWBASE+2
            STA     ROWBASE+2
; Shift ROWBASE to the left by ROWADVEXP. (ROWBASE = YCOORD * 5 * 2^ROWADVEXP)
            LDY     ROWADVEXP
ROLBASE:    ASL     ROWBASE+0
            ROL     ROWBASE+1
            ROL     ROWBASE+2
            DEY
            BNE     ROLBASE
; Add VRAMBASE to ROWBASE.
            CLC
            LDA     VRAMBASE+0
            ADC     ROWBASE+0
            STA     ROWBASE+0
            LDA     VRAMBASE+1
            ADC     ROWBASE+1
            STA     ROWBASE+1
            LDA     VRAMBASE+2
            ADC     ROWBASE+2
            STA     ROWBASE+2
; Reset address for current X coordinate.
            JMP     GOTOX

; Set X coordinate to ((A << 8) | X).
GOTOXA:     STX     XCOORD+0
            STA     XCOORD+1
; Set CURCMASK, CURCMSKD, VERA address according to the current XCOORD.
; Get the lower bits of the XCOORD: the position of the pixel within the byte.
GOTOX:      LDA     XCOORD
            AND     BITDXMASK
; Set the corresponding color mask.
            TAX
            LDA     BITDCMASK,X
            STA     CURCMASK
; Set the current masked color.
            AND     CURCOLOR
            STA     CURCMSKD
; Copy XCOORD into VERA address.
            LDA     XCOORD+0
            STA     VERAAL
            LDA     XCOORD+1
            STA     VERAAM
            LDA     #$00
            STA     VERAAH
; Shift VERA address to the right by BITDXEXP.
            LDX     BITDXEXP
            BEQ     GOTOX2
GOTOX1:     LSR     VERAAH
            ROR     VERAAM
            ROR     VERAAL
            DEX
            BNE     GOTOX1
; Add ROWBASE to VERA address.
GOTOX2:     CLC
            LDA     ROWBASE+0
            ADC     VERAAL
            STA     VERAAL
            LDA     ROWBASE+1
            ADC     VERAAM
            STA     VERAAM
            LDA     ROWBASE+2
            ADC     VERAAH
            STA     VERAAH
            RTS

; Set current color to A.
SETCOLOR:   STA     CURCOLOR
; If bit depth is 8bpp, we're done.
            LDY     BITDXEXP
            BEQ     SETCOLOR1
; Copy bits 0-3 into bits 4-7.
            ASL
            ASL
            ASL
            ASL
            ORA     CURCOLOR
            STA     CURCOLOR
; If bit depth is 4bpp, we're done.
            DEY
            BEQ     SETCOLOR1
; Copy bits 0-1, 4-5 into bits 2-3, 6-7.
            ASL
            ASL
            ORA     CURCOLOR
            STA     CURCOLOR
; If bit depth is 2bpp, we're done.
            DEY
            BEQ     SETCOLOR1
; Copy bits 0, 2, 4, 6 into bits 1, 3, 5, 7.
            ASL
            ORA     CURCOLOR
            STA     CURCOLOR
SETCOLOR1:  AND     CURCMASK
            STA     CURCMSKD
            RTS

; Clear framebuffer to color 0.
CLEARSCR:   LDA     #$00
            BRA     FILLSCRA
; Clear framebuffer to current color.
FILLSCR:    LDA     CURCOLOR
FILLSCRA:   PHA
            LDA     VRAMBASE+0
            STA     VERAAL
            LDA     VRAMBASE+1
            STA     VERAAM
            LDA     VRAMBASE+2
            ORA     #$10
            STA     VERAAH
            PLA
            LDY     #120
FILLSCR1:   LDX     #160
FILLSCR2:   STA     VERAD0
            STA     VERAD0
            STA     VERAD0
            STA     VERAD0
            DEX
            BNE     FILLSCR2
            DEY
            BNE     FILLSCR1
            RTS

; Plot a pixel.
; Get the current data minus the pixel being plotted.
PLOT:       LDA     CURCMASK
            EOR     #$FF
            AND     VERAD0
; Add in the current masked color.
            ORA     CURCMSKD
            STA     VERAD0
            RTS

; Check that YCOORD is between YMIN and YMAX (inclusive).
; Check against YMIN. First check the high byte.
CHECKY:     LDA     YCOORD+1
            CMP     YMIN+1
; If YCOORD (high) < YMIN (high), fail.
            BCC     CHECKYNG
; If YCOORD (high) > YMIN (high), pass the min check.
            BNE     CHECKY1
; Otherwise YCOORD (high) == YMIN (high), so check the low byte.
            LDA     YCOORD+0
            CMP     YMIN+0
; If YCOORD (low) < YMIN (low), fail.
            BCC     CHECKYNG
; Otherwise YCOORD (low) >= YMIN (low), so pass the min check.
; Check against YMAX. First check the high byte.
CHECKY1:    LDA     YMAX+1
            CMP     YCOORD+1
; If YMAX (high) < YCOORD (high), fail.
            BCC     CHECKYNG
; If YMAX (high) > YCOORD (high), pass the max check.
            BNE     CHECKYOK
; Otherwise YMAX (high) == YCOORD (high), so check the low byte.
            LDA     YMAX+0
            CMP     YCOORD+0
; If YMAX (low) < YCOORD (low), fail.
            BCC     CHECKYNG
; Otherwise YMAX (low) >= YCOORD (low), so pass the max check.
CHECKYOK:   CLC
            RTS
CHECKYNG:   SEC
            RTS

; Check that XCOORD is between XMIN and XMAX (inclusive).
; Check against XMIN. First check the high byte.
CHECKX:     LDA     XCOORD+1
            CMP     XMIN+1
; If XCOORD (high) < XMIN (high), fail.
            BCC     CHECKXNG
; If XCOORD (high) > XMIN (high), pass the min check.
            BNE     CHECKX1
; Otherwise XCOORD (high) == XMIN (high), so check the low byte.
            LDA     XCOORD+0
            CMP     XMIN+0
; If XCOORD (low) < XMIN (low), fail.
            BCC     CHECKXNG
; Otherwise XCOORD (low) >= XMIN (low), so pass the min check.
; Check against XMAX. First check the high byte.
CHECKX1:    LDA     XMAX+1
            CMP     XCOORD+1
; If XMAX (high) < XCOORD (high), fail.
            BCC     CHECKXNG
; If XMAX (high) > XCOORD (high), pass the max check.
            BNE     CHECKXOK
; Otherwise XMAX (high) == XCOORD (high), so check the low byte.
            LDA     XMAX+0
            CMP     XCOORD+0
; If XMAX (low) < XCOORD (low), fail.
            BCC     CHECKXNG
; Otherwise XMAX (low) >= XCOORD (low), so pass the max check.
CHECKXOK:   CLC
            RTS
CHECKXNG:   SEC
            RTS

; Draw a bitmap with address in A (high) and X (low).
DRAWBMP:    STX     BITMAPAL
            STA     BITMAPAH
; Save the current X and Y coordinates.
DRAWBMPZP:  LDA     XCOORD+0
            STA     DRAWBMPSVX+0
            LDA     XCOORD+1
            STA     DRAWBMPSVX+1
            LDA     YCOORD+0
            STA     DRAWBMPSVY+0
            LDA     YCOORD+1
            STA     DRAWBMPSVY+1
; Add bitmap Y offset to YCOORD.
            CLC
            LDY     #$00
            LDA     (BITMAPAL),Y
            ADC     YCOORD+0
            STA     YCOORD+0
; The bitmap Y offset is 8 bits signed, so extend the sign to 16 bits.
            LDA     (BITMAPAL),Y
            AND     #$80
            BEQ     DRAWBMP1
            LDA     #$FF
; Add the extended sign to YCOORD.
DRAWBMP1:   ADC     YCOORD+1
            STA     YCOORD+1
; Add bitmap X offset to XCOORD.
            CLC
            INY
            LDA     (BITMAPAL),Y
            ADC     XCOORD+0
            STA     XCOORD+0
            STA     DRAWBMPX+0
; The bitmap X offset is 8 bits signed, so extend the sign to 16 bits.
            LDA     (BITMAPAL),Y
            AND     #$80
            BEQ     DRAWBMP2
            LDA     #$FF
; Add the extended sign to XCOORD.
DRAWBMP2:   ADC     XCOORD+1
            STA     XCOORD+1
            STA     DRAWBMPX+1
; Read the bitmap height. If it is zero, skip drawing.
            INY
            LDA     (BITMAPAL),Y
            BEQ     DRAWBMP3
            STA     DRAWBMPH
; Read the bitmap width. If it is zero, skip drawing.
            INY
            LDA     (BITMAPAL),Y
            BEQ     DRAWBMP3
            STA     DRAWBMPW1
            STA     DRAWBMPW2
; Initialize the bitmap data pointer and mask.
            STY     DRAWBMPPTR
            LDA     #$00
            STA     DRAWBMPMSK
; Start of outer loop over the rows of the bitmap.
; Check and update the Y coordinate.
DRAWBMPY0:  JSR     CHECKY
            BCS     DRAWBMPX0
            JSR     GOTOY
            CLC
; Start of inner loop over the pixels in this row.
; Get the current bit mask.
DRAWBMPX0:  LDA     DRAWBMPMSK
            BNE     DRAWBMPX1
; If we are out of bits, get the next byte.
            INC     DRAWBMPPTR
            LDY     DRAWBMPPTR
            LDA     (BITMAPAL),Y
            STA     DRAWBMPDAT
            LDA     #$80
            STA     DRAWBMPMSK
; Save the carry bit (result of CHECKY).
DRAWBMPX1:  PHP
; If the Y coordinate is out of bounds, don't draw anything.
            BCS     DRAWBMPX2
; If the current bit is cleared, don't draw anything.
            AND     DRAWBMPDAT
            BEQ     DRAWBMPX2
; If the X coordinate is out of bounds, don't draw anything.
            JSR     CHECKX
            BCS     DRAWBMPX2
; Otherwise plot the pixel.
            JSR     GOTOX
            JSR     PLOT
; Move to the next pixel.
DRAWBMPX2:  LSR     DRAWBMPMSK
; Restore the carry bit (result of CHECKY).
            PLP
; If we reached the end of the row, exit the inner loop.
            DEC     DRAWBMPW1
            BEQ     DRAWBMPY2
; Otherwise increment the X coordinate and continue the inner loop.
            INC     XCOORD+0
            BNE     DRAWBMPX0
            INC     XCOORD+1
            JMP     DRAWBMPX0
; If we reached the end of the bitmap, exit the outer loop.
DRAWBMPY2:  DEC     DRAWBMPH
            BEQ     DRAWBMP3
; Restore the starting X coordinate.
            LDA     DRAWBMPX+0
            STA     XCOORD+0
            LDA     DRAWBMPX+1
            STA     XCOORD+1
; Restore the starting bitmap width.
            LDA     DRAWBMPW2
            STA     DRAWBMPW1
; Increment the Y coordinate and continue the outer loop.
            INC     YCOORD+0
            BNE     DRAWBMPY0
            INC     YCOORD+1
            JMP     DRAWBMPY0
; Restore the current X and Y coordinates.
DRAWBMP3:   LDA     DRAWBMPSVX+0
            STA     XCOORD+0
            LDA     DRAWBMPSVX+1
            STA     XCOORD+1
            LDA     DRAWBMPSVY+0
            STA     YCOORD+0
            LDA     DRAWBMPSVY+1
            STA     YCOORD+1
            JMP     GOTOY
; Saved X and Y coordinates.
DRAWBMPSVX: !WORD   0
DRAWBMPSVY: !WORD   0
; Dimensions of the bitmap, used as loop counters.
DRAWBMPX:   !WORD   0
DRAWBMPH:   !BYTE   0
DRAWBMPW1:  !BYTE   0
DRAWBMPW2:  !BYTE   0
; Bitmap data.
DRAWBMPPTR: !BYTE   0
DRAWBMPDAT: !BYTE   0
DRAWBMPMSK: !BYTE   0

; Translate high memory address from bank (in A), AX/BX, YY to linear address.
BANKTOLIN:  STA     XLATEBANK
            TXA
            ASL
            ASL
            ASL
            STA     XLATEHIGH
            LSR     XLATEBANK
            ROR     XLATEHIGH
            LSR     XLATEBANK
            ROR     XLATEHIGH
            LSR     XLATEBANK
            ROR     XLATEHIGH
            LDX     XLATEHIGH
            LDA     XLATEBANK
            RTS
; Translate high memory address from linear address to bank (in A), AX/BX, YY.
LINTOBANK:  STA     XLATEBANK
            STX     XLATEHIGH
            ASL     XLATEHIGH
            ROL     XLATEBANK
            ASL     XLATEHIGH
            ROL     XLATEBANK
            ASL     XLATEHIGH
            ROL     XLATEBANK
            LDA     XLATEHIGH
            LSR
            LSR
            LSR
            ORA     #$A0
            TAX
            LDA     XLATEBANK
            RTS
; Used during address translation.
XLATEHIGH:  !BYTE   0
XLATEBANK:  !BYTE   0

; Load a font into high memory bank A at (X << 8).
; Must be preceded by calls to SETLFS and SETNAM.
LOADFONT:   JSR     SETFONT
            LDA     FONTBANKB
            STA     RAMBANK
            LDA     #$00 ; 0 for load, 1 for verify
            LDX     #$00 ; load address, low
            LDY     FONTBASEB
            JMP     LOAD

; Set the current font to the font in high memory bank A at (X << 8).
; (Fonts must start at a page boundary, so the LSB is always zero.)
SETFONT:    STX     FONTBASEB
            STA     FONTBANKB
            JSR     BANKTOLIN
            STX     FONTBASEL
            STA     FONTBANKL
            RTS

; Returns ascent in A, descent in X, and height in Y.
; Point BITMAPAL to the font header.
GETMETRICS: LDA     FONTBANKB
            STA     RAMBANK
            LDA     FONTBASEB
            STA     BITMAPAH
            LDA     #$00
            STA     BITMAPAL
; Get the metrics from the font header.
; Get ascent and push onto the stack.
            LDY     #$FC
            LDA     (BITMAPAL),Y
            PHA
; Get descent and push onto the stack.
            INY
            LDA     (BITMAPAL),Y
            PHA
; Get height and transfer to Y.
            INY
            INY
            LDA     (BITMAPAL),Y
            TAY
; Pull descent into X and ascent into A.
            PLX
            PLA
            RTS

; Look up glyph ((A << 8) | X) in the current font.
; Returns with the advance width in A and, if the carry is clear, the bitmap
; address in BITMAPAL/AH. If the carry is set, the glyph has no bitmap.
; Set GLYPHADDR to the glyph index.
GETGLYPH:   STX     GLYPHADDR+0
            STA     GLYPHADDR+1
            LDA     #$00
            STA     GLYPHADDR+2
            STA     BITMAPAL
; Shift left twice to get the offset to the glyph record.
            ASL     GLYPHADDR+0
            ROL     GLYPHADDR+1
            ROL     GLYPHADDR+2
            ASL     GLYPHADDR+0
            ROL     GLYPHADDR+1
            ROL     GLYPHADDR+2
; Point BITMAPAL to the font header.
            LDA     FONTBANKB
            STA     RAMBANK
            LDA     FONTBASEB
            STA     BITMAPAH
; Add the offset to the glyph table to GLYPHADDR.
            CLC
            LDY     #$80
            LDA     (BITMAPAL),Y
            ADC     GLYPHADDR+1
            STA     GLYPHADDR+1
            INY
            LDA     (BITMAPAL),Y
            ADC     GLYPHADDR+2
            STA     GLYPHADDR+2
; Add the address of the font to GLYPHADDR.
            CLC
            LDA     FONTBASEL
            ADC     GLYPHADDR+1
            STA     GLYPHADDR+1
            LDA     FONTBANKL
            ADC     GLYPHADDR+2
            STA     GLYPHADDR+2
; Convert the GLYPHADDR from a linear address to a banked address.
            LDX     GLYPHADDR+1
            LDY     GLYPHADDR+0
            JSR     LINTOBANK
; Get the bitmap address.
            STA     RAMBANK
            STX     BITMAPAH
            LDA     (BITMAPAL),Y
            STA     GLYPHADDR+0
            STA     GLYPHADOK
            INY
            LDA     (BITMAPAL),Y
            STA     GLYPHADDR+1
            ORA     GLYPHADOK
            STA     GLYPHADOK
            INY
            LDA     (BITMAPAL),Y
            STA     GLYPHADDR+2
            ORA     GLYPHADOK
            STA     GLYPHADOK
; Get the glyph advance width.
            INY
            LDA     (BITMAPAL),Y
; If the bitmap address is zero, just return the advance width.
            LDY     GLYPHADOK
            BNE     GETGLYPH1
            SEC
            RTS
; Save the advance width.
GETGLYPH1:  PHA
; Add the address of the font to the bitmap address.
            CLC
            LDA     FONTBASEL
            ADC     GLYPHADDR+1
            STA     GLYPHADDR+1
            LDA     FONTBANKL
            ADC     GLYPHADDR+2
            STA     GLYPHADDR+2
; Convert the bitmap address from a linear address to a banked address.
            LDX     GLYPHADDR+1
            LDY     GLYPHADDR+0
            JSR     LINTOBANK
; Set the bitmap address.
            STA     RAMBANK
            STX     BITMAPAH
            STY     BITMAPAL
; Return the advance width.
            PLA
            CLC
            RTS
; Glyph/bitmap address.
GLYPHADDR:  !WORD   0
GLYPHBANK:  !BYTE   0
GLYPHADOK:  !BYTE   0

; Draw glyph ((A << 8) | X) in the current font.
; Advance width will be returned in A but will not change XCOORD.
; If the advance width extends past XMAX, the glyph may be clipped.
DRAWGLYPH:  JSR     GETGLYPH
            BCS     DRAWGLYPH1
            PHA
            JSR     DRAWBMPZP
            PLA
            CLC
DRAWGLYPH1: RTS

; Draw glyph ((A << 8) | X) in the current font with advance.
; If the advance pushes XCOORD past XMAX, the carry will be set
; and the glyph will not be drawn.
DRAWGLYPHA: JSR     GETGLYPH
            STA     DRAWGLADV
; Save the carry bit, which is set if the glyph has no bitmap.
            PHP
; Add the advance to the X coordinate.
            CLC
            LDA     XCOORD+0
            STA     DRAWGLOLDX+0
            ADC     DRAWGLADV
            STA     XCOORD+0
            STA     DRAWGLNEWX+0
            LDA     XCOORD+1
            STA     DRAWGLOLDX+1
            ADC     #$00
            STA     XCOORD+1
            STA     DRAWGLNEWX+1
; If it's out of bounds, skip drawing the glyph.
            JSR     CHECKX
            BCC     DRAWGLYPH2
            PLP
            SEC
            RTS
; If the glyph has no bitmap, skip drawing the glyph.
DRAWGLYPH2: PLP
            BCC     DRAWGLYPH3
            CLC
            RTS
; Restore the original X coordinate.
DRAWGLYPH3: LDA     DRAWGLOLDX+0
            STA     XCOORD+0
            LDA     DRAWGLOLDX+1
            STA     XCOORD+1
; Draw the bitmap.
            JSR     DRAWBMPZP
; Restore the new X coordinate.
            LDA     DRAWGLNEWX+0
            STA     XCOORD+0
            LDA     DRAWGLNEWX+1
            STA     XCOORD+1
            JSR     GOTOX
            CLC
            RTS
; The glyph advance width and saved X coordinates.
DRAWGLADV:  !BYTE   0
DRAWGLOLDX: !WORD   0
DRAWGLNEWX: !WORD   0

; Get the character map index for a native character in A.
; Transform into the offset into the font header.
CMAPNAT:    ROL
            ROL
            ROL
            ROL
            AND     #$06
            ORA     #$88
            JMP     CMAPTAY
; Get the character map index for the first byte of a UTF-8 sequence in A.
; For $00-$BF, the two most significant bits determine which map.
CMAPUTF8:   CMP     #$C0
            BCS     CMAPUTF8H
            ROL
            ROL
            ROL
            ROL
            AND     #$06
            ORA     #$90
            JMP     CMAPTAY
; For $C0-$FF, the six least significant bits determine which map.
CMAPUTF8H:  AND     #$3F
            ASL
            ADC     #$90
CMAPTAY:    TAY
; Point BITMAPAL to the font header.
            LDA     FONTBANKB
            STA     RAMBANK
            LDA     FONTBASEB
            STA     BITMAPAH
            LDA     #$00
            STA     BITMAPAL
; Return the character map index in ((A << 8) | X).
            LDA     (BITMAPAL),Y
            TAX
            INY
            LDA     (BITMAPAL),Y
            RTS

; Look up index Y in character map ((A << 8) | X) in the current font.
CMAPINDEX:  STY     CMAPIDX
            STX     CMAPADDR+0
            STA     CMAPADDR+1
            LDA     #$00
            STA     CMAPADDR+2
            STA     BITMAPAL
; Shift left twice to get the offset to the map header.
            ASL     CMAPADDR+0
            ROL     CMAPADDR+1
            ROL     CMAPADDR+2
            ASL     CMAPADDR+0
            ROL     CMAPADDR+1
            ROL     CMAPADDR+2
; Point BITMAPAL to the font header.
            LDA     FONTBANKB
            STA     RAMBANK
            LDA     FONTBASEB
            STA     BITMAPAH
; Add the offset to the map table to CMAPADDR.
            CLC
            LDY     #$84
            LDA     (BITMAPAL),Y
            ADC     CMAPADDR+1
            STA     CMAPADDR+1
            INY
            LDA     (BITMAPAL),Y
            ADC     CMAPADDR+2
            STA     CMAPADDR+2
; Add the address of the font to CMAPADDR.
            CLC
            LDA     FONTBASEL
            ADC     CMAPADDR+1
            STA     CMAPADDR+1
            LDA     FONTBANKL
            ADC     CMAPADDR+2
            STA     CMAPADDR+2
; Convert the CMAPADDR from a linear address to a banked address.
            LDX     CMAPADDR+1
            LDY     CMAPADDR+0
            JSR     LINTOBANK
; Get the map data address.
            STA     RAMBANK
            STX     BITMAPAH
            LDA     (BITMAPAL),Y
            STA     CMAPADDR+0
            INY
            LDA     (BITMAPAL),Y
            STA     CMAPADDR+1
            INY
            LDA     (BITMAPAL),Y
            STA     CMAPADDR+2
; Get the number of entries.
            INY
            LDA     (BITMAPAL),Y
            PHA
; Add the address of the font to the map data address.
            CLC
            LDA     FONTBASEL
            ADC     CMAPADDR+1
            STA     CMAPADDR+1
            LDA     FONTBANKL
            ADC     CMAPADDR+2
            STA     CMAPADDR+2
; Convert the map data address from a linear address to a banked address.
            LDX     CMAPADDR+1
            LDY     CMAPADDR+0
            JSR     LINTOBANK
; Set the map data address.
            STA     RAMBANK
            STX     BITMAPAH
            STY     BITMAPAL
; Search the map data for CMAPIDX.
; If there are no entries, we have no mapping.
            PLX
            BEQ     CMAPNG
; Compare against lower bound. If CMAPIDX < lower bound, we have no mapping.
            LDY     #$00
CMAPINDEX1: LDA     CMAPIDX
            CMP     (BITMAPAL),Y
            BCC     CMAPNG
; Compare against upper bound. If upper bound >= CMAPIDX, we have a mapping.
            INY
            LDA     (BITMAPAL),Y
            CMP     CMAPIDX
            BCS     CMAPOK
; If there are no more entries, we have no mapping.
            DEX
            BEQ     CMAPNG
; Otherwise get the next entry.
            INY
            INY
            INY
            JMP     CMAPINDEX1
; Entry found.
CMAPOK:     LDA     CMAPIDX
; Subtract the lower bound.
            DEY
            SEC
            SBC     (BITMAPAL),Y
; Add the low byte of the index, and return it in X.
            INY
            INY
            CLC
            ADC     (BITMAPAL),Y
            TAX
; Add the carry to the high byte of the index, and return it in A.
            INY
            LDA     #$00
            ADC     (BITMAPAL),Y
            CLC
            RTS
; No entry was found. Return 0 with carry set.
CMAPNG:     LDA     #$00
            TAX
            SEC
            RTS
CMAPADDR:   !WORD   0
CMAPBANK:   !BYTE   0
CMAPIDX:    !BYTE   0

; Get the glyph index for a native character in A.
; If there is a glyph for that character, carry will be clear
; and the glyph index will be returned in ((A << 8) | X).
; If there is not a glyph for that character, carry will be set
; and a glyph index of zero will be returned.
GLYPHNAT:   TAY
            AND     #$3F
            PHA
            TYA
            JSR     CMAPNAT
            PLY
            JMP     CMAPINDEX

; Process a UTF-8 byte in A.
; If A is the end of a valid UTF-8 sequence, Y will be 0. Also,
; if there is a glyph for that character, carry will be clear
; and the glyph index will be returned in ((A << 8) | X).
; If there is not a glyph for that character, carry will be set
; and a glyph index of zero will be returned.
; If A is the start or middle of a valid UTF-8 sequence, and
; more bytes are required, Y will be 1 and carry will be set.
; If A results in an invalid UTF-8 sequence, Y will be 2,
; carry will be set, and the decode state will be reset.
; To reset the decode state at any time, simply pass in
; an invalid UTF-8 byte such as $FF and ignore the results.
; If A < $80, this should be a single-byte sequence.
GLYPHUTF8:  CMP     #$80
            BCS     GLUTF8HI
; If we are currently inside a sequence, this is an encoding error.
            LDY     UTF8STATE
            BNE     UTF8NG
; Save the lower six bits of A for the map entry index.
            TAY
            AND     #$3F
            PHA
; Get the character map index.
            TYA
            JSR     CMAPUTF8
; Get the glyph index.
            PLY
            JSR     CMAPINDEX
; Return the glyph index in A and X, Y = 0, and carry according to CMAPINDEX.
UTF8OK:     LDY     #$00
            RTS
; If A >= $80 and A < $C0, this should be the middle or end of a sequence.
GLUTF8HI:   CMP     #$C0
            BCS     GLUTF8HDR
; If we are not inside a sequence, this is an encoding error.
            LDY     UTF8STATE
            BEQ     UTF8NG
; Get the glyph index or next character map index.
            AND     #$3F
            TAY
            LDX     UTF8CMAP+0
            LDA     UTF8CMAP+1
            JSR     CMAPINDEX
; If this was the final byte, return the glyph index, Y = 0, and carry.
            DEC     UTF8STATE
            BEQ     UTF8OK
; If this was a medial byte, store the character map index.
            STA     UTF8CMAP+1
            STX     UTF8CMAP+0
; Return Y = 1 and carry set.
            LDY     #$01
            SEC
            RTS
; If A >= $C0 and A < $F6, this should be the start of a sequence.
GLUTF8HDR:  CMP     #$F6
            BCS     UTF8NG
; If we are already inside a sequence, this is an encoding error.
            LDY     UTF8STATE
            BNE     UTF8NG
; Save A for later.
            PHA
; Get and store the initial character map index.
            JSR     CMAPUTF8
            STA     UTF8CMAP+1
            STX     UTF8CMAP+0
; Restore A and pop off the first MSB.
            PLA
            ASL
; Count the number of remaining MSBs.
; This will be the number of remaining bytes in the sequence.
GLUTF8INC:  INC     UTF8STATE
            ASL
            BMI     GLUTF8INC
; Return Y = 1 and carry set.
            LDY     #$01
            SEC
            RTS
; If an encoding error occurs, reset the encoding state,
; then return Y = 2 and carry set.
UTF8NG:     LDY     #$00
            STY     UTF8STATE
            LDY     #$02
            SEC
            RTS
; The number of bytes remaining in a sequence.
UTF8STATE:  !BYTE   0
; The current character map.
UTF8CMAP:   !WORD   0

; Set the X coordinate to XMIN.
PRINTLN:    LDA     XMIN+0
            STA     XCOORD+0
            LDA     XMIN+1
            STA     XCOORD+1
; Add the font height to YCOORD.
            JSR     GETMETRICS
            CLC
            TYA
            ADC     YCOORD+0
            STA     YCOORD+0
            LDA     #$00
            ADC     YCOORD+1
            STA     YCOORD+1
; Check if the Y coordinate is valid.
            JSR     CHECKY
            BCS     PRINTLN1
            JSR     GOTOY
            CLC
PRINTLN1:   RTS

; Prints a string at ((A << 8) | X) in the native character set.
; Set the string address.
PRINTNAT:   STA     STRINGAH
            STX     STRINGAL
; Get a character.
PRINTNAT1:  LDY     #$00
            LDA     (STRINGAL),Y
; If it is null, return.
            BEQ     PRINTNAT4
; If it is CR or LF, call PRINTLN and get the next character.
            CMP     #$0A
            BEQ     PRINTNAT2
            CMP     #$0D
            BEQ     PRINTNAT2
; Get the glyph index and try to draw the glyph.
            JSR     GLYPHNAT
            JSR     DRAWGLYPHA
; If the glyph was drawn, get the next character.
            BCC     PRINTNAT3
; Otherwise call PRINTLN and try drawing the glyph again.
            JSR     PRINTLN
            JMP     PRINTNAT1
; Call PRINTLN.
PRINTNAT2:  JSR     PRINTLN
; Get the next character.
PRINTNAT3:  INC     STRINGAL
            BNE     PRINTNAT1
            INC     STRINGAH
            JMP     PRINTNAT1
; Return.
PRINTNAT4:  RTS

; Prints a string at ((A << 8) | X) in UTF-8.
; Set the byte counter (STRINGAH/AL) and codepoint counter (PRINTUTFPH/PL).
PRINTUTF8:  STA     STRINGAH
            STX     STRINGAL
            STA     PRINTUTFPH
            STX     PRINTUTFPL
; Get a BYTE.
PRINTUTF1:  LDY     #$00
            LDA     (STRINGAL),Y
; If it is null, return.
            BEQ     PRINTUTF6
; If it is CR or LF, call PRINTLN and get the next character.
            CMP     #$0A
            BEQ     PRINTUTF3
            CMP     #$0D
            BEQ     PRINTUTF3
; Try to get the glyph index.
            JSR     GLYPHUTF8
; If Y is nonzero, we don't have a complete sequence yet.
; Advance the byte counter but not the codepoint counter.
            CPY     #$00
            BEQ     PRINTUTF2
            INC     STRINGAL
            BNE     PRINTUTF1
            INC     STRINGAH
            JMP     PRINTUTF1
; We have a complete sequence so try to draw the glyph.
PRINTUTF2:  JSR     DRAWGLYPHA
; If the glyph was drawn, get the next character.
            BCC     PRINTUTF4
; Otherwise call PRINTLN, rewind to the start of the codepoint, and try again.
            JSR     PRINTLN
            LDA     PRINTUTFPL
            STA     STRINGAL
            LDA     PRINTUTFPH
            STA     STRINGAH
            JMP     PRINTUTF1
; Call PRINTLN.
PRINTUTF3:  JSR     PRINTLN
; Get the next character.
PRINTUTF4:  INC     STRINGAL
            LDA     STRINGAL
            STA     PRINTUTFPL
            BNE     PRINTUTF5
            INC     STRINGAH
PRINTUTF5:  LDA     STRINGAH
            STA     PRINTUTFPH
            JMP     PRINTUTF1
PRINTUTF6:  RTS
; Pointer to start of codepoint.
PRINTUTFPL: !BYTE   0
PRINTUTFPH: !BYTE   0

; Calculates the total visual width of a string at ((A << 8) | X) in
; the native character set. The total is returned in ((A << 8) | X).
; Set the string address.
WIDTHNAT:   STA     STRINGAH
            STX     STRINGAL
            LDA     #$00
            STA     WIDTHTOTAL+0
            STA     WIDTHTOTAL+1
; Get a character.
WIDTHNAT1:  LDY     #$00
            LDA     (STRINGAL),Y
; If it is null, return.
            BEQ     WIDTHRTS
; Get the glyph advance width.
            JSR     GLYPHNAT
            JSR     GETGLYPH
; Add the advance width to the running total.
            CLC
            ADC     WIDTHTOTAL+0
            STA     WIDTHTOTAL+0
            LDA     #$00
            ADC     WIDTHTOTAL+1
            STA     WIDTHTOTAL+1
; Get the next character.
            INC     STRINGAL
            BNE     WIDTHNAT1
            INC     STRINGAH
            JMP     WIDTHNAT1

; Calculates the total visual width of a string at ((A << 8) | X)
; in UTF-8. The total is returned in ((A << 8) | X).
; Set the string address.
WIDTHUTF8:  STA     STRINGAH
            STX     STRINGAL
            LDA     #$00
            STA     WIDTHTOTAL+0
            STA     WIDTHTOTAL+1
; Get a character.
WIDTHUTF1:  LDY     #$00
            LDA     (STRINGAL),Y
; If it is null, return.
            BEQ     WIDTHRTS
; Try to get the glyph index.
            JSR     GLYPHUTF8
; If Y is nonzero, we don't have a complete sequence yet.
; Skip to the next character.
            CPY     #$00
            BNE     WIDTHUTF2
; Get the advance width.
            JSR     GETGLYPH
; Add the advance width to the running total.
            CLC
            ADC     WIDTHTOTAL+0
            STA     WIDTHTOTAL+0
            LDA     #$00
            ADC     WIDTHTOTAL+1
            STA     WIDTHTOTAL+1
; Get the next character.
WIDTHUTF2:  INC     STRINGAL
            BNE     WIDTHUTF1
            INC     STRINGAH
            JMP     WIDTHUTF1

; Return the total width.
WIDTHRTS:   LDX     WIDTHTOTAL+0
            LDA     WIDTHTOTAL+1
            RTS
; The running total width.
WIDTHTOTAL: !WORD   0



; Demo program.
; Clear text screen to transparent and initialize.
DEMO:       LDA     #$01
            JSR     CHROUT
            LDA     #$90
            JSR     CHROUT
            LDA     #$01
            JSR     CHROUT
            LDA     #$93
            JSR     CHROUT
            JSR     U8MINIT1
; Load the fonts.
            LDA     #$07 ; logical file number
            LDX     #$08 ; device number
            LDY     #$01 ; secondary address
            JSR     SETLFS
; Load font #1.
            LDA     #FONTNAME2-FONTNAME1
            LDX     #<FONTNAME1
            LDY     #>FONTNAME1
            JSR     SETNAM
            LDA     #$01 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     LOADFONT
; Load font #2.
            LDA     #FONTNAME3-FONTNAME2
            LDX     #<FONTNAME2
            LDY     #>FONTNAME2
            JSR     SETNAM
            LDA     #$03 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     LOADFONT
; Load font #3.
            LDA     #FONTNAME4-FONTNAME3
            LDX     #<FONTNAME3
            LDY     #>FONTNAME3
            JSR     SETNAM
            LDA     #$05 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     LOADFONT
; Load font #4.
            LDA     #FONTNAME5-FONTNAME4
            LDX     #<FONTNAME4
            LDY     #>FONTNAME4
            JSR     SETNAM
            LDA     #$07 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     LOADFONT
; Load font #5.
            LDA     #MESSAGE1-FONTNAME5
            LDX     #<FONTNAME5
            LDY     #>FONTNAME5
            JSR     SETNAM
            LDA     #$09 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     LOADFONT
; Select font #1.
            LDA     #$01 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     SETFONT
; Add ascent.
            JSR     GETMETRICS
            TAY
            LDA     #$00
            JSR     GOTOYA
; Draw string #1.
            LDX     #<MESSAGE1
            LDA     #>MESSAGE1
            JSR     PRINTUTF8
; Select font #2.
            LDA     #$03 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     SETFONT
; Draw string #2.
            LDX     #<MESSAGE2
            LDA     #>MESSAGE2
            JSR     PRINTUTF8
; Select font #3.
            LDA     #$05 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     SETFONT
; Draw string #3.
            LDX     #<MESSAGE3
            LDA     #>MESSAGE3
            JSR     PRINTUTF8
; Select font #4.
            LDA     #$07 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     SETFONT
; Draw string #4.
            LDX     #<MESSAGE4
            LDA     #>MESSAGE4
            JSR     PRINTUTF8
; Select font #5.
            LDA     #$09 ; font address, bank
            LDX     #$A0 ; font address, high
            JSR     SETFONT
; Draw string #5.
            LDX     #<MESSAGE5
            LDA     #>MESSAGE5
            JSR     PRINTUTF8
; Draw native character set.
            LDA     #$20
            LDX     #$40
            JSR     DEMO4
            LDA     #$40
            LDX     #$60
            JSR     DEMO4
            LDA     #$60
            LDX     #$80
            JSR     DEMO4
            LDA     #$A0
            LDX     #$C0
            JSR     DEMO4
            LDA     #$C0
            LDX     #$E0
            JSR     DEMO4
            LDA     #$E0
            LDX     #$00
            JSR     DEMO4
; Wait for break.
DEMO3:      JSR     GETIN
            CMP     #$1A
            BNE     DEMO3A
            JSR     U8MEXIT1
            BRA     DEMO3
DEMO3A:     CMP     #$18
            BNE     DEMO3B
            JSR     U8MCONT1
            BRA     DEMO3
DEMO3B:     CMP     #$03
            BNE     DEMO3
; Clear text screen to blue and exit.
            LDA     #$01
            JSR     CHROUT
            LDA     #$1F
            JSR     CHROUT
            LDA     #$01
            JSR     CHROUT
            LDA     #$93
            JSR     CHROUT
            JMP     U8MEXIT1

DEMO4:      STA     DEMOTEMP1
            STX     DEMOTEMP2
DEMO5:      JSR     GLYPHNAT
            JSR     DRAWGLYPHA
            INC     DEMOTEMP1
            LDA     DEMOTEMP1
            CMP     DEMOTEMP2
            BNE     DEMO5
            JMP     PRINTLN
DEMOTEMP1:  !BYTE   0
DEMOTEMP2:  !BYTE   0

FONTNAME1:  !TEXT   "MAGDALENA.U8M"
FONTNAME2:  !TEXT   "MCMILLEN.U8M"
FONTNAME3:  !TEXT   "MISCHKE.U8M"
FONTNAME4:  !TEXT   "MONTEREY.U8M"
FONTNAME5:  !TEXT   "PETME.U8M"
MESSAGE1:   !TEXT   "H",$C3,$A9,"ll",$C3,$B8,", w",$C3,$B8,"rld",$E2,$80,$BC,$0A,$0A
            !TEXT   "Your eyes do not deceive you. You are indeed seeing proportional ",$C3,$9C,$C3,$B1,$C3,$AE,$C3,$A7,$C3,$B8,$C4,$91,$C3,$AB,$20,$C5,$A3,$C4,$95,$78,$C5,$A7," on the ",$EF,$97,$BF," Commander X16.",$0A,$0A
            !TEXT   "Have some Greek:",$0A
            !TEXT   $CF,$84,$CF,$89,$CE,$BD,$20,$CE,$B4,$CE,$B5,$20,$CE,$99,$CE,$BF,$CF,$85,$CE,$B4,$CE,$B1,$CE,$B9,$CF,$89,$CE,$BD,$20,$CE,$BF,$CF,$85,$CE,$B4,$CE,$B5,$CE,$B9,$CF,$82,$20,$CE,$B5,$CE,$BD,$CE,$B9,$CF,$88,$CE,$B1,$CF,$84,$CE,$BF,$20,$CF,$84,$CE,$B1,$CF,$82,$20,$CF,$87,$CE,$B5,$CE,$B9,$CF,$81,$CE,$B1,$CF,$82,$2C,$20,$CE,$BF,$CF,$85,$CE,$B4,$CE,$B5,$20,$CE,$97,$CF,$81,$CF,$89,$CE,$B4,$CE,$B7,$CF,$82,$20,$CE,$BF,$CF,$85,$CE,$B4,$20,$CE,$B5,$CE,$B9,$CF,$82,$20,$CF,$84,$CF,$89,$CE,$BD,$20,$CE,$BA,$CF,$81,$CE,$B9,$CF,$84,$CF,$89,$CE,$BD,$20,$CE,$B1,$CF,$85,$CF,$84,$CE,$BF,$CF,$85,$2C,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CF,$84,$CF,$89,$CE,$BD,$20,$CE,$B2,$CE,$BF,$CF,$85,$CE,$BB,$CE,$B7,$CE,$B8,$CE,$B5,$CE,$BD,$CF,$84,$CF,$89,$CE,$BD,$20,$CE,$BD,$CE,$B9,$CF,$88,$CE,$B1,$CF,$83,$CE,$B8,$CE,$B1,$CE,$B9,$2E,$20,$CE,$B1,$CE,$BD,$CE,$B5,$CF,$83,$CF,$84,$CE,$B7,$20,$CE,$A0,$CE,$B5,$CE,$B9,$CE,$BB,$CE,$B1,$CF,$84,$CE,$BF,$CF,$82,$2C,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CF,$84,$CE,$BF,$CF,$84,$CE,$B5,$20,$CE,$BA,$CE,$B5,$CE,$BB,$CE,$B5,$CF,$85,$CE,$B5,$CE,$B9,$20,$CE,$97,$CF,$81,$CF,$89,$CE,$B4,$CE,$B7,$CF,$82,$20,$CE,$BF,$20,$CE,$B2,$CE,$B1,$CF,$83,$CE,$B9,$CE,$BB,$CE,$B5,$CF,$85,$CF,$82,$20,$CF,$80,$CE,$B1,$CF,$81,$CE,$B1,$CE,$BB,$CE,$B7,$CE,$BC,$CF,$86,$CE,$B8,$CE,$B7,$CE,$BD,$CE,$B1,$CE,$B9,$20,$CF,$84,$CE,$BF,$CE,$BD,$20,$CE,$BA,$CF,$85,$CF,$81,$CE,$B9,$CE,$BF,$CE,$BD,$2C,$20,$CE,$B5,$CE,$B9,$CF,$80,$CF,$89,$CE,$BD,$20,$CE,$B1,$CF,$85,$CF,$84,$CE,$BF,$CE,$B9,$CF,$82,$20,$CE,$BF,$CF,$84,$CE,$B9,$20,$CE,$9F,$CF,$83,$CE,$B1,$20,$CE,$B5,$CE,$BA,$CE,$B5,$CE,$BB,$CE,$B5,$CF,$85,$CF,$83,$CE,$B1,$20,$CF,$85,$CE,$BC,$CE,$B9,$CE,$BD,$20,$CF,$80,$CE,$BF,$CE,$B9,$CE,$B7,$CF,$83,$CE,$B1,$CE,$B9,$20,$CE,$B1,$CF,$85,$CF,$84,$CF,$89,$2C,$20,$CF,$80,$CE,$BF,$CE,$B9,$CE,$BD,$CF,$83,$CE,$B1,$CF,$84,$CE,$B5,$2E,$20,$CE,$97,$CE,$BA,$CE,$B5,$CE,$B9,$20,$CE,$B4,$CE,$B5,$20,$CE,$B5,$CE,$BA,$CE,$B5,$CE,$B9,$20,$CE,$99,$CF,$89,$CF,$83,$CE,$B7,$CF,$86,$20,$CE,$BF,$20,$CF,$86,$CE,$B9,$CE,$BB,$CE,$BF,$CF,$82,$20,$CE,$A0,$CE,$B5,$CE,$B9,$CE,$BB,$CE,$B1,$CF,$84,$CE,$BF,$CF,$85,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CF,$84,$CE,$BF,$CF,$85,$20,$CE,$BA,$CF,$85,$CF,$81,$CE,$B9,$CE,$BF,$CF,$85,$2C,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CE,$B5,$CE,$B9,$CE,$B4,$CF,$89,$CF,$82,$20,$CE,$BF,$CF,$84,$CE,$B9,$20,$CF,$83,$CF,$84,$CE,$B1,$CF,$85,$CF,$81,$CE,$B9,$CF,$83,$CE,$BA,$CE,$B5,$CE,$B9,$CE,$BD,$20,$CE,$B1,$CF,$85,$CF,$84,$CE,$BF,$CE,$BD,$20,$CE,$BC,$CE,$B5,$CE,$BB,$CE,$BB,$CE,$BF,$CF,$85,$CF,$83,$CE,$B9,$CE,$BD,$2C,$20,$CE,$B7,$CE,$BB,$CE,$B8,$CE,$B5,$CE,$BD,$20,$CF,$80,$CF,$81,$CE,$BF,$CF,$82,$20,$CF,$84,$CE,$BF,$CE,$BD,$20,$CE,$A0,$CE,$B5,$CE,$B9,$CE,$BB,$CE,$B1,$CF,$84,$CE,$BF,$CE,$BD,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CE,$B7,$CF,$84,$CE,$B7,$CF,$83,$CE,$B5,$20,$CF,$84,$CE,$BF,$20,$CF,$83,$CF,$89,$CE,$BC,$CE,$B1,$20,$CF,$84,$CE,$BF,$CF,$85,$20,$CE,$BA,$CF,$85,$CF,$81,$CE,$B9,$CE,$BF,$CF,$85,$20,$CF,$80,$CF,$81,$CE,$BF,$CF,$82,$20,$CF,$84,$CE,$B1,$CF,$86,$CE,$B7,$CE,$BD,$2E,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CE,$BF,$20,$CE,$A0,$CE,$B5,$CE,$B9,$CE,$BB,$CE,$B1,$CF,$84,$CE,$BF,$CF,$82,$20,$CF,$80,$CE,$B5,$CE,$BC,$CF,$88,$CE,$B1,$CF,$82,$20,$CF,$80,$CF,$81,$CE,$BF,$CF,$82,$20,$CE,$97,$CF,$81,$CF,$89,$CE,$B4,$CE,$B7,$CE,$BD,$20,$CE,$B7,$CF,$84,$CE,$B7,$CF,$83,$CE,$B5,$CE,$BD,$20,$CE,$B1,$CF,$85,$CF,$84,$CE,$BF,$CF,$85,$20,$CF,$84,$CE,$BF,$20,$CF,$83,$CF,$89,$CE,$BC,$CE,$B1,$2C,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CE,$BF,$20,$CE,$97,$CF,$81,$CF,$89,$CE,$B4,$CE,$B7,$CF,$82,$20,$CE,$B5,$CF,$86,$CE,$B7,$20,$CE,$B1,$CE,$B4,$CE,$B5,$CE,$BB,$CF,$86,$CE,$B5,$20,$CE,$A0,$CE,$B5,$CE,$B9,$CE,$BB,$CE,$B1,$CF,$84,$CE,$B5,$2C,$20,$CE,$B5,$CE,$B9,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CE,$BC,$CE,$B7,$20,$CF,$84,$CE,$B9,$CF,$82,$20,$CE,$B1,$CF,$85,$CF,$84,$CE,$BF,$CE,$BD,$20,$CE,$B7,$CF,$84,$CE,$B7,$CE,$BA,$CE,$B5,$CE,$B9,$20,$CE,$B7,$CE,$BC,$CE,$B5,$CE,$B9,$CF,$82,$20,$CE,$B1,$CF,$85,$CF,$84,$CE,$BF,$CE,$BD,$20,$CE,$B5,$CE,$B8,$CE,$B1,$CF,$80,$CF,$84,$CE,$BF,$CE,$BC,$CE,$B5,$CE,$BD,$2C,$20,$CE,$B5,$CF,$80,$CE,$B5,$CE,$B9,$20,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CF,$83,$CE,$B1,$CE,$B2,$CE,$B2,$CE,$B1,$CF,$84,$CE,$BF,$CE,$BD,$20,$CE,$B5,$CF,$80,$CE,$B9,$CF,$86,$CF,$89,$CF,$83,$CE,$BA,$CE,$B1,$CE,$B9,$20,$CE,$B3,$CE,$B5,$CE,$B3,$CF,$81,$CE,$B1,$CF,$80,$CF,$84,$CE,$B1,$CE,$B9,$20,$CE,$B3,$CE,$B1,$CF,$81,$20,$CE,$B5,$CE,$BD,$20,$CF,$84,$CF,$89,$20,$CE,$BD,$CE,$BF,$CE,$BC,$CF,$89,$20,$CE,$B7,$CE,$BB,$CE,$B9,$CE,$BF,$CE,$BD,$20,$CE,$BC,$CE,$B7,$20,$CE,$B4,$CF,$85,$CE,$BD,$CE,$B1,$CE,$B9,$20,$CE,$B5,$CF,$80,$CE,$B9,$20,$CF,$80,$CE,$B5,$CF,$86,$CE,$BF,$CE,$BD,$CE,$B5,$CF,$85,$CE,$BC,$CE,$B5,$CE,$BD,$CF,$89,$20,$CF,$80,$CF,$81,$CE,$BF,$20,$CE,$BC,$CE,$B9,$CE,$B1,$CF,$82,$20,$CF,$84,$CF,$89,$CE,$BD,$20,$CE,$B1,$CE,$B6,$CF,$85,$CE,$BC,$CF,$89,$CE,$BD,$20,$2D,$20,$CF,$84,$CE,$B7,$CF,$82,$20,$CE,$B5,$CE,$BF,$CF,$80,$CF,$84,$CE,$B7,$CF,$82,$20,$CE,$B1,$CF,$85,$CF,$84,$CF,$89,$CE,$BD,$2E,$0A,$0A
            !TEXT   "Or Cyrillic:",$0A
            !TEXT   $D0,$9C,$D0,$B0,$D0,$BA,$D0,$B5,$D0,$B4,$D0,$BE,$D0,$BD,$D1,$81,$D0,$BA,$D0,$B8,$D0,$BE,$D1,$82,$20,$D1,$98,$D0,$B0,$D0,$B7,$D0,$B8,$D0,$BA,$20,$D0,$B2,$D0,$BE,$20,$D0,$B1,$D0,$B0,$D0,$BB,$D0,$BA,$D0,$B0,$D0,$BD,$D1,$81,$D0,$BA,$D0,$B0,$D1,$82,$D0,$B0,$20,$D1,$98,$D0,$B0,$D0,$B7,$D0,$B8,$D1,$87,$D0,$BD,$D0,$B0,$20,$D1,$81,$D1,$80,$D0,$B5,$D0,$B4,$D0,$B8,$D0,$BD,$D0,$B0,$20,$D0,$B8,$20,$D0,$BD,$D0,$B0,$D1,$81,$D0,$BF,$D1,$80,$D0,$B5,$D0,$BC,$D0,$B0,$20,$D1,$81,$D0,$BE,$D1,$81,$D0,$B5,$D0,$B4,$D0,$BD,$D0,$B8,$D1,$82,$D0,$B5,$20,$D1,$81,$D0,$BB,$D0,$BE,$D0,$B2,$D0,$B5,$D0,$BD,$D1,$81,$D0,$BA,$D0,$B8,$20,$D1,$98,$D0,$B0,$D0,$B5,$D0,$B8,$D1,$86,$D0,$B8,$2E,$20,$31,$2E,$20,$D0,$9C,$D0,$B0,$D0,$BA,$D0,$B5,$D0,$B4,$D0,$BE,$D0,$BD,$D1,$81,$D0,$BA,$D0,$B8,$D0,$BE,$D1,$82,$20,$D1,$98,$D0,$B0,$D0,$B7,$D0,$B8,$D0,$BA,$20,$D1,$81,$D0,$B5,$20,$D0,$B3,$D0,$BE,$D0,$B2,$D0,$BE,$D1,$80,$D0,$B8,$20,$D0,$B2,$D0,$BE,$20,$D0,$A1,$D0,$A0,$20,$D0,$9C,$D0,$B0,$D0,$BA,$D0,$B5,$D0,$B4,$D0,$BE,$D0,$BD,$D0,$B8,$D1,$98,$D0,$B0,$2C,$20,$D0,$B8,$20,$D0,$BD,$D0,$B0,$D0,$B4,$D0,$B2,$D0,$BE,$D1,$80,$20,$D0,$BE,$D0,$B4,$20,$D0,$BD,$D0,$B5,$D1,$98,$D0,$B7,$D0,$B8,$D0,$BD,$D0,$B8,$D1,$82,$D0,$B5,$20,$D0,$B3,$D1,$80,$D0,$B0,$D0,$BD,$D0,$B8,$D1,$86,$D0,$B8,$2C,$20,$D0,$B2,$D0,$BE,$20,$D0,$BE,$D0,$BD,$D0,$B8,$D0,$B5,$20,$D0,$B4,$D0,$B5,$D0,$BB,$D0,$BE,$D0,$B2,$D0,$B8,$20,$D0,$BD,$D0,$B0,$20,$D0,$9C,$D0,$B0,$D0,$BA,$D0,$B5,$D0,$B4,$D0,$BE,$D0,$BD,$D0,$B8,$D1,$98,$D0,$B0,$20,$D1,$88,$D1,$82,$D0,$BE,$20,$D0,$BF,$D0,$BE,$20,$D0,$B1,$D0,$B0,$D0,$BB,$D0,$BA,$D0,$B0,$D0,$BD,$D1,$81,$D0,$BA,$D0,$B8,$D1,$82,$D0,$B5,$20,$D0,$B2,$D0,$BE,$D1,$98,$D0,$BD,$D0,$B8,$20,$D0,$B2,$D0,$BB,$D0,$B5,$D0,$B3,$D0,$BE,$D0,$B0,$20,$D0,$B2,$D0,$BE,$20,$D1,$81,$D0,$BE,$D1,$81,$D1,$82,$D0,$B0,$D0,$B2,$D0,$BE,$D1,$82,$20,$D0,$BD,$D0,$B0,$20,$D0,$93,$D1,$80,$D1,$86,$D0,$B8,$D1,$98,$D0,$B0,$20,$D0,$B8,$20,$D0,$91,$D1,$83,$D0,$B3,$D0,$B0,$D1,$80,$D0,$B8,$D1,$98,$D0,$B0,$2E,$0A,$0A
            !TEXT   "The driver is just under 2K. Its native language is UTF-8. It is called U8/M, or UTF-8 for Microcomputers.",$0A,$0A,0
MESSAGE2:   !TEXT   "You can load multiple fonts and switch between them. ",0
MESSAGE3:   !TEXT   "Like this extra fancy cursive one. ",0
MESSAGE4:   !TEXT   "Or this boxy retro-futuristic one.",$0A,$0A,0
MESSAGE5:   !TEXT   "With the right font you can also get PETSCII. Both sets at the same time!",$0A,$0A,0
