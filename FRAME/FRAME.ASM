.model tiny
.code
org 100h

locals @@


MIDDLE_SCREEN 	 equ 80d * 2d * 12d + 40d * 2d
SIZE_VIDMEM_LINE equ 80d
VIDMEM_ADR 	  	 equ 0B800h

START_INPUT_PARAM equ 0080h

COLOR_FRAME	equ 3Fh 			;white symbol on cyan frame				

NOPE equ nop

;-----------------------------------------------------------------
;Exit
;-----------------------------------------------------------------
;Entrt: nope
;Exit: N/A
;Destroy: N/A
;-----------------------------------------------------------------

Exit	macro code
		NOPE

		mov AX, 4c00h or code
		int 21h		

		NOPE
		endm

;-----------------------------------------------------------------
;Set Memory Address
;-----------------------------------------------------------------
;Entrt: nope
;Exit: es
;Destroy: di
;-----------------------------------------------------------------

SET_MEMADRESS	macro
				NOPE

				mov di, VIDMEM_ADR		;video memory address
				mov es, di

				NOPE
				endm

;-----------------------------------------------------------------
;Define vide address by coord
;-----------------------------------------------------------------
;Entrt: cx (ch:cl = xCoord:yCoord)
;Exit: di (video mem address)
;Destroy: ax (after ah:al = 00:00)
;-----------------------------------------------------------------

DEF_COORD_ADDRESS	macro
					NOPE

					mov al, SIZE_VIDMEM_LINE	;al = 80d
					mul cl						;shift by coord y

					mov di, ax  				;save start line video mem addres to di

					xor ax, ax
					mov al, ch

					add di, ax				;shift addres to coord x
				
					xor ax, ax					;clear ah:al = 00:00
					
					NOPE
					endm

;-----------------------------------------------------------------
;Set symbol to video memory from array
;-----------------------------------------------------------------
;Entrt: di (vide memory addres)
;Exit: nope
;Destroy: si
;-----------------------------------------------------------------

DRAW_CH_FROM_ARR	macro arr, drawMod 
					NOPE

					mov si, drawMod 		  ;lower left corner
					mov dl, arr[si]	 		  ;-----------------
					mov word ptr es:[di], dx  ;set in video mem symbol's frame					
								
					NOPE
				db 3d dup (03h)	endm

Start:

	SET_MEMADRESS

	lea si, frameParam3 ;pointer to command line arguments

	mov ah, COLOR_FRAME ;def color
	mov dh, 5d
	mov dl, 10d

	mov di, SIZE_VIDMEM_LINE * 2d + 2d

	call DrawFrame

	Exit 0

	;-----------------------------------------------------------------
	;Draw frame in video memory
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR, ds = sourse sigment
	;Entry: ah(frame's color) dh (frame's hight), dl (frame's width)
	;		di (destination index), si (source index)
	;Exit: none
	;Destroy: ax, dh, dl, di, si,
	;-----------------------------------------------------------------
	DrawFrame	proc

		mov cl, dl 	;cx = width
		call DrawLine

		mov cl, dl	;expansion to word
		shl cl, 1d  ;change shift

		sub di, cx	;return di to start line
		add di, SIZE_VIDMEM_LINE * 2d ;next line

		sub dh, 2d 	;dh -= 2

		@@next:
			mov cl, dl 	;cx = width
			call DrawLine
		
			sub si, 3d	;return si to cur 3 sybmbols

			mov cl, dl	;expansion to word
			shl cl, 1d  ;change shift
			
			sub di, cx	;return di to start line
			add di, SIZE_VIDMEM_LINE * 2d ;next line
			
		dec dh 		;dh--
		cmp dh, 0d	;-----------------------
		jne @@next  ;check condition вр != 0
		
		add si, 3d  ;next three symbol

		mov cl, dl 	;cx = width
		call DrawLine

		ret
		
	DrawFrame	endp

	;-----------------------------------------------------------------
	;Draw line in video memory from source
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR, ds = sourse sigment
	;Entry: ah(frame's color), cx (counter)
	;Exit: none
	;Destroy: al, cx, di, si
	;-----------------------------------------------------------------
	DrawLine	proc

		cld 		;DF = 0

		LODSB		;save symbol to al
		STOSW 		;draw left symbol

		sub cx, 2d	;counter -= 2	
		LODSB		;save symbol to al
		rep STOSW 	;draw middle symbol

		LODSB		;save symbol to al
		STOSW 		;draw right symbol

		ret	
		
	DrawLine	endp

.const 
frameParam1 db 0dah, 0c4h, 0bfh, 0c3h, 20h, 0b4h, 0c0h, 0c4h, 0d9h  ;patern for frame
frameParam2 db 0feh, 0cdh, 0feh, 0b3h, 020h, 0b3h, 0feh, 0cdh, 0feh ;patern for frame
frameParam3 db 3d dup (03h), 03h, 20h, 03h, 3d dup (03h)		    ;patern for frame	

end Start