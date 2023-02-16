.model tiny
.code
org 100h

locals @@



MIDDLE_SCREEN 	 equ 80d * 2d * 12d + 40d * 2d
SIZE_VIDMEM_LINE equ 80d * 2d
VIDMEM_ADR 	  	 equ 0B800h

EMTY_CHAR	equ 9F00h			;symbol space on black frame

COLOR_FRAME	equ 0F00h 			;light blue symbol on black frame				

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

Start:

	SET_MEMADRESS

	push 2d ;argument xc 
	push 2d ;argument yc

	push 20d ;argument wp
	push 5d ;argument hp		

	call DrawFrame

	Exit 0

	;-----------------------------------------------------------------
	;Draw frame in video memory
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR
	;Param: wp (frame's width), hp (frame's hight)
	;		xc (x coord upper left corner) yc (y coord upper left corner)
	;Exit: none
	;Destroy: ax, di, cx
	;-----------------------------------------------------------------
	DrawFrame	proc

		push bp		;save bp pointer
		mov bp, sp	;bp = sp
		
		xc equ bp + 10	;define param 'xc'
		yc equ bp + 8	;define param 'yc'

		wp equ bp + 6	;define param 'wp'
		hp equ bp + 4	;define param 'hp'

		push [xc]	;set in ClearFrame argument xc
		push [yc]	;set in ClearFrame argument yc

		push [wp]   ;set in ClearFrame argument wp
		push [hp]   ;set in ClearFrame argument hp

		call ClearFrame

		;{COVER ON MACROS OR FUNC!!!!!!!!!!!!!
			mov al, SIZE_VIDMEM_LINE	;ah = 80d
			mul byte ptr [yc]			;shift by coord y

			mov di, ax  				;save start line video mem addres to di
				
			mov al, 2d 					;-----------------
			mul byte ptr [xc]			;shift by coord x

			add di, ax					;start video mem addres
		;}



		mov si, 4d	;border top symbol
		xor cl, cl  ;counter for x coord
		@@loopXC:
		
			mov dl, frameParam1[si]
			or dx, COLOR_FRAME
			mov word ptr es:[di], frameParam1[si]  ;set in video mem symbol's frame
			add di, 2d 							   ;increse di pointer

		inc cl 					;cl += 1
		cmp byte ptr [wp], cl   ;check out condition 'loopXC'
		jne @@loopXC


		
		pop bp			;bp recovery
		ret 2d * 4d 	;free memory in stack, param : 'xc', 'yc', 'wp', 'hp'
	DrawFrame	endp

	;-----------------------------------------------------------------
	;'clears' a chunk of video memory
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR
	;Param: wp (frame's width), hp (frame's hight)
	;		xc (x coord upper left corner) yc (y coord upper left corner)
	;Exit: none
	;Destroy: ax, di, cx
	;-----------------------------------------------------------------
	ClearFrame	proc

		push bp		;save bp pointer
		mov bp, sp	;bp = sp
		
		xc equ bp + 10	;define param 'xc'
		yc equ bp + 8	;define param 'yc'

		wp equ bp + 6	;define param 'wp'
		hp equ bp + 4	;define param 'hp'


		mov al, SIZE_VIDMEM_LINE	;ah = 80d
		mul byte ptr [yc]			;shift by coord y

		mov di, ax  				;save start line video mem addres to di
			
		mov al, 2d 					;-----------------
		mul byte ptr [xc]			;shift by coord x

		add di, ax					;start video mem addres



		xor ch, ch 		;counter for y coord
		@@loopYC:

			xor cl, cl  ;counter for x coord
			@@loopXC:
			
				mov word ptr es:[di], EMTY_CHAR	;set in video mem 'space'
				add di, 2d 						;increse di pointer

			inc cl 					;cl += 1
			cmp byte ptr [wp], cl   ;check out condition 'loopXC'
			jne @@loopXC

			mov al, 2d 	;al = 2
			mul cl 		;ax = al * cl

			sub di, ax	;shift to start cur line in video mem

			add di, SIZE_VIDMEM_LINE 	;shift to next line in video mem


		inc ch 					;ch += 1
		cmp byte ptr [hp], ch   ;check out condition 'loopYC'
		jne @@loopYC		


		pop bp			;bp recovery
		ret 2d * 4d 	;free memory in stack, param : 'xc', 'yc', 'wp', 'hp'
	ClearFrame	endp

.const 
frameParam1 db '+'. '+', '+', '+', '-', '-', '|', '|'


end Start