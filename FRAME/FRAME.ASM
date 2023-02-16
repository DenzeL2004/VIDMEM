.model tiny
.code
org 100h

locals @@



MIDDLE_SCREEN 	 equ 80d * 2d * 12d + 40d * 2d
SIZE_VIDMEM_LINE equ 80d * 2d
VIDMEM_ADR 	  	 equ 0B800h

EMTY_CHAR	equ 0931h			;symbol space on black frame

COLOR_FRAME	equ 0900h 			;light blue symbol on black frame				

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
;Destroy: bx
;-----------------------------------------------------------------

SET_MEMADRESS	macro
				NOPE

				mov bx, VIDMEM_ADR		;video memory address
				mov es, bx

				NOPE
				endm

Start:

	SET_MEMADRESS

	push 2d ;argument xc 
	push 2d ;argument yc

	push 10d ;argument wp
	push 10d ;argument hp		

	call DrawFrame

	Exit 0

	;-----------------------------------------------------------------
	;draw frame in video memory
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR
	;Param: wp (frame's width), hp (frame's hight)
	;		xc (x coord upper left corner) yc (y coord upper left corner)
	;Exit: none
	;Destroy: bx, cx, dx
	;-----------------------------------------------------------------
	DrawFrame	proc

		push bp		;save bp pointer
		mov bp, sp	;bp = sp
		
		xc equ bp + 10	;define param 'xc'
		yc equ bp + 8	;define param 'yc'

		wp equ bp + 6	;define param 'wp'
		hp equ bp + 4	;define param 'hp'


		mov al, SIZE_VIDMEM_LINE	;ah = 80d
		mul byte ptr [yc]			;shift by coord y

		mov bx, ax  					;save start mem addres to bx
		
		mov al, 2d
		mul byte ptr [xc]				;shift by coord x

		add bx, ax	;start video mem addres



		xor ch, ch 		;counter for y coord
		@@loopYC:

			xor cl, cl  ;counter for x coord
			@@loopXC:
			
				mov word ptr es:[bx], EMTY_CHAR	;set in video mem 'space'
				add bx, 2d 						;increse bx pointer

			inc cl 			;cl += 1
			
			mov dl, cl
			mov dh, byte ptr [bp+6]
			sub dh, dl

			cmp byte ptr [wp], cl   ;check out condition 'loopXY'
			jne @@loopXC

			mov al, 2d 	;al = 2
			mul cl 		;ax = al * cl

			sub bx, ax	;shift to start cur line in video mem

			add bx, SIZE_VIDMEM_LINE 	;shift to next line in video mem


		inc ch 	;ch += 1
		cmp byte ptr [hp], ch 		;check out condition 'loopCY'
		jne @@loopYC		


		pop bp			;bp recovery
		ret 2d * 4d 	;four param : 'xc', 'yc', 'wp', 'hp'
	DrawFrame	endp

.const 
frameParam1 db '+'. '+', '+', '+', '_', '_', '|', '|'


end Start