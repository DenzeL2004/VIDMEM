.model tiny
.code
org 100h

locals @@



MIDDLE_SCREEN 	 equ 80d * 2d * 12d + 40d * 2d
SIZE_VIDMEM_LINE equ 80d
VIDMEM_ADR 	  	 equ 0B800h

COLOR_FRAME	equ 03F20h 			;white symbol on cyan frame				

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
					endm

Start:

	SET_MEMADRESS

	push 40d ;argument xc 
	push 2d ;argument yc

	push 30d ;argument wp
	push 10d ;argument hp		

	call DrawFrame

	Exit 0

	;-----------------------------------------------------------------
	;Draw frame in video memory
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR
	;Param: wp (frame's width), hp (frame's hight)
	;		xc (x coord upper left corner) yc (y coord upper left corner)
	;Exit: none
	;Destroy: ax, di, cx, dx
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

		shl byte ptr [wp], 1d
		shl byte ptr [hp], 1d

		shl byte ptr [xc], 1d
		shl byte ptr [yc], 1d

		mov ch, byte ptr [xc]
		mov cl, byte ptr [yc]

		DEF_COORD_ADDRESS

		mov dx, COLOR_FRAME 	;define color print's symbol

		;---------------------------------------------------------------
		;draw top border
		;-----------------------------------------------------------------
		mov si, 4d		;border top symbol
		mov dl, frameParam1[si] ;-----------------

		xor cl, cl  ;counter for x coord
		@@loopXRigt:
			
			mov word ptr es:[di], dx  ;set in video mem symbol's frame
			add di, 2d 				  ;increse di pointer

		add cl, 2d				;cl += 2
		cmp byte ptr [wp], cl   ;check out condition 'loopXRigt'
		jne @@loopXRigt


		;---------------------------------------------------------------
		;draw right border
		;-----------------------------------------------------------------
		mov si, 7d				;border right symbol
		mov dl, frameParam1[si]	;-------------------

		xor ch, ch  ;free counter for y coord
		@@loopYDown:
			
			mov word ptr es:[di], dx  	   ;set in video mem symbol's frame
			add di, SIZE_VIDMEM_LINE * 2d  ;increse di pointer

		add ch, 2d 				;ch += 2
		cmp byte ptr [hp], ch   ;check out condition 'loopYDown'
		jne @@loopYDown


		;---------------------------------------------------------------
		;draw bottom border
		;-----------------------------------------------------------------
		mov si, 5d				;border bottom symbol
		mov dl, frameParam1[si] ;-------------------

		@@loopXLeft:
			
			mov word ptr es:[di], dx  ;set in video mem symbol's frame
			sub di, 2d 				  ;decrese di pointer

		sub cl, 2 		;cl -= 2
		cmp cl, 0   	;check out condition 'loopXLeft'
		jne @@loopXLeft


		;---------------------------------------------------------------
		;draw left border
		;-----------------------------------------------------------------
		mov si, 6d				;border left symbol
		mov dl, frameParam1[si]	;-------------------

		@@loopYUp:
			
			mov word ptr es:[di], dx      ;set in video mem symbol's frame
			sub di, SIZE_VIDMEM_LINE * 2d ;increse di pointer

		sub ch, 2 	;ch -= 2
		cmp ch, 0 	;check out condition 'loopYUp'
		jne @@loopYUp


		;----------set symbol on upper left crner-----------
		DRAW_CH_FROM_ARR frameParam1, 0d
		;---------------------------------------------------

		;----------set symbol on upper right crner-----------
		add di, [wp]			  ;shift right di to frame's width

		DRAW_CH_FROM_ARR frameParam1, 1d
		;---------------------------------------------------

		;----------set symbol on lower right crner-----------
		mov al, SIZE_VIDMEM_LINE	;al = 80d
		mul byte ptr [hp]			;calc shift
		add di, ax  				;shift by frame's hight

		DRAW_CH_FROM_ARR frameParam1, 2d
		;---------------------------------------------------

		;----------set symbol on lower left crner-----------
		sub di, [wp]			  ;shift right di to frame's width

		DRAW_CH_FROM_ARR frameParam1, 3d
		;---------------------------------------------------

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

		shl byte ptr [wp], 1d
		shl byte ptr [hp], 1d

		shl byte ptr [xc], 1d
		shl byte ptr [yc], 1d

		mov ch, byte ptr [xc]
		mov cl, byte ptr [yc]
		DEF_COORD_ADDRESS


		xor ch, ch 		;counter for y coord
		@@loopYC:

			xor cl, cl  ;counter for x coord
			@@loopXC:
			
				mov word ptr es:[di], COLOR_FRAME	;set in video mem 'space'
				add di, 2d 							;increse di pointer

			add cl, 2d 				;cl += 2
			cmp byte ptr [wp], cl   ;check out condition 'loopXC'
			jne @@loopXC

			mov al, cl  ;------------------------------------
			sub di, ax	;shift to start cur line in video mem

			add di, SIZE_VIDMEM_LINE * 2d	;shift to next line in video mem


		add ch, 2d 				;ch += 2
		cmp byte ptr [hp], ch   ;check out condition 'loopYC'
		jne @@loopYC		


		pop bp			;bp recovery
		ret 2d * 4d 	;free memory in stack, param : 'xc', 'yc', 'wp', 'hp'
	ClearFrame	endp

.const 
frameParam1 db 0dah, 0bfh, 0d9h, 0c0h, 0c4h, 0c4h, 0c3h, 0b4h


end Start