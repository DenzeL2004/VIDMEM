.model tiny
.code
org 100h

locals @@



MIDDLE_SCREEN 	 equ 80d * 2d * 12d + 40d * 2d
SIZE_VIDMEM_LINE equ 80d * 2d
VIDMEM_ADR 	  	 equ 0B800h

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

	mov ax, 60123d		;set val to register ax
	mov bx, SIZE_VIDMEM_LINE * 1d	;third line in video mem
	call DecRep

	mov ax, 10100001110001b		    ;set val to register ax
	mov bx, SIZE_VIDMEM_LINE * 2d	;third line in video mem
	call BinRep

	mov ax, 0F23Ch					;set val to register ax
	mov bx, SIZE_VIDMEM_LINE * 3d	;third line in video mem
	call HexRep

	Exit 0

	;-----------------------------------------------------------------
	;binary representation of a number
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR
	;Entry: ax (input number), bx (address in video mem)
	;Exit: none
	;Destroy: bx, cx, dx
	;-----------------------------------------------------------------
	BinRep	proc

		mov cl, 15d 	;binary mask 
		@@next:
			mov dx, 1d 				;------------
			shl dx, cl				;get cur mask
			
			and dx, ax				;check i bit
			shr dx, cl				;move to dl

			or dx, COLOR_FRAME		;define color frame and color symbol
			or dl, '0'				;suppose cur byte is zero

			mov word ptr es:[bx], dx	;set cur sumbol to video mem
			add bx, 2d 					;increse bx pointere

			dec cl 						;decrease counter by 1
			
		cmp cx, 0d
		jne @@next

		mov word ptr es:[bx], COLOR_FRAME or 'b'	;set 'b' sumbol to video mem<-|

		ret
	BinRep	endp

	;-----------------------------------------------------------------
	;hex representation of a number
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR
	;Entry: ax (input number), bx (address in video mem)
	;Exit: none
	;Destroy: bx, cx, dx
	;-----------------------------------------------------------------
	HexRep	proc

		xor cx, cx      ;free cx 
		mov cl, 16d 	;counter
		@@next:
			sub cl, 4h 				;decrement counter
			
			mov dx, 1111b			;hex mask
			shl dx, cl				;mask shift

			and dx, ax				;get cur symbol
			shr dx, cl 				;move to dl

			add dl, '0'				;suppose cur symbol is num

			cmp dl, '0' + 10
			jl @@isNum
				add dl, 7d			;symbol is letter
			@@isNum:
				
			or dx, COLOR_FRAME			;define color frame and color symbol
			mov word ptr es:[bx], dx	;set cur sumbol to video mem
			add bx, 2d 					;increse bx pointere

		cmp cl, 0d
		jne @@next

		mov word ptr es:[bx], COLOR_FRAME or 'h'		;set 'h' sumbol to video mem<-|

		ret
	HexRep	endp

	;-----------------------------------------------------------------
	;Deс representation of a number
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR
	;Entry: ax (input number), bx (address in video mem)
	;Exit: none
	;Destroy: ax, cx, dx
	;-----------------------------------------------------------------
	DecRep	proc

		push bp
		mov bp, sp
		sub sp, 2d 		     ;local var

		quotient equ word ptr [bp-2] ;local var 'quotient'

		mov quotient, ax	;dublicate ax value
		mov cx, 10000d
		
		@@next:
			xor dx, dx
			mov ax, quotient
			div cx 				;dx:ax div cx
			
			or al, '0'
			or ax, COLOR_FRAME
			mov word ptr es:[bx], ax	;set symbol to video mem
			add bx, 2d 					;increse bx pointere

			mov quotient, dx

			mov ax, cx		;ax = 10^i
			mov dx, 0d 		;

			mov cx, 10d 	;-----------------------+
			div cx			;dx:ax div cx (cx / 10)	|
			mov cx, ax 		;save quotient to cx

		cmp quotient, 0d
		jne @@next

		mov word ptr es:[bx], COLOR_FRAME or 'd' ;set 'd' sumbol to video mem

		mov sp, bp 		;abandoning the place of local variables
		pop bp 			;bp recovery

		ret
	DecRep	endp

	

end Start