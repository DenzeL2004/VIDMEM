.model tiny
.code
org 100h

locals @@


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

	
	call ReadNum
	push dx 		;first num

	call ReadNum
	push dx			;second num

	call ReadNum
	push dx			;command

	pop cx			;save command
	pop dx          ;save srcond num
	pop ax			;save first  num 

	
	call Calc

	mov di, 80d * 2d * 5d	;third line in video mem
	call DecRep

	Exit 0

	;-----------------------------------------------------------------
	;Read positive num from consol
	;-----------------------------------------------------------------
	;Entrt: nope
	;Exit: dx
	;Destroy: ax, bx
	;Info: if string has non-digit (exception '\n') symbol, program write ErrorMsg
	;-----------------------------------------------------------------	

	ReadNum proc

		xor ax, ax 	;free ax
		xor dx, dx	;free dx
		xor bx, bx  ;free bx

		@@isNum:
			mov bl, al  ;save last symbol
 			mov ax, dx  ;restored the number
			mov dx, 10d ;dx = 10
			mul dx		;ax * 10
			add ax, bx  ;add last symbol

			mov dx, ax  ;save num

			mov ah, 01h		;---------------------------
			int 21h			;read one symbol from consol 

			cmp al, 0dh		;check symbol id '\n'
			je @@ExitSituation
			
			sub al, '0'

			mov ah, 9d
			cmp ah, al
		jae @@isNum     ;check cur symbol is digit
		
		;error situation
			mov ax, 0FFFFh	;poison val

			mov dx, offset ErrorMsg		
			mov ah, 09h  	;print error message
			int 21h 	    ;-------------------

			Exit 1

		@@ExitSituation:
		
		ret

	ReadNum endp

	;-----------------------------------------------------------------
	;calculate value from registers
	;-----------------------------------------------------------------
	;Entrt: ax, dx, (input value), cl (command)
	;Exit:  ax
	;Destroy: ax, dx, cx
	;-----------------------------------------------------------------	

	Calc proc

		cmp cl, 0d
		jne @@ComSub
			add ax, dx
		
		@@ComSub:
		cmp cl, 1d
		jne @@ComMul
			sub ax, dx


		@@ComMul:
		cmp cl, 2d
		jne @@ComDiv
			mul dx

		@@ComDiv:
			mov cx, dx
			xor dx, dx
			div cx

		ret

	Calc endp

	DecRep	proc

		push bp  		;save bp to stack
 		mov bp, sp
		sub sp, 2d 		 ;local var

		quotient equ word ptr [bp-2] ;local var 'quotient'

		mov quotient, ax	;dublicate ax value
		mov cx, 10000d
		
		@@next:
			xor dx, dx
			mov ax, quotient
			div cx 				;dx:ax div cx
			
			or al, '0'
			or ax, COLOR_FRAME
			mov word ptr es:[di], ax	;set symbol to video mem
			add di, 2d 					;increse di pointere

			mov quotient, dx

			mov ax, cx		;ax = 10^i
			xor dx, dx 		;

			mov cx, 10d 	;-----------------------+
			div cx			;dx:ax div cx (cx / 10)	|
			mov cx, ax 		;save quotient to cx

		cmp cx, 0d
		jne @@next

		mov word ptr es:[di], COLOR_FRAME or 'd' ;set 'd' sumbol to video mem

		mov sp, bp 		;abandoning the place of local variables
		pop bp 			;bp recovery

		ret
	DecRep	endp


.const 
ErrorMsg db 0dh, "ERROR: misrepresentation of a number$"

end Start
