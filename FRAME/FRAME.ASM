.model tiny
.code
org 100h

locals @@


MIDDLE_SCREEN 	 equ 80d * 12d + 40d 
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
;multiplies the value of a register bx by 10 use bit shifting
;-----------------------------------------------------------------
;Entrt: bx (input val)
;Exit: bx 
;Destroy: dx
;-----------------------------------------------------------------
MUL10		macro
			NOPE

			mov dx, bx 	;save bx val

			shl bx, 3d 	;bx = bx * 8
			shl dx, 1d	;dx = dx * 2

			add bx, dx	
			
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

Start:
	
	SET_MEMADRESS

	mov si, START_INPUT_PARAM + 1d ;set pointer to start input arguments

	;(Read frame's color)
	call ParceDecNum ;read 5th argument - frame's color
	mov cx, bx 		 ;Save read arguments
	;(Read frame's color)

	;(Read frame's mode)
	call ParceDecNum ;read 5th argument - frame's color

	mov ah, cl 	;save frame's color
	mov cl, bl  ;save frame's mode
	;(Read frame's color)

	;define frame's sample
		mov bx, si 	;save the pointer where the string starts

		cmp cl, 0d  ;check frame's sample = 1
		jne @@Sample1
			add bx, 10d		;shift ptr by custom sample 

	@@Sample1:		
		cmp cl, 1d		;check frame's sample = 1
		jne @@Sample2	
			mov si, offset frameSample1

	@@Sample2:
		cmp cl, 2d		;check frame's sample = 2
		jne @@Sample3
			mov si, offset frameSample2

	@@Sample3:		
		cmp cl, 3d		;check frame's sample = 3
		jne @@ENDIF
			mov si, offset frameSample3
	@@ENDIF:
	;define frame's sample

	;Get frame's hifht and width
	push si 	;save address frame's sample
	mov si, bx  ;addres start string

	call GetFrameParam 

	pop si ;record address frame's sample
	;Get frame's hifht and width

	mov cl, dl	;save frame's hight
	mov ch, dh  ;save frame's width

	;calc di address
	mov di, MIDDLE_SCREEN * 2d;
	push bx
	
	xor bx, bx
	mov bl, ch	
	MUL10	   ;bx * 10
	shl bx, 3d ;calc number videomem line

	shl bx, 1d ;bx * 2

	add bl, cl   ;shift be x coord
	and bl, 0FEh ;parity byte
	sub di, bx; 
	
	pop bx
	;calc di address
	
	mov dh, ch
	mov dl, cl

	push di 	;save di pointer

	call DrawFrame

	pop di      ;restore frame's width
	add di, SIZE_VIDMEM_LINE * 2d + 2d ;start address to write message to video memory

	mov si, bx  ;mov si to start message

	call PrintToMemory

	Exit 0

	;-----------------------------------------------------------------
	;Draw frame in video memory
	;-----------------------------------------------------------------
	;Assumes: es = VIDMEM_ADR, ds = sourse segment
	;Entry: ah(frame's color) dh (frame's hight), dl (frame's width)
	;		di (destination index), si (source index)
	;Exit: none
	;Destroy: ax, ch, cl, dh, dl, di, si
	;-----------------------------------------------------------------
	DrawFrame	proc
		xor cx, cx 	;free cx

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

	;-----------------------------------------------------------------
	;writing a string from one segment of memory to another
	;-----------------------------------------------------------------
	;Assumes: command line arguments ends with $
	;Assumes: es - destination segment, ds - source segment
	;Entry: ah(start string color)
	;Exit: none
	;Destroy: ah, al, bx, cl, ch, dh, di, si
	;-----------------------------------------------------------------
	PrintToMemory	proc
		
		xor cl, cl	;free cl
		@@next:
			LODSB		;save symbol to al

			cmp al, '$' ;check al is '$'
			je @@goToRet

			cmp al, '\' ;check al is '|'
			jne @@changeColor
				xor ch, ch ;free ch
				sub di, cx ;shift to start line

				add di, SIZE_VIDMEM_LINE * 2d ;next line
				
				xor cl, cl ;free cl
				jmp @@next


		@@changeColor:
			cmp al, '%' ;check al is '<'
			jne @@print
				mov ch, ah   ;save cur color
				and ch, 0F0h ;delete privios color

				call ParceDecNum ;read new symbols color

				or  ch, bl  ;define new symbols color
				mov ah, ch	;define new color	
				jmp @@next

		@@print:
			STOSW 		;draw cur symbol
			add cl, 2d	
		
		jmp @@next
		
		@@goToRet:

		ret

	PrintToMemory	endp

	;-----------------------------------------------------------------
	;get string parameters (height and width)
	;-----------------------------------------------------------------
	;Entry: si
	;Exit: dh, dl
	;Destroy: al, ch, cl
	;-----------------------------------------------------------------
	GetFrameParam	proc

		xor dx, dx ;free dx
		xor ch, ch ;free ch
		@@next:

			LODSB  ;save byte to al from arguments line
			inc ch ;counter++

			cmp al, '$' ;check al is '$'
			je @@break 

			cmp al, '\'		;check symbol al is '\'
			jne @@next
				inc dh ;frame's hight++
				
				mov cl, ch	
				xor ch, ch	;free counter

				cmp cl, dl ;check cl > dl, find max 
				jbe @@next 
					mov dl, cl

		jmp @@next
		
		@@break: 

		add dh, 2d ;frame border
		add dl, 2d ;frame border
		
		ret	
		
	GetFrameParam	endp

	;-----------------------------------------------------------------
	;Parce numbers argument command line (only in decimal representation)
	;-----------------------------------------------------------------
	;Assumes: si = START_INPUT_Sample
	;Entry: none
	;Exit: bx (val)
	;Destroy: ah, al, dh
	;WARNING: if the entry contains characters other than spaces or digits, 
	;         then when they are found, the function terminates the entire program
	;-----------------------------------------------------------------
	ParceDecNum	proc

		call SkipSpace

		xor bx, bx

		@@isNum:
			LODSB ;save byte to al from arguments line

			cmp al, ' '		;check symbol al is ' '
			je @@goToRet

			cmp al, 0dh		;check symbol al is '\n'
			je @@goToRet

			cmp al, 09h 	;check condition al == '\t'
			je @@goToRet	
			
			sub al, '0'		;al - ascii('0')

			MUL10 		;bx * 10
			add bl, al 	;add cur symbol

			mov ah, 9d		;ah = 9
			cmp ah, al
		jae @@isNum     ;check cur symbol is digit
		
		;error situation
			mov bx, 0FFFFh	;poison val

			mov dx, offset NotDigiSymbol		
			mov ah, 09h  	;print error message
			int 21h 	    ;-------------------

			Exit 1

		@@goToRet:
		
		ret	
		
	ParceDecNum	endp

	;-----------------------------------------------------------------
	;gets to the first symbol that is not a space or tab
	;-----------------------------------------------------------------
	;Entry: si
	;Exit: si
	;Destroy: none
	;-----------------------------------------------------------------
	SkipSpace	proc

		@@isSpace:

			LODSB ;save byte to al from arguments line

		cmp al, ' ' 	;check condition al == ' '
 		je @@isSpace	
		
		cmp al, 09h 	;check condition al == '\t'
		je @@isSpace	

		dec si 	;last symbol wasn't space
		
		ret	
		
	SkipSpace	endp


.const 
frameSample1 db 0dah, 0c4h, 0bfh, 0c3h, 20h, 0b4h, 0c0h, 0c4h, 0d9h  ;patern for frame
frameSample2 db 0feh, 0cdh, 0feh, 0b3h, 020h, 0b3h, 0feh, 0cdh, 0feh ;patern for frame
frameSample3 db 3d dup (03h), 03h, 20h, 03h, 3d dup (03h)		    ;patern for frame

NotDigiSymbol db 0dh, "ERROR: misrepresentation of a number", 0dh, '$'

end Start