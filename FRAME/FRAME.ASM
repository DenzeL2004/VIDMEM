.model tiny
.code
org 100h

locals @@


MIDDLE_SCREEN 	 equ 80d * 12d + 40d
WIDTH_LENGTH equ 80d
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

		mov ax, 4c00h or code
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

SET_ES_VIDEOMEM	macro
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
MUL10BX		macro
			NOPE

			mov dx, bx 	;save bx val

			shl bx, 3d 	;bx = bx * 8
			shl dx, 1d	;dx = dx * 2

			add bx, dx	
			
			NOPE
			endm

Start:
	
	SET_ES_VIDEOMEM

	mov si, START_INPUT_PARAM + 1d ;set pointer to start input arguments
	call ParceInputArguments

	;Get frame's hight and width
	mov si, message_address_start  ;addres start string
	call GetFrameParam 

	call CalcFrameAddres
	
	mov di, frame_addres_in_vidmem

	mov dh, frame_hight
	mov dl, frame_width	

	push dx				;frame's hight and width
	push frame_sample 	;frame's sample 
	push frame_color	;frame's color
	call DrawFrame

	mov di, frame_addres_in_vidmem ;restore frame's address
	add di, WIDTH_LENGTH * 2d + 2d ;start address to write message in frame

	mov si, message_address_start

	call PrintToMemory

	Exit 0

	;-----------------------------------------------------------------
	;Get the arguments of the command line
	;-----------------------------------------------------------------
	;Assumes: si = start addres input arguments
	;Entry: none
	;Exit: frame_color, frame_sample message_address_start
	;Destroy: ax, bx, cx, dx, si, di
	;-----------------------------------------------------------------
	ParceInputArguments proc

		;(Read frame's color)
		call ParceDecNum 	  ;frame's color
		mov frame_color, bx   ;Save read arguments
		;(Read frame's color)

		;(Read frame's mosamplede)
		call ParceDecNum 	  ;read frame's sample
		mov frame_sample, bx  ;save frame's sample
		;(Read frame's color)

		;define frame's sample
		mov message_address_start, si 	;save the address where the string starts

		cmp frame_sample, 0d  ;check frame's sample = 0
		jne @@else
			mov frame_sample, si
			add message_address_start, 10d ;shift to start string address
			jmp @@endif

		@@else:	;installings samples
			dec frame_sample
			mov bx, frame_sample
			
			mov al, 09d
			mul bl		 ;calc sample's shift
			xchg ax, bx 

			add bx, offset frameSample1 ;set sample

			mov frame_sample, bx

		@@endif:
		;define frame's sample
		
		ret
	ParceInputArguments endp

	;-----------------------------------------------------------------
	;Draw frame in video memory
	;-----------------------------------------------------------------
	;Param: [bp + 4] - frame's color, [bp + 6] - frame's sample
	;		[bp + 8] - frame's width, [bp + 9] - frame's highth
	;Assumes: es = VIDMEM_ADR, ds = sourse segment
	;Entry: di (destination index), si (source index)
	;Exit: none
	;Destroy: ax, ch, cl, di, si
	;-----------------------------------------------------------------
	DrawFrame	proc
		push bp
		mov bp, sp
		
		mov ah, byte ptr [bp + 4] ;define frame's color
		mov si, word ptr [bp + 6] ;define frame's sample

		xor cx, cx 	;free cx

		mov cl, byte ptr [bp + 8] 	;cx = widthGetFrameParam
		call DrawLine

		mov cl, byte ptr [bp + 8]	;expansion to word
		shl cl, 1d  ;change shift

		sub di, cx	;return di to start line
		add di, WIDTH_LENGTH * 2d ;next line

		sub byte ptr [bp + 9], 2d 	;hight -= 2

		@@next:
			mov cl, byte ptr [bp + 8] 	;cx = width
			call DrawLine
		
			sub si, 3d	;return si to cur 3 sybmbols

			mov cl, byte ptr [bp + 8]	;expansion to word
			shl cl, 1d  ;change shift
			
			sub di, cx	;return di to start line
			add di, WIDTH_LENGTH * 2d ;next line
			
		dec byte ptr [bp + 9] 		;hight--
		cmp byte ptr [bp + 9], 0d	;-----------------------
		jne @@next  ;check condition вр != 0
		
		add si, 3d  ;next three symbol

		mov cl, byte ptr [bp + 8] 	;cx = width
		call DrawLine

		pop bp
		ret 2d * 3d
		
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

		lodsb		;save symbol to al
		stosw 		;draw left symbol

		sub cx, 2d	;counter -= 2	
		lodsb		;save symbol to al
		rep stosw 	;draw middle symbol

		lodsb		;save symbol to al
		stosw 		;draw right symbol

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

			cmp al, '\' ;check al is '\'
			jne @@changeColor
				xor ch, ch ;free ch
				sub di, cx ;shift to start line

				add di, WIDTH_LENGTH * 2d ;next line
				
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
	;Exit: frame_width, frame_hight
	;Destroy: al, ch, cl
	;-----------------------------------------------------------------
	GetFrameParam	proc
		mov frame_hight, 0 ;free frame's hight
		mov frame_width, 0 ;free frame's width
		
		xor ch, ch ;free ch
		@@next:

			lodsb  ;save byte to al from arguments line
			inc ch ;counter++

			cmp al, '$' ;check al is '$'
			je @@break 

			cmp al, '\'		;check symbol al is '\'
			jne @@next
				inc frame_hight ;frame's hight++
				
				mov cl, ch	
				xor ch, ch	;free counter

				cmp cl, frame_width ;check cur_width < max_width (find max width).
				jbe @@next 
					mov frame_width, cl

		jmp @@next
		
		@@break: 

		add frame_hight, 2d ;frame border
		add frame_width, 2d ;frame border

		ret	
		
	GetFrameParam	endp

	;-----------------------------------------------------------------
	;Calc frames addres in vidmem
	;-----------------------------------------------------------------
	;Assume: frame_hight, frame_width
	;Entry: si
	;Exit: frame_width, frame_hight
	;Destroy: bx
	;-----------------------------------------------------------------
	CalcFrameAddres	proc
		mov frame_addres_in_vidmem, MIDDLE_SCREEN * 2d;
	
		xor bx, bx
		mov bl, frame_hight	;shift by y coord
		MUL10BX	   ;bx * 10
		shl bx, 3d ;calc number videomem line
		shl bx, 1d
		
		add bl, frame_width ;shift by x coord
		and bx, 0FFFEh		;parity byte
		sub frame_addres_in_vidmem, bx; 
	
		ret	
		
	CalcFrameAddres	endp

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

			MUL10BX 		;bx * 10
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

			lodsb ;save byte to al from arguments line

		cmp al, ' ' 	;check condition al == ' '
 		je @@isSpace	
		
		cmp al, 09h 	;check condition al == '\t'
		je @@isSpace	

		dec si 	;last symbol wasn't space
		
		ret	
		
	SkipSpace	endp


.data 
	frame_width db 0
	frame_hight db 0

	frame_color  dw 0
	frame_sample dw 0

	message_address_start  dw 0
	frame_addres_in_vidmem dw 0

.const 
frameSample1 db 0dah, 0c4h, 0bfh, 0c3h, 20h, 0b4h, 0c0h, 0c4h, 0d9h  ;patern for frame
frameSample2 db 0feh, 0cdh, 0feh, 0b3h, 020h, 0b3h, 0feh, 0cdh, 0feh ;patern for frame
frameSample3 db 3d dup (03h), 03h, 20h, 03h, 3d dup (03h)		    ;patern for frame

NotDigiSymbol db 0dh, "ERROR: misrepresentation of a number", 0dh, '$'

end Start