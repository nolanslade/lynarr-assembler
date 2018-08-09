; Name:	Nolan Slade
; 2015-11-14

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; P R O G R A M   S T A R T ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

%include "asm_io.inc"
MAXARGS equ 2	; Maximum allowable arguments
MAXLEN	equ	20 	; Maximum allowable string length

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

section .data
errArgMessage 	db 	"Error: incorrect argument count.",0,10			; Error for argument count =! 2
errLenMessage 	db 	"Error: incorrect input length.",0,10			; Error for string length not within 1-20
errCharMessage 	db 	"Error: invalid character encountered.",0,10	; Error for invalid ASCII input

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

section .bss
X 				resb 	20 	; 20 bytes for 'X' : the byte array
Y 				resd 	20 	; 80 bytes for 'Y' : the integer array
N	 			resd 	1 	; 4 bytes for integer for stringLength
currentChar 	resd 	1 	; Temporary variable for filling
fillCounter 	resd 	1	; Counter for number of elements filled
lyndonCounter 	resd 	1 	; Counter for number of lyndons found
displayCounter 	resd 	1 	; Counter for number of elements displayed
flag 			resd 	1 	; Flag for display (byte/integer)
intgOffset		resd 	1 	; Offset for storing integers and reading integers (multiples of 4)

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

section .text
global asm_main

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; A S M _ M A I N ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

asm_main:				; Main routine
	enter 	0, 0		; Standard setup routine
	pusha				; Save all registers, push onto stack

	mov 	eax, dword [ebp+8] 		; Number of arguments
	cmp 	eax, MAXARGS 			; Compare number of arguments with arguments expected
	je 		numArgsValid 			; If the arguments are correct, proceed with program
	mov 	eax, errArgMessage 		; If the arguments are not correct, we display an error
	call 	print_string			; Print the error
	call 	print_nl				; Print a new line for cleanliness of terminal
	jmp 	mainEnd 				; Commence program termination
	
	numArgsValid:
		mov 	eax, dword [ebp+12] 	; location of arguments
		mov 	ebx, dword [eax+4] 		; first arg
		mov 	[N], dword 0 			; initialize length to 0
		jmp 	checkCharLoop			; check for valid input chars

	checkCharLoop:
		mov 	al, byte[ebx]		; Load current character
		cmp 	al, 0				; End of string reached
		je 		fillArray			; Proceed to load byte array X
		
		cmp 	al, 'z'
		jg 		charNotValid		; ASCII value too high
		
		cmp 	al, 'a'
		jl 		charNotValid 		; ASCII value too low

		add 	[N], dword 1 		; One more valid character found
		cmp 	dword [N], MAXLEN 	; Find if length exceeds max
		jg 		lenStringNotValid 	; Error

		inc 	ebx 				; Prepare for next char
		jmp 	checkCharLoop		; Check next char

	fillArray:
		cmp 	[N], dword 0		; Ensure no blank string passed
		je 		lenStringNotValid 	; Error

		mov 	[fillCounter], dword 0		; Counter for filling the array X
		mov 	eax, dword [ebp+12]			; Location of arguments
		mov 	ebx, dword [eax+4]			; First argument
		mov 	[currentChar], dword ebx 	; Store address of first argument
		
		fillLoop:
			mov 	ebx, dword [currentChar]	; Current position 
			cmp 	byte[ebx], 0				; Compare to null to determine if finished
			je 		endFill						; Null reached, we may stop filling

			mov 	ecx, X						; Ecx now holds the base address of byte array X
			add 	ecx, dword [fillCounter]	; Offset ecx by the number of elements filled
			mov 	al, byte [ebx]				; Prepare character to be loaded
			mov 	[ecx], al					; Element loaded into the address value held by ecx
			add 	[fillCounter], dword 1		; Increment number of elements filled
			add 	[currentChar], dword 1		; Increment current character
			jmp 	fillLoop					; Keep filling until necessary

		endFill:						; Filling complete
			mov 	[flag], dword 0		; Flag is set to 0 to signify byte array printing
			jmp 	display				; Display is jumped to, with parameter flag = 0
		startLyndonProcess:				; Move to next step
			jmp 	startLyndon			; Jump to commence lyndon computation

	startLyndon:
		mov 	[lyndonCounter], dword 0		; K value such that 0 <= k <= [stringLength-1]
		jmp 	maxLyn							; Compute all integer Lyndons, and load them into array Y
		endOfLyndon:
		mov 	[flag], dword 1 				; We want to print integers, set flag to 1
		jmp 	display							; Display the array of integers
		endOfDisplay:
		cmp 	[flag], dword 1					; Just comparing to ensure proper exit (theoretically not necessary)
		je 	mainEnd 							; Commence termination
		
	charNotValid:
		mov 	eax, errCharMessage
		call 	print_string			; Generate error
		call 	print_nl
		jmp 	mainEnd 				; Commence termination

	lenStringNotValid:
		mov 	eax, errLenMessage
		call	print_string			; Generate error
		call 	print_nl
		jmp 	mainEnd					; Commence termination

	mainEnd:
		popa 						; Standard restore all regs
		mov 	eax, 0				; Return back to caller
		leave 
		ret							; End of program

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; ; M A X L Y N ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;

maxLyn:
	enter 	0, 0 	; Standard start routine
	pusha			; Save all registers

	; Z = byte array X
	; n = [N]
	; k = [lyndonCounter]
	; p = ecx
	; iterator i = edx
	; edi used for i-p
	; eax = Z[i-p]
	; ebx = Z[i]

	mov 	[intgOffset], dword 0 		; Initialize the offset for integers (increments of 4 bytes)

	initialize:
	mov 	edx, [lyndonCounter]		; Initalization process for number of lyndons found.
										; Will reset for each maxLyndon found.

	loopStart:
		mov 	ecx, [N]			; Ecx now holds the numerical length of string
		sub 	ecx, dword 1		; Subtract one to get N-1
		cmp 	ecx, edx			; Compare i to N-1
		mov 	ecx, dword 1		; Restore to true value
		je 		storeValue			; Commence storing if i = N-1

		; Else, we move to the for loop
		forLoop:
			add 	edx, dword 1		
			cmp 	[N], edx		; We need to break the loop if edx has reached [stringLength]
			je 		storeValue		; Store the value and restart process if necessary

			mov 	edi, edx		; edi = i
			sub 	edi, ecx		; edi = i - p
				
			mov 	eax, dword 0	; Finding Z[i-p]
			add 	eax, X			; Eax = base address of X
			add 	eax, edi		; Add i - p to that address
			mov 	al, byte[eax] 	; We are now at the correct position, al now holds Z[i-p]

			mov 	ebx, dword 0	; Finding Z[i]
			add 	ebx, X			; Ebx = base address of X
			add 	ebx, edx		; Add i
			mov 	bl, byte[ebx]	; We are now at the correct position, bl now holds Z[i]

			cmp 	al, bl				; Compare Z[i-p] to Z[i]
			jne 	innerConditional	; Further steps required
			jmp		forLoop 			; Next iteration of for loop, as Z[i-p] = Z[i]

		innerConditional:
			cmp 	al, bl			; We are to store if Z[i-p] > Z[i]
			jg 		storeValue		; Proceed to store
	
			mov 	ecx, 0					; Reset p
			add 	ecx, edx				; p = i
			add 	ecx, dword 1			; p = i + 1
			sub 	ecx, [lyndonCounter]	; p = i + 1 - k
			jmp 	forLoop					; Next iteration of for loop

	storeValue:
		mov 	ebx, dword [lyndonCounter]		; Ebx now holds number of lyndons found
		mov 	eax, dword [N]					; Eax now holds the length of the input string
		cmp 	eax, ebx						; Compare string length to number of lyndons found
		je 		maxLynEnd						; If the number of lyndonds found = string length, we know we
												; have finished all necessary lyndon computation. Next step.
		; Else, we have storing to do
		mov 	eax, dword 0					; Reset eax
		add 	eax, Y							; Eax now holds the base address of integer array Y
		add 	eax, [intgOffset]				; Add the appropriate offset, which is a multiple of 4 bytes
		add 	[intgOffset], dword 4			; Next offset of four, for next dword.

		mov 	[eax], ecx						; Store value
		add 	[lyndonCounter], dword 1		; Increase number of found lyndons for next maxLyn
		jmp 	initialize 						; to next k

	maxLynEnd:	
		popa									; Restore all registers
		jmp 	endOfLyndon						; Commence next process

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; D I S P L A Y ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;


display:
	enter 	0, 0 		; Standard startup routine
	pusha				; Save all registers

	; esi = location of array referenced (x or y)
	; edi = stopping point (number of elements - 1 = stringlength - 1)
	; If flag = 0; display byte array X. If flag = 1, display int array Y.

	; Depending on the flag, we will display either X or Y, accordingly.
	
	mov 	edi, dword [N]
	sub 	edi, dword 1					; Edi = number of elements - 1
	mov 	[displayCounter], dword 0
	mov 	esi, X							; We will display the strings first
	cmp 	[flag], dword 0
	je 		displayString					; If flag is 0, we need to display the input string.
	mov 	edi, dword [N]
	mov 	esi, y
	mov 	[intgOffset], dword 0
	jmp 	displayIntegers					; Else, we will display integers.
	
	displayString:
		cmp 	[displayCounter], edi		; We need to determine if we have displayed all characters
		jg 		displayEndString			; End of display process. Onto next step.

		mov 	edx, X						; Edx holds base address of byte array X
		add 	edx, dword [displayCounter]	; Edx now holds the address which we need to load a character from
		mov 	al, byte [edx]				; Load the character to be printed
		call 	print_char					; Print the character
		add 	[displayCounter], dword 1	; Increment the number of chars we have displayed
		jmp 	displayString				; Restart loop

	displayEndString:
		call 	read_char					; Enter key will start next step
		popa 								; Restore all registers
		jmp 	startLyndonProcess 			; Jump back to start the process of lyndon calculations

	displayIntegers:
		cmp 	[displayCounter], edi		; See if we have displayed all integers
		jge 	displayEndInteger			; Jump if this is the case

		mov 	edx, Y						; Edx now holds the base address of the integer array Y
		add 	edx, dword [intgOffset]		; Add the offset to ensure we are displaying the correct element
		mov 	eax, dword [edx]			; Eax now holds dword numerical value of edx (maxLyndon)
		call 	print_int					; Print the maxLyndon
		mov 	al, ' '						; Space separator for next integer printed
		call 	print_char					; Print the space
		add 	[intgOffset], dword 4		; Increase the offset for next integer to be displayed.
		add 	[displayCounter], dword 1	; Increase the counter for the amount of integers we have displayed
		jmp 	displayIntegers				; Repeat the loop

	displayEndInteger:
		call 	read_char			; Enter key will start next step
		popa						; Restore all registers
		jmp 	endOfDisplay		; Proceed towards termination of program

; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; P R O G R A M   E N D ; ; ; ; ; ; ; ; ;
; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ; ;	
