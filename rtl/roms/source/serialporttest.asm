        .org    0               ; Start at address 0

init:	mvi	A, 03h	; Reset the 2SIO port 1
		out	10h
		mvi	A, 11h	; Set port 1 to 8,2,n
		out	10h

loop:	in	10h	; wait for a character to arrive
		rrc
		jnc	loop

		in	11h	; get the character
		out	11h	; send it back out
		jmp	loop	; continue forever

		.end
		