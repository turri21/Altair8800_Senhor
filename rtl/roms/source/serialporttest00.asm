        .org    0               ; Start at address 0

init:	mvi	A, 03h	; Reset the 2SIO port 1
		out	10h
		mvi	A, 01h	; Set port 1 to 8,2,n
		out	00h

loop:	in	00h	; wait for a character to arrive
		rrc
		jnc	loop

		in	01h	; get the character
		out	01h	; send it back out
		jmp	loop	; continue forever

		.end
		