		.area	_DATA
		.area	_CODE
GTSTCK	=		0x00D5
usr::
		inc     hl
		inc     hl
		push    hl
		ld		a, (hl)
		call	GTSTCK
		pop     hl
		ld      (hl),a
		ret
