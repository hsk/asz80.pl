		.area	_DATA
		.area	_CODE

usr::
		inc     hl
		inc     hl
		ld		a, (hl)
		inc     a
		ld      (hl),a
		ret
