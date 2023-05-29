	org(0xc000).
	inc(hl).
	inc(hl).
	ld(a,[hl]).
	inc(a).
	ld([hl],a).
	ret.
