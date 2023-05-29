:- use_module(library(readutil)).
:- dynamic(label/2).

set_gvar(Name, X) :- retractall(gvar(Name,_)), asserta(gvar(Name, X)).
mod_gvar(Name, X,X2,Pred):- gvar(Name, X),call(Pred),set_gvar(Name, X2).

sub(A,B,C) :- integer(A),integer(B),integer(C),C is A-B,!.  % 逆変換可能な差の計算
sub(A,B,C) :- var(C),integer(A),integer(B),C is A-B,!.
sub(A,B,C) :- var(A),integer(B),integer(C),A is B+C,!.
sub(A,B,C) :- var(B),integer(A),integer(C),B is A-C,!.

check_error:- gvar(count,I), I1 is I + 1, format('~d 行目辺り：範囲エラー~n', I1), halt, !.

p7(D,S):- label(D,S), 0=<S, S=<127, !.
p7(D,D):- integer(D), 0=<D, D=<127, !.
p7(D,***):- atom(D), !.
p7(_,_):- check_error.

m7(D,S1):- label(D,S), 0=<S, S=<128, sub(0x100,S,S1), !.
m7(D,D1):- integer(D), 0=<D, D=<128, sub(0x100,D,D1), !.
m7(D,***):- atom(D), !.
m7(_,_):- check_error.

ui8(N,S):- label(N,S), 0=<S, S=<255, !.
ui8(N,S2):- label(N,S), -128=<S, S<0, S1 is S * -1, sub(0x100,S1,S2), !.
ui8(N,N):- integer(N), 0=<N, N=<255, !.
ui8(N,N2):- integer(N), -128=<N, N<0, N1 is N * -1, sub(0x100,N1,N2), !.
ui8(N,***):- atom(N), !.
ui8(_,_):- check_error.

u16(LM,M,L):- label(LM,A), 0=<A,A=<0xffff,M is A mod 0x100, L is A // 0x100, !.
u16(LM,M,L):- integer(LM), 0=<LM,LM=<0xffff,M is LM mod 0x100, L is LM // 0x100, !.
u16(LM,***,***):- atom(LM), !.
u16(_,_,_):- check_error.

short_jmp(E,S):- label(E,E1), gvar(address, A), S is E1 - A - 2, 0=<S, S=<127, !.
short_jmp(E,S2):- label(E,E1), gvar(address, A), S is E1 - A - 2, -128=<S, S<0, S1 is S*(-1), sub(0x100,S1,S2), !.
short_jmp(E,_):- integer(E), gvar(count,I), I1 is I + 1, format('~d 行辺り。相対ジャンプ先は、ラベルを使って下さい。~n',I1), halt.
short_jmp(E,_):- atom(E), !.
short_jmp(_,_):- gvar(count,I), I1 is I + 1, format('~d 行辺り。アドレス範囲エラー~n', I1), halt.

map(end, []):- !.
map(label(L), []):- gvar(address,Address), retractall(label(L,_)),!, asserta(label(L,Address)), !.
% 擬似命令
map(org(Ad), []):- set_gvar(address,Ad), !.
map(equ(E,Exp), []):- X is Exp, retractall(label(E,_)), !, asserta(label(E,X)), !.
map(defs(S), []):- gvar(address,Ad), A is Ad + S, set_gvar(address,A), !.
map(defb(B), B):- B=[A], A=<0xff,!.
map(defb(B), P1):- B=[A|Z], A=<0xff, map(defb(Z), P), append([A],P,P1),!.
map(defw(W), [M,L]):- W=[A], A=<0xffff, M is A mod 0x100, L is A // 0x100, !.
map(defw(W), P1):- W=[A|Z], A=<0xffff, M is A mod 0x100, L is A // 0x100, map(defw(Z),P),append([M,L],P,P1), !.
%
map(adc(a,a), [0x8f]):- !.
map(adc(a,b), [0x88]):- !.
map(adc(a,c), [0x89]):- !.
map(adc(a,d), [0x8a]):- !.
map(adc(a,e), [0x8b]):- !.
map(adc(a,h), [0x8c]):- !.
map(adc(a,l), [0x8d]):- !.
map(adc(a,[hl]), [0x8e]):- !.
map(adc(a,[ix+D]), [0xdd,0x8e,A]):- p7(D,A), !.
map(adc(a,[ix-D]), [0xdd,0x8e,A]):- m7(D,A), !.
map(adc(a,[iy+D]), [0xfd,0x8e,A]):- p7(D,A), !.
map(adc(a,[iy-D]), [0xfd,0x8e,A]):- m7(D,A), !.
map(adc(a,N), [0xce,A]):- ui8(N,A), !.
map(adc(hl,bc), [0xed,0x4a]):- !.
map(adc(hl,de), [0xed,0x5a]):- !.
map(adc(hl,hl), [0xed,0x6a]):- !.
map(adc(hl,sp), [0xed,0x7a]):- !.
map(add(a,a), [0x87]):- !.
map(add(a,b), [0x80]):- !.
map(add(a,c), [0x81]):- !.
map(add(a,d), [0x82]):- !.
map(add(a,e), [0x83]):- !.
map(add(a,h), [0x84]):- !.
map(add(a,l), [0x85]):- !.
map(add(a,[hl]), [0x86]):- !.
map(add(a,[ix+D]), [0xdd,0x86,A]):- p7(D,A), !.
map(add(a,[ix-D]), [0xdd,0x86,A]):- m7(D,A), !.
map(add(a,[iy+D]), [0xfd,0x86,A]):- p7(D,A), !.
map(add(a,[iy-D]), [0xfd,0x86,A]):- m7(D,A), !.
map(add(a,N), [0xc6,A]):- ui8(N,A), !.
map(add(hl,bc),[0x09]):- !.
map(add(hl,de),[0x19]):- !.
map(add(hl,hl),[0x29]):- !.
map(add(hl,sp),[0x39]):- !.
map(add(ix,bc),[0xdd,0x09]):- !.
map(add(ix,de),[0xdd,0x19]):- !.
map(add(ix,ix),[0xdd,0x29]):- !.
map(add(ix,sp),[0xdd,0x39]):- !.
map(add(iy,bc),[0xfd,0x09]):- !.
map(add(iy,de),[0xfd,0x19]):- !.
map(add(iy,iy),[0xfd,0x29]):- !.
map(add(iy,sp),[0xfd,0x39]):- !.
map(and(a), [0xa7]):- !.
map(and(b), [0xa0]):- !.
map(and(c), [0xa1]):- !.
map(and(d), [0xa2]):- !.
map(and(e), [0xa3]):- !.
map(and(h), [0xa4]):- !.
map(and(l), [0xa5]):- !.
map(and([hl]), [0xa6]):- !.
map(and([ix+D]), [0xdd,0xa6,A]):- p7(D,A), !.
map(and([ix-D]), [0xdd,0xa6,A]):- m7(D,A), !.
map(and([iy+D]), [0xfd,0xa6,A]):- p7(D,A), !.
map(and([iy-D]), [0xfd,0xa6,A]):- m7(D,A), !.
map(and(N), [0xe6,A]):- ui8(N,A), !.
map(bit(0,a), [0xcb,0x47]):- !.
map(bit(0,b), [0xcb,0x40]):- !.
map(bit(0,c), [0xcb,0x41]):- !.
map(bit(0,d), [0xcb,0x42]):- !.
map(bit(0,e), [0xcb,0x43]):- !.
map(bit(0,h), [0xcb,0x44]):- !.
map(bit(0,l), [0xcb,0x45]):- !.
map(bit(0,[hl]), [0xcb,0x46]):- !.
map(bit(0,[ix+D]), [0xdd,0xcb,A,0x46]):- p7(D,A), !.
map(bit(0,[ix-D]), [0xdd,0xcb,A,0x46]):- m7(D,A), !.
map(bit(0,[iy+D]), [0xfd,0xcb,A,0x46]):- p7(D,A), !.
map(bit(0,[iy-D]), [0xfd,0xcb,A,0x46]):- m7(D,A), !.
map(bit(1,a), [0xcb,0x4f]):- !.
map(bit(1,b), [0xcb,0x48]):- !.
map(bit(1,c), [0xcb,0x49]):- !.
map(bit(1,d), [0xcb,0x4a]):- !.
map(bit(1,e), [0xcb,0x4b]):- !.
map(bit(1,h), [0xcb,0x4c]):- !.
map(bit(1,l), [0xcb,0x4d]):- !.
map(bit(1,[hl]), [0xcb,0x4e]):- !.
map(bit(1,[ix+D]), [0xdd,0xcb,A,0x4e]):- p7(D,A), !.
map(bit(1,[ix-D]), [0xdd,0xcb,A,0x4e]):- m7(D,A), !.
map(bit(1,[iy+D]), [0xfd,0xcb,A,0x4e]):- p7(D,A), !.
map(bit(1,[iy-D]), [0xfd,0xcb,A,0x4e]):- m7(D,A), !.
map(bit(2,a), [0xcb,0x57]):- !.
map(bit(2,b), [0xcb,0x50]):- !.
map(bit(2,c), [0xcb,0x51]):- !.
map(bit(2,d), [0xcb,0x52]):- !.
map(bit(2,e), [0xcb,0x53]):- !.
map(bit(2,h), [0xcb,0x54]):- !.
map(bit(2,l), [0xcb,0x55]):- !.
map(bit(2,[hl]), [0xcb,0x56]):- !.
map(bit(2,[ix+D]), [0xdd,0xcb,A,0x56]):- p7(D,A), !.
map(bit(2,[ix-D]), [0xdd,0xcb,A,0x56]):- m7(D,A), !.
map(bit(2,[iy+D]), [0xfd,0xcb,A,0x56]):- p7(D,A), !.
map(bit(2,[iy-D]), [0xfd,0xcb,A,0x56]):- m7(D,A), !.
map(bit(3,a), [0xcb,0x5f]):- !.
map(bit(3,b), [0xcb,0x58]):- !.
map(bit(3,c), [0xcb,0x59]):- !.
map(bit(3,d), [0xcb,0x5a]):- !.
map(bit(3,e), [0xcb,0x5b]):- !.
map(bit(3,h), [0xcb,0x5c]):- !.
map(bit(3,l), [0xcb,0x5d]):- !.
map(bit(3,[hl]), [0xcb,0x5e]):- !.
map(bit(3,[ix+D]), [0xdd,0xcb,A,0x5e]):- p7(D,A), !.
map(bit(3,[ix-D]), [0xdd,0xcb,A,0x5e]):- m7(D,A), !.
map(bit(3,[iy+D]), [0xfd,0xcb,A,0x5e]):- p7(D,A), !.
map(bit(3,[iy-D]), [0xfd,0xcb,A,0x5e]):- m7(D,A), !.
map(bit(4,a), [0xcb,0x67]):- !.
map(bit(4,b), [0xcb,0x60]):- !.
map(bit(4,c), [0xcb,0x61]):- !.
map(bit(4,d), [0xcb,0x62]):- !.
map(bit(4,e), [0xcb,0x63]):- !.
map(bit(4,h), [0xcb,0x64]):- !.
map(bit(4,l), [0xcb,0x65]):- !.
map(bit(4,[hl]), [0xcb,0x66]):- !.
map(bit(4,[ix+D]), [0xdd,0xcb,A,0x66]):- p7(D,A), !.
map(bit(4,[ix-D]), [0xdd,0xcb,A,0x66]):- m7(D,A), !.
map(bit(4,[iy+D]), [0xfd,0xcb,A,0x66]):- p7(D,A), !.
map(bit(4,[iy-D]), [0xfd,0xcb,A,0x66]):- m7(D,A), !.
map(bit(5,a), [0xcb,0x6f]):- !.
map(bit(5,b), [0xcb,0x68]):- !.
map(bit(5,c), [0xcb,0x69]):- !.
map(bit(5,d), [0xcb,0x6a]):- !.
map(bit(5,e), [0xcb,0x6b]):- !.
map(bit(5,h), [0xcb,0x6c]):- !.
map(bit(5,l), [0xcb,0x6d]):- !.
map(bit(5,[hl]), [0xcb,0x6e]):- !.
map(bit(5,[ix+D]), [0xdd,0xcb,A,0x6e]):- p7(D,A), !.
map(bit(5,[ix-D]), [0xdd,0xcb,A,0x6e]):- m7(D,A), !.
map(bit(5,[iy+D]), [0xfd,0xcb,A,0x6e]):- p7(D,A), !.
map(bit(5,[iy-D]), [0xfd,0xcb,A,0x6e]):- m7(D,A), !.
map(bit(6,a), [0xcb,0x77]):- !.
map(bit(6,b), [0xcb,0x70]):- !.
map(bit(6,c), [0xcb,0x71]):- !.
map(bit(6,d), [0xcb,0x72]):- !.
map(bit(6,e), [0xcb,0x73]):- !.
map(bit(6,h), [0xcb,0x74]):- !.
map(bit(6,l), [0xcb,0x75]):- !.
map(bit(6,[hl]), [0xcb,0x76]):- !.
map(bit(6,[ix+D]), [0xdd,0xcb,A,0x76]):- p7(D,A), !.
map(bit(6,[ix-D]), [0xdd,0xcb,A,0x76]):- m7(D,A), !.
map(bit(6,[iy+D]), [0xfd,0xcb,A,0x76]):- p7(D,A), !.
map(bit(6,[iy-D]), [0xfd,0xcb,A,0x76]):- m7(D,A), !.
map(bit(7,a), [0xcb,0x7f]):- !.
map(bit(7,b), [0xcb,0x78]):- !.
map(bit(7,c), [0xcb,0x79]):- !.
map(bit(7,d), [0xcb,0x7a]):- !.
map(bit(7,e), [0xcb,0x7b]):- !.
map(bit(7,h), [0xcb,0x7c]):- !.
map(bit(7,l), [0xcb,0x7d]):- !.
map(bit(7,[hl]), [0xcb,0x7e]):- !.
map(bit(7,[ix+D]), [0xdd,0xcb,A,0x7e]):- p7(D,A), !.
map(bit(7,[ix-D]), [0xdd,0xcb,A,0x7e]):- m7(D,A), !.
map(bit(7,[iy+D]), [0xfd,0xcb,A,0x7e]):- p7(D,A), !.
map(bit(7,[iy-D]), [0xfd,0xcb,A,0x7e]):- m7(D,A), !.
map(call(nz,LM), [0xc4,M,L]):- u16(LM,M,L), !.
map(call(z,LM), [0xcc,M,L]):- u16(LM,M,L), !.
map(call(nc,LM), [0xd4,M,L]):- u16(LM,M,L), !.
map(call(c,LM), [0xdc,M,L]):- u16(LM,M,L), !.
map(call(po,LM), [0xe4,M,L]):- u16(LM,M,L), !.
map(call(pe,LM), [0xec,M,L]):- u16(LM,M,L), !.
map(call(p,LM), [0xf4,M,L]):- u16(LM,M,L), !.
map(call(m,LM), [0xfc,M,L]):- u16(LM,M,L), !.
map(call(LM), [0xcd,M,L]):- u16(LM,M,L), !.
map(ccf, [0x3f]):- !.
map(cp(a), [0xbf]):- !.
map(cp(b), [0xb8]):- !.
map(cp(c), [0xb9]):- !.
map(cp(d), [0xba]):- !.
map(cp(e), [0xbb]):- !.
map(cp(h), [0xbc]):- !.
map(cp(l), [0xbd]):- !.
map(cp([hl]), [0xbe]):- !.
map(cp([ix+D]), [0xdd,0xbe,A]):- p7(D,A), !.
map(cp([ix-D]), [0xdd,0xbe,A]):- m7(D,A), !.
map(cp([iy+D]), [0xfd,0xbe,A]):- p7(D,A), !.
map(cp([iy-D]), [0xfd,0xbe,A]):- m7(D,A), !.
map(cp(N), [0xfe,A]):- ui8(N,A), !.
map(cpd, [0xed,0xa9]):- !.
map(cpdr, [0xed,0xb9]):- !.
map(cpi, [0xed,0xa1]):- !.
map(cpir, [0xed,0xb1]):- !.
map(cpl, [0x2f]):- !.
map(daa, [0x27]):- !.
map(dec(a), [0x3d]):- !.
map(dec(b), [0x05]):- !.
map(dec(c), [0x0d]):- !.
map(dec(d), [0x15]):- !.
map(dec(e), [0x1d]):- !.
map(dec(h), [0x25]):- !.
map(dec(l), [0x2d]):- !.
map(dec([hl]), [0x35]):- !.
map(dec([ix+D]), [0xdd,0x35,A]):- p7(D,A), !.
map(dec([ix-D]), [0xdd,0x35,A]):- m7(D,A), !.
map(dec([iy+D]), [0xfd,0x35,A]):- p7(D,A), !.
map(dec([iy-D]), [0xfd,0x35,A]):- m7(D,A), !.
map(dec(bc), [0x0b]):- !.
map(dec(de), [0x1b]):- !.
map(dec(hl), [0x2b]):- !.
map(dec(sp), [0x3b]):- !.
map(dec(ix), [0xdd,0x2b]):- !.
map(dec(iy), [0xfd,0x2b]):- !.
map(di, [0xf3]):- !.
map(djnz(E), [0x10,A]):- short_jmp(E,A), !.
map(ei, [0xfb]):- !.
map(ex([sp],hl), [0xe3]):- !.
map(ex([sp],ix), [0xdd,0xe3]):- !.
map(ex([sp],iy), [0xfd,0xe3]):- !.
map(ex(af,afd), [0x08]):- !.
map(ex(de,hl), [0xeb]):- !.
map(exx, [0xd9]):- !.
map(halt, [0x76]):- !.
map(im(0), [0xed,0x46]):- !.
map(im(1), [0xed,0x56]):- !.
map(im(2), [0xed,0x5e]):- !.
map(inc(a), [0x3c]):- !.
map(inc(b), [0x04]):- !.
map(inc(c), [0x0c]):- !.
map(inc(d), [0x14]):- !.
map(inc(e), [0x1c]):- !.
map(inc(h), [0x24]):- !.
map(inc(l), [0x2c]):- !.
map(inc([hl]), [0x34]):- !.
map(inc([ix+D]), [0xdd,0x34,A]):- p7(D,A), !.
map(inc([ix-D]), [0xdd,0x34,A]):- m7(D,A), !.
map(inc([iy+D]), [0xfd,0x34,A]):- p7(D,A), !.
map(inc([iy-D]), [0xfd,0x34,A]):- m7(D,A), !.
map(inc(bc), [0x03]):- !.
map(inc(de), [0x13]):- !.
map(inc(hl), [0x23]):- !.
map(inc(sp), [0x33]):- !.
map(inc(ix), [0xdd,0x23]):- !.
map(inc(iy), [0xfd,0x23]):- !.
map(in(a,[c]), [0xed,0x78]):- !.
map(in(b,[c]), [0xed,0x40]):- !.
map(in(c,[c]), [0xed,0x48]):- !.
map(in(d,[c]), [0xed,0x50]):- !.
map(in(e,[c]), [0xed,0x58]):- !.
map(in(h,[c]), [0xed,0x60]):- !.
map(in(l,[c]), [0xed,0x68]):- !.
map(in(a,[N]), [0xdb,A]):- ui8(N,A), !.
map(cp(N), [0xfe,A]):- ui8(N,A), !.
map(ind, [0xed,0xaa]):- !.
map(indr, [0xed,0xba]):- !.
map(ini, [0xed,0xa2]):- !.
map(inir, [0xed,0xb2]):- !.
map(jp([hl]), [0xe9]):- !.
map(jp([ix]), [0xdd,0xe9]):- !.
map(jp([iy]), [0xfd,0xe9]):- !.
map(jp(LM), [0xc3,M,L]):- u16(LM,M,L), !.
map(jp(nz,LM), [0xc2,M,L]):- u16(LM,M,L), !.
map(jp(z,LM), [0xca,M,L]):- u16(LM,M,L), !.
map(jp(nc,LM), [0xd2,M,L]):- u16(LM,M,L), !.
map(jp(c,LM), [0xda,M,L]):- u16(LM,M,L), !.
map(jp(po,LM), [0xe2,M,L]):- u16(LM,M,L), !.
map(jp(pe,LM), [0xea,M,L]):- u16(LM,M,L), !.
map(jp(p,LM), [0xf2,M,L]):- u16(LM,M,L), !.
map(jp(m,LM), [0xfa,M,L]):- u16(LM,M,L), !.
map(jr(E), [0x18,A]):- short_jmp(E,A), !.
map(jr(nz,E), [0x20,A]):- short_jmp(E,A), !.
map(jr(z,E), [0x28,A]):- short_jmp(E,A), !.
map(jr(nc,E), [0x30,A]):- short_jmp(E,A), !.
map(jr(c,E), [0x38,A]):- short_jmp(E,A), !.
map(ld(a,i), [0xed,0x57]):- !.
map(ld(a,r), [0xed,0x5f]):- !.
map(ld(a,a), [0x7f]):- !.
map(ld(a,b), [0x78]):- !.
map(ld(a,c), [0x79]):- !.
map(ld(a,d), [0x7a]):- !.
map(ld(a,e), [0x7b]):- !.
map(ld(a,h), [0x7c]):- !.
map(ld(a,l), [0x7d]):- !.
map(ld(a,[bc]), [0x0a]):- !.
map(ld(a,[de]), [0x1a]):- !.
map(ld(a,[hl]), [0x7e]):- !.
map(ld(a,[ix+D]), [0xdd,0x7e,A]):- p7(D,A), !.
map(ld(a,[ix-D]), [0xdd,0x7e,A]):- m7(D,A), !.
map(ld(a,[iy+D]), [0xfd,0x7e,A]):- p7(D,A), !.
map(ld(a,[iy-D]), [0xfd,0x7e,A]):- m7(D,A), !.
map(ld(a,[LM]), [0x3a,M,L]):- u16(LM,M,L), !.
map(ld(a,N), [0x3e,A]):- ui8(N,A), !.
map(ld(b,a), [0x47]):- !.
map(ld(b,b), [0x40]):- !.
map(ld(b,c), [0x41]):- !.
map(ld(b,d), [0x42]):- !.
map(ld(b,e), [0x43]):- !.
map(ld(b,h), [0x44]):- !.
map(ld(b,l), [0x45]):- !.
map(ld(b,[hl]), [0x46]):- !.
map(ld(b,[ix+D]), [0xdd,0x46,A]):- p7(D,A), !.
map(ld(b,[ix-D]), [0xdd,0x46,A]):- m7(D,A), !.
map(ld(b,[iy+D]), [0xfd,0x46,A]):- p7(D,A), !.
map(ld(b,[iy-D]), [0xfd,0x46,A]):- m7(D,A), !.
map(ld(b,N), [0x06,A]):- ui8(N,A), !.
map(ld(c,a), [0x4f]):- !.
map(ld(c,b), [0x48]):- !.
map(ld(c,c), [0x49]):- !.
map(ld(c,d), [0x4a]):- !.
map(ld(c,e), [0x4b]):- !.
map(ld(c,h), [0x4c]):- !.
map(ld(c,l), [0x4d]):- !.
map(ld(c,[hl]), [0x4e]):- !.
map(ld(c,[ix+D]), [0xdd,0x4e,A]):- p7(D,A), !.
map(ld(c,[ix-D]), [0xdd,0x4e,A]):- m7(D,A), !.
map(ld(c,[iy+D]), [0xfd,0x4e,A]):- p7(D,A), !.
map(ld(c,[iy-D]), [0xfd,0x4e,A]):- m7(D,A), !.
map(ld(c,N), [0x0e,A]):- ui8(N,A), !.
map(ld(d,a), [0x57]):- !.
map(ld(d,b), [0x50]):- !.
map(ld(d,c), [0x51]):- !.
map(ld(d,d), [0x52]):- !.
map(ld(d,e), [0x53]):- !.
map(ld(d,h), [0x54]):- !.
map(ld(d,l), [0x55]):- !.
map(ld(d,[hl]), [0x56]):- !.
map(ld(d,[ix+D]), [0xdd,0x56,A]):- p7(D,A), !.
map(ld(d,[ix-D]), [0xdd,0x56,A]):- m7(D,A), !.
map(ld(d,[iy+D]), [0xfd,0x56,A]):- p7(D,A), !.
map(ld(d,[iy-D]), [0xfd,0x56,A]):- m7(D,A), !.
map(ld(d,N), [0x16,A]):- ui8(N,A), !.
map(ld(e,a), [0x5f]):- !.
map(ld(e,b), [0x58]):- !.
map(ld(e,c), [0x59]):- !.
map(ld(e,d), [0x5a]):- !.
map(ld(e,e), [0x5b]):- !.
map(ld(e,h), [0x5c]):- !.
map(ld(e,l), [0x5d]):- !.
map(ld(e,[hl]), [0x5e]):- !.
map(ld(e,[ix+D]), [0xdd,0x5e,A]):- p7(D,A), !.
map(ld(e,[ix-D]), [0xdd,0x5e,A]):- m7(D,A), !.
map(ld(e,[iy+D]), [0xfd,0x5e,A]):- p7(D,A), !.
map(ld(e,[iy-D]), [0xfd,0x5e,A]):- m7(D,A), !.
map(ld(e,N), [0x1e,A]):- ui8(N,A), !.
map(ld(h,a), [0x67]):- !.
map(ld(h,b), [0x60]):- !.
map(ld(h,c), [0x61]):- !.
map(ld(h,d), [0x62]):- !.
map(ld(h,e), [0x63]):- !.
map(ld(h,h), [0x64]):- !.
map(ld(h,l), [0x65]):- !.
map(ld(h,[hl]), [0x66]):- !.
map(ld(h,[ix+D]), [0xdd,0x66,A]):- p7(D,A), !.
map(ld(h,[ix-D]), [0xdd,0x66,A]):- m7(D,A), !.
map(ld(h,[iy+D]), [0xfd,0x66,A]):- p7(D,A), !.
map(ld(h,[iy-D]), [0xfd,0x66,A]):- m7(D,A), !.
map(ld(h,N), [0x26,A]):- ui8(N,A), !.
map(ld(l,a), [0x6f]):- !.
map(ld(l,b), [0x68]):- !.
map(ld(l,c), [0x69]):- !.
map(ld(l,d), [0x6a]):- !.
map(ld(l,e), [0x6b]):- !.
map(ld(l,h), [0x6c]):- !.
map(ld(l,l), [0x6d]):- !.
map(ld(l,[hl]), [0x6e]):- !.
map(ld(l,[ix+D]), [0xdd,0x6e,A]):- p7(D,A), !.
map(ld(l,[ix-D]), [0xdd,0x6e,A]):- m7(D,A), !.
map(ld(l,[iy+D]), [0xfd,0x6e,A]):- p7(D,A), !.
map(ld(l,[iy-D]), [0xfd,0x6e,A]):- m7(D,A), !.
map(ld(l,N), [0x2e,A]):- ui8(N,A), !.
map(ld(i,a), [0xed,0x47]):- !.
map(ld(r,a), [0xed,0x4f]):- !.
map(ld([bc],a), [0x02]):- !.
map(ld([de],a), [0x12]):- !.
map(ld([hl],a), [0x77]):- !.
map(ld([hl],b), [0x70]):- !.
map(ld([hl],c), [0x71]):- !.
map(ld([hl],d), [0x72]):- !.
map(ld([hl],e), [0x73]):- !.
map(ld([hl],h), [0x74]):- !.
map(ld([hl],l), [0x75]):- !.
map(ld([hl],N), [0x36,A]):- ui8(N,A), !.
map(ld([LM],a), [0x32,M,L]):- u16(LM,M,L), !.
map(ld([ix+D],a), [0xdd,0x77,A]):- p7(D,A), !.
map(ld([ix-D],a), [0xdd,0x77,A]):- m7(D,A), !.
map(ld([ix+D],b), [0xdd,0x70,A]):- p7(D,A), !.
map(ld([ix-D],b), [0xdd,0x70,A]):- m7(D,A), !.
map(ld([ix+D],c), [0xdd,0x71,A]):- p7(D,A), !.
map(ld([ix-D],c), [0xdd,0x71,A]):- m7(D,A), !.
map(ld([ix+D],d), [0xdd,0x72,A]):- p7(D,A), !.
map(ld([ix-D],d), [0xdd,0x72,A]):- m7(D,A), !.
map(ld([ix+D],e), [0xdd,0x73,A]):- p7(D,A), !.
map(ld([ix-D],e), [0xdd,0x73,A]):- m7(D,A), !.
map(ld([ix+D],h), [0xdd,0x74,A]):- p7(D,A), !.
map(ld([ix-D],h), [0xdd,0x74,A]):- m7(D,A), !.
map(ld([ix+D],l), [0xdd,0x75,A]):- p7(D,A), !.
map(ld([ix-D],l), [0xdd,0x75,A]):- m7(D,A), !.
map(ld([iy+D],a), [0xfd,0x77,A]):- p7(D,A), !.
map(ld([iy-D],a), [0xfd,0x77,A]):- m7(D,A), !.
map(ld([iy+D],b), [0xfd,0x70,A]):- p7(D,A), !.
map(ld([iy-D],b), [0xfd,0x70,A]):- m7(D,A), !.
map(ld([iy+D],c), [0xfd,0x71,A]):- p7(D,A), !.
map(ld([iy-D],c), [0xfd,0x71,A]):- m7(D,A), !.
map(ld([iy+D],d), [0xfd,0x72,A]):- p7(D,A), !.
map(ld([iy-D],d), [0xfd,0x72,A]):- m7(D,A), !.
map(ld([iy+D],e), [0xfd,0x73,A]):- p7(D,A), !.
map(ld([iy-D],e), [0xfd,0x73,A]):- m7(D,A), !.
map(ld([iy+D],h), [0xfd,0x74,A]):- p7(D,A), !.
map(ld([iy-D],h), [0xfd,0x74,A]):- m7(D,A), !.
map(ld([iy+D],l), [0xfd,0x75,A]):- p7(D,A), !.
map(ld([iy-D],l), [0xfd,0x75,A]):- m7(D,A), !.
map(ld([ix+D],N), [0xdd,0x36,D1,N1]):- p7(D,D1), !, ui8(N,N1), !.
map(ld([ix-D],N), [0xdd,0x36,D1,N1]):- m7(D,D1), !, ui8(N,N1), !.
map(ld([iy+D],N), [0xfd,0x36,D1,N1]):- p7(D,D1), !, ui8(N,N1), !.
map(ld([iy-D],N), [0xfd,0x36,D1,N1]):- m7(D,D1), !, ui8(N,N1), !.
map(ld(sp,hl), [0xf9]):- !.
map(ld(sp,ix), [0xdd,0xf9]):- !.
map(ld(sp,iy), [0xfd,0xf9]):- !.
map(ld(bc,LM), [0x01,M,L]):- u16(LM,M,L), !.
map(ld(bc,[LM]), [0xed,0x4b,M,L]):- u16(LM,M,L), !.
map(ld(de,LM), [0x11,M,L]):- u16(LM,M,L), !.
map(ld(de,[LM]), [0xed,0x5b,M,L]):- u16(LM,M,L), !.
map(ld(hl,LM), [0x21,M,L]):- u16(LM,M,L), !.
map(ld(hl,[LM]), [0x2a,M,L]):- u16(LM,M,L), !.
map(ld(sp,LM), [0x31,M,L]):- u16(LM,M,L), !.
map(ld(sp,[LM]), [0xed,0x7b,M,L]):- u16(LM,M,L), !.
map(ld(ix,LM), [0xdd,0x21,M,L]):- u16(LM,M,L), !.
map(ld(ix,[LM]), [0xdd,0x2a,M,L]):- u16(LM,M,L), !.
map(ld(iy,LM), [0xfd,0x21,M,L]):- u16(LM,M,L), !.
map(ld(iy,[LM]), [0xfd,0x2a,M,L]):- u16(LM,M,L), !.
map(ld([LM],bc), [0xed,0x43,M,L]):- u16(LM,M,L), !.
map(ld([LM],de), [0xed,0x53,M,L]):- u16(LM,M,L), !.
map(ld([LM],hl), [0x22,M,L]):- u16(LM,M,L), !.
map(ld([LM],sp), [0xed,0x73,M,L]):- u16(LM,M,L), !.
map(ld([LM],ix), [0xdd,0x22,M,L]):- u16(LM,M,L), !.
map(ld([LM],iy), [0xfd,0x22,M,L]):- u16(LM,M,L), !.
map(ldd, [0xed,0xa8]):- !.
map(lddr, [0xed,0xb8]):- !.
map(ldi, [0xed,0xa0]):- !.
map(ldir, [0xed,0xb0]):- !.
map(neg, [0xed,0x44]):- !.
map(nop, [0x00]):- !.
map(or(a), [0xb7]):- !.
map(or(b), [0xb0]):- !.
map(or(c), [0xb1]):- !.
map(or(d), [0xb2]):- !.
map(or(e), [0xb3]):- !.
map(or(h), [0xb4]):- !.
map(or(l), [0xb5]):- !.
map(or([hl]), [0xb6]):- !.
map(or([ix+D]), [0xdd,0xb6,A]):- p7(D,A), !.
map(or([ix-D]), [0xdd,0xb6,A]):- m7(D,A), !.
map(or([iy+D]), [0xfd,0xb6,A]):- p7(D,A), !.
map(or([iy-D]), [0xfd,0xb6,A]):- m7(D,A), !.
map(or(N), [0xf6,A]):- ui8(N,A), !.
map(out([c],a), [0xed,0x79]):- !.
map(out([c],b), [0xed,0x41]):- !.
map(out([c],c), [0xed,0x49]):- !.
map(out([c],d), [0xed,0x51]):- !.
map(out([c],e), [0xed,0x59]):- !.
map(out([c],h), [0xed,0x61]):- !.
map(out([c],l), [0xed,0x69]):- !.
map(out([N],a), [0xd3,A]):- ui8(N,A), !.
map(outd, [0xed,0xab]):- !.
map(otdr, [0xed,0xbb]):- !.
map(outi, [0xed,0xa3]):- !.
map(otir, [0xed,0xb3]):- !.
map(pop(af), [0xf1]):- !.
map(pop(bc), [0xc1]):- !.
map(pop(de), [0xd1]):- !.
map(pop(hl), [0xe1]):- !.
map(pop(ix), [0xdd,0xe1]):- !.
map(pop(iy), [0xfd,0xe1]):- !.
map(push(af), [0xf5]):- !.
map(push(bc), [0xc5]):- !.
map(push(de), [0xd5]):- !.
map(push(hl), [0xe5]):- !.
map(push(ix), [0xdd,0xe5]):- !.
map(push(iy), [0xfd,0xe5]):- !.
map(res(0,a), [0xcb,0x87]):- !.
map(res(0,b), [0xcb,0x80]):- !.
map(res(0,c), [0xcb,0x81]):- !.
map(res(0,d), [0xcb,0x82]):- !.
map(res(0,e), [0xcb,0x83]):- !.
map(res(0,h), [0xcb,0x84]):- !.
map(res(0,l), [0xcb,0x85]):- !.
map(res(0,[hl]), [0xcb,0x86]):- !.
map(res(0,[ix+D]), [0xdd,0xcb,A,0x86]):- p7(D,A), !.
map(res(0,[ix-D]), [0xdd,0xcb,A,0x86]):- m7(D,A), !.
map(res(0,[iy+D]), [0xfd,0xcb,A,0x86]):- p7(D,A), !.
map(res(0,[iy-D]), [0xfd,0xcb,A,0x86]):- m7(D,A), !.
map(res(1,a), [0xcb,0x8f]):- !.
map(res(1,b), [0xcb,0x88]):- !.
map(res(1,c), [0xcb,0x89]):- !.
map(res(1,d), [0xcb,0x8a]):- !.
map(res(1,e), [0xcb,0x8b]):- !.
map(res(1,h), [0xcb,0x8c]):- !.
map(res(1,l), [0xcb,0x8d]):- !.
map(res(1,[hl]), [0xcb,0x8e]):- !.
map(res(1,[ix+D]), [0xdd,0xcb,A,0x8e]):- p7(D,A), !.
map(res(1,[ix-D]), [0xdd,0xcb,A,0x8e]):- m7(D,A), !.
map(res(1,[iy+D]), [0xfd,0xcb,A,0x8e]):- p7(D,A), !.
map(res(1,[iy-D]), [0xfd,0xcb,A,0x8e]):- m7(D,A), !.
map(res(2,a), [0xcb,0x97]):- !.
map(res(2,b), [0xcb,0x90]):- !.
map(res(2,c), [0xcb,0x91]):- !.
map(res(2,d), [0xcb,0x92]):- !.
map(res(2,e), [0xcb,0x93]):- !.
map(res(2,h), [0xcb,0x94]):- !.
map(res(2,l), [0xcb,0x95]):- !.
map(res(2,[hl]), [0xcb,0x96]):- !.
map(res(2,[ix+D]), [0xdd,0xcb,A,0x96]):- p7(D,A), !.
map(res(2,[ix-D]), [0xdd,0xcb,A,0x96]):- m7(D,A), !.
map(res(2,[iy+D]), [0xfd,0xcb,A,0x96]):- p7(D,A), !.
map(res(2,[iy-D]), [0xfd,0xcb,A,0x96]):- m7(D,A), !.
map(res(3,a), [0xcb,0x9f]):- !.
map(res(3,b), [0xcb,0x98]):- !.
map(res(3,c), [0xcb,0x99]):- !.
map(res(3,d), [0xcb,0x9a]):- !.
map(res(3,e), [0xcb,0x9b]):- !.
map(res(3,h), [0xcb,0x9c]):- !.
map(res(3,l), [0xcb,0x9d]):- !.
map(res(3,[hl]), [0xcb,0x9e]):- !.
map(res(3,[ix+D]), [0xdd,0xcb,A,0x9e]):- p7(D,A), !.
map(res(3,[ix-D]), [0xdd,0xcb,A,0x9e]):- m7(D,A), !.
map(res(3,[iy+D]), [0xfd,0xcb,A,0x9e]):- p7(D,A), !.
map(res(3,[iy-D]), [0xfd,0xcb,A,0x9e]):- m7(D,A), !.
map(res(4,a), [0xcb,0xa7]):- !.
map(res(4,b), [0xcb,0xa0]):- !.
map(res(4,c), [0xcb,0xa1]):- !.
map(res(4,d), [0xcb,0xa2]):- !.
map(res(4,e), [0xcb,0xa3]):- !.
map(res(4,h), [0xcb,0xa4]):- !.
map(res(4,l), [0xcb,0xa5]):- !.
map(res(4,[hl]), [0xcb,0xa6]):- !.
map(res(4,[ix+D]), [0xdd,0xcb,A,0xa6]):- p7(D,A), !.
map(res(4,[ix-D]), [0xdd,0xcb,A,0xa6]):- m7(D,A), !.
map(res(4,[iy+D]), [0xfd,0xcb,A,0xa6]):- p7(D,A), !.
map(res(4,[iy-D]), [0xfd,0xcb,A,0xa6]):- m7(D,A), !.
map(res(5,a), [0xcb,0xaf]):- !.
map(res(5,b), [0xcb,0xa8]):- !.
map(res(5,c), [0xcb,0xa9]):- !.
map(res(5,d), [0xcb,0xaa]):- !.
map(res(5,e), [0xcb,0xab]):- !.
map(res(5,h), [0xcb,0xac]):- !.
map(res(5,l), [0xcb,0xad]):- !.
map(res(5,[hl]), [0xcb,0xae]):- !.
map(res(5,[ix+D]), [0xdd,0xcb,A,0xae]):- p7(D,A), !.
map(res(5,[ix-D]), [0xdd,0xcb,A,0xae]):- m7(D,A), !.
map(res(5,[iy+D]), [0xfd,0xcb,A,0xae]):- p7(D,A), !.
map(res(5,[iy-D]), [0xfd,0xcb,A,0xae]):- m7(D,A), !.
map(res(6,a), [0xcb,0xb7]):- !.
map(res(6,b), [0xcb,0xb0]):- !.
map(res(6,c), [0xcb,0xb1]):- !.
map(res(6,d), [0xcb,0xb2]):- !.
map(res(6,e), [0xcb,0xb3]):- !.
map(res(6,h), [0xcb,0xb4]):- !.
map(res(6,l), [0xcb,0xb5]):- !.
map(res(6,[hl]), [0xcb,0xb6]):- !.
map(res(6,[ix+D]), [0xdd,0xcb,A,0xb6]):- p7(D,A), !.
map(res(6,[ix-D]), [0xdd,0xcb,A,0xb6]):- m7(D,A), !.
map(res(6,[iy+D]), [0xfd,0xcb,A,0xb6]):- p7(D,A), !.
map(res(6,[iy-D]), [0xfd,0xcb,A,0xb6]):- m7(D,A), !.
map(res(7,a), [0xcb,0xbf]):- !.
map(res(7,b), [0xcb,0xb8]):- !.
map(res(7,c), [0xcb,0xb9]):- !.
map(res(7,d), [0xcb,0xba]):- !.
map(res(7,e), [0xcb,0xbb]):- !.
map(res(7,h), [0xcb,0xbc]):- !.
map(res(7,l), [0xcb,0xbd]):- !.
map(res(7,[hl]), [0xcb,0xbe]):- !.
map(res(7,[ix+D]), [0xdd,0xcb,A,0xbe]):- p7(D,A), !.
map(res(7,[ix-D]), [0xdd,0xcb,A,0xbe]):- m7(D,A), !.
map(res(7,[iy+D]), [0xfd,0xcb,A,0xbe]):- p7(D,A), !.
map(res(7,[iy-D]), [0xfd,0xcb,A,0xbe]):- m7(D,A), !.
map(ret, [0xc9]):- !.
map(ret(nz), [0xc0]):- !.
map(ret(z), [0xc8]):- !.
map(ret(nc), [0xd0]):- !.
map(ret(c), [0xd8]):- !.
map(ret(po), [0xe0]):- !.
map(ret(pe), [0xe8]):- !.
map(ret(p), [0xf0]):- !.
map(ret(m), [0xf8]):- !.
map(reti, [0xed,0x4d]):- !.
map(retn, [0xed,0x45]):- !.
map(rla, [0x17]):- !.
map(rlca, [0x07]):- !.
map(rra, [0x1f]):- !.
map(rrca, [0x0f]):- !.
map(rl(a), [0xcb,0x17]):- !.
map(rl(b), [0xcb,0x10]):- !.
map(rl(c), [0xcb,0x11]):- !.
map(rl(d), [0xcb,0x12]):- !.
map(rl(e), [0xcb,0x13]):- !.
map(rl(h), [0xcb,0x14]):- !.
map(rl(l), [0xcb,0x15]):- !.
map(rl([hl]), [0xcb,0x16]):- !.
map(rl([ix+D]), [0xdd,0xcb,A,0x16]):- p7(D,A), !.
map(rl([ix-D]), [0xdd,0xcb,A,0x16]):- m7(D,A), !.
map(rl([iy+D]), [0xfd,0xcb,A,0x16]):- p7(D,A), !.
map(rl([iy-D]), [0xfd,0xcb,A,0x16]):- m7(D,A), !.
map(rlc(a), [0xcb,0x07]):- !.
map(rlc(b), [0xcb,0x00]):- !.
map(rlc(c), [0xcb,0x01]):- !.
map(rlc(d), [0xcb,0x02]):- !.
map(rlc(e), [0xcb,0x03]):- !.
map(rlc(h), [0xcb,0x04]):- !.
map(rlc(l), [0xcb,0x05]):- !.
map(rlc([hl]), [0xcb,0x06]):- !.
map(rlc([ix+D]), [0xdd,0xcb,A,0x06]):- p7(D,A), !.
map(rlc([ix-D]), [0xdd,0xcb,A,0x06]):- m7(D,A), !.
map(rlc([iy+D]), [0xfd,0xcb,A,0x06]):- p7(D,A), !.
map(rlc([iy-D]), [0xfd,0xcb,A,0x06]):- m7(D,A), !.
map(rr(a), [0xcb,0x1f]):- !.
map(rr(b), [0xcb,0x18]):- !.
map(rr(c), [0xcb,0x19]):- !.
map(rr(d), [0xcb,0x1a]):- !.
map(rr(e), [0xcb,0x1b]):- !.
map(rr(h), [0xcb,0x1c]):- !.
map(rr(l), [0xcb,0x1d]):- !.
map(rr([hl]), [0xcb,0x1e]):- !.
map(rr([ix+D]), [0xdd,0xcb,A,0x1e]):- p7(D,A), !.
map(rr([ix-D]), [0xdd,0xcb,A,0x1e]):- m7(D,A), !.
map(rr([iy+D]), [0xfd,0xcb,A,0x1e]):- p7(D,A), !.
map(rr([iy-D]), [0xfd,0xcb,A,0x1e]):- m7(D,A), !.
map(rrc(a), [0xcb,0x0f]):- !.
map(rrc(b), [0xcb,0x08]):- !.
map(rrc(c), [0xcb,0x09]):- !.
map(rrc(d), [0xcb,0x0a]):- !.
map(rrc(e), [0xcb,0x0b]):- !.
map(rrc(h), [0xcb,0x0c]):- !.
map(rrc(l), [0xcb,0x0d]):- !.
map(rrc([hl]), [0xcb,0x0e]):- !.
map(rrc([ix+D]), [0xdd,0xcb,A,0x0e]):- p7(D,A), !.
map(rrc([ix-D]), [0xdd,0xcb,A,0x0e]):- m7(D,A), !.
map(rrc([iy+D]), [0xfd,0xcb,A,0x0e]):- p7(D,A), !.
map(rrc([iy-D]), [0xfd,0xcb,A,0x0e]):- m7(D,A), !.
map(rld, [0xed,0x6f]):- !.
map(rrd, [0xed,0x67]):- !.
map(rst(0x00), [0xc7]):- !.
map(rst(0x08), [0xcf]):- !.
map(rst(0x10), [0xd7]):- !.
map(rst(0x18), [0xdf]):- !.
map(rst(0x20), [0xe7]):- !.
map(rst(0x28), [0xef]):- !.
map(rst(0x30), [0xf7]):- !.
map(rst(0x38), [0xff]):- !.
map(sbc(a,a), [0x9f]):- !.
map(sbc(a,b), [0x98]):- !.
map(sbc(a,c), [0x99]):- !.
map(sbc(a,d), [0x9a]):- !.
map(sbc(a,e), [0x9b]):- !.
map(sbc(a,h), [0x9c]):- !.
map(sbc(a,l), [0x9d]):- !.
map(sbc(a,[hl]), [0x9e]):- !.
map(sbc(a,[ix+D]), [0xdd,0x9e,A]):- p7(D,A), !.
map(sbc(a,[ix-D]), [0xdd,0x9e,A]):- m7(D,A), !.
map(sbc(a,[iy+D]), [0xfd,0x9e,A]):- p7(D,A), !.
map(sbc(a,[iy-D]), [0xfd,0x9e,A]):- m7(D,A), !.
map(sbc(a,N), [0xde,A]):- ui8(N,A), !.
map(sbc(hl,bc), [0xed,0x42]):- !.
map(sbc(hl,de), [0xed,0x52]):- !.
map(sbc(hl,hl), [0xed,0x62]):- !.
map(sbc(hl,sp), [0xed,0x72]):- !.
map(scf, [0x37]):- !.
map(set(0,a), [0xcb,0xc7]):- !.
map(set(0,b), [0xcb,0xc0]):- !.
map(set(0,c), [0xcb,0xc1]):- !.
map(set(0,d), [0xcb,0xc2]):- !.
map(set(0,e), [0xcb,0xc3]):- !.
map(set(0,h), [0xcb,0xc4]):- !.
map(set(0,l), [0xcb,0xc5]):- !.
map(set(0,[hl]), [0xcb,0xc6]):- !.
map(set(0,[ix+D]), [0xdd,0xcb,A,0xc6]):- p7(D,A), !.
map(set(0,[ix-D]), [0xdd,0xcb,A,0xc6]):- m7(D,A), !.
map(set(0,[iy+D]), [0xfd,0xcb,A,0xc6]):- p7(D,A), !.
map(set(0,[iy-D]), [0xfd,0xcb,A,0xc6]):- m7(D,A), !.
map(set(1,a), [0xcb,0xcf]):- !.
map(set(1,b), [0xcb,0xc8]):- !.
map(set(1,c), [0xcb,0xc9]):- !.
map(set(1,d), [0xcb,0xca]):- !.
map(set(1,e), [0xcb,0xcb]):- !.
map(set(1,h), [0xcb,0xcc]):- !.
map(set(1,l), [0xcb,0xcd]):- !.
map(set(1,[hl]), [0xcb,0xce]):- !.
map(set(1,[ix+D]), [0xdd,0xcb,A,0xce]):- p7(D,A), !.
map(set(1,[ix-D]), [0xdd,0xcb,A,0xce]):- m7(D,A), !.
map(set(1,[iy+D]), [0xfd,0xcb,A,0xce]):- p7(D,A), !.
map(set(1,[iy-D]), [0xfd,0xcb,A,0xce]):- m7(D,A), !.
map(set(2,a), [0xcb,0xd7]):- !.
map(set(2,b), [0xcb,0xd0]):- !.
map(set(2,c), [0xcb,0xd1]):- !.
map(set(2,d), [0xcb,0xd2]):- !.
map(set(2,e), [0xcb,0xd3]):- !.
map(set(2,h), [0xcb,0xd4]):- !.
map(set(2,l), [0xcb,0xd5]):- !.
map(set(2,[hl]), [0xcb,0xd6]):- !.
map(set(2,[ix+D]), [0xdd,0xcb,A,0xd6]):- p7(D,A), !.
map(set(2,[ix-D]), [0xdd,0xcb,A,0xd6]):- m7(D,A), !.
map(set(2,[iy+D]), [0xfd,0xcb,A,0xd6]):- p7(D,A), !.
map(set(2,[iy-D]), [0xfd,0xcb,A,0xd6]):- m7(D,A), !.
map(set(3,a), [0xcb,0xdf]):- !.
map(set(3,b), [0xcb,0xd8]):- !.
map(set(3,c), [0xcb,0xd9]):- !.
map(set(3,d), [0xcb,0xda]):- !.
map(set(3,e), [0xcb,0xdb]):- !.
map(set(3,h), [0xcb,0xdc]):- !.
map(set(3,l), [0xcb,0xdd]):- !.
map(set(3,[hl]), [0xcb,0xde]):- !.
map(set(3,[ix+D]), [0xdd,0xcb,A,0xde]):- p7(D,A), !.
map(set(3,[ix-D]), [0xdd,0xcb,A,0xde]):- m7(D,A), !.
map(set(3,[iy+D]), [0xfd,0xcb,A,0xde]):- p7(D,A), !.
map(set(3,[iy-D]), [0xfd,0xcb,A,0xde]):- m7(D,A), !.
map(set(4,a), [0xcb,0xe7]):- !.
map(set(4,b), [0xcb,0xe0]):- !.
map(set(4,c), [0xcb,0xe1]):- !.
map(set(4,d), [0xcb,0xe2]):- !.
map(set(4,e), [0xcb,0xe3]):- !.
map(set(4,h), [0xcb,0xe4]):- !.
map(set(4,l), [0xcb,0xe5]):- !.
map(set(4,[hl]), [0xcb,0xe6]):- !.
map(set(4,[ix+D]), [0xdd,0xcb,A,0xe6]):- p7(D,A), !.
map(set(4,[ix-D]), [0xdd,0xcb,A,0xe6]):- m7(D,A), !.
map(set(4,[iy+D]), [0xfd,0xcb,A,0xe6]):- p7(D,A), !.
map(set(4,[iy-D]), [0xfd,0xcb,A,0xe6]):- m7(D,A), !.
map(set(5,a), [0xcb,0xef]):- !.
map(set(5,b), [0xcb,0xe8]):- !.
map(set(5,c), [0xcb,0xe9]):- !.
map(set(5,d), [0xcb,0xea]):- !.
map(set(5,e), [0xcb,0xeb]):- !.
map(set(5,h), [0xcb,0xec]):- !.
map(set(5,l), [0xcb,0xed]):- !.
map(set(5,[hl]), [0xcb,0xee]):- !.
map(set(5,[ix+D]), [0xdd,0xcb,A,0xee]):- p7(D,A), !.
map(set(5,[ix-D]), [0xdd,0xcb,A,0xee]):- m7(D,A), !.
map(set(5,[iy+D]), [0xfd,0xcb,A,0xee]):- p7(D,A), !.
map(set(5,[iy-D]), [0xfd,0xcb,A,0xee]):- m7(D,A), !.
map(set(6,a), [0xcb,0xf7]):- !.
map(set(6,b), [0xcb,0xf0]):- !.
map(set(6,c), [0xcb,0xf1]):- !.
map(set(6,d), [0xcb,0xf2]):- !.
map(set(6,e), [0xcb,0xf3]):- !.
map(set(6,h), [0xcb,0xf4]):- !.
map(set(6,l), [0xcb,0xf5]):- !.
map(set(6,[hl]), [0xcb,0xf6]):- !.
map(set(6,[ix+D]), [0xdd,0xcb,A,0xf6]):- p7(D,A), !.
map(set(6,[ix-D]), [0xdd,0xcb,A,0xf6]):- m7(D,A), !.
map(set(6,[iy+D]), [0xfd,0xcb,A,0xf6]):- p7(D,A), !.
map(set(6,[iy-D]), [0xfd,0xcb,A,0xf6]):- m7(D,A), !.
map(set(7,a), [0xcb,0xff]):- !.
map(set(7,b), [0xcb,0xf8]):- !.
map(set(7,c), [0xcb,0xf9]):- !.
map(set(7,d), [0xcb,0xfa]):- !.
map(set(7,e), [0xcb,0xfb]):- !.
map(set(7,h), [0xcb,0xfc]):- !.
map(set(7,l), [0xcb,0xfd]):- !.
map(set(7,[hl]), [0xcb,0xfe]):- !.
map(set(7,[ix+D]), [0xdd,0xcb,A,0xfe]):- p7(D,A), !.
map(set(7,[ix-D]), [0xdd,0xcb,A,0xfe]):- m7(D,A), !.
map(set(7,[iy+D]), [0xfd,0xcb,A,0xfe]):- p7(D,A), !.
map(set(7,[iy-D]), [0xfd,0xcb,A,0xfe]):- m7(D,A), !.
map(sla(a), [0xcb,0x27]):- !.
map(sla(b), [0xcb,0x20]):- !.
map(sla(c), [0xcb,0x21]):- !.
map(sla(d), [0xcb,0x22]):- !.
map(sla(e), [0xcb,0x23]):- !.
map(sla(h), [0xcb,0x24]):- !.
map(sla(l), [0xcb,0x25]):- !.
map(sla([hl]), [0xcb,0x26]):- !.
map(sla([ix+D]), [0xdd,0xcb,A,0x26]):- p7(D,A), !.
map(sla([ix-D]), [0xdd,0xcb,A,0x26]):- m7(D,A), !.
map(sla([iy+D]), [0xfd,0xcb,A,0x26]):- p7(D,A), !.
map(sla([iy-D]), [0xfd,0xcb,A,0x26]):- m7(D,A), !.
map(sra(a), [0xcb,0x2f]):- !.
map(sra(b), [0xcb,0x28]):- !.
map(sra(c), [0xcb,0x29]):- !.
map(sra(d), [0xcb,0x2a]):- !.
map(sra(e), [0xcb,0x2b]):- !.
map(sra(h), [0xcb,0x2c]):- !.
map(sra(l), [0xcb,0x2d]):- !.
map(sra([hl]), [0xcb,0x2e]):- !.
map(sra([ix+D]), [0xdd,0xcb,A,0x2e]):- p7(D,A), !.
map(sra([ix-D]), [0xdd,0xcb,A,0x2e]):- m7(D,A), !.
map(sra([iy+D]), [0xfd,0xcb,A,0x2e]):- p7(D,A), !.
map(sra([iy-D]), [0xfd,0xcb,A,0x2e]):- m7(D,A), !.
map(srl(a), [0xcb,0x3f]):- !.
map(srl(b), [0xcb,0x38]):- !.
map(srl(c), [0xcb,0x39]):- !.
map(srl(d), [0xcb,0x3a]):- !.
map(srl(e), [0xcb,0x3b]):- !.
map(srl(h), [0xcb,0x3c]):- !.
map(srl(l), [0xcb,0x3d]):- !.
map(srl([hl]), [0xcb,0x3e]):- !.
map(srl([ix+D]), [0xdd,0xcb,A,0x3e]):- p7(D,A), !.
map(srl([ix-D]), [0xdd,0xcb,A,0x3e]):- m7(D,A), !.
map(srl([iy+D]), [0xfd,0xcb,A,0x3e]):- p7(D,A), !.
map(srl([iy-D]), [0xfd,0xcb,A,0x3e]):- m7(D,A), !.
map(sub(a), [0x97]):- !.
map(sub(b), [0x90]):- !.
map(sub(c), [0x91]):- !.
map(sub(d), [0x92]):- !.
map(sub(e), [0x93]):- !.
map(sub(h), [0x94]):- !.
map(sub(l), [0x95]):- !.
map(sub([hl]), [0x96]):- !.
map(sub([ix+D]), [0xdd,0x96,A]):- p7(D,A), !.
map(sub([ix-D]), [0xdd,0x96,A]):- m7(D,A), !.
map(sub([iy+D]), [0xfd,0x96,A]):- p7(D,A), !.
map(sub([iy-D]), [0xfd,0x96,A]):- m7(D,A), !.
map(sub(N), [0xd6,A]):- ui8(N,A), !.
map(xor(a), [0xaf]):- !.
map(xor(b), [0xa8]):- !.
map(xor(c), [0xa9]):- !.
map(xor(d), [0xaa]):- !.
map(xor(e), [0xab]):- !.
map(xor(h), [0xac]):- !.
map(xor(l), [0xad]):- !.
map(xor([hl]), [0xae]):- !.
map(xor([ix+D]), [0xdd,0xae,A]):- p7(D,A), !.
map(xor([ix-D]), [0xdd,0xae,A]):- m7(D,A), !.
map(xor([iy+D]), [0xfd,0xae,A]):- p7(D,A), !.
map(xor([iy-D]), [0xfd,0xae,A]):- m7(D,A), !.
map(xor(N), [0xee,A]):- ui8(N,A), !.
map(_, _):- gvar(count,I), I1 is I + 1, format('~d 行目辺り：定義されていない。または、範囲エラー。~n', I1),halt, !.

map_asm(Codes,Insts) :-
    set_gvar(address, 0),set_gvar(count, 0),
    findall((Ad,M,Code),(
        member(Code,Codes),map(Code,M),length(M, Len),
        mod_gvar(count,C,C1,C1 is C+1),
        mod_gvar(address,Ad,Ad1,Ad1 is Ad + Len)
    ),Insts).

put8(Sum):- V is Sum /\ 0xff, format('~|~`0t~16R~2+', V).
put16(V):- put8(V>>8),put8(V).
put_bytes(Is):- forall(member(I,Is),put8(I)).
put_line(Mode,Ad,Bytes):- length(Bytes,Len),
    write(:),put8(Len),put16(Ad),put8(Mode),put_bytes(Bytes),
    foldl([D,A,A1]>>(A1 is (A-D)/\0xff),[Len,Ad,Ad>>8,Mode|Bytes],0,Sum), % サムチェック
    put8(Sum),nl.
put_ihx(Datas):-
    forall(member((Ad,Bytes,_),Datas),put_line(0,Ad,Bytes)),
    put_line(1,0,[]).

asm(X):- read_file_to_terms(X,Cs,[]),map_asm(Cs,_), map_asm(Cs,Is), put_ihx(Is), !.

% コマンドラインオプションを取得
getopt([O,V|L],O,V,L).
getopt([I|L],O,V,[I|R]):- getopt(L,O,V,R).

% メイン処理
main :- current_prolog_flag(argv,ARGV),                 % コマンドライン引数読込
    (getopt(ARGV,'-o',OUT,[IN]);ARGV=[IN]),             % 入出力ファイル名取得
    (var(OUT);tell(OUT)),                               % 出力先設定
    asm(IN).                                            % 出力
main :- halt(1).
:- main.
:- halt.
