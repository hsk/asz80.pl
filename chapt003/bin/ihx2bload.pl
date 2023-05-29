:- use_module(library(pure_input)).
% ihxファイルのパース
parse_ihx(FILE,R2:R):-
    phrase_from_file(lines(Ls), FILE), % 全行パース
    % 範囲とコードのリストを生成
    findall(A-B:D, (member(Len:A:0:D,Ls),B is Len+A-1),R),
    % 全体の範囲取得
    foldl([S-E:_,S1-E1,S2-E2]>>(
        (S<S1->S2=S;S2=S1),
        (E>E1->E2=E;E2=E1)
    ),R,0x10000-0,R2).
% 全行パース
lines([L|R])--> line(L),!,lines(R).
lines([])--> !.
% 1行パース
line(Ct:Ad:Md:Bs) -->
    `:`,hex(2,Ct),hex(4,Ad),hex(2,Md),bytes(Ct,Bs),hex(2,Sum),(`\n`|`\r\n`),
    % サムチェック
    {foldl([D,A,A1]>>(A1 is (A-D)/\255),[Ct,Ad>>8,Ad/\255,Md|Bs],0,Sum1),
     (Sum=Sum1;format('sum error ~w ~w\n',[Sum,Sum1]),fail)
    }.
% xx を1バイトデータとしてパース
hex(N,R)-->xdigit(N,Cs),{number_codes(R,[0'0, 0'x |Cs])}.
% n個の16進数文字列を取得
xdigit(1,[R])-->xdigit(R).
xdigit(N,[R1|R2])--> xdigit(R1),{N1 is N-1},xdigit(N1,R2).
% 16進数文字を取得
xdigit(R)--> [R], {member(R,`0123456789ABCDEF`)}.
% n個のバイトリストをパース
bytes(0,[])--> !.
bytes(N,[V|Vs])-->hex(2,V),{N1 is N-1},bytes(N1,Vs).

% 数式１バイト出力
putb(Exp):- V is Exp /\ 0xff, put_byte(V).
% 数式２バイト出力
putw(V):- putb(V),putb(V>>8).
% BLOAD形式のヘッダ出力
put_bloadheader(S,E):- putb(0xfe),putw(S),putw(E),putw(S).
% データ出力 (Datasには抜けがありうるので間も埋めて出力)
put_datas(S-E:Datas):-
    retractall(data(_,_)),assert(data(_,0)),        % data述語のデフォルト値設定
    forall(member(S1-_:Ds,Datas),% データをループ処理
        %data(アドレス,データ)を生成
        foldl([D,A,A1]>>(asserta(data(A,D)),A1 is A+1),Ds,S1,_)),
    forall(between(S,E,I),(data(I,V),format('~c',[V]))). % S-Eまでdataを出力

% コマンドラインオプションを取得
getopt([O,V|L],O,V,L).
getopt([I|L],O,V,[I|R]):- getopt(L,O,V,R).

% メイン処理
main :- current_prolog_flag(argv,ARGV),                 % コマンドライン引数読込
    (getopt(ARGV,'-o',OUT,[IN]);ARGV=[IN]),             % 入出力ファイル名取得
    parse_ihx(IN,S-E:R),format('~w ~16r-~16r\n',[OUT,S,E]),% ihxパース、結果出力
    (var(OUT);tell(OUT)),                               % 出力先設定
    set_stream(current_output, encoding(octet)),        % バイナリモード設定
    put_bloadheader(S,E),put_datas(S-E:R).              % 出力
main :- halt(1).
:- main.
:- halt.
