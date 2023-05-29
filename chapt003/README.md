# prologでアセンブラを作ってihxファイルを出力しよう

とりあえず１ファイルをアセンブルしてihxファイルを出力するアセンブラを作りましょう。
アセンブラのソースコードは、prologの項を使って書くことにします。
prologの項をファイルから読み込んでパースするには、`:- use_module(library(readutil)).` の `read_file_to_terms/3` を使って読み込無と便利です。

Demo:

http://webmsx.org/?DISK=https://github.com/hsk/asz80.pl/raw/main/chapt003/objects/usr.dsk
