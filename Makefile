all: semantics.tex
	latexmk main

semantics.tex: semantics.ott
	ott -tex_wrap false -tex_show_meta false -i semantics.ott -o semantics.tex
