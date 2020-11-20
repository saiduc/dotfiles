FILE=~/.config/emacs/lisp/ox-ipynb.el
if [ -f "$FILE" ]; then
    :
else
	mkdir ~/.config/emacs/lisp &&\
	git clone https://github.com/jkitchin/ox-ipynb.git &&\
	mv ./ox-ipynb/ox-ipynb.el ~/.config/emacs/lisp/ &&\
	rm -rf ./ox-ipynb
fi
