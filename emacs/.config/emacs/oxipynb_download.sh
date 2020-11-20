FILE=/Users/saipandian/.emacs.d/lisp/ox-ipynb.el
if [ -f "$FILE" ]; then
    :
else
    mkdir /Users/saipandian/.emacs.d/lisp &&\
	git clone https://github.com/jkitchin/ox-ipynb.git &&\
	mv ./ox-ipynb/ox-ipynb.el /Users/saipandian/.emacs.d/lisp/ &&\
	rm -rf ./ox-ipynb
fi
