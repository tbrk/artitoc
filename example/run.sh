#!/bin/sh

# setup test environment
ln -s ../artitoc.sty
echo showpage | ps2pdf -sPAPERSIZE=a4 - woke_paper_1.pdf
cp woke_paper_1.pdf woke_paper_2.pdf

# make example
pdflatex example.tex
../artitoc.ml --toc toc.tex --authors authors.tex example.atoc
pdflatex example.tex

# cleanup
rm artitoc.sty woke_paper_1.pdf woke_paper_2.pdf example.log

