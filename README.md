Artitoc
=======

This simple LaTeX package and OCaml script are designed to help generate a 
table of contents and author index for a proceedings file constructed using 
[pdfpages](https://ctan.org/pkg/pdfpages).

The idea is to include the package in the proceedings source.
```
\usepackage{artitoc}
```

Then to use the `\newtitle` and `\addauthor` macros before each included pdf 
to specify its title and authors. These macros add entries to a `.atoc` 
file.

The OCaml script `artitoc.ml` reads the generated `.atoc` file and produces 
two `.tex` files:
* a table-of-contents in the right order and with page numbers;
* an author index sorted by last name with page numbers.

Note that the author sorting is not perfect. It only handles the western 
European particles "de", "du", "van der", "van", and "von". Some manual 
adjustment may be required.

