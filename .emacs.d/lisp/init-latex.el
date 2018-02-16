;;; LaTeX
;;; WARNING: This file is loaded from a hook.

;;; TODO: `beginning-of-defun'/`end-of-defun' should go to next section.

;;; We use the TeX setup.
(require 'init-tex)
(require 'latex-pretty-symbols nil t)

;;; Since current buffer is a LaTeX one, we can use `local-set-key'.
(ambrevar/local-set-keys
 "C-c m" 'ambrevar/latex-article
 "C-c l" 'ambrevar/latex-lstinline
 "C-c o" 'ambrevar/latex-orgtbl)

(dolist (fun '(turn-on-orgtbl ambrevar/turn-on-skeleton-markers))
  ;; Since this file is loaded from `latex-mode-hook', these functions will not
  ;; be applied to the current buffer.  We do it manually.
  (funcall fun)
  (add-hook 'latex-mode-hook fun))

;;; latex-math-preview
;;; Needs dvipng.
;;; With TeXlive, the following packages are needed: psnfss, symbol, zapfding
(when (and (executable-find "dvipng") (require 'latex-math-preview nil t))
  (local-set-key (kbd "C-c p") 'latex-math-preview-expression)
  (local-set-key (kbd "C-c j") 'latex-math-preview-insert-symbol)
  (local-set-key (kbd "C-c C-j") 'latex-math-preview-last-symbol-again)
  ;; Any color package should be filtered out as they will have unexpected impact on coloring.
  (add-to-list 'latex-math-preview-usepackage-filter-alist '("color")))

;;; For some unknown reasons, `skeleton-end-hook' is set to nil in tex-mode.
;; (dolist (fun '(latex-set-compiler turn-on-orgtbl ambrevar/turn-on-skeleton-markers))
;;   ;; Since this file is loaded from `latex-mode-hook', these functions will not
;;   ;; be applied to the current buffer. We do it manually.
;;   (funcall fun)
;;   (add-hook 'latex-mode-hook fun))

(with-eval-after-load 'latex ; AUCTeX
  (advice-add 'LaTeX-insert-item :before (lambda () (end-of-line)))

  (with-eval-after-load 'font-latex
    (set-face-foreground 'font-latex-sectioning-5-face "white"))

  ;; TODO: Add "tabu" and "longtabu" to environment list.

  (require 'tex-fold)
  (add-hook 'LaTeX-mode-hook 'TeX-fold-mode)
  (add-hook 'find-file-hook 'TeX-fold-buffer t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Skeletons

;;; TODO: implement orgtbl directly with latex tables and remove this skel.
(define-skeleton ambrevar/latex-orgtbl
  "Insert orgtbl skel."
  "Table name: "
  > "\\begin{center}" \n
  "% BEGIN RECEIVE ORGTBL " str \n
  "% END RECEIVE ORGTBL " str \n
  "\\end{center}" > \n
  "\\begin{comment}" \n
  "#+ORGTBL: SEND " str " orgtbl-to-latex" \n
  "| " @ _ " |" \n
  "%$" \n
  "\\end{comment}" > \n @)

(define-skeleton ambrevar/latex-lstinline
  "Insert inline listing." nil
  "\\lstinline @" @ _ "@" @)

(define-skeleton ambrevar/latex-graphics
  "Insert centered picture."
  nil
  > "\\begin{center}" \n
  "\\includegraphics[width=" @ (skeleton-read "Width: " "\\linewidth") "]{" @ _ "}" \n
  "\\end{center}" > \n @)

(define-skeleton ambrevar/latex-article
  "Insert article template."
  nil
  > "%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\documentclass[10pt,a4paper]{article}
\\usepackage[utf8]{inputenc}
\\usepackage[T1]{fontenc}
\\usepackage{lmodern}
\\usepackage{marvosym}
\\usepackage{textcomp}
\\DeclareUnicodeCharacter{20AC}{\\EUR{}}
\\DeclareUnicodeCharacter{2260}{\\neq}
\\DeclareUnicodeCharacter{2264}{\\leq}
\\DeclareUnicodeCharacter{2265}{\\geq}
\\DeclareUnicodeCharacter{22C5}{\\cdot}
\\DeclareUnicodeCharacter{A0}{~}
\\DeclareUnicodeCharacter{B1}{\\pm}
\\DeclareUnicodeCharacter{D7}{\\times}

%%=============================================================================
%% Packages

\\usepackage{amsmath,amssymb,amsfonts}
% \\usepackage{comment}
\\usepackage{geometry}
% \\usepackage{graphicx}
% \\usepackage{lipsum}
% \\usepackage{needspace}
\\usepackage[svgnames]{xcolor}
\\usepackage{xspace}

%%=============================================================================
%% Properties

\\title{" @ (skeleton-read "Title: " "Title") "}
\\author{" @ (skeleton-read "Author: " "P.~\\textsc{Neidhardt}") "}

\\makeatletter
\\let\\thetitle\\@title
\\let\\theauthor\\@author
\\let\\thedate\\@date
\\makeatother" \n

'(setq latex-setup-list '(ambrevar/latex-preamble-aliases ambrevar/latex-preamble-tables ambrevar/latex-preamble-listing))
'(while (and latex-setup-list
             (= (read-key (concat "Insert " (symbol-name (car latex-setup-list)) "? (y)")) ?y))
   (newline-and-indent)
   (funcall (pop latex-setup-list))
   (newline-and-indent))
\n
"%%=============================================================================
%% Babel (load near the end before 'hyperref')
\\usepackage[english]{babel}
%\\iflanguage{french}{
%}{"
"%% Narrow items" \n
"%\\newlength{\\wideitemsep}" \n
"%\\setlength{\\wideitemsep}{.5\\itemsep}" \n
"%\\addtolength{\\wideitemsep}{-7pt}" \n
"%\\let\\olditem\\item" \n
"%\\renewcommand{\\item}{\\setlength{\\itemsep}{\\wideitemsep}\\olditem}"\n
"%}" > \n
"
%%=============================================================================
%% Formatting

% \\usepackage{parskip}
% \\setlength{\\parindent}{15pt}
% \\setlength{\\parskip}{5pt plus 4pt}

% \\renewcommand{\\thefigure}{\\arabic{section}.\\arabic{figure}}
\\renewcommand{\\arraystretch}{1.4}
% \\renewcommand{\\familydefault}{\\sfdefault}

%% Header
% \\usepackage{fancyhdr}
% \\setlength{\\headheight}{15.2pt}
% \\pagestyle{fancy}
% \\lhead{\\thetitle}
% \\rhead{\\theauthor}" > \n
"
%%==============================================================================
%% Hyperref (load last)
\\usepackage[]{hyperref}
\\hypersetup{"
"colorlinks=true," \n
"citecolor=DarkRed," \n
"linkcolor=DarkRed," \n
"linktoc=page," \n
"urlcolor=blue," \n
"}" > \n
"
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\begin{document}

\\maketitle
\\vfill
\\thispagestyle{empty}

\\tableofcontents
\\clearpage" \n
> @ _ \n \n
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\end{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
" \n)

(define-skeleton ambrevar/latex-preamble-aliases
  "Insert setup template."
  nil
  > "%%=============================================================================
%% Aliases

\\let\\latexbak\\LaTeX
\\renewcommand{\\LaTeX}{\\textrm{\\latexbak}\\xspace}

\\let\\texbak\\TeX
\\renewcommand{\\TeX}{\\textrm{\\texbak}\\xspace}

\\def\\unix{\\textsc{Unix}\\xspace}
\\def\\ie{\\textsl{i.e.}\\xspace}
\\def\\eg{\\textsl{e.g.}\\xspace}" \n)

(define-skeleton ambrevar/latex-preamble-tables
  "Insert setup template."
  nil
  > "%%==============================================================================
%% Tables

\\usepackage{longtable}
\\usepackage{tabu}" \n)

(define-skeleton ambrevar/latex-preamble-graphics
  "Insert setup template."
  nil
  > "%%==============================================================================
%% Graphics

\\usepackage{tikz}

\\newcommand{\\fancybox}[1]{" \n
"\\begin{tikzpicture}" \n
"\\node[draw,rounded corners]{#1};" \n
"\\end{tikzpicture}" > \n
"}" > \n)

(define-skeleton ambrevar/latex-preamble-listing
  "Insert listing setup template."
  ;; WARNING: we need to put a '-' at the very end so that the cursor will end
  ;; there. Otherwise it will be placed at the beginning. This is due to some
  ;; unicode or escape characters in the literate array, which `skeleton-insert'
  ;; does not seem to parse correctly.
  nil
  > "%%=============================================================================
%% Listings

\\usepackage{listings}

%% Source code.
\\lstdefinestyle{custom}{" \n
"% numbers=left," \n
"belowcaptionskip=1\\baselineskip," \n
"breaklines=true," \n
"frame=L," \n
"xleftmargin=\\parindent," \n
"% framexleftmargin=\\parindent," \n
"language=C," \n
"showstringspaces=false," \n
"basicstyle=\\footnotesize\\ttfamily," \n
"keywordstyle=\\bfseries\\color{green!40!black}," \n
"commentstyle=\\itshape\\color{purple!40!black}," \n
"identifierstyle=\\color{blue}," \n
"stringstyle=\\color{orange}," \n
"numberstyle=\\ttfamily," \n
"}" > \n
"
\\lstset{escapechar=,style=custom," \n
"literate=" \n
"{á}{{\\'a}}1 {é}{{\\'e}}1 {í}{{\\'i}}1 {ó}{{\\'o}}1 {ú}{{\\'u}}1" \n
"{Á}{{\\'A}}1 {É}{{\\'E}}1 {Í}{{\\'I}}1 {Ó}{{\\'O}}1 {Ú}{{\\'U}}1" \n
"{à}{{\\`a}}1 {è}{{\\'e}}1 {ì}{{\\`i}}1 {ò}{{\\`o}}1 {ù}{{\\`u}}1" \n
"{À}{{\\`A}}1 {È}{{\\'E}}1 {Ì}{{\\`I}}1 {Ò}{{\\`O}}1 {Ù}{{\\`U}}1" \n
"{ä}{{\\\"a}}1 {ë}{{\\\"e}}1 {ï}{{\\\"i}}1 {ö}{{\\\"o}}1 {ü}{{\\\"u}}1" \n
"{Ä}{{\\\"A}}1 {Ë}{{\\\"E}}1 {Ï}{{\\\"I}}1 {Ö}{{\\\"O}}1 {Ü}{{\\\"U}}1" \n
"{â}{{\\^a}}1 {ê}{{\\^e}}1 {î}{{\\^i}}1 {ô}{{\\^o}}1 {û}{{\\^u}}1" \n
"{Â}{{\\^A}}1 {Ê}{{\\^E}}1 {Î}{{\\^I}}1 {Ô}{{\\^O}}1 {Û}{{\\^U}}1" \n
"{œ}{{\\oe}}1 {Œ}{{\\OE}}1 {æ}{{\\ae}}1 {Æ}{{\\AE}}1 {ß}{{\\ss}}1" \n
"{ç}{{\\c c}}1 {Ç}{{\\c C}}1 {ø}{{\\o}}1 {å}{{\\r a}}1 {Å}{{\\r A}}1" \n
"{€}{{\\EUR}}1 {£}{{\\pounds}}1" \n
"}" > \n
"
\\newcommand{\\includecode}[2][custom]{" \n
"\\lstinputlisting[caption=#2, escapechar=, style=#1]{#2}}" \n
"\\let\\verbatim\\relax%" \n
"\\lstnewenvironment{verbatim}[1][]{\\lstset{style=custom}}{}%" > \n -)

(provide 'init-latex)
