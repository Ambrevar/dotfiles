;;==============================================================================
;; LaTeX, using the TeX setup
;;==============================================================================
(require 'mode-tex)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

(defun latex-itemize ()
  "Prepend \\item to the beginning of the line if not already
  there, otherwise insert it on next line. On region, append
  \item to every line and surround the region by an `itemize'
  environment. If bound to M-RET, you can then easily apply this
  command on the paragraph at point with M-h M-RET."
  (interactive)
  (let (min max case-fold-search)
    (if (not (region-active-p))

        (if (string-match "\\item" (buffer-substring (line-beginning-position) (line-end-position)))
            (progn
              (goto-char (line-end-position))
              (newline)
              (insert "\\item "))
          (goto-char (line-beginning-position))
          (insert "\\item")
          (just-one-space))

      (replace-regexp "^ *\\([^
 ]\\)" "\\\\item \\1" nil (region-beginning) (region-end))
      (goto-char (region-end))
      (goto-char (line-end-position))
      (newline)
      (insert "\\end{itemize}")
      (goto-char (region-beginning))
      (goto-char (line-beginning-position))
      (insert "\\begin{itemize}")
      (newline))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX setup

(setq latex-block-default "itemize")
(setq latex-block-names '("listing"))

(mapcar
 (lambda (latex-block)
   (add-to-list 'latex-block-body-alist `(,latex-block nil '(delete-horizontal-space t) _)))
 '("listing" "verbatim" "verbatim*"))

(add-hook-and-eval
 'latex-mode-hook
 (lambda ()
   (set (make-local-variable 'tex-extension-list)
         '("aux" "glg" "glo" "gls" "idx" "ilg" "ind" "lof" "log" "nav" "out" "snm" "synctex" "synctex.gz" "tns" "toc" "xdy"))
   (set (make-local-variable 'tex-command) "pdflatex")
   ;; For some unknown reasons, `skeleton-end-hook' is set to nil in tex-mode.
   (add-hook 'skeleton-end-hook 'skeleton-make-markers)
   (local-set-key (kbd "C-c C-a") 'latex-article)
   (local-set-key (kbd "C-c C-c") 'latex-smallcaps)
   (local-set-key (kbd "C-c C-e") 'latex-emph)
   (local-set-key (kbd "C-c C-l") 'latex-slanted)
   (local-set-key (kbd "C-c C-p") 'latex-paragraph)
   (local-set-key (kbd "C-c C-s") 'latex-section)
   (local-set-key (kbd "C-c C-u") 'latex-superscript)
   (local-set-key (kbd "C-c L") 'latex-listing)
   (local-set-key (kbd "C-c l") 'latex-lstinline)
   (local-set-key (kbd "C-c u") 'latex-superscript)
   (local-set-key (kbd "M-RET") 'latex-itemize)
   (turn-on-orgtbl)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Skeletons

(define-skeleton latex-emph "Insert emph." nil "\\emph{" @ _ "}" @)
(define-skeleton latex-slanted "Insert slanted text." nil "\\textsl{" @ _ "}" @)
(define-skeleton latex-smallcaps "Insert smallcaps text." nil "\\textsc{" @ _ "}" @)
(define-skeleton latex-superscript "Insert supercript text." nil "\\textsuperscript{" @ _ "}" @)

(define-skeleton latex-package "Use package." "Package: " \n "\\usepackage[" @ "]{" @ _ "}" \n @)

(define-skeleton latex-paragraph "Insert paragraph command." nil "\\paragraph{" @ _ "}" \n)
(define-skeleton latex-subparagraph "Insert subparagraph command." nil "\\subparagraph{" @ _ "}" \n)

;; TODO: use argument to change level and starred version.
(define-skeleton latex-section "Insert section command." nil "\\section{" @ _ "}" \n)
(define-skeleton latex-subsection "Insert section command." nil "\\subsection{" @ _ "}" \n)
(define-skeleton latex-subsubsection "Insert section command." nil "\\subsubsection{" @ _ "}" \n)

;; TODO: If tabular, center.
;; TODO: complete with '(tabular, align, "pmatrix" "bmatrix" "Bmatrix" "vmatrix" "Vmatrix" "smallmatrix"))}}
(define-skeleton latex-matrix
  "Insert matrix/align."
  nil
  '(require 'functions)
  > "\\begin{"
  '(setq str (skeleton-read "Type: " "align"))
  str "}{"
  '(setq str (skeleton-read "Format: " "ll"))
  str "}" \n
  '(setq v1 (count-occurences "[a-z]" str))
  @ (mapconcat 'identity (split-string (make-string v1 ?&) "" t) " ") " \\\\" \n
  @ _ "\\\\" \n
  "\\end{" str "}" > \n @)

(define-skeleton latex-orgtbl
  "Insert skel.
TODO: orgtbl broken?
TODO: implement orgtbl directly with latex tables and remove this
skel."
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

(define-skeleton latex-minipage
  "Insert skel."
  "Width: "
  > "\\begin{minipage}{" str "}" \n
  @ _ \n
  "\\end{minipage}" > \n @)

(define-skeleton latex-parbox
  "Insert skel."
  "Width: "
  > "\\parbox{" str "}{" \n
  @ _ \n
  "}" > \n @)

(define-skeleton latex-lstinline
  "Insert inline listing." nil
  "\\lstinline @" @ _ "@" @)

(define-skeleton latex-graphics
  "Insert centered picture."
  nil
  > "\\begin{center}" \n
  "\\includegraphics[width=" @ (skeleton-read "Width: " "\\linewidth") "]{" @ _ "}" \n
  "\\end{center}" > \n @)

(define-skeleton latex-article
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
% \\usepackage{lipsum}
% \\usepackage{needspace}
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

'(setq latex-setup-list '(latex-preamble-aliases latex-preamble-formatting latex-preamble-tables latex-preamble-graphics latex-preamble-listing))
'(while (and latex-setup-list
             (= (read-key (concat "Insert " (symbol-name (car latex-setup-list)) "? (y)")) ?y))
   (newline-and-indent)
   (funcall (pop latex-setup-list))
   (newline-and-indent))
\n
"%%=============================================================================
%% Babel (load last before 'hyperref')
\\usepackage[french,ngerman,english]{babel}
\\iflanguage{french}{
}{
  %% Narrow items
  \\newlength{\\wideitemsep}
  \\setlength{\\wideitemsep}{.5\\itemsep}
  \\addtolength{\\wideitemsep}{-7pt}
  \\let\\olditem\\item
  \\renewcommand{\\item}{\\setlength{\\itemsep}{\\wideitemsep}\\olditem}
}

%%==============================================================================
%% Hyperref (load last)
\\usepackage[]{hyperref}
\\hypersetup{
  colorlinks=true,
  linkcolor=DarkRed,
  linktoc=page,
  urlcolor=blue,
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\begin{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\maketitle
% \\vfill
% \\thispagestyle{empty}
% \\tableofcontents
" \n
> @ _ \n \n
"%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\\end{document}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
" \n)

(define-skeleton latex-preamble-aliases
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

(define-skeleton latex-preamble-formatting
  "Insert setup template."
  nil
  > "%%=============================================================================
%% Formatting

% \\usepackage{parskip}
% \\setlength{\\parindent}{15pt}

% \\renewcommand{\\thefigure}{\\arabic{section}.\\arabic{figure}}
\\renewcommand{\\arraystretch}{1.4}
% \\renewcommand{\\familydefault}{\\sfdefault}

%% Header
% \\usepackage{fancyhdr}
% \\setlength{\\headheight}{15.2pt}
% \\pagestyle{fancy}
% \\lhead{\\thetitle}
% \\rhead{\\theauthor}" \n)

(define-skeleton latex-preamble-tables
  "Insert setup template."
  nil
  > "%%==============================================================================
%% Tables

\\usepackage{longtable}
\\usepackage{tabu}" \n)

(define-skeleton latex-preamble-graphics
  "Insert setup template."
  nil
  > "%%==============================================================================
%% Graphics

%% Load TikZ after xcolor.
\\usepackage[svgnames]{xcolor}
\\usepackage{graphicx}
\\usepackage{tikz}

\\newcommand{\\fancybox}[1]{
  \\begin{tikzpicture}
    \\node[draw,rounded corners]{#1};
  \\end{tikzpicture}
}" \n)

(define-skeleton latex-preamble-listing
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
\\lstdefinestyle{custom}{
  % numbers=left,
  belowcaptionskip=1\\baselineskip,
  breaklines=true,
  frame=L,
  xleftmargin=\\parindent,
  % framexleftmargin=\\parindent,
  language=C,
  showstringspaces=false,
  basicstyle=\\footnotesize\\ttfamily,
  keywordstyle=\\bfseries\\color{green!40!black},
  commentstyle=\\itshape\\color{purple!40!black},
  identifierstyle=\\color{blue},
  stringstyle=\\color{orange},
  numberstyle=\\ttfamily,
}

\\lstset{escapechar=,style=custom,
  literate=
  {á}{{\\'a}}1 {é}{{\\'e}}1 {í}{{\\'i}}1 {ó}{{\\'o}}1 {ú}{{\\'u}}1
  {Á}{{\\'A}}1 {É}{{\\'E}}1 {Í}{{\\'I}}1 {Ó}{{\\'O}}1 {Ú}{{\\'U}}1
  {à}{{\\`a}}1 {è}{{\\'e}}1 {ì}{{\\`i}}1 {ò}{{\\`o}}1 {ù}{{\\`u}}1
  {À}{{\\`A}}1 {È}{{\\'E}}1 {Ì}{{\\`I}}1 {Ò}{{\\`O}}1 {Ù}{{\\`U}}1
  {ä}{{\\\"a}}1 {ë}{{\\\"e}}1 {ï}{{\\\"i}}1 {ö}{{\\\"o}}1 {ü}{{\\\"u}}1
  {Ä}{{\\\"A}}1 {Ë}{{\\\"E}}1 {Ï}{{\\\"I}}1 {Ö}{{\\\"O}}1 {Ü}{{\\\"U}}1
  {â}{{\\^a}}1 {ê}{{\\^e}}1 {î}{{\\^i}}1 {ô}{{\\^o}}1 {û}{{\\^u}}1
  {Â}{{\\^A}}1 {Ê}{{\\^E}}1 {Î}{{\\^I}}1 {Ô}{{\\^O}}1 {Û}{{\\^U}}1
  {œ}{{\\oe}}1 {Œ}{{\\OE}}1 {æ}{{\\ae}}1 {Æ}{{\\AE}}1 {ß}{{\\ss}}1
  {ç}{{\\c c}}1 {Ç}{{\\c C}}1 {ø}{{\\o}}1 {å}{{\\r a}}1 {Å}{{\\r A}}1
  {€}{{\\EUR}}1 {£}{{\\pounds}}1
}

\\newcommand{\\includecode}[2][custom]{
  \\lstinputlisting[caption=#2, escapechar=, style=#1]{#2}}" \n -)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The magnificent latex-math-preview mode!
;; TODO: some symbols are not generated properly.
(autoload 'latex-math-preview-expression "latex-math-preview" nil t)
(autoload 'latex-math-preview-insert-symbol "latex-math-preview" nil t)
(autoload 'latex-math-preview-save-image-file "latex-math-preview" nil t)
(autoload 'latex-math-preview-beamer-frame "latex-math-preview" nil t)
(autoload 'latex-math-preview-text-symbol-datasets "latex-math-preview" nil t)

(setq latex-math-preview-cache-directory-for-insertion
      (concat emacs-cache-folder "latex-math-preview-cache"))

;; Extra for latex-math-preview-mode.
;; TODO: latex-math-preview-mode extra does not work.
(require 'latex-math-preview-extra-data nil t)
(add-hook
 'latex-mode-hook
 (lambda ()
   ;; (local-set-key (kbd "C-c p") 'latex-math-preview-expression)
   ;; (local-set-key (kbd "C-c C-p") 'latex-math-preview-save-image-file)
   (local-set-key (kbd "C-c j") 'latex-math-preview-insert-symbol)
   (local-set-key (kbd "C-c C-j") 'latex-math-preview-last-symbol-again)
   ;; (local-set-key (kbd "C-c C-b") 'latex-math-preview-beamer-frame)
   ;; (add-to-list 'latex-math-preview-text-symbol-datasets
   ;;              latex-math-preview-textcomp-symbol-data)
   ;; (add-to-list 'latex-math-preview-text-symbol-datasets
   ;;              latex-math-preview-pifont-zapf-dingbats-symbol-data)
   ;; (add-to-list 'latex-math-preview-text-symbol-datasets
   ;;              latex-math-preview-pifont-symbol-fonts-symbol-data)))
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(provide 'mode-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
