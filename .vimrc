""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim config
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" All system-wide defaults are set in $VIMRUNTIME/archlinux.vim (usually just
" /usr/share/vim/vimfiles/archlinux.vim) and sourced by the call to :runtime
" you can find below.  If you wish to change any of those settings, you should
" do it in this file (/etc/vimrc), since archlinux.vim will be overwritten
" everytime an upgrade of the vim packages is performed.  It is recommended to
" make changes after sourcing archlinux.vim since it alters the value of the
" 'compatible' option.

" This line should not be removed as it ensures that various options are
" properly set to work with the Vim-related packages.
runtime! archlinux.vim

" If you prefer the old-style vim functionalty, add 'runtime! vimrc_example.vim'
" Or better yet, read /usr/share/vim/vim72/vimrc_example.vim or the vim manual
" and configure vim to your own liking!

" Vim5 and later versions support syntax highlighting. Uncommenting the next
" line enables syntax highlighting by default.
syntax on

" If using a dark background within the editing area and syntax highlighting
" turn on this option as well
set background=dark

" Uncomment the following to have Vim jump to the last position when
" reopening a file
if has("autocmd")
  au BufReadPost * if line("'\"") > 0 && line("'\"") <= line("$")
    \| exe "normal! g'\"" | endif
endif

" Uncomment the following to have Vim load indentation rules and plugins
" according to the detected filetype.
if has("autocmd")
  filetype plugin indent on
endif

" Source a global configuration file if available
" XXX Deprecated, please move your changes here in /etc/vim/vimrc
"if filereadable("/etc/vim/vimrc.local")
"  source /etc/vim/vimrc.local
"endif


" The following are commented out as they cause vim to behave a lot
" differently from regular Vi. They are highly recommended though.
"set hidden             " Hide buffers when they are abandoned
"set ignorecase		" Do case insensitive matching
"set mouse=a		" Enable mouse usage (all modes) in terminals
set autoindent
set autowrite		" Automatically save before commands like :next and :make
set hlsearch
set incsearch		" Incremental search
set number
set showcmd		" Show (partial) command in status line.
set showmatch		" Show matching brackets.
set smartcase		" Do smart case matching
set smartindent
set smarttab
set expandtab
set tabstop=4
set shiftwidth=4
" Mapleader
let mapleader = ","


" Toggle autoindent when pasting with mouse
nnoremap <F2> :set invpaste paste?<CR>
set pastetoggle=<F2>
set showmode

" Colortheme
"colorscheme anotherdark

" Press Space to turn off highlighting and clear any message already displayed.
nnoremap <silent> <Space> :nohlsearch<Bar>:echo<CR>

" Replace visual selection
vnoremap <C-r> "hy:%s/<C-r>h//gc<left><left><left>

" Substitution with incrementing numbers
function! SubsInc(str1, str2, init, step)
	let @i=a:init
	execute("%s/".a:str1."/\\=\"".a:str2."\".@i.(setreg('i',@i+".a:step.")?'':'')/g")
endfunction

" Toggle Comment
imap <C-e> <ESC><leader>c<Space>i
nmap <C-e> <leader>c<Space>
vmap <C-e> <leader>c<Space>

" Duplicate line
imap <C-d> <ESC>yypi
nmap <C-d> yyp

" Tab Navigation Like Firefox
"nmap <C-S-tab> :tabprevious<CR>
"nmap <C-tab> :tabnext<CR>
"map <C-S-tab> :tabprevious<CR>
"map <C-tab> :tabnext<CR>
"imap <C-S-tab> <Esc>:tabprevious<CR>i
"imap <C-tab> <Esc>:tabnext<CR>i
"nmap <C-t> :tabnew<CR>
"imap <C-t> <Esc>:tabnew<CR>

" Increment correctly 'case' statement in bash scripts. Useless ??
"let g:sh_indent_case_labels=1 


" Title case
function! TwiddleCase(str)
	if a:str ==# toupper(a:str)
		let result = tolower(a:str)
	elseif a:str ==# tolower(a:str)
		let result = substitute(a:str,'\(\<\w\+\>\)', '\u\1', 'g')
	else
		let result = toupper(a:str)
	endif
	return result
endfunction
vnoremap ~ ygv"=TwiddleCase(@")<CR>Pgv

" Format sentence (first letter upper, last char dot).
map <silent> <C-p> :s/\(\w\)\([^\.]*\)\.\=/\u\1\2./g<CR>:noh<CR>

" Cursor moves
imap <C-up> <Esc>{i
imap <C-down> <Esc>}i
"nmap <C-up> { 
"nmap <C-down> }
"vmap <C-up> { 
"vmap <C-down> }

" Toggle Insertion / Normal
"inoremap <C-q> <Esc> 
"nmap <C-q> i

" Spellchecking
set spelllang=en,fr
map <F4> 1z=
imap <F4> <Esc>1z=i

" Toggle spellchecking ???
"map <silent> <F5> :set spell!<CR>
map <silent> <F5> :set spell!<CR>

noremap <silent> <Leader>w :call ToggleWrap()<CR>
function ToggleWrap()
  if &wrap
    echo "Wrap OFF"
    setlocal nowrap
    set virtualedit=all
    silent! nunmap <buffer> <Up>
    silent! nunmap <buffer> <Down>
    silent! nunmap <buffer> <Home>
    silent! nunmap <buffer> <End>
    silent! iunmap <buffer> <Up>
    silent! iunmap <buffer> <Down>
    silent! iunmap <buffer> <Home>
    silent! iunmap <buffer> <End>
  else
    echo "Wrap ON"
    setlocal wrap linebreak nolist
    set virtualedit=
    setlocal display+=lastline
    noremap  <buffer> <silent> <Up>   gk
    noremap  <buffer> <silent> <Down> gj
    noremap  <buffer> <silent> <Home> g<Home>
    noremap  <buffer> <silent> <End>  g<End>
    inoremap <buffer> <silent> <Up>   <C-o>gk
    inoremap <buffer> <silent> <Down> <C-o>gj
    inoremap <buffer> <silent> <Home> <C-o>g<Home>
    inoremap <buffer> <silent> <End>  <C-o>g<End>
  endif
endfunction
set nowrap


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugins
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Plugin: ShowMarks
"map <F4> :ShowMarksToggle<CR>

" Plugin: Vim-Taglist
let Tlist_Ctags_Cmd = "/usr/bin/ctags"
let Tlist_WinWidth = 40

" Plugin: NERDTree toggle
map <silent> <F3> :NERDTreeToggle<CR>

" Plugin: LaTeX Suite
"set grepprg=grep\ -nH\ $*
let g:tex_flavor = "LaTeX"
let g:Tex_CompileRule_pdf = 'pdflatex --interaction=nonstopmode $*'
let g:Tex_DefaultTargetFormat ='pdf'

"let TTarget pdf

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Abbreviations
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"iab item itemize
iab Arch Arch Linux
iab arch architecture
iab auto automatique
iab Bcp Beaucoup
iab bcp beaucoup
iab biblio bibliothèque
iab Chai Je ne sais
iab chai je ne sais
iab config configuration
iab configs configurations
iab Dc Donc
iab dc donc
iab dep dépendance
iab deps dépendances
iab dispo disponible
iab dispos disponibles
iab distro distribution
iab Ds Dans
iab ds dans
iab easy easylist
iab fun function
iab info information
iab infos informations
iab latex LaTeX
iab linux GNU/Linux
iab lng language
iab lst lstlisting
iab mail e-mail
iab Mm Même
iab mm même
iab Mnt Maintenant
iab mnt maintenant
iab pb problème
iab ms mais
iab Ms Mais
iab pbs problèmes
iab Pcq Parce que
iab pcq parce que
iab pgrm programme
iab pgrms programmes
iab Plsu Plus
iab plsu plus
iab poru pour
iab Pê Peut-être
iab pê peut-être
iab Qd Quand
iab qd quand
iab Qq Quelque
iab qq quelque
iab qqch quelque chose
iab Qqch Quelque chose
iab Qqn Quelqu'un
iab qqn quelqu'un
iab Qqns Quelques-uns
iab qqns quelques-uns
iab Qqs Quelques
iab qqs quelques
iab ques question
iab Ss Sous
iab ss sous
iab stp s'il te plaît
iab svp s'il vous plaît
iab teh the
iab tex TeX
iab tjrs toujours
iab Tjrs Toujours
iab ts tous
iab tt tout
iab tte toute
iab ttes toutes
iab var variable
iab Ya Il y a
iab ya il y a
iab Yen Il y en
iab yen il y en
