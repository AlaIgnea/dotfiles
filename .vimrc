" vim: set sw=4 ft=vim :
"-----------------------------------------------
" Script:    $HOME/.vimrc
" Author:    Uwe <keksvernichter AT gmail DOT com>
"-----------------------------------------------

"-----------------------------------------------
" plugins/colorschemes used
"-----------------------------------------------
" 
" secure modelines:      http://github.com/ciaranm/securemodelines
" candycode colorscheme: http://www.vim.org/scripts/script.php?script_id=1635
" calendar:              http://www.vim.org/scripts/script.php?script_id=52
" cmdline complete:      http://www.vim.org/scripts/script.php?script_id=2222
" comments:              http://www.vim.org/scripts/script.php?script_id=1528
" uptime:                http://www.vim.org/scripts/script.php?script_id=965
"


"-----------------------------------------------
" modelines (secure modelines plugin req.)
"-----------------------------------------------
" disable internal modelines parser
set modelines=0
let g:secure_modelines_verbose=0       " securemodelines vimscript
let g:secure_modelines_modelines = 15  " 15 available modelines


"-----------------------------------------------
" general settings
"-----------------------------------------------
set nocompatible   " disable vi compatibility
set shiftwidth=4   " number of spaces to indent
set tabstop=2      " number of spaces <TAB> counts for
set textwidth=0    " disable automatic word-wrapping
set number         " show line numbers
set incsearch      " incremental searching
set ignorecase     " search ignoring case
set hlsearch       " highlight searches
set showmatch      " show matching brackets
set showmode       " show current mode
set ruler          " show position of cursor
set title          " show title in console status bar
set noautoindent   " don't autoindent code
set nosmartindent
set cmdheight=1    " commandbar height
set linebreak      " break line at word
set nostartofline  " remember the cursor position
set scrolloff=10   " keep 10 lines for scope


"-----------------------------------------------
" colorscheme (candycode colorscheme req)
"-----------------------------------------------
if !has("gui_running")
      colorscheme candycode
endif


"-----------------------------------------------
" syntax-highlighting 
"-----------------------------------------------
" enable syntax-highlighting
if has ("syntax")
	syntax on
endif


"-----------------------------------------------
" backup
"-----------------------------------------------
" create a backup file in the backup directory
set backup
set backupdir=~/.backup
set history=200


"-----------------------------------------------
" statusline
"-----------------------------------------------
" always show status line 
set laststatus=2

" format the statusline
set statusline=\ %{HasPaste()}%F%m%r%h\ %w\ \ CWD:\ %r%{CurDir()}%h\ \ \ Line:\ %l/%L:%c

function! CurDir()
    let curdir = substitute(getcwd(), '/Users/amir/', "~/", "g")
    return curdir
endfunction

function! HasPaste()
    if &paste
        return 'PASTE MODE  '
    else
        return ''
    endif
endfunction


"-----------------------------------------------
" misc. commands
"-----------------------------------------------

" turn off any existing searches
if has ("autocmd")
	au VimEnter * nohls
endif

