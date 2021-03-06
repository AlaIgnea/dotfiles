autoload -U compinit promptinit zmv url-quote-magic
compinit
promptinit
prompt bart                     # bart theme

setopt always_to_end            # When complete from middle, move cursor
setopt autocd                   # switch to the directory by typing it
setopt complete_in_word         # Not just at the end
setopt correctall               # Correct everything
setopt extended_glob            # Weird & wacky pattern matching - yay zsh!
setopt globdots                 # list all files without specifying the dot
setopt histignorespace          # don't save lines starting with spaces
setopt ignoreeof                # instead of ^D type exit or logout
setopt noclobber                # don't accidentially overwrite an existing file

# History 
HISTFILE=~/.zsh-history
HISTSIZE=500
SAVEHIST=50000
setopt hist_ignore_all_dups hist_reduce_blanks \
hist_save_no_dups inc_append_history \
extended_history share_history multios

source "${HOME}/.login"
source "${HOME}/.zsh/aliases"
source "${HOME}/.zsh/comp"
source "${HOME}/.zsh/S60_prompt"
source "${HOME}/.zsh/functions"

# url quoting
zle -N self-insert url-quote-magic
