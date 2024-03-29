# Author: Sai Pandian
# Email:  saipandian97@gmail.com

# =============================================================================
#                         GENERAL SETTINGS AND PATHS
# =============================================================================

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /Users/saipandian/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

# Created by `pipx` on 2021-05-19 18:55:06
set PATH $PATH /Users/saipandian/.local/bin

# Path to curl
export PATH="/usr/local/opt/curl/bin:$PATH"

# Path to llvm
export PATH="/usr/local/opt/llvm/bin:$PATH"

# Path to homebrew's sbin
export PATH="/usr/local/sbin:$PATH"

# Disable welcome message
set fish_greeting

# =============================================================================
#                                 ALIASES
# =============================================================================

alias ca="conda activate"
alias cde="conda deactivate"
alias ec='emacsclient -n'
alias ex='emacsclient -n'
alias vim='nvim'
alias vi='nvim'
alias sshvps="ssh -Y sai@51.195.139.81"
alias convert="python ~/dotfiles/scripts/convert_ipynb_to_org.py"
alias createstub="~/.config/emacs/.cache/lsp/npm/pyright/bin/pyright --createstub"
alias update="brew update && brew upgrade && brew cleanup -s --prune 0"
alias fv="vim (fzf)"
alias fe="ec (fzf)"
