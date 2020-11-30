# Author: Sai Pandian
# Email:  saipandian97@gmail.com

# =============================================================================
#                         GENERAL SETTINGS AND PATHS
# =============================================================================

# fzf stuff
export FZF_DEFAULT_COMMAND='rg --files --hidden --no-ignore --glob "!.git/*"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Path to curl
export PATH="/usr/local/opt/curl/bin:$PATH"

# Path to llvm
export PATH="/usr/local/opt/llvm/bin:$PATH"

# Path to homebrew's sbin
export PATH="/usr/local/sbin:$PATH"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /Users/saipandian/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

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
alias sshvps="ssh sai@51.195.139.81"
alias convert="python /Users/saipandian/Documents/Scripts/convert.py"
