#   _____       _ _____             
#  / ____|     (_)  __ \            
# | (___   __ _ _| |  | |_   _  ___ 
#  \___ \ / _` | | |  | | | | |/ __|
#  ____) | (_| | | |__| | |_| | (__       Sai Pandian
# |_____/ \__,_|_|_____/ \__,_|\___|      github.com/saiduc


# =============================================================================
#                         GENERAL SETTINGS AND PATHS
# =============================================================================

# Path to your oh-my-zsh installation.
export ZSH="/Users/saipandian/.oh-my-zsh"

# fzf stuff
[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh
export FZF_DEFAULT_COMMAND='rg --files --hidden --no-ignore --glob "!.git/*"'
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"

# Path to curl
export PATH="/usr/local/opt/curl/bin:$PATH"

ZSH_THEME="dracula"
plugins=()
source $ZSH/oh-my-zsh.sh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/saipandian/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/saipandian/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/saipandian/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/saipandian/miniconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

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
alias getlog="python /Users/saipandian/Documents/Scripts/getlog.py"
alias convert="python /Users/saipandian/Documents/Scripts/convert.py"
alias icat="kitty +kitten icat"


# =============================================================================
#                                 PROMPT
# =============================================================================
clear
