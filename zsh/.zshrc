# Author: Sai Pandian
# Email:  saipandian97@gmail.com

# =============================================================================
#                         GENERAL SETTINGS AND PATHS
# =============================================================================

# Path to your oh-my-zsh installation.
export ZSH="/home/saipandian/.oh-my-zsh"

ZSH_THEME="robbyrussell"
plugins=()
source $ZSH/oh-my-zsh.sh

PYDEVD_DISABLE_FILE_VALIDATION=1

export PATH="$HOME/.local/bin:$PATH:$HOME/.cargo/bin"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/saipandian/miniconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/saipandian/miniconda3/etc/profile.d/conda.sh" ]; then
        . "/home/saipandian/miniconda3/etc/profile.d/conda.sh"
    else
        export PATH="/home/saipandian/miniconda3/bin:$PATH"
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
alias update='sudo apt update && sudo apt upgrade -y && sudo apt autoremove -y && flatpak update -y'
