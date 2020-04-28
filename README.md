# dotfiles

Simple no-fuss dotfiles for easy setup of new machines.

## Setup

Should be able to clone this repo to the home directory of Linux and Mac
machines and extract the necessary dotfiles.

### Emacs

Just move the init.el file to the ~/.emacs.d directory and the init-org.org
file to a ~/.emacs/init/ directory. Then open the init-org.org file and follow
the instructions under the "Setting up New Installation" heading.

It will also be necessary to make a separate virtual environment for the Elpy
dependencies and point to it in the Python section of the config file.

### Neovim

Move the init.vim file to the ~/.config/nvim/ directory. Then, install VimPlug
and do :PlugInstall

It is necessary to make new virtual environments (in either virtualenv, pyenv or
conda) for Python 2 and Python 3 and install pynvim and jedi in both using pip.
Then, point to it in the init.vim file and python completion should work
correctly with deoplete, and should show correct completions for any new virtual
environment you are in.

The location of libclang must also be changed for c++ completion to work.

Run :checkhealth to make sure everything works correctly.

### Zsh

I have a very basic .zshrc file that requires an installation of zsh and
oh-my-zsh. FZF and ripgrep must be installed in order to use fuzzy searching.
