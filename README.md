# dotfiles

Simple no-fuss dotfiles for easy setup of new machines.

## Setup

Should be able to clone this repo to the home directory of linux and mac machines and extract the necessary dotfiles.

### Vim

Will need to use Vundle to install and manage plugins. Install Vundle by: 
```
git clone https://github.com/VundleVim/Vundle.vim.git ~/.vim/bundle/Vundle.vim
```

Then use Vim command:
```
:PluginInstall
```

### Terminal.app

I use Dracula theme in Terminal.app on mac which can be downloaded by:
```
git clone https://github.com/dracula/terminal-app.git
```

The theme is activated by:
1) Terminal > Settings Tab
2) Click "Gear" icon
3) Click Import...
4) Select the Dracula.terminal file
5) Click Default

### zsh Shell

To get set up with zsh you will need to install an up-to-date version of zsh in whichever way is easiest.
For macs with homebrew installed:
```
brew install zsh
```
You then need to grab oh-my-zsh:
```
sh -c "$(curl -fsSL https://raw.githubusercontent.com/robbyrussell/oh-my-zsh/master/tools/install.sh)"
```
To use the dracula theme with zsh, first download dracula:
```
git clone https://github.com/dracula/zsh.git
```
Then copy the theme file to the /.oh-my-zsh/themes directory:
```
cp ./zsh/dracula.zsh-theme ~/.oh-my-zsh/themes/
```
You will also need to change your default shell to zsh in whichever way is appropriate for your OS.


## Acknowledgements

* Instructions on installing Dracula theme from https://www.draculatheme.com
* Vim ideas from Real Python website https://www.realpython.com
* oh-my-zsh is a fantastic bit of community software from https://github.com/robbyrussell/oh-my-zsh



