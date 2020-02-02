export ZSH="/Users/kylegwinnup/.oh-my-zsh"
ZSH_THEME="af-magic"

plugins=(git)

source $ZSH/oh-my-zsh.sh

function remac {
  sudo /System/Library/PrivateFrameworks/Apple80211.framework/Resources/airport -z
  sudo ifconfig en0 ether $(openssl rand -hex 6 | sed 's/\(..\)/\1:/g; s/.$//')
  sudo networksetup -detectnewhardware
  echo $(ifconfig en0 | grep ether)
}

export GOPATH=~/go
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/anaconda3/bin:$PATH"

alias nvim=/usr/local/Cellar/neovim/0.4.3/bin/nvim
alias vim=/usr/local/Cellar/neovim/0.4.3/bin/nvim


