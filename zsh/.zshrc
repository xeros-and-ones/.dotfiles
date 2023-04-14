 
### RANDOM COLOR SCRIPT ###
colorscript random
#
#
#
# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
#
#
#
####### Options ###########-------------------------------------------------------------
# Changing/making/removing directory
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus
setopt interactivecomments
#
#
####### exports ##########--------------------------------------------------------------
export PATH=$PATH:/usr/local/go/bin
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="/home/xero/.detaspace/bin:$PATH"
fpath+=~/.config/zsh/zfunc

export EDITOR=nvim
export VISUAL=nvim
# export PAGER=nvim
export HISTFILE="$HOME/.zsh_history"    # History filepath
export HISTSIZE=10000                   # Maximum events for internal history
export SAVEHIST=10000                   # Maximum events in history file
#
#
#
######## PLUGINS & THEMES #############--------------------------------------------------
export ZSHCONFIG=~/.config/zsh
source $ZSHCONFIG/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $ZSHCONFIG/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZSHCONFIG/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source $ZSHCONFIG/plugins/dotnet/dotnet.plugin.zsh
source $ZSHCONFIG/plugins/git/git.plugin.zsh
source $ZSHCONFIG/themes/powerlevel10k/powerlevel10k.zsh-theme
#
#
#
###### Key Bindings & Aliases & Functions #####------------------
source $ZSHCONFIG/key_bindings.zsh
source $ZSHCONFIG/aliases.zsh
source $ZSHCONFIG/functions.zsh
#
#
#
#

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

