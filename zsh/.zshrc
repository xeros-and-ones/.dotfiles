 
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
setopt AUTO_CD
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus
setopt EXTENDED_GLOB
# Use modern file-locking mechanisms, for better safety & performance.
setopt HIST_FCNTL_LOCK
# Keep only the most recent copy of each duplicate entry in history.
setopt HIST_IGNORE_ALL_DUPS
# Auto-sync history between concurrent sessions.
setopt SHARE_HISTORY
# Sort numbers numerically, not lexicographically.
setopt NUMERIC_GLOB_SORT
# Don't let > silently overwrite files. To overwrite, use >! instead.
setopt NO_CLOBBER
# Treat comments pasted into the command line as comments, not code.
setopt INTERACTIVE_COMMENTS
####### exports ##########--------------------------------------------------------------
export EDITOR=nvim
export VISUAL=nvim
# export PAGER=/usr/bin/moar
export HISTFILE="$HOME/.zsh_history"    # History filepath
# Max number of entries to keep in history file.
SAVEHIST=$(( 100 * 1000 ))      # Use multiplication for readability.

# Max number of history entries to keep in memory.
HISTSIZE=$(( 1.2 * SAVEHIST ))  # Zsh recommended value

export PATH=$PATH:/usr/local/go/bin
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="/home/xero/.detaspace/bin:$PATH"
fpath+=~/.config/zsh/zfunc

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

