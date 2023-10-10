 
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
export PATH="$HOME/.local/bin:$PATH"
fpath+=~/.config/zsh/zfunc

#
#
#
######## PLUGINS & THEMES #############--------------------------------------------------
source ~/.config/zsh/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source ~/.config/zsh/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source ~/.config/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source ~/.config/zsh/plugins/dotnet/dotnet.plugin.zsh
source ~/.config/zsh/plugins/git/git.plugin.zsh
source ~/.config/zsh/plugins/aliases/aliases.plugin.zsh
source ~/.config/zsh/themes/powerlevel10k/powerlevel10k.zsh-theme
#
#
#
###### Key Bindings & Aliases & Functions #####------------------
source ~/.config/zsh/key_bindings.zsh
source ~/.config/zsh/aliases.zsh
source ~/.config/zsh/functions.zsh
#
#
#
#

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#

if type complete &>/dev/null; then
  _npm_completion () {
    local words cword
    if type _get_comp_words_by_ref &>/dev/null; then
      _get_comp_words_by_ref -n = -n @ -n : -w words -i cword
    else
      cword="$COMP_CWORD"
      words=("${COMP_WORDS[@]}")
    fi

    local si="$IFS"
    if ! IFS=$'\n' COMPREPLY=($(COMP_CWORD="$cword" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${words[@]}" \
                           2>/dev/null)); then
      local ret=$?
      IFS="$si"
      return $ret
    fi
    IFS="$si"
    if type __ltrim_colon_completions &>/dev/null; then
      __ltrim_colon_completions "${words[cword]}"
    fi
  }
  complete -o default -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    local si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    if ! IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)); then

      local ret=$?
      IFS="$si"
      return $ret
    fi
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi
###-end-npm-completion-###


