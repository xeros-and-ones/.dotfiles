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
export ZDOTDIR="$HOME/.config/zsh"
export EDITOR=nvim
export VISUAL=nvim
# export EDITOR="emacsclient -t"
# export CODEEDITOR="emacsclient -c -a 'emacs'"
# export VISUAL="emacsclient -c -a 'emacs'"
# export SUDO_EDITOR="emacsclient -t"
# export TERM=wezterm
# export PAGER=/usr/bin/moar
export HISTFILE="$HOME/.zsh_history"    # History filepath
# Max number of entries to keep in history file.
SAVEHIST=$(( 100 * 1000 ))      # Use multiplication for readability.

# Max number of history entries to keep in memory.
HISTSIZE=$(( 1.2 * SAVEHIST ))  # Zsh recommended value


path=(
    /usr/local/go/bin(N-/)
    $HOME/.rustup/toolchains/*/bin(N-/)
    $HOME/.config/emacs/bin(N-/)
    $HOME/.detaspace/bin(N-/)
    $HOME/.local/bin(N-/)
    $HOME/.config/zsh/shell-colorscripts/bin(N-/)
    $path
)
export PATH

fpath=(
    $HOME/.config/zsh/zfunc(N-/)
    $fpath
)
export FPATH
#
#
### RANDOM COLOR SCRIPT ###
colorscript random
#
######## PLUGINS & THEMES #############--------------------------------------------------
source $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $ZDOTDIR/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source $ZDOTDIR/themes/powerlevel10k/powerlevel10k.zsh-theme
source $ZDOTDIR/plugins/conda-zsh-completion/conda-zsh-completion.plugin.zsh
source $ZDOTDIR/plugins/git/git.plugin.zsh
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
[ -f /opt/miniconda3/etc/profile.d/conda.sh ] && source /opt/miniconda3/etc/profile.d/conda.sh
