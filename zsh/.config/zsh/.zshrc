export ZELLIJ_AUTO_ATTACH=true
export ZELLIJ_AUTO_EXIT=false
# Auto Start zellij
if [[ -z "$ZELLIJ" ]]; then
	if [[ "$ZELLIJ_AUTO_ATTACH" == "true" ]]; then
		zellij attach default -c
	fi

	if [[ "$ZELLIJ_AUTO_EXIT" == "true" ]]; then
		exit
	fi
fi
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
# setopt EXTENDED_GLOB
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
VISUAL="emacsclient -c -a ''"
EDITOR="emacsclient -t -a ''"
SUDO_EDITOR="emacsclient -t -a ''"
GIT_EDITOR='emacsclient -t -a '''
HISTFILE="$HOME/.zsh_history" # History filepath
SAVEHIST=$((100 * 1000))      # Use multiplication for readability.
HISTSIZE=$((1.2 * SAVEHIST))  # Zsh recommended value

# Safely apply -march=native only if supported
export CFLAGS="-O2 -flto=auto -pipe"
case "$(gcc -march=native -Q --help=target | grep march)" in
*native*) CFLAGS+=" -march=native" ;;
esac

# needed for emacs-lsp-booster
export LSP_USE_PLISTS=true

path=(
	$GOPATH/bin(N-/)
	$HOME/.rustup/toolchains/*/bin(N-/)
	$HOME/.config/emacs/bin(N-/)
	$HOME/.cargo/bin/(N-/)
	$HOME/.local/bin(N-/)
	# $HOME/.config/zsh/shell-colorscripts/bin(N-/)
	$HOME/.dotnet/tools(N-/)
	$path
)
export PATH

fpath=(
	$ZDOTDIR/zfunc(N-/)
	$fpath
)
export FPATH
#
#
### RANDOM COLOR SCRIPT ###
# colorscript random
#
######## PLUGINS & THEMES #############--------------------------------------------------
source $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $ZDOTDIR/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source $ZDOTDIR/themes/powerlevel10k/powerlevel10k.zsh-theme
source $ZDOTDIR/plugins/git/git.plugin.zsh
# source $ZDOTDIR/plugins/dotnet/dotnet.plugin.zsh
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
