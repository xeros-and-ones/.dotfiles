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

bindkey -e
####### exports ##########--------------------------------------------------------------
VISUAL="emacsclient -c -a ''"
EDITOR="emacsclient -t -a ''"
SUDO_EDITOR="emacsclient -t -a ''"
GIT_EDITOR='emacsclient -t -a '''
HISTFILE="$HOME/.zsh_history" # History filepath
SAVEHIST=$((100 * 1000))      # Use multiplication for readability.
HISTSIZE=$((1.2 * SAVEHIST))  # Zsh recommended value
# needed for emacs-lsp-booster
LSP_USE_PLISTS=true

# Safely apply -march=native only if supported
# export CFLAGS="-O2 -flto=auto -pipe"
# case "$(gcc -march=native -Q --help=target | grep march)" in
# *native*) CFLAGS+=" -march=native" ;;
# esac

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
######## PLUGINS & THEMES #############--------------------------------------------------
source $ZDOTDIR/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $ZDOTDIR/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZDOTDIR/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source $ZDOTDIR/plugins/git/git.plugin.zsh
# source $ZDOTDIR/plugins/dotnet/dotnet.plugin.zsh
#
#
#
###### Aliases #####------------------

alias zshconf="$EDITOR $ZDOTDIR/.zshrc"
alias matrix='neo-matrix -D -s -m" WE RULE THE WORLD! FROM THE SHADOWS "'
alias printcolors='for i in {0..255}; do print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$"\n"};done'
alias nvidia="sudo optirun -b none nvidia-settings -c :8" #
alias nv="envycontrol"
alias smi="sudo nvidia-smi"
alias lg="lazygit"
alias tl="erd -fI.H --long -yinverted -Cauto --dir-order=first --sort=name --no-git"
alias t="erd -flI.H -yinverted -Cauto --dir-order=first --sort=name --no-git"
alias c="clear"
alias q="exit"
alias v="nvim"
alias e="emacsclient -t -a ''"
alias ev="emacsclient -c -a ''"
alias sv="sudoedit"
alias vconf="cd ~/.config/nvim/ && nvim"
alias chadconf="cd ~/.dotfiles/XeroChad/ && nvim"
alias vd='deactivate'
alias stows='stow -vSt ~'
alias stowd='stow -vDt ~'
alias z='zellij'
alias zd='zellij attach default -c'
alias soz='source ~/.config/zsh/.zshrc'

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'
alias -g ......='../../../../..'

alias -- -='cd -'
alias 1='cd -1'
alias 2='cd -2'
alias 3='cd -3'
alias 4='cd -4'
alias 5='cd -5'
alias 6='cd -6'
alias 7='cd -7'
alias 8='cd -8'
alias 9='cd -9'

########## folders aliases ###########---------------
alias l="eza --long --grid --group-directories-first --git --header --icons --all"
alias lf="eza --grid --group-directories-first --git --header --icons --all"
alias docs="cd ~/Documents/"
alias data="cd /mnt/Data/"
alias apps="cd /mnt/Data/Apps/"
alias dotfiles="cd ~/.dotfiles/"

########## yt-dlp aliases ###########---------------
alias ytv-720='noglob yt-dlp -f "bv*[height<=720]+ba/b[height<=720]"'
alias ytv-1080='noglob yt-dlp -f "bv*[height<=1080]+ba/b[height<=1080]"'
alias ytv-720-sub='noglob yt-dlp -f "bv*[height<=720]+ba/b[height<=720]" --sub-langs=eng --embed-subs'
alias ytv-1080-sub='noglob yt-dlp -f "bv*[height<=1080]+ba/b[height<=1080]" --sub-langs=eng --embed-subs'
alias ytp-720="noglob yt-dlp -f 'bv*[height=720]+ba/b[height<=720]' -o '//mnt//Data//Downloads//Youtube//%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s'"
alias ytp-720-test="noglob yt-dlp -f 'bv*[height=720]+ba' -o '//mnt//Data//Downloads//Youtube//%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s' --merge-outout-format mp4"
alias ytp-1080="noglob yt-dlp -f 'bv*[height=1080]+ba/b[height<=1080]' -o '//mnt//Data//Downloads//Youtube//%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s'"
alias ytp-720-sub="noglob yt-dlp -f 'bv*[height=720]+ba/b[height<=720]' -o '//mnt//Data//Downloads//Youtube//%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s' --sub-langs=eng --embed-subs"
alias ytp-1080-sub="noglob yt-dlp -f 'bv*[height=1080]+ba/b[height<=1080]' -o '//mnt//Data//Downloads//Youtube//%(playlist)s/%(playlist_index)s - %(title)s.%(ext)s' --sub-langs=eng --embed-subs"
alias yta-1080="noglob yt-dlp -f 'ba/b[height<=1080]' -x --audio-format mp3 --embed-thumbnail -o '//mnt//Data//Downloads//Music - %(title)s.%(ext)s'"

eval "$(oh-my-posh init zsh)"
