 
### RANDOM COLOR SCRIPT ###
colorscript random

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi
#
#
#
#
#
#
#
#
#
#
#
#
######## Functions ##########-----------------------------------------------------------
# Figure out the SHORT hostname
if [[ "$OSTYPE" = darwin* ]]; then
  # macOS's $HOST changes with dhcp, etc. Use ComputerName if possible.
  SHORT_HOST=$(scutil --get ComputerName 2>/dev/null) || SHORT_HOST="${HOST/.*/}"
else
  SHORT_HOST="${HOST/.*/}"
fi
#
#
#
#
#
#
#
#
#
#
####### Options ###########-------------------------------------------------------------
# Changing/making/removing directory
setopt auto_cd
setopt auto_pushd
setopt pushd_ignore_dups
setopt pushdminus
#
#
#
#
#
#
#
#
#
#
####### exports ##########--------------------------------------------------------------
export PATH=$PATH:/usr/local/go/bin
export PATH="$HOME/.config/emacs/bin:$PATH"
export PATH="/home/xero/.detaspace/bin:$PATH"
fpath+=~/.zfunc

export EDITOR='nvim'
export HISTFILE="$HOME/.zsh_history"    # History filepath
export HISTSIZE=10000                   # Maximum events for internal history
export SAVEHIST=10000                   # Maximum events in history file
#
#
#
#
#
#
#
#
#
#
#
#
######## PLUGINS & THEMES #############--------------------------------------------------
ZSHCONFIG=$HOME/.config/zsh

source $ZSHCONFIG/plugins/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
source $ZSHCONFIG/plugins/zsh-autocomplete/zsh-autocomplete.plugin.zsh
source $ZSHCONFIG/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.plugin.zsh
source $ZSHCONFIG/plugins/dotnet/dotnet.plugin.zsh
source $ZSHCONFIG/plugins/git/git.plugin.zsh
source $ZSHCONFIG/themes/powerlevel10k/powerlevel10k.zsh-theme
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
######### Aliases #########-------------------------------------------------------------
####### python aliases ##############--------------
alias py="python3"

# Remove python compiled byte-code and mypy/pytest cache in either the current
# directory or in a list of specified directories (including sub directories).
function pyclean() {
  find "${@:-.}" -type f -name "*.py[co]" -delete
  find "${@:-.}" -type d -name "__pycache__" -delete
  find "${@:-.}" -depth -type d -name ".mypy_cache" -exec rm -r "{}" +
  find "${@:-.}" -depth -type d -name ".pytest_cache" -exec rm -r "{}" +
}

# Find python file
alias pyfind="find . -name '*.py'"

# Grep among .py files
alias pygrep='rg -pg "*.py"'
# alias pygrep='grep -nr --include="*.py"'

###### Pip aliases ########------------------------
alias pipin="pip install"
alias pipup="pip install --upgrade"
alias pipun="pip uninstall"
alias pipgrep="pip freeze | rg"
alias pipout="pip list --outdated"

# Create requirements file
alias pipreq="pip freeze > requirements.txt"

# Install packages from requirements file
alias pipir="pip install -r requirements.txt"

# Update all installed packages
function pipupall {
  # non-GNU xargs does not support nor need `--no-run-if-empty`
  local xargs="xargs --no-run-if-empty"
  xargs --version 2>/dev/null | grep -q GNU || xargs="xargs"
  pip list --outdated | awk 'NR > 2 { print $1 }' | ${=xargs} pip install --upgrade
}

# Uninstall all installed packages
function pipunall {
  # non-GNU xargs does not support nor need `--no-run-if-empty`
  local xargs="xargs --no-run-if-empty"
  xargs --version 2>/dev/null | grep -q GNU || xargs="xargs"
  pip list --format freeze | cut -d= -f1 | ${=xargs} pip uninstall
}

######### venv utilities #########-----------------
# Activate the python virtual environment specified.
# If none specified, use 'venv'.
function runvenv() {
  local name="${1:-venv}"
  local venvpath="${name:P}"

  if [[ ! -d "$venvpath" ]]; then
    echo >&2 "Error: no such venv in current directory: $name"
    return 1
  fi

  if [[ ! -f "${venvpath}/bin/activate" ]]; then
    echo >&2 "Error: '${name}' is not a proper virtual environment"
    return 1
  fi

  . "${venvpath}/bin/activate" || return $?
  echo "Activated virtual environment ${name}"
}

# Create a new virtual environment, with default name 'venv'.
function mkvenv() {
  local name="${1:-venv}"
  local venvpath="${name:P}"

  python3 -m venv "${name}" || return
  echo >&2 "Created venv in '${venvpath}'"
  runvenv "${name}"
}


########## Misc aliases ##########------------------
alias zshconf="nvim ~/.zshrc"
alias matrix='neo-matrix -D -s -m" WE RULE THE WORLD! FROM THE SHADOWS "'
alias printcolors='for i in {0..255}; do print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$"\n"};done'
alias nvidia="sudo optirun -b none nvidia-settings -c :8"
alias smi="sudo nvidia-smi"
alias lg="lazygit"
alias t="et --hidden --icons --dirs-first"
alias c="clear"
alias q="exit"
alias v="nvim"

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
alias l="exa --long --grid --group-directories-first --git --header --icons --all"
alias lf="exa --grid --group-directories-first --git --header --icons --all"
alias docs="cd ~/Documents/"
alias data="cd /run/media/xero/Data/"
alias apps="cd /run/media/xero/Data/Apps/"
alias dotfiles="cd ~/.dotfiles/"

########## yt-dlp aliases ###########---------------
alias ytv-mkv-720='yt-dlp -f "bestvideo\[height<=720]+bestaudio/best\[height<=720]" -o "//run//media//xero//Data//Downloads//%(title)s.%(ext)s" --add-metadata --merge-output-format mkv'
alias ytv-mkv-1080='yt-dlp -f "bestvideo\[height<=1080]+bestaudio/best\[height<=1080]" -o "//run//media//xero//Data//Downloads//%(title)s.%(ext)s" --add-metadata --merge-output-format mkv'
alias ytv-mp4='yt-dlp -f bestvideo\[height<=720]+bestaudio -o "//run//media//xero//Data//Downloads//%(title)s.%(ext)s" --add-metadata --merge-output-format mp4'
alias ytv-best='yt-dlp -f bestvideo+bestaudio -o "//run//media//xero//Data//Downloads//%(title)s.%(ext)s" --add-metadata --merge-output-format mp4'
alias yta-mp3='yt-dlp -f bestaudio --extract-audio --audio-format mp3 -o "//run//media//xero//Data//Downloads//%(title)s.%(ext)s" --add-metadata'
alias ytp-mkv-list-720="yt-dlp -f 'bv*[height=720]+ba' -o '//run//media//xero//Data//Downloads//%(playlist_title)s//%(title)s.%(ext)s' --add-metadata --merge-output-format mkv"
alias ytp-mkv-list-1080="yt-dlp -f 'bv*[height=1080]+ba' -o '//run//media//xero//Data//Downloads//%(playlist_title)s//%(title)s.%(ext)s' --add-metadata --merge-output-format mkv"
alias ytp-mp4-list-720="yt-dlp -f 'bv*[height=720]+ba' -o '//run//media//xero//Data//Downloads//%(playlist_title)s//%(title)s.%(ext)s' --add-metadata --merge-output-format mp4"
alias ytp-mp4-list-1080="yt-dlp -f 'bv*[height=1080]+ba' -o '//run//media//xero//Data//Downloads//%(playlist_title)s//%(title)s.%(ext)s' --add-metadata --merge-output-format mp4"

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
autoload -U compinit; compinit
. "$HOME/.cargo/env"

