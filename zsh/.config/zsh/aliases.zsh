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
alias t="erd --hidden --icons --dirs-first --inverted --follow --human"
alias c="clear"
alias q="exit"
alias v="nvim"
alias sv="sudoedit"
alias snvim="sudoedit"

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
