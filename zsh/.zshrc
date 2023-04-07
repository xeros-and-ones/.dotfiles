### RANDOM COLOR SCRIPT ###
colorscript random

# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block; everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"
export PATH=$PATH:/usr/local/go/bin
export PATH="$HOME/.emacs.d/bin:$PATH"
export PATH="/home/xero/.detaspace/bin:$PATH"
fpath+=~/.zfunc
# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/ohmyzsh/ohmyzsh/wiki/Themes
ZSH_THEME="powerlevel10k/powerlevel10k"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in $ZSH/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"


# Uncomment one of the following lines to change the auto-update behavior
# zstyle ':omz:update' mode disabled  # disable automatic updates
# zstyle ':omz:update' mode auto      # update automatically without asking
zstyle ':omz:update' mode reminder  # just remind me to update when it's time

# Uncomment the following line to change how often to auto-update (in days).
zstyle ':omz:update' frequency 13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS="true"

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# You can also set it to another string to have that shown instead of the default red dots.
# e.g. COMPLETION_WAITING_DOTS="%F{yellow}waiting...%f"
# Caution: this setting can cause issues with multiline prompts in zsh < 5.7.1 (see #5765)
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# You can set one of the optional three formats:
# "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# or set a custom format using the strftime function format specifications,
# see 'man strftime' for details.
HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in $ZSH/plugins/
# Custom plugins may be added to $ZSH_CUSTOM/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
    git
    zsh-autosuggestions
    # zsh-syntax-highlighting
    fast-syntax-highlighting
    # zsh-interactive-cd
    zsh-autocomplete
    virtualenv
    aliases
    rust
    fzf
    command-not-found
    alias-finder
    dotnet
    )

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='nvim'
# else
export EDITOR='nvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"
# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
LC_ALL=en_US.UTF-8

####### python aliases ##############----------------------------------------------------
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


########## Misc aliases ##########################--------------------------------------
alias zshconf="nvim ~/.zshrc"
alias matrix='neo-matrix -D -s -m" WE RULE THE WORLD! FROM THE SHADOWS "'
alias printcolors='for i in {0..255}; do print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$"\n"};done'
alias nvidia="sudo optirun -b none nvidia-settings -c :8"
alias smi="sudo nvidia-smi"
alias lg="lazygit"
alias t="et"
alias c="clear"
alias q="exit"
alias v="nvim"

########## folders aliases ########################-------------------------------------
alias l="exa --long --grid --group-directories-first --git --header --icons --all"
alias lf="exa --grid --group-directories-first --git --header --icons --all"
alias docs="cd ~/Documents/"
alias data="cd /run/media/xero/Data/"
alias apps="cd /run/media/xero/Data/Apps/"
alias dotfiles="cd ~/.dotfiles/"

########## yt-dlp aliases #########################--------------------------------------
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
