######### Aliases #########-------------------------------------------------------------

alias zshconf="$EDITOR $ZDOTDIR/.zshrc"
alias matrix='neo-matrix -D -s -m" WE RULE THE WORLD! FROM THE SHADOWS "'
alias printcolors='for i in {0..255}; do print -Pn "%K{$i}  %k%F{$i}${(l:3::0:)i}%f " ${${(M)$((i%6)):#3}:+$"\n"};done'
alias nvidia="sudo optirun -b none nvidia-settings -c :8"
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
