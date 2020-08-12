# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH

# Path to your oh-my-zsh installation.
export ZSH="/home/creed/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="avit"

# Set list of themes to pick from when loading at random
# Setting this variable when ZSH_THEME=random will cause zsh to load
# a theme from this variable instead of looking in ~/.oh-my-zsh/themes/
# If set to an empty array, this variable will have no effect.
# ZSH_THEME_RANDOM_CANDIDATES=( "robbyrussell" "agnoster" )

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion.
# Case-sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to automatically update without prompting.
# DISABLE_UPDATE_PROMPT="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line if pasting URLs and other text is messed up.
# DISABLE_MAGIC_FUNCTIONS=true

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
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
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load?
# Standard plugins can be found in ~/.oh-my-zsh/plugins/*
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(git)

source $ZSH/oh-my-zsh.sh

# User configuration

# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

# Preferred editor for local and remote sessions
# if [[ -n $SSH_CONNECTION ]]; then
#   export EDITOR='vim'
# else
#   export EDITOR='mvim'
# fi

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Set personal aliases, overriding those provided by oh-my-zsh libs,
# plugins, and themes. Aliases can be placed here, though oh-my-zsh
# users are encouraged to define aliases within the ZSH_CUSTOM folder.
# For a full list of active aliases, run `alias`.
#
# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# ---------------------------- MY CONFIG ----------------------------

# Desktop place :-)
alias desktop='cd /mnt/c/Users/mota/Desktop'
alias prog='cd /mnt/c/Users/mota/Desktop/LinuxWorkStation/Prog'
alias l='vim .log'
alias vim='nvim'

export PATH=$PATH:/home/creed/.cabal/bin/
export PATH=$PATH:/home/creed/haskell/cabal/bin/

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion

# Cabal with diff versions
GHC865=/opt/ghc/8\.6\.5/bin/ghc
GHC_LIVE=/mnt/c/Users/mota/Desktop/LinuxWorkStation/Prog/ghc/inplace/bin/ghc-stage2

# Entering the nix environment
alias enter-nix='source ~/.nix-profile/etc/profile.d/nix.sh'

# Adding .local/bin to PATH
export PATH=/home/creed/.local/bin:$PATH

# Unset <c-s>
stty -ixon

# Entering the nix environment
alias enter-nix='source ~/.nix-profile/etc/profile.d/nix.sh'

# For vim colors to work properly
export TERM=screen-256color

# Tmux usage
#[[ -z "$TMUX" && -n "$USE_TMUX" ]] && {
#    [[ -n "$ATTACH_ONLY" ]] && {
#        tmux a 2>/dev/null || {
#            cd && exec tmux
#        }
#        exit
#    }
#
#    tmux new-window -c "$PWD" 2>/dev/null && exec tmux a
#    exec tmux
#}

# export LS_COLORS="$LS_COLORS:ow=7:tw=7:"
export LS_COLORS='rs=0:di=01;34:ln=01;36:mh=00:pi=40;33'

export GIT_EDITOR='nvr --remote-wait-silent'

alias eme='
export DISPLAY=:0.0
export LIBGL_ALWAYS_INDIRECT=1
setxkbmap -layout us
setsid emacs
'

alias dotfiles='git --git-dir=$HOME/.dotfiles/ --work-tree=$HOME'

# export DISPLAY=:0.0
# export LIBGL_ALWAYS_INDIRECT=1
#
# setxkbmap -option caps:swapescape
#
# # Map an unused modifier's keysym to the spacebar's keycode and make it a
# # control modifier. It needs to be an existing key so that emacs won't
# # spazz out when you press it. Hyper_L is a good candidate.
# spare_modifier="Hyper_L"
# xmodmap -e "keycode 65 = $spare_modifier"
# xmodmap -e "remove mod4 = $spare_modifier" # hyper_l is mod4 by default
# xmodmap -e "add Control = $spare_modifier"
#
# # Map space to an unused keycode (to keep it around for xcape to
# # use).
# xmodmap -e "keycode any = space"
#
# # Finally use xcape to cause the space bar to generate a space when tapped.
# xcape -e "$spare_modifier=space"

if [[ $TERM = dumb ]]; then
  unset zle_bracketed_paste
fi
[ -f "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env" ] && source "${GHCUP_INSTALL_BASE_PREFIX:=$HOME}/.ghcup/env"
if [ -e /home/creed/.nix-profile/etc/profile.d/nix.sh ]; then . /home/creed/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

eval "$(direnv hook zsh)"
