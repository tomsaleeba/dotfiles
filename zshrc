# zmodload zsh/zprof # uncomment, along with last line, for profiling

# Useful keybindings:
#   c-x c-e: will edit the current command in $EDITOR

# If you come from bash you might have to change your $PATH.
export PATH=$HOME/.config/i3/scripts/:$PATH
export PATH=$HOME/.config/yarn/global/node_modules/.bin:$PATH
export PATH=$HOME/bin:$PATH # needs to be early in the list

if [ "$(uname)" = "Darwin" ]; then
  export PATH="/usr/local/opt/mysql-client/bin:$PATH"
fi
export PASSWORD_STORE_ENABLE_EXTENSIONS=true

export NVM_DIR=$HOME/.nvm

possibleJavaHome=/usr/lib/jvm/default-runtime
[ -d $possibleJavaHome ] && {
  export JAVA_HOME=$possibleJavaHome
}

# Path to your oh-my-zsh installation.
export ZSH="$HOME/.oh-my-zsh"

# Set name of the theme to load --- if set to "random", it will
# load a random theme each time oh-my-zsh is loaded, in which case,
# to know which specific one was loaded, run: echo $RANDOM_THEME
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME=${ZSH_THEME_OVERRIDE:-agnoster}

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

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
COMPLETION_WAITING_DOTS="true"

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
plugins=(zsh-autosuggestions zsh-z)
# git clone https://github.com/agkozak/zsh-z ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-z
# git clone https://github.com/zsh-users/zsh-autosuggestions ${ZSH_CUSTOM:-~/.oh-my-zsh/custom}/plugins/zsh-autosuggestions

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
EDITOR='nvim'

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

alias l=ls
alias lrt='ls -lrt'
alias venv2='virtualenv -p python2 .venv && . .venv/bin/activate'
alias venv3='virtualenv -p python3 .venv && . .venv/bin/activate'
alias vim='nvim'
alias vi='vim'
alias vf='f=$(fd --hidden --exclude .git --type f | fzf); [ -n "$f" ] && vim $f'
alias edit='vim'
alias editzsh='/usr/bin/nvim ~/.zshrc && source ~/.zshrc'
alias editi3='/usr/bin/nvim ~/.config/i3/config'
alias editrofi='nvim ~/.config/rofi/config'
alias editvim='nvim ~/.vimrc'
editssh() {
  /usr/bin/nvim ~/.ssh/config
  # clear rofi SSH cache
  # TODO be smarter and only clear the entries that no longer exit
  rm -f ~/.cache/rofi-2.sshcache
}
alias xo=xdg-open
alias icat='kitty +kitten icat'
alias kcat=icat
alias tw=timew
alias tws="timew summary :ids"
alias tww="timew week"
alias gci="git commit -m"
alias gco="git checkout"
alias gd="git diff"
alias gds="git diff --staged"
alias ga="git add"
alias ga.="git add ."
alias gs="git status"
alias gpl="git pull --prune"
alias gps="git push"
alias gb="git branch"
alias gbD="git branch -D"
alias gm="git merge"
function nofj {
  # runs a command without firejail
  PATH=/usr/bin:$PATH $*
}
alias gcloud='firejail --quiet /zeta/tools/google-cloud-sdk/bin/gcloud'


bindkey \^U backward-kill-line

if [ "$(uname)" = "Darwin" ]; then
  bindkey "\e[1;3D" backward-word # ⌥←
  bindkey "\e[1;3C" forward-word # ⌥→
fi

# make a temp dir and cd to it
cdtemp() {
  suffix=$1
  if [ ! -z $suffix ]; then
    fragment="--suffix=-$1"
  fi
  cd `mktemp --directory $fragment`
}

# thanks https://stackoverflow.com/a/24347344/1410035
ssh-auth() {
  # make sure your ssh keys are added with: ssh-add <key>
  local theFile=/tmp/ssh-agent.sh
  [ "$(uname)" = "Darwin" ] && {
    local sshAgentPid=$(ps -A | grep ssh-agent | awk '{print $1}')
    return # FIXME remove line, actually handle for macOS
  } || {
    local sshAgentPid=`ps -C ssh-agent -o pid:1=` # :1 means no padding spaces
  }

  # if we have a file, source it
  [[ -f "$theFile" ]] && {
    # echo "[SSH] Sourcing $theFile" > /dev/stderr
    source $theFile > /dev/null
  } || ( [[ ! -z "$sshAgentPid" ]] && {
    # if the agent is running, but there's no file (not sure why), fix the situation
    echo "[SSH] ssh-agent is running, but $theFile does not exist" > /dev/stderr
    pkill ssh-agent
    unset SSH_AGENT_PID
  } )

  # if we have an agent PID, but it's wrong, fix it
  [[ ! -z "$SSH_AGENT_PID" ]] && [[ "$sshAgentPid" != "$SSH_AGENT_PID" ]] && {
    echo "[SSH] SSH_AGENT_PID($SSH_AGENT_PID) != PID from ps($sshAgentPid)" > /dev/stderr
    rm -f $theFile
    pkill ssh-agent
    unset SSH_AGENT_PID
  } || {
    # echo "[SSH] SSH_AGENT_PID is not set or SSH_AGENT_PID($SSH_AGENT_PID) == PID from ps($sshAgentPid)" > /dev/stderr
  }

  # Start the SSH agent only if not running
  [[ -z "$sshAgentPid" ]] && {
    echo "[SSH] starting ssh-agent" > /dev/stderr
    echo $(ssh-agent) > $theFile
  }

  # Identify the running SSH agent
  [[ -z $SSH_AGENT_PID ]] && {
    echo "[SSH] SSH_AGENT_PID is not set, sourcing $theFile" > /dev/stderr
    source $theFile > /dev/null
  }

  # be sure to add your keys in ~/.ssh/config with AddKeysToAgent and they'll be lazy loaded
  # echo "[SSH] SSH_AGENT_PID=$SSH_AGENT_PID" > /dev/stderr
}
git() { ssh-auth; command git "$@" }
export ssh() { ssh-auth; command ssh "$@"; }
scp() { ssh-auth; command scp "$@"; }
pass() {
  export EDITOR='firejail --net=none --noprofile /usr/bin/nvim -u NORC'
  ssh-auth
  command pass "$@"
}

kssh() {
  ssh-auth
  kitty +kitten ssh $@
}

# thanks for the lazy-load idea https://medium.com/@dannysmith/little-thing-2-speeding-up-zsh-f1860390f92
nvm() {
  unset -f nvm
  if [ -f /usr/share/nvm/nvm.sh ]; then
    # install the "nvm" package from AUR
    echo "NVM not loaded! Loading AUR installation now..."
    source /usr/share/nvm/nvm.sh
    source /usr/share/nvm/install-nvm-exec
  elif [ -f ~/.nvm/nvm.sh ]; then
    # install from nvm site
    echo "NVM not loaded! Loading user installation now..."
    source ~/.nvm/nvm.sh
    # source ~/.nvm/nvm-exec # FIXME do we need this?
  elif [ -f $BREW_NVM_DIR/nvm.sh ]; then
    # install from homebrew on macOS
    source /usr/local/opt/nvm/nvm.sh
    # source /usr/local/opt/nvm/nvm-exec # FIXME do we need this?
  else
    echo "[ERROR] no nvm installation found"
    return 1
  fi
  source $ZSH/plugins/nvm/nvm.plugin.zsh
  nvm "$@"
}

nvmPreload() {
  theCmd=${1:?name of command to run}
  which npm &> /dev/null || {
    echo "running 'nvm ls' to load correct node"
    nvm ls > /dev/null
  }
  unalias $theCmd
  eval $@
}
alias mix='nvmPreload mix'
alias pnpm='nvmPreload pnpm'
# can't make an npm alias because it's our canary above
alias quasar='nvmPreload quasar'
alias yarn='nvmPreload yarn'

secretFile=$HOME/.secret-zshrc
if [ -f $secretFile ]; then
  source $secretFile
fi

# thanks for once-a-day compinit https://medium.com/@dannysmith/little-thing-2-speeding-up-zsh-f1860390f92
autoload -Uz compinit
for dump in ~/.zcompdump(N.mh+24); do
  compinit
done
compinit -C

if [ -z $INSIDE_EMACS -a -z $VIMRUNTIME ]; then # emacs gets really messed up with a right side of the prompt, it's merely annoying in neovim
  # add timestamp and execution time to the right prompt, thanks https://coderwall.com/p/kmchbw/zsh-display-commands-runtime-in-prompt
  function preexec() {
    timer=${timer:-$SECONDS}
  }
  function precmd() {
    if [ $timer ]; then
      timer_show=$(($SECONDS - $timer))
      # thanks for the time layout https://stackoverflow.com/a/48341347/1410035
      export RPROMPT="%F{cyan}${timer_show}s %{$fg[yellow]%}[%D{%H:%M:%S} %D] %{$reset_color%}"
      unset timer
    fi
  }
fi

# change word navigation to treat hyphens like I want
# thanks https://unix.stackexchange.com/a/48589/68885
WORDCHARS='|*?_-.[]~=&;!#$%^(){}<>'

export jsbeautify_indent_size=2

export BROWSER=/usr/local/bin/firefox

# cache busting for command completion
# thanks https://unix.stackexchange.com/a/2180/68885
zstyle ":completion:*:commands" rehash 1

AUR_NVM_DIR=/usr/share/nvm
BREW_NVM_DIR=/usr/local/opt/nvm
if [ -s "$NVM_DIR/bash_completion" ]; then
  # For nvm installed by bash script from their site
  \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
elif [ -s "$AUR_NVM_DIR/bash_completion" ]; then
  # For nvm installed from AUR
  \. "$AUR_NVM_DIR/bash_completion"  # This loads nvm bash_completion
elif [ -s "$BREW_NVM_DIR/etc/bash_completion.d/nvm" ]; then
  # For nvm installed from homebrew on macOS
  \. "$BREW_NVM_DIR/etc/bash_completion.d/nvm"  # This loads nvm bash_completion
  \. "/usr/local/opt/nvm/nvm.sh"                # This loads nvm
else
  echo "No NVM bash completion"
fi

# The next line enables shell command completion for gcloud.
if [ -f '/zeta/tools/google-cloud-sdk/completion.zsh.inc' ]; then . '/zeta/tools/google-cloud-sdk/completion.zsh.inc'; fi

# zprof # uncomment, along with first line, for profiling
