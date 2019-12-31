
PS1='\u@\h \w\$ '
HISTIGNORE="&:ls:cd:fg:jobs:pushd:popd:dirs:history"

complete -d {cd,pushd,rmdir}
complete -c {which,type}
complete -u {su,finger}

alias ls='ls -lFtrG'
alias pwd='pwd -P'
alias dir='ls -C'
alias cp='cp -ip'
alias mv='mv -iv'
alias rm='rm -v'
alias grep='grep --color=auto'
alias emacs='emacs -nw'
alias u='pushd'
alias o='popd'

export TERM="xterm-256color"

export PATH='~/nox/bin:/usr/local/bin:/usr/local/sbin:/bin:/sbin:/usr/bin:/usr/sbin:~/.roswell/bin'

export LDFLAGS="-L/usr/local/opt/readline/lib"
export CPPFLAGS="-I/usr/local/opt/readline/include"

export BLOCKSIZE=K
export EDITOR='emacs -nw'
export PAGER='less'
export LESS='-mR'

export PATH="/usr/local/opt/openssl/bin:$PATH"
export LDFLAGS="-L/usr/local/opt/openssl/lib $LDFLAGS"
export CPPFLAGS="-I/usr/local/opt/openssl/include $CPPFLAGS"
export PKG_CONFIG_PATH="/usr/local/opt/openssl/lib/pkgconfig"
# Initialization for FDK command line tools.Thu Nov 22 18:07:36 2018
FDK_EXE="/Users/nyx-a/bin/FDK/Tools/osx"
PATH=${PATH}:"/Users/nyx-a/bin/FDK/Tools/osx"
export PATH
export FDK_EXE

# ruby
export RBENV_ROOT="${HOME}/.rbenv"
if [ -d "${RBENV_ROOT}" ]; then
  export PATH="${RBENV_ROOT}/bin:${PATH}"
  eval "$(rbenv init -)"
fi

# go
GOPATH='~/GO'
export PATH="$GOPATH/bin:$PATH"

# rust
export PATH="$HOME/.cargo/bin:$PATH"

# qt
export CPPFLAGS="-I/usr/local/opt/qt5/include"
export LDFLAGS="-L/usr/local/opt/qt5/lib"
export PATH=/usr/local/opt/qt5/bin:$PATH

# nvm
export NVM_DIR="$HOME/.nvm"
[ -s "/usr/local/opt/nvm/nvm.sh" ] && . "/usr/local/opt/nvm/nvm.sh"  # This loads nvm
[ -s "/usr/local/opt/nvm/etc/bash_completion" ] && . "/usr/local/opt/nvm/etc/bash_completion"  # This loads nvm bash_completion
