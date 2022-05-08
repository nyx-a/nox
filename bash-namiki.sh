
PS1='\u@\h \w\$ '
HISTIGNORE="&:ls:cd:fg:jobs:pushd:popd:dirs:history"

complete -d {cd,pushd,rmdir}
complete -c {which,type}
complete -u {su,finger}

export PATH="~/nox/bin:$PATH"
export LESS='-R'

# BEGIN_KITTY_SHELL_INTEGRATION
if test -n "$KITTY_INSTALLATION_DIR" -a -e "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; then source "$KITTY_INSTALLATION_DIR/shell-integration/bash/kitty.bash"; fi
# END_KITTY_SHELL_INTEGRATION

# homebrew
eval "$(/opt/homebrew/bin/brew shellenv)"
export HOMEBREW_NO_INSTALL_CLEANUP=true
export HOMEBREW_NO_ENV_HINTS=true

# ruby
export RBENV_ROOT="${HOME}/.rbenv"
if [ -d "${RBENV_ROOT}" ]; then
  export PATH="${RBENV_ROOT}/bin:${PATH}"
  eval "$(rbenv init -)"
fi

. "$HOME/.cargo/env"

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
alias exa='exa -l -snew'
alias s='git status -s'

if [[ -x `which colordiff` ]]; then
  alias diff='colordiff -u'
else
  alias diff='diff -u'
fi

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi
