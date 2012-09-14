echo "Reading .bashrc..."

function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ \(\1\)$(parse_git_dirty)/"
}

export PS1="\033[0;33m\]\u@\h \033[0;36m\]\w\[\033[0;32m\]\$(parse_git_branch)\033[1;37m\]$ "


[[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm" # Load RVM into a shell session *as a function*

PATH=$PATH:$HOME/.rvm/bin:~/bin:/usr/local/sbin

if [ -f `brew --prefix`/etc/bash_completion ]; then
    . `brew --prefix`/etc/bash_completion
fi

alias ctags="`brew --prefix`/bin/ctags"