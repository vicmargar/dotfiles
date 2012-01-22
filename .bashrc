function parse_git_dirty {
  [[ $(git status 2> /dev/null | tail -n1) != "nothing to commit (working directory clean)" ]] && echo "*"
}

function parse_git_branch {
  git branch --no-color 2> /dev/null | sed -e '/^[^*]/d' -e "s/* \(.*\)/ \(\1\)$(parse_git_dirty)/"
}

export PS1="\033[0;33m\]\u@\h \033[0;36m\]\w\[\033[0;32m\]\$(parse_git_branch)\033[1;37m\]$ "

alias ns-fe1='ssh deployer@fe1.niftyschool.com'
alias ns-db1='ssh deployer@db1.niftyschool.com'
alias st-ns-fe1='ssh deployer@fe1.niftyschool-staging.com'
alias st-ns-db1='ssh deployer@db1.niftyschool-staging.com'

alias uk_o2_tunnel='ssh -L 7600:158.230.68.2:8100 91.216.137.155 -N'

export PATH=~/bin:/opt/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/opt/local/sbin:/opt/R13B04/bin:/usr/local/lib/erlang/bin:/usr/local/lib/erlang/lib/rabbitmq-server-1.7.0/scripts:/usr/local/bin:/usr/local/sbin:/opt/nginx/sbin/

killname(){
 name = $1
 `ps axu | grep $name | grep -v grep | awk '{print $2 " " $11}' | grep $name | awk '{print $1}'`
}

if [[ -s /Users/victor/.rvm/scripts/rvm ]] ; then source /Users/victor/.rvm/scripts/rvm ; fi
  [[ -s "$HOME/.rvm/scripts/rvm" ]] && source "$HOME/.rvm/scripts/rvm"  # This loads RVM into a shell session.
