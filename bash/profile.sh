alias ls='ls --color'
export EDITOR="emacs -nw"
alias e='emacs -nw'
alias sudoe='sudo -E bash -c "PATH=$PATH LD_LIBRARY_PATH=$LD_LIBRARY_PATH emacs -nw"'

#To enable aliases with sudo
alias sudo='/usr/bin/sudo '

# Pretty prompt with git branch
function parse_git_branch()
{
    branch=$(git branch 2> /dev/null | grep '* ' | sed -e 's/* //')
    if [[ -z $branch ]]; then
        echo ""
    else
        #On croppe le nom s'il est trop long
        if (( $(expr length "$branch") > 30 )); then
            branch=$(printf '%-.30s...' "$branch")
        fi
        echo "($branch) "
    fi
}
red='\[\e[0;31m\]'
blue='\[\e[1;34m\]'
white='\[\e[0;37m\]'
magenta='\[\e[0;35m\]'
green='\[\e[0;32m\]'
cyan='\[\e[0;36m\]'
export PS1="${red}[\t] ${blue}\u${white}@${magenta}\h ${cyan}\$(parse_git_branch)${green}\w ${white}> "
