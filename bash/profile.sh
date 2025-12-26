alias ls='ls --color'
export EDITOR="emacs -nw"
alias e='emacs -nw'
function sudoe()
{
    sudo -E bash -c "PATH=$PATH LD_LIBRARY_PATH=$LD_LIBRARY_PATH emacs -nw $@"
}

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

function addpath() {
    args=()
    variables=("PATH")
    append=false
    remove=false
    while [[ "$#" -gt 0 ]]; do
        case $1 in
            -h|--help)
                cat <<EOF
Adds the input folders to the PATH or LD_LIBRARY_PATH env variables

By default, the paths are added only to PATH

Usage :
      addpath [OPTIONS] FOLDERS_TO_ADD

Options :
        -h, --help   : Displays this help message
        -a, --append : The paths are appended instead of prepended
        -r, --remove : The paths are removed instead of added
        --and-ld     : The paths are added both to PATH and LD_LIBRARY_PATH
        --ld         : The paths are added only to LD_LIBRARY_PATH, not to PATH
EOF
                return 0
                ;;
            -a|--append) append=true;;
            -r|--remove) remove=true;;
            --and-ld) variables=("PATH" "LD_LIBRARY_PATH");;
            --ld) variables=("LD_LIBRARY_PATH");;
            *)
                if [ -d "$1" ]; then
                    args+=("$1");
                else
                    echo "WARNING : Ignoring folder $1 which does not exist"
                fi ;;
        esac
        shift
    done
    for arg in "${args[@]}"; do
        for var in "${variables[@]}"; do
            if [[ $remove = true ]]; then
                eval $var=$(echo ${!var} | sed "s|:*$arg:*|:|g" | sed 's/:$//g' | sed 's/^:*//g')
            elif [[ ":${!var}:" != *":$arg:"* ]]; then
                if [[ $append == true ]]; then
                    eval $var="${!var:+${!var}:}$arg"
                else
                    eval $var="$arg${!var:+:${!var}}"
                fi
                export $var
            fi
        done
    done
}
test -d ~/.local/bin || mkdir ~/.local/bin
addpath ~/.local/bin

# Add npm binaries to the PATH if enabled, typically
#   > npm config set prefix=$HOME/.local/npm
#   > npm install -g typescript-language-server
NPM_BIN_DIR=~/.local/npm/bin
if [[ -d $NPM_BIN_DIR ]]; then
    addpath $NPM_BIN_DIR
fi

alias paclare="uvx --with-editable ~/dev/horrih/paclare/ paclare"
