
#Get which commit was used in submodule $2 for super commit $1
# arg 1, super rev to parse
# arg 2, submodule to parse
get_submodule_commit(){
    local superrev
    local rev
    superrev=`git rev-parse --verify $1 2> /dev/null`
    if [ $? -ne 0 ]; then
        echo "Can't figure out what $1 points to"
        exit 1;
    fi
    rev=`git ls-tree ${superrev} $2  | \
        sed -nr "s/^160000 commit ([a-z0-9]*).*$/\1/p" 2>/dev/null`
    if [ -z "${rev}" ] 
    then
        echo "Can't find $2 commit in super project ${superrev}"
        exit 1
    else
        echo "${rev}"
        exit 0
    fi
}

#Takes two commit and returns the decendant if possible
get_decendant(){
    git merge-base --is-ancestor $1 $2
    if [ $? -eq 0 ]
    then 
        echo "$2"
        return 0
    else
        if [ $? -eq 1 ]
        then
            git merge-base --is-ancestor $2 $1
            if [ $? -eq 0 ] 
            then
                echo "$1"
                return 0
            else
                return 1
            fi
        else
            echo "Unkown error"
            return 2
        fi
    fi  
}
