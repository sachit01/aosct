#!/bin/bash

# This script is intended to be used to update 
# this repo after each of your submodule changes
# have been merged to master. Before running, 
# checkout the change branch in each submodule 
# wish to update.

# When run this script finds all submodules that have 
# changed from last commit. For each of them it tries
# to find a commit on the submodules' master branch 
# that have the same change id. If found it does a checkout 
# of this commit.

IFS=$'\n'

source scripts/lib.sh

function checkClean(){
   git diff-index --quiet HEAD --
   if [ ! 0 -eq $? ]; then
        echoError "Submodule $1 contains uncommited changes, ABORTING" 
        exit 1
    fi 
}

function getGitState(){
    gitState=$(git rev-parse --abbrev-ref HEAD)
    if [ $? -eq 0 ]; then
        if [ "$ref" == "HEAD" ]; then
               gitState=$(git rev-parse --verify HEAD)
               if [ $? -eq 0 ]; then
                    return 0    
                fi
        else
            return 0
        fi
    fi
    echoError "Failed to get state"
    exit 1
}

function revertGitState(){
    if [ ! -z "$gitState" ]; then
        out=$(git checkout $gitState 2>&1)
        if [ ! 0 -eq $? ]; then
            echo "$out"
        else
            return 0
        fi
    fi
    echoError "Could not restore submodule state correctly"
    exit 1
}

function echoError(){
    echo -e "[\e[31m-\e[39m] \e[31m$1\e[39m" 
}
function echoSuccess(){
    echo -e "[\e[32m+\e[39m] $1" 
}
function echoStatus(){
    echo -e "[\e[33m~\e[39m] $1" 
}

function setSubmodule(){
    getGitState
    git rev-parse --abbrev-ref HEAD -- | grep master > /dev/null
    if [ 0 -eq $? ]; then
        echoError "$1 is already on master, ABORTING"
        echo "Please checkout the feature branch"
        return 1
    fi
    

    local commit=$(git rev-parse --verify HEAD)
    local r1=$?
    local body=$(git show --format="%b" --no-abbrev-commit --no-patch $commit )
    local r2=$?
    ( [ ! 0 -eq $r1 ] || [ ! 0 -eq $r2 ]  ) && echoError "Failed to parse $1" && return 1
    local changeID=$(echo "$body" |sed  -rn 's/^[ \t]*Change-Id:[ \t]*([a-zA-z0-9]*)[ \t]*$/\1/p')
    if [ -z "$changeID" ]; then
        echoError "HEAD on $1 does not contain a changeID" 
        return 1
    fi
    out=$(git checkout master 2>&1)
    if [ ! 0 -eq $? ]; then
       echo "$out"
       echoError "Failed to checkout master branch"
       return 1
    fi

    out=$(git merge origin/master 2>&1)
    if [ ! 0 -eq $? ]; then
       echo "$out"
       echoError "Failed to merge master branch"
       revertGitState
       return 1
    fi
    
    for c in $(git rev-list --max-count=100 HEAD); do
        local tmpBody=$(git show --format="%b" --no-abbrev-commit --no-patch $c )
        local tmpID=$(echo "$tmpBody" | sed -rn "s/^[ \t]*Change-Id:[ \t]*($changeID)[ \t]*$/\1/p" )
        if [ "$tmpID" == "$changeID"  ]; then
            echoSuccess "Selecting revision $c from $1" 
            out=$(git checkout $c 2>&1)
            if [ ! 0  -eq $? ]; then
                echo $out
                echoError "Failed to checkout $c"
                revertGitState
                return 1
            else
                git show --no-patch --abbrev-commit --no-decorate  --format=short $c
                echo ""
                decendant=$(get_decendant $c $lc)
                if [ $? -eq 0 ]; then
                    if [ "$decendant" == "$lc" ]; then
                        echo ""
                        echoStatus "\e[33mWARNING:\e[39m Commiting this would revert aos_bhp"
                        echo    "    to use an older commit for $1."
                    fi 
                fi
                    
                return 0
            fi
        fi
    done
    echoError "Could not find change $changeID on master"
    revertGitState
    return 1
}



#Fetch all submodules
echoStatus "Fetching latest master for each submodule..."
out=$(git submodule foreach git fetch origin master:refs/remotes/origin/master 2>&1)
if [ ! $? -eq 0 ]; then
    echo $out
    echoError "Failed to fetch each submodule"
    exit 1
fi

#Unstage all changes so we don't end up including them
out=$(git reset 2>&1)
if [ ! $? -eq 0 ]; then
    echo $out
    echoError "Failed to unstage changes"
    exit 1
fi
#Check all submodules that have changes
subs=$(git submodule status | sed -nr "s/^(\+)*[0-9a-z]+ ([^ ]*)( .*$)?$/\2/p")


for sub in $subs; do
    pushd $sub > /dev/null
    checkClean $sub
    popd > /dev/null
done

changed=false
for sub in $subs; do
    echoSuccess "Selecting $sub"
    lc=$(get_submodule_commit HEAD $sub)
    if [ ! 0 -eq $? ]; then
        echoError "Failed to get commited submodule revision for $sub"
    fi
    pushd $sub > /dev/null
    setSubmodule $sub 
    tmp=$?
    popd  > /dev/null
    if [ ! 0 -eq $tmp ]; then
        exit 1
    fi
    git add $sub
    changed=true
done

if $changed; then
    echo ""
    echo "-------------------------DONE---------------------------------"
    echo "Verify that the changes above are the ones you wish to commit."
    echo "If so, execute the following command to commit the changes: "
    echo "   git commit "
    exit 0
fi
exit 1
