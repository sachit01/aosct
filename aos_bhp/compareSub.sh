#!/bin/bash

source scripts/lib.sh

compareSubmodule(){
    local ours
    local theirs
    local decendant
    local ret

    ours=`get_submodule_commit $2 $1`
    if [ $? -ne 0 ]; then 
        echo "$ours"
        return 1
    fi

    theirs=`get_submodule_commit $3 $1`
    if [ $? -ne 0 ]; then 
        echo "$theirs"
        return 1
    fi

    if [ "${ours}" = "${theirs}" ]; then
        echo -e "$1:unchanged:$ours"
        return 0
    else
        pushd $1  > /dev/null
        decendant=`get_decendant ${ours} ${theirs}`
        if [ $? -eq 0 ]
        then
            if [ "$ours" = "$decendant" ]
            then 
                echo -e "$1:newer:$ours"
            else
                echo -e "$1:older:$theirs"
            fi
            ret=0
        elif [ $? -eq 1 ]
        then
            echo -e "$1:diverged."
            ret =0
        else
            ret=1
        fi
        popd > /dev/null
        return $ret
    fi
}

subs=` git submodule status | sed -nr "s/^(U| |\+)*[0-9a-z]* ([^ ]*)( .*$)?$/\2/p"`
IFS=$'\n'
if [ -z "$2" ]; then
    ours="HEAD"
else
    ours="$2"
fi

if [ "$1" = "merge" ]
then
    theirs="MERGE_HEAD"
else
    [ -z "$1" ] &&  echo "Usage: $0 THEIR_REF [OUR_REF]" && \
            echo " THEIR_REF, tag/commit/ref describing the commit to compare against" && \
                echo " OUR_REF,   tag/commit/ref describing our commit to be compared. Defaults to HEAD"    && \
                exit 1
    theirs=$1
fi
output="repo:status:newest commit\n----:------:-------------\n"
for sub in $subs; do
    tmp="$(compareSubmodule $sub $ours $theirs)"
    [ $? -ne 0 ]&& echo $tmp && exit 1
    output=`echo "$output\n$tmp"`
done
echo "Comparing ours ($ours) to theirs ($theirs)"
echo ""
echo -e $output | column -t -s ':'


