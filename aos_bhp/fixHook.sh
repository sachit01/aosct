#!/bin/bash


IFS='
'

#Copy the hooks to the submodules
src=.git/hooks/commit-msg
if [ ! -f $src ]; then
    echo "Hook not found, might you have forgotten to clone with hook?"
    exit 1
fi

# This line disables the add_DependsOn line in the commit hook as this does not allow
# submodules to be in a detached head state, which would hinder us.
echo "Disabling dependsOn function in hook"
sed -i -r 's/^(add_DependsOn) *$/#\1 This function is has been disabled as it does not allow detatched state in submodules/' .git/hooks/commit-msg

repos='atp_bhp
atp_core
dispatcher
atc
dmi_bhp
tools_bhp
'

for i in $repos; do
    echo "Installing hook in $i"
    cp $src .git/modules/$i/hooks/commit-msg
done
