#!/bin/bash
# checks if a branch has been squash merged into master
# run in batch with
#  git branch -a | grep remotes | head -n 10 | xargs -L1 xargs git-has-been-squash-merged.sh
# ...and tweak the `head` command to control how many you check
# Thanks https://blog.takanabe.tokyo/en/2020/04/remove-squash-merged-local-git-branches/

# To do the cleanup, you can do something like
#   grep '^mer' results-from-above.txt | sort > merged.txt
#   cat merged.txt | cut -d ' ' -f2 | sed 's_remotes/origin/__' | xargs -I'{}' echo 'git checkout {} && git push --delete origin {}' > commands.sh
# then you can inspect `commands.sh` and if you're happy, run it
#   bash commands.sh

set -euo pipefail
target=${1:?first param must be branch name}
ANCESTOR=`git merge-base master $target`
TEMP_TREE=`git commit-tree $(git rev-parse $target^{tree}) -p $ANCESTOR -m "temporary tree object"`
found=$(git cherry master $TEMP_TREE | grep '^-' || true)

if [ -z "$found" ]; then
  echo -n "not merged"
else
  echo -n "merged"
fi
echo ": $target"
# run `git gc --prune=now --aggressive` to remove orphan commits
