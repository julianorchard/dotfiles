#!/bin/sh

##  git-commit-date-fix.sh  ---  Fix commit dates of recent commits.

## Description:

# Use this script to fix commit dates on recent git commits. You can
# specify looking at more historic commits by inputting an integer as
# an argument, for example, this will show 20 of the most recent
# commits for you to select from (default is 10):

# $ ./git-commit-date-fix.sh 20

## License:

# See the /LICENSE file in the root of this repository.

## Code:

# If there's an argument that's an int,
# we should use that as the default results
[ "${1}" ] && DEFAULT_RESULTS=$1 || DEFAULT_RESULTS=5

LOG_OUTPUT="temp-log-output.txt"
COMMIT_LINE="commit "

function main() {
  # TODO: Bonus, check we're in a repo which has commit history!
  if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: We're not in a Git repository right now."
    exit 0
  fi

  # Get the current `git log` to output
  git log>"${LOG_OUTPUT}"

  # TODO:
  # Thinking about it, we could achieve the same thing with a counter
  # where we just increment it by X from the line we match. Then we
  # just reset and stop incrememting it past a certain point.

  total_counter=0
  c=0
  commit_id_array=()

  while read p; do
    # ${total_counter} will be used to calculate the position of the
    # commit message, which the user will use to ID the commit to select
    total_counter=$((total_counter + 1))

    # Check line contains 'commit '
    if echo "$p" | grep -q "${COMMIT_LINE}"; then
      # ${c} is the index of the array to select from later
      c=$((c+1))
      # ${p} is the only value needed in the array we're building
      commit_id_array+=(${p:7})
      # Output the line containing the commit message. Calculated by using
      # ${total_counter} (current line) + 5 (how far away it is!)
      commit_message_line=$((total_counter + 4))
      commit_message=$(sed "${commit_message_line}q;d" ${LOG_OUTPUT})
      echo "${c}) ${commit_message} (${p:7})"
    fi

    # Stop listing results after the DEFAULT RESULTS amount
    [ ${c} -eq ${DEFAULT_RESULTS} ] && break
  done<"${LOG_OUTPUT}"

  # get the line containing "commit " in `git log` output, then 4 lines down

  echo "Enter a number from above: "
  read input_number
  # minus one because array
  input_number=$((input_number - 1))

  echo "Enter date (format; Mon Jan 1 00:00:00 2020 +0000): "
  read date_string

  # hol' onto ur butts
  git filter-branch --env-filter \
      "if [ $GIT_COMMIT = ${commit_id_array[${input_number}]} ] ; then
          export GIT_AUTHOR_DATE=\"${date_string}\"
          export GIT_COMMITTER_DATE=\"${date_string}\"
      fi"

  rm $LOG_OUTPUT
}

main
