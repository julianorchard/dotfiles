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

LOG_OUTPUT="$TMPDIR/git-commit-date-fix-temp-output.txt"
COMMIT_LINE="commit "

function main() {
  if ! git rev-parse --git-dir > /dev/null 2>&1; then
    echo "Error: We're not in a Git repository right now."
    exit 0
  elif ! git log | grep -q commit ; then
    echo "Error: There are no commits in this git repository at the moment."
    exit 0
  fi

  # Get the current `git log` to output
  git log>"${LOG_OUTPUT}"

  match_counter=0

  # pass_* are responsible for getting the line with the commit message
  pass_counter=0
  pass_boolean=0
  commit_id_array=()

  while read line; do
    # Check line contains 'commit '
    if echo "$line" | grep -q "${COMMIT_LINE}"; then

      # ${match_counter} is the index of the array to select from later
      match_counter=$((match_counter+1))

      # ${line} is the only value needed in the array being built:
      # becomes ${commit_id_array} (after cutting "commit " substr), here
      commit_id_array+=(${line:7})

      # Alternative method means we don't need to keep seding the file
      pass_boolean=1
    fi

    # Initially, I used sed to get the line with the commit message, but it
    # was extremely slow. This just sets a bool and a counter in motion, where,
    # when we get 4 lines past the "commit HASH" match line (above), we know
    # we're at the line with the commit message:
    if [ $pass_boolean -eq 1 ] && [ $pass_counter -lt 4 ] ; then
      # Increment pass_counter if we're matched "commit HASH" but not
      # yet reached the correct line (4 down)
      pass_counter=$((pass_counter + 1))
    elif [ $pass_boolean -eq 1 ] && [ $pass_counter -ge 4 ] ; then
      # Output the line which the user can use to identify the right
      # commit to select. Then reset the counter/bool
      echo "$match_counter) $line"
      pass_counter=0
      pass_boolean=0
    fi

    # Stop listing results after the DEFAULT RESULTS amount
    [ ${match_counter} -eq ${DEFAULT_RESULTS} ] && break
  done<"${LOG_OUTPUT}"

  # We don't need the log output file anymore
  rm $LOG_OUTPUT

  while : ; do
    echo "Enter a number from above: "
    read input_number

    # Only break if the input_number is in the range allowed (for
    # which the best value to use is match_counter):
    [ ! $input_number -lt 1 ] && [ ! $input_number -gt $match_counter ] && break
  done

  # TODO: Would be nicer to make this easier to do
  echo "Enter date (format; Mon Jan 1 00:00:00 2020 +0000): "
  read date_string

  # Make the git change
  git filter-branch -f --env-filter \
    "if [ \$GIT_COMMIT = ${commit_id_array[$((${input_number} - 1))]} ] ; then
       export GIT_AUTHOR_DATE=\"${date_string}\"
       export GIT_COMMITTER_DATE=\"${date_string}\"
     fi"

}

main
