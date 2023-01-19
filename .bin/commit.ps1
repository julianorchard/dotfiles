##   commit.ps1  ---  Helper for creating semantic commits.

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## Description:

# Should only be used until learnt properly (I don't really use
# it anymore).

# Used for creating semantic commits. Text taken from this Gist:
# https://gist.github.com/joshbuchea/6f47e86d2510bce28f8e7f42ae84c716

## Code:

$validCommitTypes = @(
    'feat', 'fix', 'docs', 'style', 'refactor', 'test', 'chore')

$commitTypeDescriptions = @(
    ' -> new feature for the user, not a new feature for build script',
    ' -> bug fix for the user, not a fix to a build script',
    ' -> changes to the documentation',
    ' -> formatting, missing semi colons, etc; no production code change',
    ' -> refactoring production code, eg. renaming a variable',
    ' -> adding missing tests, refactoring tests; no production code change',
    ' -> updating grunt tasks etc; no production code change'
)

Write-Host(@"

Here are the types of commit that are valid:

"@)

# For: it's easier than ForEach in getting the current index of the array:
For ($i = 0; $i -le $validCommitTypes.length - 1; $i++) {
    # gapChars is for aligning the output:
    $gapChars = " " * (8 - $validCommitTypes[$i].Length)
    Write-Host(" - '" + $validCommitTypes[$i] + "'" +
            $gapChars + $commitTypeDescriptions[$i])
}
Write-Host ""

# Checking the user is inputting a valid commit type:
$checkInput = Read-Host "What type of commit is this? "
While (-Not ($validCommitTypes -Contains $checkInput)) {
    $checkInput = Read-Host "What type of commit is this? "
}
$type = $checkInput

# Input commit message:
$msg  = Read-Host("Commit message")

# Concat and use Git to commit this
$commitMessage = $type + ": " + $msg
git commit -m "$commitMessage"

