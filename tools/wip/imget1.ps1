# Clone of shell script to 
# get all images in a folder 
# from Imgur
 
# Select-String   ~= grep
# -match -replace ~= sed

# Arguments
  param($album,$name)
# Get them if not definied
  if (!$album) { $album = Read-Host "What is the name of the album you want to get? " }
  if (!$name)  { $name = Read-Host "What would you like to name the album and images? (Enter to leave as default random imgur string) " }

# Get .html file to C:\tmp\temp.html 
  $temp = "C:\tmp\temp-imgur.html"
  iwr -outf $temp $album 

# Search through file for img urls
  Select-String -Path "C:\tmp\temp-imgur.html" -Pattern '<img class="image-placeholder" src="https://i.imgur.com/*"' -CaseSensitive -SimpleMatch


