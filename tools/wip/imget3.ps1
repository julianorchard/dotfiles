# Clone of shell script to 
# get all images in a folder 
# from Imgur
 
# Select-String   ~= grep
# -match -replace ~= sed

# Get Parameters First
  param($album,$name)

# Define
  $temp = "C:\tmp\temp-imgur"
  $tem2 = "C:\tmp\temp-imgu2"

# Get Album URL
  if (!$album) { $album = Read-Host "What is the name of the album you want to get? " }

# Download Name
  if (!$name) 
  { 
    $name = Read-Host "What would you like to name the album and images? (Enter to leave as default random imgur string) " 
    if (!$name) { $name = "yooy" } # FIX THIS
  }
  Write-Host "Nice name is $name"

# Get URL and find images (SO https://superuser.com/questions/25538/how-to-download-files-from-command-line-in-windows-like-wget-or-curl)
  $client = new-object System.Net.WebClient
  $client.DownloadFile($album,$temp)
  #wget -q $album -O $temp 
  
  # this is where .jpg is added... maybe review
  $commander = Select-String "post-image-container" $temp
  Write-Output $commander
