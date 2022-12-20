#!/c/MAMP/bin/ruby/bin/ruby.exe

##   pape-sort.rb  ---  Sort my wallpaper files.

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## License:

# See /LICENSE file in the root of this repository.

## Code:

def main
  done_renaming = true
  Dir.glob("files/*") do |d|

    next if File.file?(d)

    dir_name = d.split("/")[1]
    Dir.glob(d + "/*") do |f|

      # Skip if it's already an appropriate name
      # or if it's a folder
      next\
        if File.basename(f, "-*").include? dir_name\
      or File.directory?(f)

      # Do the file renaming
      print "\nRenaming:    '" + f + "'   "

      # Gets the number of files in the directory
      f_num = Dir[File.join(d, '*')].count { |file| File.file?(file) }

      # Paths and rename
      original_path = Dir.pwd + "/" + f
      rename_path =  Dir.pwd + "/" + d + "/" + dir_name + "-" + f_num.to_s + File.extname(f)

      # Show Renamed Name
      print "'" + d + "/" + dir_name + "-" + f_num.to_s + File.extname(f) + "'\n"
      File.rename(original_path, rename_path)
      done_renaming = false # We renamed something
    end
  end

  # Feedback if no renaming done
  puts "Nothing to rename." if done_renaming
end

main
