![Another example of the desktop](wallpapers/Examples/Examples-2.jpg)

# CMD (Windows Config)

Windows configuration files that live in `C:\cmd` for me, hence the name.

## Installation

I used to use a script to install (it's in the
`tools\archive` folder, called `windot.bat`) but
it wasn't good and below is just far easier... Run
CMD then;

`cd \`

`git clone https://github.com/julianorchard/cmd cmd`

`cmd\tools\fixpath.bat`

I don't swap Windows machines very often, so this
is totally fine. `cd tools\` and run `add-path.bat` to get started. From here,
refresh the environment (taken from Chocolatey) using `bin\refresh.bat` and
just add all the important folders to PATH.

## Features

Some of the folders don't contain much interesting stuff, but I'll mention some
of the most used/important scripts below: 

### AutoHotkey (`\ahk`)

There's quite a lot in this folder, but it's mainly all in `General.ahk`; feel
free to have a read of the file as it's *fairly* well annotated. Some of the
features of this file include: 

- Drawing boxes around stuff with `Alt + B`, and
    underline stuff with `Alt + U` (*very* useful for the
    snipping tool)
- Insert date/time and random underline stuff,
    sign emails etc.
- Run certain folders with `Alt + E`, to replicate
    the normal Windows `Start + E` functionality,
    but better (e.g. `Alt + E` then `C` opens `C:\Users\USER\`)
- Disable Keyboard and Mouse clicking, and refresh the screen to bypass screen
lock IT Policy with `Alt + L` (for Lock) and `Alt + I` (for Idle)

### Hours (`\hours`)

This is a VBScript program that writes my hours to a CSV file and just lets me
track hours worked very easily from the command line. More information is in the
folder.

### Tools (`\tools`)

All of the tools, pretty much always batch. Most should have good descriptions for what they do, but some of the most useful ones include:

- `renr.bat` : ren (rename) but recursively
- `ll.bat` : lists files and folders nicely
- `gui.bat` : open current location in windows explorer
- `search.bat` : file search on the command line
- `shortcut.bat` and `drives.bat` : create shortcuts to folders or drives with minimal effort (for use in the command line; just type 'c', to take you to C:\, etc. - works out what drives are actually available so you're not binding random letters for no reason)
- `addpath.bat` : add a folder to the path environment, and then refresh using `refresh.bat` (stolen from [Chocolatey](https://github.com/chocolatey/choco/blob/b6495f72d1f2b9901747d857467c4ed3f7306391/src/chocolatey.resources/redirects/RefreshEnv.cmd))
- `layout`/`mklayout`: create and deploy a folder
    layout to be reused (useful if you're
    repeating folder structure for some reason; I
    needed it)
- `amiconnected.bat`: Used to test if your internet is
    connected; accepts an address to ping, and accepts
    `notify` as an argument, which will run
    `msgBox.vbs` to notify you when it thinks your
    connection is re-established

### VBA (`\vba`)

This contains some of my more useful snippets of VBA code. Kept in this repo
because I needed to store it somewhere, and it's nice to have on hand when using
Windows machines. 

### Wallpapers (`\wallpapers`)

![Example of what my desktop on Windows looks like, using papetime.vbs](wallpapers/Examples/Examples-1.jpg)

A few nice scripts to manage my wallpapers. Wallpapers aren't in this repo
(there's some examples, as above). `papesort.sh` renames all the images in the
folder to match the folder name, `papetime.vbs` overlays text using
ImageMagick, and I regret writing it in VBScript (it really wasn't the best
idea...).


