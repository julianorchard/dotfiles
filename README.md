# Dotfiles & Configurations

> These are my `dotfiles`. There are many like them, but these ones are mine.
>
> My `dotfiles` are my best friends. They are my life. I must master them as I master my life.
>
> My `dotfiles`, without me, are useless. Without my `dotfiles`, I am useless.

They're mainly used on Windows, Ubuntu, and occasionally with Arch on my ThinkPad x220. They contain scripts for said systems, as well as [Emacs](https://www.gnu.org/software/emacs/), [Vim](https://www.vim.org/), and [VS Code](https://code.visualstudio.com/) configurations.

## Background

I started my journey storing these files with a bare git repo (following steps [here](https://www.atlassian.com/git/tutorials/dotfiles)). I then moved to a [literate configuration](https://en.wikipedia.org/wiki/Literate_programming) (which you can still see, [here](https://github.com/julianorchard/dotfiles/tree/literate)), which used Emacs [Org Mode](https://orgmode.org/). Finally, I've moved back to using a bare repo.

Basically, using a literate configuration made it feel like I was working on a large text document rather than directly with a computer. I like Org mode for task management, but writing code in a literate way wasn't as fun (and the same kind of detailed documentation can be achieved with long code comments).

## Deploy

I use a [bare git repo](https://www.atlassian.com/git/tutorials/dotfiles) to manage these files, as I think it's one of the most system-agnostic methods. It does mean you need Cygwin or Git Bash on Windows (I mainly use the latter).

You can execute a command like this to install the dotfiles from a remote source:

```sh
curl -Lks https://julianorchard.co.uk/bin/install.sh | /bin/bash
```

## Featured Scripts

The below is generated by `.bin/readme-details.py`!
### vpn-protonvpn-status <sup>[file](.bin/vpn-protonvpn-status)</sup>

Proton VPN status checker for i3 bar.

### spooler.ps1 <sup>[file](.bin/spooler.ps1)</sup>

Created for a colleague to help them print things more easily. It works in tandem with [spooler.ahk](../.ahk/misc/spooler.ahk). It requires an external AutoHotkey (v1.1) script to run (you can read more about this in the AutoHotkey part of the file). This part of the project handles some of the printing part. It prints PDF's, so we needed to route it through the default PDF reader.

### hack <sup>[file](.bin/hack)</sup>

Make it look like you're hacking! Doesn't work, for me, on Windows.

### touch.bat <sup>[file](.bin/touch.bat)</sup>

Because `copy NUL filename.txt >NUL` is not in my muscle memory.

### git-commit-date-fix.sh <sup>[file](.bin/git-commit-date-fix.sh)</sup>

Use this script to fix commit dates on recent git commits. You can specify looking at more historic commits by inputting an integer as an argument, for example, this will show 20 of the most recent commits for you to select from (default is 5): ```sh $ ./git-commit-date-fix.sh 20 ``` The other thing you can do is include a string argument. This will be used to change the git command used (for example, my `config` custom bash function).

### ll.bat <sup>[file](.bin/ll.bat)</sup>

`dir` isn't it my muscle memory at all.

### Wallpaper-Set.ps1 <sup>[file](.bin/Wallpaper-Set.ps1)</sup>

This wallpaper setter uses ImageMagick to add text with the current time before setting it as the wallpaper.

I don't really use this anymore, but it worked really nicely at one point.

### sxiv.sh <sup>[file](.bin/sxiv.sh)</sup>

Run sxiv with an alias command.

### config.bat <sup>[file](.bin/config.bat)</sup>

The command used to manage the bare repo: ```cmd git --git-dir=%HOME%/.dotfiles/ --work-tree=%HOME% ``` ... for use in the Windows CMD.

### brightness <sup>[file](.bin/brightness)</sup>

file=/sys/class/backlight/intel_backlight/brightness if [ ! -z ${1} ] && [ ${1} -lt 100 ] && [ ${1} -gt 0 ] ; then

echo $(((3000/100)*${1})) > ${file} else

echo "This command requires a value between 1 and 100." fi ### pm-cal.py <sup>[file](.bin/pm-cal.py)</sup>

Simply returns your next ical event from Proton Calendar.

### Install-Fonts.ps1 <sup>[file](.bin/Install-Fonts.ps1)</sup>

Install some of my favourite (and most relevant to this set of dotfiles) fonts: - [Anonymice NF](https://github.com/ryanoasis/nerd-fonts/) - [Fira Code](https://github.com/tonsky/FiraCode/) - [Fira Code Mono Italic](https://github.com/zwaldowski/Fira/raw/zwaldowski/mod-new/otf/)

### mamp-fix.sh <sup>[file](.bin/mamp-fix.sh)</sup>

MAMP, for me, seems to have an issue. This is the quickfix™.

### toktik.py <sup>[file](.bin/toktik.py)</sup>

Really simple RSS feed scraper that looks at a list of TikTok accounts (through the input file `.toktikrc`), and uses [ntfy](https://ntfy.sh/)

send notifications when someone you're 'following' posts. Uses [Proxitok](https://github.com/pablouser1/ProxiTok) for the RSS bit!

### cp.bat <sup>[file](.bin/cp.bat)</sup>

In case I forget I'm using CMD.

### multi-screen.sh <sup>[file](.bin/multi-screen.sh)</sup>

dual monitor setup. xrandr --output VGA-1 --mode "1920x1080_60.00" --output LVDS-1 --auto --left-of VGA-1

### numberddomainseach.bat <sup>[file](.bin/numberddomainseach.bat)</sup>

Used to find out how many instances of a domain with numbers in (e.g. test1.com, test2.com, ...) are live/pingable.

### refreshprompt.bat <sup>[file](.bin/refreshprompt.bat)</sup>

The main use case for this being whenever we change Git profiles. This isn't something I do as often as I used to.

### ps.bat <sup>[file](.bin/ps.bat)</sup>

I need to open PowerShell with `-ExecutionPolicy bypass` when I'm at work, because *reasons*. This also adds the `-NoLogo` flag when opening. It also makes sure the PowerShell profile is loaded.

### callsign.py <sup>[file](.bin/callsign.py)</sup>

Slight refactor attempt of [this repo](https://www.zappshelter.com/wp-content/uploads/2022/01/Headshots-Stav.jpg) but to include things like ability to write in uppercase/lowercase. It was a fun script to write, if only for experience refactoring.

### msgBox.vbs <sup>[file](.bin/msgBox.vbs)</sup>

This is mainly called by a few CMD scripts to provide a GUI popup.

### readme-details.py <sup>[file](.bin/readme-details.py)</sup>

This is the script that generates what you might be reading *now*! It scrapes the `.bin/` folder and gets the text under the *'description'* heading (above). It's pretty simple, but nice to give an overview of what files are in this repo.

### shortcuts.bat <sup>[file](.bin/shortcuts.bat)</sup>

Use this script to create a shortcut to the current folder you're in. I've found this especially useful for CMD navigation.

### nitter-rss-generator.py <sup>[file](.bin/nitter-rss-generator.py)</sup>

Gets a list of your Twitter followers and makes a nice little Nitter formatted URL RSS feed. Uses [tweepy](https://www.tweepy.org/).

### install.sh <sup>[file](.bin/install.sh)</sup>

Install the dotfiles. Edited from the [original source](https://bitbucket.org/durdn/cfg/src/master/.bin/install.sh).

### seo-kw-audit <sup>[file](.bin/seo-kw-audit)</sup>

This is a simple tool used to generate a little CSV report on keywords. The only places it looks (at the moment) are in `<title>` and `<h1>` tags. It is also mainly configured to look at `.php` files, as this is what was required for my work.

### mklist.bat <sup>[file](.bin/mklist.bat)</sup>

I use this surprisingly frequently. Probably not best practice because using DIR ouput (which I *assume* would run into similar issues to doing the same with bash, which I know is definitely not good practice).

### commit.ps1 <sup>[file](.bin/commit.ps1)</sup>

Should only be used until learnt properly (I don't really use it anymore). Used for creating semantic commits. Text taken from this Gist: https://gist.github.com/joshbuchea/6f47e86d2510bce28f8e7f42ae84c716

### countdown.ps1 <sup>[file](.bin/countdown.ps1)</sup>

Send annoying reminder emails every X until X (counting down until then).

### screenshot <sup>[file](.bin/screenshot)</sup>

Define Name name=${1} if [[ name = "" ]] ; then name="test" ; fi Capture scrot ${HOME}/%Y-%m-%d-${name}.jpg -e $(notify-send "Screenshot Captured" "%Y-%m-%d-$name")

### rm.bat <sup>[file](.bin/rm.bat)</sup>

Because `del` is not in my muscle memory.

### amiconnected.bat <sup>[file](.bin/amiconnected.bat)</sup>

Simply uses ping to check if we have an internet connection.

### addpath.bat <sup>[file](.bin/addpath.bat)</sup>

Add current path to PATH.

### drives.bat <sup>[file](.bin/drives.bat)</sup>

Add shortcuts to available drives.

## Contributing

These are my personal dotfiles. If you want to fork this repo and use them, that's absolutely fine, but (unless you find a breaking change) I probably won't be accepting PRs.

## License

Everything here, unless otherwise stated, is under the MIT License. See [License](/LICENSE) for more information.

## Credits

These credits don't cover every resource that has helped me on my way to these configurations, but they're hopefully giving credit to some of the biggest influences on them...

#### Emacs

- [Daviwil's literate dotfiles](https://github.com/daviwil/dotfiles) and [his Emacs From Scratch series](https://github.com/daviwil/emacs-from-scratch) (which is what got me into Emacs in the first place!)
- [Tecosaur's config](https://tecosaur.github.io/emacs-config/config.html); *the most beautiful literate configuration I've ever seen*
- Even though I don't use it direcitly, [Doom Emacs](https://github.com/doomemacs/doomemacs) has been a massive help
- [RedGuardToo's](https://github.com/redguardtoo/emacs.d) dotfiles, plus his [Mastering Emacs in One Year](https://github.com/redguardtoo/mastering-emacs-in-one-year-guide) Guide
- [MatthewZMD's M-Emacs](https://github.com/MatthewZMD/.emacs.d)
- [Protesilaos'](https://protesilaos.com/emacs/dotemacs) great Emacs tips, packages (especially themes), and [YouTube videos](https://www.youtube.com/@protesilaos)

#### Vim

- [Tim Pope](https://github.com/tpope), for obvious reasons
- The dotfiles of [Daniel Hahler](https://github.com/blueyed)

#### Windows

- [Chocolatey](https://chocolatey.org/) for their very useful [`refresh.cmd`](https://github.com/chocolatey/choco/blob/develop/src/chocolatey.resources/redirects/RefreshEnv.cmd)
