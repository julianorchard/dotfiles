# Dotfiles and Configurations

These are my personal dotfiles and configs.
They're mainly used on Windows, Ubuntu, and
occasionally with Arch. They mainly contain
scripts for said systems, as well as Emacs, Vim,
and VS Code configurations.

## History

I started my journey storing these files with a
bare git repo (following steps
[here](https://www.atlassian.com/git/tutorials/dotfiles).
I then moved on to a [literate
configuration](https://en.wikipedia.org/wiki/Literate_programming),
using Emacs [Org Mode](https://orgmode.org/) (I
still use this for notes and other organizational
things!). Finally, now, in this iteration, I've
moved *back to using a bare repo*!

Why did I change this? Using a literate
configuration made everything reproducible, easy
to manage, and thoroughly documented! Well, the
main reason is because I realised I was getting
*very* stuck into Emacs as an ecosystem. And I
still enjoy using Vim and have even started using
VS Code (with the Vim plugin installed, of course):
this started to feel a little bit weird. This also
didn't work well with my using these config files
on remote servers (I'd always used graphical
Emacs, and didn't really enjoy having to install
Emacs and use it to combine the files before
possibly uninstalling Emacs again...).

Using a literate configuration made it feel like I
was working on a large text document rather than
with a computer. I didn't write scripts to improve
workflow, like I had previously: I started to
write everything in Emacs, and even that started
to feel annoyingly far from writing proper code.

## Deploy

As previously mentioned, I use a [bare git repo](https://www.atlassian.com/git/tutorials/dotfiles) to
manage these files. This is probably the most
system-agnostic way of doing it. It does mean you
need Cygwin or Git Bash on Windows (I mainly use
the latter).

```sh
$ install.sh
```

(TODO: Add some more detail here!)

## Contributing

These are my personal dotfiles. If you want to
fork this repo and use them, that's absolutely
fine, but (unless you find a breaking change) I
probably won't be accepting PRs.

## License

Everything here, unless otherwise stated[^1], is under
the MIT License. See [License](/LICENSE) for more
information.

[^1]: Although Emacs typically uses GPLv3+, these are both
compatible.

## Credits

These credits don't cover every resource that has
helped me on my way to these configurations, but
they're hopefully giving credit to some of the
biggest influences on them...

#### Emacs

- [Daviwil's literate
  dotfiles](https://github.com/daviwil/dotfiles)
  and [his Emacs From Scratch
  series](https://github.com/daviwil/emacs-from-scratch)
  (which is what got me into Emacs in the first
  place!)
- [Tecosaur's
  config](https://tecosaur.github.io/emacs-config/config.html);
  *the most beautiful literate configuration I've
  ever seen*
- Even though I don't use it direcitly, [Doom
  Emacs](https://github.com/doomemacs/doomemacs)
  has been a massive help
- [RedGuardToo's](https://github.com/redguardtoo/emacs.d)
  dotfiles, plus his [Mastering Emacs in One
  Year](https://github.com/redguardtoo/mastering-emacs-in-one-year-guide)
  Guide
- [MatthewZMD's
  M-Emacs](https://github.com/MatthewZMD/.emacs.d)
- [Protesilaos'](https://protesilaos.com/emacs/dotemacs)
  great Emacs tips, packages (especially themes),
  and [YouTube
  videos](https://www.youtube.com/@protesilaos)

#### Vim

- [Tim Pope](https://github.com/tpope), for
  obvious reasons
- The dotfiles of [Daniel
  Hahler](https://github.com/blueyed)

#### Windows

- [Chocolatey](https://chocolatey.org/) for their
  very useful
  [`refresh.cmd`](https://github.com/chocolatey/choco/blob/develop/src/chocolatey.resources/redirects/RefreshEnv.cmd)
