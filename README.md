# Dotfiles and Configurations

These are my personal dotfiles and configs. They're mainly used on Windows, Ubuntu, and occasionally with Arch. 

I've moved from a literate configuration using Org. This is because, although I really enjoyed using Org to manage my dotfiles, as I started getting into more custom Elisp, I was finding I was having less fun using an Org document. I found it was easier to do everything with normal `.el` files instead, so I could add custom functions and get to know the language a little better. I think the literate configuration was better suited to small snippets of code with long explainations around them. I found this was a great way to document what I was doing. However, the same can be achieved with nice code comments.

## Deploy

One feature of Org I've not yet managed to sort (with this idea of moving configurations) is the idea of deploying these dotfiles. This is still a work in progress.

Although managing them with Org was extremely succinct in this regard, I think it did come with the rather large drawback of needing Emacs installed/open to actually deploy the configurations. This was most prevelantly a problem when I started to use the configurations on my remote server and things. 

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
