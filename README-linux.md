# Developing the MARISA Climate Tools site under Linux

For the most part, we've been using a custom Docker container for development of this site on macOS because of the difficulties we've had with maintaining a working Jekyll stack on that platform. We've had better luck doing this "natively" on Linux machines and that's what's documented here.

The following assumes you're operating on a Linux distribution in the Debian family (Debian, Ubuntu, and derivatives) and that you have administrator privileges. The procedure would require some modification to get it working on ICDS Roar.


## Install the necessary system software

`sudo apt install ruby ruby-dev`


## Install the Jekyll in a system location

`sudo gem install jekyll jekyll-feed jekyll-sitemap webrick`


## Alternatively, install Jekyll just for yourself

`gem install -- user-install jekyll jekyll-feed jekyll-sitemap webrick`

And add the following to your `.bashrc`:

```
if which ruby >/dev/null && which gem >/dev/null; then
    PATH="$(ruby -r rubygems -e 'puts Gem.user_dir')/bin:$PATH"
fi
```


## Clone the site from Github

`gh repo clone midatlanticrisa/marisa-cdp`

or

`git clone https://github.com/midatlanticrisa/marisa-cdp.git`


## Development

Edit with your favorite text editor (I really like [micro](https://micro-editor.github.io/) version 2.0.10 or later with `set softwrap true` and `set wordwrap true`) and then...

build: `jekyll build`

test: `jekyll serve`

push to server `./linux-shipit`
