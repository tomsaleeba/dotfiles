# Cheatsheet
 - `(` and `)` to rotate selections
 - `:echo %opt{filetype}` to see current filetype
 - `:doc options` to read more about options
 - `:plug-install` to install plugins, it's not automatic
 - `g a` go to alt buffer
 - `<c-x> f` to autocomplete filenames
 - use `v` in fzf mode to only search VCS files

# TODO
 - figure out window management. Seems like the default is to tie into tmux, but can I make it work with Kitty?
 - ability to list
     - buffers
     - registers
 - quick switch buffers like vim `<c-6>`
 - get `<c-u>` in insert mode working, could try https://gist.github.com/akkartik/4150524bb0ad059ac4f315ffebbe74fb#file-kakrc-L20
 - cursor change for insert/normal mode
 - searching doesn't highlight on all clients. The converse is that selections are completely separate.
 - smart indenting
 - line wrapping (both the config and command).
     https://cosine.blue/2019-09-06-kakoune.html mentions how there are options
     but essentially you use an external formatter to do it like `par` or `fmt`.
 - wow/store/obs syntax highlighting craps out around line 250-ish
 - move line up/down with `<a-up/down>`
 - get easymotion highlighting to have a different face for the curr char and subsequent ones
 - is it worth integrating ranger, at least to launch in the current or root dir: https://github.com/Crote/kakoune-ranger/blob/master/ranger.kak
 - show total number of matches for a search
 - when `:addhl global/ wrap` is on, can we have a "move up/down one visual line" like in vim?
 - check out https://github.com/mawww/config/blob/master/kakrc
 - quit all like `:qa`
 - change surrounding. I can do `<a-a> ' s ' <ret> c "` but that sucks a bit, check out https://github.com/h-youhei/kakoune-surround
 - toggle comments
 - can I use the tricks from https://dev.to/staab/open-output-in-a-new-buffer-2dak to get `ag` working? Would be nice to feed it through fzf so a selection opens that line

# Pros
 - syntax highlighting for .vue files out-of-the-box
 - client-server by default so I can connect to an existing session from remote
   and keep going. No need to wrap in screen/byobu, although it'll likely be
   wrapped in byobu so the window management works.

# Answers
 - can I select a buffer I have open in another window and have the window grab
   focus? Not out of the box, but if tmux will let us focus, we could make it happen.
