# Cheatsheet
 - `(` and `)` to rotate selections
 - `:echo %opt{filetype}` to see current filetype
 - `:doc options` to read more about options
 - `:plug-install` to install plugins, it's not automatic

# TODO
 - figure out window management. Seems like the default is to tie into tmux, but can I make it work with Kitty?
 - ability to list
     - buffers
     - registers
 - quick switch buffers like vim `<c-6>`
 - get `<c-w>` in insert mode working
 - cursor change for insert/normal mode
 - highlight cursor line
 - easymotion, https://github.com/danr/kakoune-easymotion
 - smart indenting
 - line wrapping (both the config and command).
     https://cosine.blue/2019-09-06-kakoune.html mentions how there are options
     but essentially you use an external formatter to do it like `par` or `fmt`.
 - wow/store/obs syntax highlighting craps out around line 250-ish
 - have fzf honour .gitignore. fzf just reads from stdin so `git ls-files | fzf` does the trick

# Pros
 - syntax highlighting for .vue files out-of-the-box
 - client-server by default so I can connect to an existing session from remote
   and keep going. No need to wrap in screen/byobu, although it'll likely be
   wrapped in byobu so the window management works.

# Answers
 - can I select a buffer I have open in another window and have the window grab
   focus? Not out of the box, but if tmux will let us focus, we could make it happen.
