# Cheatsheet
- move lines: `M-(up|down)` from drag-stuff.el
- paste into `/` (search): `C-y` (emacs yank)
- `C-h b`: show active key bindings ([link](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Summary.html#Help-Summary))
- `C-h o <symbol>`: `describe-symbol` for describing function, variables, etc ([link](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Summary.html#Help-Summary))
- `C-c p ...`: projectile prefix
- in neotree `-` is horizontal split and `|` is vertical
- `C-x t ...`: prefix for tab commands

# TODO
- `*`/`#` search for whole word under cursor (including `_`, etc)
- `*`/`#` don't start a visual selection at the same time
- highlight find over all splits
- change font size of all splits at the same time, and the default for new
    splits
- open in new window (and let i3 handle tabbing)
- selecting from buffer list takes you to already open view if in another frame
- "recently opened files" so I can close a split, then re-open it from a list of
    files. Like I do with `C-b` in vim.
- picking up an extra line at the bottom for
    - visual line, then toggle comment
    - :co-1
- persist search highlight, https://github.com/juanjux/evil-search-highlight-persist
- get neotree to have once instance per frame
- prettier https://github.com/prettier/prettier-emacs
     - do we bind to js2-mode or to vue-mode too?
     - should I define exec-path in emacs or pull it from shell?
- https://github.com/noctuid/general.el for easier bindings?
- highlight trailing spaces (auto remove?)
- highlight yanked line
- after opening in split, focus the new window
- electric indent on vim 'S'
- avy only in current frame
- get camelCase autocomplete working. Currently using company-mode
  (not sure what backend). Might be able to use
  [company-helm](https://www.reddit.com/r/emacs/comments/c61oio/helm_instead_of_with_company/)
- `:q` only closes current buffer/split, not whole frame with all tabs in it

probably lots to learn from https://github.com/cbowdon/Config/blob/master/emacs/init.org

Might be able to take some ideas from
https://github.com/purcell/emacs.d . Flycheck looks useful.
