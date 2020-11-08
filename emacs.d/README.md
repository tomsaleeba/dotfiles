# Cheatsheet
- move lines: `M-(up|down)` from drag-stuff.el
- paste into `/` (search): `C-y` (emacs yank)
- `C-h b`: show active key bindings ([link](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Summary.html#Help-Summary))
- `C-h o <symbol>`: `describe-symbol` for describing function, variables, etc ([link](https://www.gnu.org/software/emacs/manual/html_node/emacs/Help-Summary.html#Help-Summary))
- `C-c p ...`: projectile prefix
- in neotree `-` is horizontal split and `|` is vertical

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
- visual line, then toggle commet picks up an extra line at the bottom
- add "open in split" actions to recent file list (`C-x f`)
