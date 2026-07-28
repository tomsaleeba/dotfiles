# waybar widgets

All custom module text (from `tombar`, see `tombar/main.go`, or any script
invoked by a `custom/*` module in `config.jsonc`) must render at a **fixed
width** across every state it can be in - normal readings, placeholders
("--", "n/a"), and error states alike. Waybar reflows the whole bar when a
module's text width changes, so a widget whose width varies makes
neighbouring widgets jump around every update. `style.css`'s per-module
`min-width` is only a belt-and-suspenders backstop, not a substitute for
actually padding the text.

When adding or changing a widget: figure out the widest value it can ever
show, and pad every other state (including "no data yet" / "n/a") out to
that same width.
