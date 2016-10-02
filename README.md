### Continuous Roguelike

Most Roguelikes are discrete in both space (they're played on a grid) and time
(they're turn-based). This is an experiment in building a _continuous_
Roguelike:
- there's no grid, just continuous space.
- there are no turns, but it's not realtime. Monsters don't move unless you do.

[Try it out!](https://nornagon.github.io/continuous-rl/target/scala-2.11/classes/index.html)
Press `W` (or click) to move around, `Space` to shoot, `R` to reload. Probably
only works in Chrome (and maybe Firefox) because it needs
[`KeyboardEvent.code`](http://caniuse.com/#feat=keyboardevent-code).

There's not much here yet beyond a prototype, but I think this is an
interesting start!

Graphics are all by the inimitable [Kenney](http://kenney.nl/). [Buy their asset packs!](https://kenney.itch.io/) They're amazing.
