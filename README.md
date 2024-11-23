# con_artist
Make beautiful terminal applications with Erlang

## Colors

Supports 8/16 color mode, 256 color mode, and true color mode. Available colors in 8 color mode can be found in `con:basic_colors/0` and the additional 8 colors for 16 color mode can be found in `con:basic_colors_bright/0`.

``` erlang
con:color(red), % 8 color mode 
con:color(bright_yellow), % 16 color mode 
con:color(43), % 256 color mode 
con:color({190, 255, 96}), % true color mode 
con:color(<<190:8, 255:8, 96:8>>), %% true color mode (binary)
con:color(<<0:24>>), %% true color (binary-alt)
```

By default `con:color/1` will adjust the text (foreground) color. To adjust the foreground and background colors pass a 2-tuple `{Foreground, Background}` to `con:color/1`. The color modes are interchangeable between foreground and background.

``` erlang
con:color({red, <<66:8, 42:8, 190:8>>}).
```

## Styles

Set styles with `con:style/1` and revert style with `con:reset/1`. Revert all style changes with `con:style(default)` or `con:reset/0`. The list of available styles is returned in `con:style/0`. Which styles are supported will vary between terminal emulators.

## Cursor 

The cursor can be moved with `con:cursor/1`. Arguments include directional atoms `[up, down, left, right]`, and 2-tuples of direction and value: `{up, 10}`.

## Erase 

Sections of the terminal can be cleared with `con:erase_line/1` and `con:erase_screen/1`. Options include `[back, forward, all]`, relative to the cursor.
