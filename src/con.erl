-module(con).

-include("con.hrl").

-export([erase_line/1, erase_display/1, color/1, cursor/1, style/1, reset/0, reset/1]).
-export([scroll_up/0, scroll_up/1, scroll_down/0, scroll_down/1]).

-export([basic_colors/0, basic_colors_bright/0, styles/0]).

-export([gradient/4]).

-type con_style() :: default | bold | faint | underline | italic |
                     blink | fast_blink | invert | hide | strike |
                     double_underline | framed | encircled |
                     overlined | ideogram_underine | ideogram_overline |
                     ideogram_double_overline | ideogram_stress |
                     superscript | subscript.
%% Color Types
-type con_color_4() :: black | red | green | yellow |
                       blue | cyan | magenta | white | default |
                       bright_black | bright_red | bright_green | bright_yellow |
                       bright_blue | bright_cyan | bright_magenta | bright_white.

-type con_color_8() :: 0 .. 255 | <<_:8>>.

-type con_color_24() :: {con_color_8(), con_color_8(), con_color_8()} |
                        <<_:24>>.

-type con_color() :: con_color_4() |
                     con_color_8() |
                     con_color_24().

-type con_cursor_dir() :: up | down | left | right | next | prev | horizontal.
-type con_cursor() :: con_cursor_dir() | {con_cursor_dir() | integer()} | {pos, integer(), integer()}.

-type con_erase() :: back | forward | all.

-spec styles() -> [atom()].
styles() ->
    [default, bold, faint, underline, italic,
                     blink, fast_blink, invert, hide, strike,
                     double_underline, framed, encircled,
                     overlined, ideogram_underine, ideogram_overline,
                     ideogram_double_overline, ideogram_stress,
                     superscript, subscript].

-spec style(con_style()) -> iodata().
style(Style) ->
    ?CSI([style_code(Style), $m]).

style_code(default) -> "0";
style_code(bold) -> "1";
style_code(faint) -> "2";
style_code(italic) -> "3";
style_code(underline) -> "4";
style_code(blink) -> "5";
style_code(fast_blink) -> "6";
style_code(invert) -> "7";
style_code(hide) -> "8";
style_code(strike) -> "9";
style_code(double_underline) -> "21";
style_code(framed) -> "51";
style_code(encircled) -> "52";
style_code(overlined) -> "53";
style_code(ideogram_underline) -> "60";
style_code(ideogram_double_underline) -> "61";
style_code(ideogram_overline) -> "62";
style_code(ideogram_double_overline) -> "63";
style_code(ideogram_stress) -> "64";
style_code(superscript) -> "73";
style_code(subscript) -> "74".

-spec reset() -> iodata().
reset() ->
    style(default).

-spec reset(con_style()) -> iodata().
reset(Style) ->
    ?CSI([reset_code(Style), $m]).

reset_code(bold) -> "22";
reset_code(faint) -> "22";
reset_code(italic) -> "23";
reset_code(underline) -> "24";
reset_code(blink) -> "25";
reset_code(fast_blink) -> "25";
reset_code(invert) -> "27";
reset_code(hide) -> "28";
reset_code(strike) -> "29";
reset_code(double_underline) -> "24";
reset_code(framed) -> "54";
reset_code(encircled) -> "54";
reset_code(overlined) -> "55";
reset_code(ideogram_underline) -> "65";
reset_code(ideogram_double_underline) -> "65";
reset_code(ideogram_overline) -> "65";
reset_code(ideogram_double_overline) -> "65";
reset_code(ideogram_stress) -> "65";
reset_code(superscript) -> "75";
reset_code(subscript) -> "75".


basic_colors() -> [black, red, green, yellow,
                   blue, magenta, cyan, white].
basic_colors_bright() -> [bright_black, bright_red, bright_green,
                          bright_yellow, bright_blue, bright_magenta,
                          bright_cyan, bright_white].

color_code({fg, black}) -> "30";
color_code({fg, red}) -> "31";
color_code({fg, green}) -> "32";
color_code({fg, yellow}) -> "33";
color_code({fg, blue}) -> "34";
color_code({fg, magenta}) -> "35";
color_code({fg, cyan}) -> "36";
color_code({fg, white}) -> "37";
color_code({fg, default}) -> "39";
color_code({bg, black}) -> "40";
color_code({bg, red}) -> "41";
color_code({bg, green}) -> "42";
color_code({bg, yellow}) -> "43";
color_code({bg, blue}) -> "44";
color_code({bg, magenta}) -> "45";
color_code({bg, cyan}) -> "46";
color_code({bg, white}) -> "47";
color_code({bg, default}) -> "49";
color_code({fg, bright_black}) -> "90";
color_code({fg, bright_red}) -> "91";
color_code({fg, bright_green}) -> "92";
color_code({fg, bright_yellow}) -> "93";
color_code({fg, bright_blue}) -> "94";
color_code({fg, bright_magenta}) -> "95";
color_code({fg, bright_cyan}) -> "96";
color_code({fg, bright_white}) -> "97";
color_code({bg, bright_black}) -> "100";
color_code({bg, bright_red}) -> "101";
color_code({bg, bright_green}) -> "102";
color_code({bg, bright_yellow}) -> "103";
color_code({bg, bright_blue}) -> "104";
color_code({bg, bright_magenta}) -> "105";
color_code({bg, bright_cyan}) -> "106";
color_code({bg, bright_white}) -> "107";
color_code({fg, Color})
  when is_integer(Color) andalso Color < 256 ->
    "38;5;"++integer_to_list(Color);
color_code({bg, Color})
  when is_integer(Color) andalso Color < 256 ->
    "48;5;"++integer_to_list(Color);
color_code({FB, <<R:8, G:8, B:8>>}) ->
    color_code({FB, {R, G, B}});
color_code({fg, {R, G, B}}) ->
    "38;2;"++lists:join($;, lists:map(fun integer_to_list/1, [R, G, B]));
color_code({bg, {R, G, B}}) ->
    "48;2;"++lists:join($;, lists:map(fun integer_to_list/1, [R, G, B])).

-doc "Supports basic ANSI, xterm-256-color, and true color.".
-spec color(con_color() | {con_color(), con_color()}) -> iodata().
color({Fg, Bg}) ->
    [?CSI([color_code({fg, Fg}), $m]), ?CSI([color_code({bg, Bg}), $m])];
color(Fg) ->
    ?CSI([color_code({fg, Fg}), $m]).

gradient(word, Text, Color1, Color2) ->
    Sections = string:split(lists:flatten(Text), " ", all),
    Steps = length(Sections)-1,
    Colors = con_tools:color_fade(Color1, Color2, Steps),
    lists:join(" ", [[color(C), S] || {C, S} <- lists:zip(Colors, Sections)]);
gradient(letter, Text, Color1, Color2) ->
    Sections = lists:flatten(Text),
    Steps = length(Sections)-1,
    Colors = con_tools:color_fade(Color1, Color2, Steps),
    [[color(C), S] || {C, S} <- lists:zip(Colors, Sections)].



cursor_code(up) -> $A;
cursor_code(down) -> $B;
cursor_code(right) -> $C;
cursor_code(left) -> $D;
cursor_code(next) -> $E;
cursor_code(prev) -> $F;
cursor_code(horizontal) -> $G.

-doc "Used to move, hide, save, or restore the cursor position".
-spec cursor(con_cursor()) -> iodata().
cursor(save) ->
    ?CSI($s);
cursor(restore) ->
    ?CSI($u);
cursor(show) ->
    ?CSI("?25h");
cursor(hide) ->
    ?CSI("?25l");
cursor({Dir, N}) ->
    ?CSI([integer_to_list(N), cursor_code(Dir)]);
cursor({pos, X, Y}) ->
    ?CSI([integer_to_list(Y), $;, integer_to_list(X), $H]);
cursor(Dir) ->
    ?CSI(cursor_code(Dir)).

-doc "Erase the display before, after, or the entire screen.".
-spec erase_display(con_erase()) -> iodata().
erase_display(Mode) ->
    ?CSI([erase_code(Mode), $J]).

-doc "Erase the line before, after, or the entire line.".
-spec erase_line(con_erase()) -> iodata().
erase_line(Mode) ->
    ?CSI([erase_code(Mode), $K]).

erase_code(back) ->
    "0";
erase_code(forward) ->
    "1";
erase_code(all) ->
    "2".

-spec scroll_up() -> iodata().
scroll_up() ->
    ?CSI($S).

-doc "Scrolls up N lines.".
-spec scroll_up(integer()) -> iodata().
scroll_up(N) ->
    ?CSI([integer_to_list(N), $S]).

-spec scroll_down() -> iodata().
scroll_down() ->
    ?CSI($T).

-doc "Scrolls down N lines.".
-spec scroll_down(integer()) -> iodata().
scroll_down(N) ->
    ?CSI([integer_to_list(N), $T]).
