-module(con_struct).

-export([border/2]).

-include("con.hrl").

border(Width, Height) ->
    Top = lists:duplicate(Width, $#),
    Side = [$#, lists:duplicate(Width - 2, " "), $#, ?CR, ?LF],
    Sides = lists:duplicate(Height - 2, Side),
    [Top, ?CR, ?LF, Sides, Top, ?CR, ?LF].
