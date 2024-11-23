-module(con_tools).

-export([color_fade/3]).

color_fade({Fg1, Bg1}, {Fg2, Bg2}, Steps) ->
    FgFade = color_fade(Fg1, Fg2, Steps),
    BgFade = color_fade(Bg1, Bg2, Steps),
    lists:zip(FgFade, BgFade);

color_fade({R1,G1,B1},{R2,G2,B2}, Steps) ->
    [{R1, G1, B1} | [{fade(R1, R2, Step/Steps),
                      fade(G1, G2, Step/Steps),
                      fade(B1, B2, Step/Steps)} || Step <- lists:seq(1, Steps)]].
fade(A, B, F) ->
    trunc((B - A) * F + A).
