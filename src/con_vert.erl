-module(con_vert).

-export([rgb_to_hsv/1, hsv_to_rgb/1]).

rgb_to_hsv({R, G, B}) ->
  {R1, G1, B1} = {R / 255, G / 255, B / 255},
  Cmax = lists:max([R1, G1, B1]),
  Cmin = lists:min([R1, G1, B1]),
  Hue =
    case hue_calc(R1, G1, B1, Cmax, Cmin) of
      H when H < 0.0 ->
        H + 360;
      H ->
        H
    end,
  Sat = sat_calc(Cmax, Cmin),
  Value = Cmax,
  {Hue, Sat, Value}.

hue_calc(_, _, _, Cmax, Cmax) ->
  0;
hue_calc(R1, G1, B1, R1, Cmin) ->
  math:fmod((G1 - B1) / (R1 - Cmin), 6) * 60;
hue_calc(R1, G1, B1, G1, Cmin) ->
  ((B1 - R1) / (G1 - Cmin) + 2) * 60;
hue_calc(R1, G1, B1, B1, Cmin) ->
  ((R1 - G1) / (B1 - Cmin) + 4) * 60.

sat_calc(+0.0, _) ->
  0.0;
sat_calc(Cmax, Cmin) ->
  (Cmax - Cmin) / Cmax.

hsv_to_rgb({Hue, Sat, Val}) ->
  C = Val * Sat,
  X = C * (1 - abs(math:fmod(Hue / 60, 2) - 1)),
  M = Val - C,
  {R1, G1, B1} = rgb_calc(Hue, X, C),
  {trunc((R1 + M) * 255), trunc((G1 + M) * 255), trunc((B1 + M) * 255)}.

rgb_calc(H, X, C) when H < 60 ->
  {C, X, 0};
rgb_calc(H, X, C) when H < 120 ->
  {X, C, 0};
rgb_calc(H, X, C) when H < 180 ->
  {0, C, X};
rgb_calc(H, X, C) when H < 240 ->
  {0, X, C};
rgb_calc(H, X, C) when H < 300 ->
  {X, 0, C};
rgb_calc(H, X, C) when H < 360 ->
  {C, 0, X}.
