-define(Name, <<"Rogue">>).

-define(ScreenWidth, 180).
-define(ScreenHeight, 60).
-define(MaxWidth, 30).
-define(MinWidth, 3).
-define(MaxHeight, 10).
-define(MinHeight, 3).

-define(InfoRow, ?ScreenHeight).
-define(CommandRow, ?InfoRow + 1).
-define(MessageRow, ?CommandRow + 1).

-define(InitialStrength, 10).
-define(ReciprocalStrengthLossOnMove, 10).