-define(Name, <<"Rogue">>).
-define(Treasure, <<"The Ring of Happiness">>).

-define(ScreenWidth, 180).
-define(ScreenHeight, 60).

-define(InfoRow, ?ScreenHeight + 1).
-define(CommandRow, ?InfoRow + 1).
-define(MessageRow, ?CommandRow + 1).

-define(InitialStrength, 100).
-define(ReciprocalStrengthLossOnMove, 10).
-define(StrengthLossOnHittingWall, 1).

-define(MaxRoomWidth, 30).
-define(MinRoomWidth, 3).
-define(MaxRoomHeight, 10).
-define(MinRoomHeight, 3).
-define(MaxCorridorSegmentCount, 10).
-define(MaxCorridorSegmentLength, 10).