-define(Name, "Rogue").
-define(Treasure, "The Thingamajig").

-define(BoardWidth, 180).
-define(BoardHeight, 60).

-define(InfoRow, ?BoardHeight + 1).
-define(CommandRow, ?InfoRow + 1).
-define(MessageRow, ?CommandRow + 1).

-define(InitialStrength, 100).
-define(ReciprocalStrengthLossOnMove, 10).
-define(StrengthLossOnHittingWall, 1).

-define(MazeComplexity, 10).
-define(MaxRoomWidth, 30).
-define(MinRoomWidth, 3).
-define(MaxRoomHeight, 10).
-define(MinRoomHeight, 3).
-define(MaxCorridorSegmentCount, 10).
-define(MaxCorridorSegmentLength, 10).
-define(ReciprocalDeadEnd, 5).