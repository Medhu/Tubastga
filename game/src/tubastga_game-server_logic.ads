--
--
--      Tubastga Game
--      Copyright (C) 2015-2021  Frank J Jorgensen
--
--      This program is free software: you can redistribute it and/or modify
--      it under the terms of the GNU General Public License as published by
--      the Free Software Foundation, either version 3 of the License, or
--      (at your option) any later version.
--
--      This program is distributed in the hope that it will be useful,
--      but WITHOUT ANY WARRANTY; without even the implied warranty of
--      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--      GNU General Public License for more details.
--
--      You should have received a copy of the GNU General Public License
--      along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

with Landscape;
with Player;
with Piece;
with Piece.Server.Fighting_Piece;
with Piece.Server.House_Piece;
with Hexagon.Area.Server_Area;
with Hexagon.Server_Map;
with Utilities;
with Status;
with Effect;
with Goods;
with Ada.Containers.Ordered_Maps;
with Landscape.Server;
with Effect.Server;
with Lua;
with Action;
with Hexagon.Server_Navigation;
with Ada.Real_Time;
with Attempt;

package Tubastga_Game.Server_Logic is
   Not_Implementet : exception;

   type Type_Attack_State is (Idle, Start_Attacking, Defence_Done);

   type Type_My_Tubastga_Piece is new Piece.Server.Fighting_Piece.Type_Piece with record
      Storage : Goods.Type_Storage_Access    := null;
      Health  : Integer                      := 0;
      Energy  : Integer                      := 0;
      Attack_State : Type_Attack_State       := Idle;
      Next_Move_Attempt : Ada.Real_Time.Time := Ada.Real_Time.Time_First;
   end record;

   type Type_My_Tubastga_Piece_Access_Class is access all Type_My_Tubastga_Piece'Class;

   type Type_My_Tubastga_House is new Piece.Server.House_Piece.Type_House with record
      Storage : Goods.Type_Storage_Access := null;
   end record;

   type Type_My_Tubastga_House_Access_Class is access all Type_My_Tubastga_House'Class;

   function Create_Piece_Name
     (P_Piece : in Type_My_Tubastga_House) return Utilities.RemoteString.Type_String;

   function Observation_Area
     (P_Piece : in Type_My_Tubastga_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   function Observation_Area
     (P_Piece : in Type_My_Tubastga_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   --
   -- Create Piece
   -- Piece
   function Validate_Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece) return Boolean;

   procedure Before_Create_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Create_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Create Piece
   -- Piece
   function Validate_Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House) return Boolean;

   procedure Before_Create_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   procedure End_Create_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Put Piece
   -- Piece
   function Validate_Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece) return Boolean;

   procedure Before_Put_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Put Piece
   -- House
   function Validate_Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House) return Boolean;

   procedure Before_Put_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Remove Piece
   -- Piece
   function Validate_Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece) return Boolean;

   procedure Before_Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Patch              : in out Landscape.Type_Patch;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Remove Piece
   -- House
   function Validate_Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House) return Boolean;

   procedure Before_Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Patch              : in out Landscape.Type_Patch;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Perform Attack
   --
   function Validate_Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece)
      return Boolean;

   procedure Before_Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Result             :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure Calculate_Attack_Result
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             :    out Player.Type_Player_Id;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             : in     Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Perform Ranged Attack
   --
   function Validate_Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece)
      return Boolean;

   procedure Before_Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Result             :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure Calculate_Ranged_Attack_Result
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             :    out Player.Type_Player_Id;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             : in     Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Perform Move
   --
   function Validate_Perform_Move
     (P_Player_Id    : in Player.Type_Player_Id;
      P_Action_Type  : in Action.Type_Action_Type;
      P_Moving_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_To_Pos       : in Hexagon.Type_Hexagon_Position) return Boolean;

   procedure Before_Perform_Move
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos     : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos       : in out Hexagon.Type_Hexagon_Position;
      P_End_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure Before_Perform_Move_Step
     (P_Player_Id    : in     Player.Type_Player_Id;
      P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos     : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos       : in out Hexagon.Type_Hexagon_Position;
      P_End_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Result       :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Perform_Move
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Moving_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_End_Pos            : in     Hexagon.Type_Hexagon_Position;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Perform Patch Effect
   -- Piece
   function Validate_Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name      : in Effect.Type_Effect_Name;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean;

   procedure Before_Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Perform Patch Effect
   -- House
   function Validate_Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name      : in Effect.Type_Effect_Name;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean;

   procedure Before_Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Perform Piece Effect
   -- Piece
   function Validate_Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name      : in Effect.Type_Effect_Name) return Boolean;

   procedure Before_Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Perform Piece Effect
   -- House
   function Validate_Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name      : in Effect.Type_Effect_Name) return Boolean;

   procedure Before_Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name      : in     Effect.Type_Effect_Name;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name             : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Grant Piece Effect
   -- Piece
   function Validate_Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect      : in Effect.Type_Effect) return Boolean;

   procedure Before_Grant_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Grant Piece Effect
   -- Housse
   function Validate_Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect      : in Effect.Type_Effect) return Boolean;

   procedure Before_Grant_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Revoke Piece Effect
   -- Piece
   function Validate_Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean;

   procedure Before_Revoke_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Result             : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Grant Piece Effect
   -- House
   function Validate_Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean;

   procedure Before_Revoke_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Result             : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Grant Patch Piece
   -- Piece
   function Validate_Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in Effect.Type_Effect) return Boolean;

   procedure Before_Grant_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Grant Patch Effect
   -- House
   function Validate_Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in Effect.Type_Effect) return Boolean;

   procedure Before_Grant_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect      : in     Effect.Type_Effect;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Revoke Patch Effect
   -- Piece
   function Validate_Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean;

   procedure Before_Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   --
   -- Revoke Patch Effect
   -- House
   function Validate_Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name) return Boolean;

   procedure Before_Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result      :    out Status.Type_Result_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info);

   procedure End_Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempt_Info       : in out Attempt.Type_Attempt_Info);

   procedure Upkeep
     (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece : in out Type_My_Tubastga_Piece);

   function Movement_Cost
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Patch  : in out Landscape.Type_Patch;
      P_To_Patch    : in out Landscape.Type_Patch) return Integer;

   procedure Upkeep
     (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House : in out Type_My_Tubastga_House);


   procedure Tubastga_Creating_Game
     (P_Map_Name      : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String);
   procedure Tubastga_Saving_Game
     (P_Map_Name      : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String);
   procedure Tubastga_Loading_Game
     (P_Map_Name      : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String);

   procedure Tubastga_Joining_Game;
   procedure Tubastga_Leaving_Game;
   procedure Tubastga_Start_Game;
   procedure Tubastga_Upkeep_Game;
   procedure Tubastga_End_Game (P_Game_Status : out Status.Type_Game_Status);

   -- Attack_Defence Table
   type Type_Attack_Defence_Record is record
      Attack, Defence : Natural;
   end record;

   type Type_Attack_Defence_Table is
     array
     (Pieces_Type_Info_List'First ..
          Pieces_Type_Info_List'Last,
        Pieces_Type_Info_List'First ..
          Pieces_Type_Info_List'Last) of Type_Attack_Defence_Record;

   Attack_Defence_Table : Type_Attack_Defence_Table :=
     Type_Attack_Defence_Table'(others => (others => Type_Attack_Defence_Record'(30, 20)));

   type Type_Attack_Defence_Types is (Attacking_Wins, Attacked_Wins);
   --


   function Can_Load
     (P_Piece : in Type_My_Tubastga_Piece;
      P_Goods : in Goods.Type_Goods_Info) return Boolean;

   package Carrier_Paths_List is new Ada.Containers.Ordered_Maps
     (Piece.Type_Piece_Id,
      Hexagon.Server_Navigation.Type_Path,
      Piece."<",
      Hexagon.Server_Navigation."=");

   All_Paths : Carrier_Paths_List.Map;

   Lua_State : Lua.Lua_State;

   function Roll
     (P_Attack_Defence : in Tubastga_Game.Server_Logic.Type_Attack_Defence_Record)
      return Type_Attack_Defence_Types;

end Tubastga_Game.Server_Logic;
