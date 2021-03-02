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

package Tubastga_Game.Server_Logic is
   Not_Implementet : exception;

   type Type_Attack_State is (Idle, Start_Attacking, Defence_Done);

   type Type_My_Tubastga_Piece is new Piece.Server.Fighting_Piece.Type_Piece with record
      Storage : Goods.Type_Storage_Access := null;
      Health  : Integer                   := 0;
      Energy  : Integer                   := 0;
      Attack_State : Type_Attack_State := Idle;
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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Create_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Create_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Put_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Pos                : in     Hexagon.Type_Hexagon_Position;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Patch              : in out Landscape.Type_Patch;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Remove_Piece
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Patch              : in out Landscape.Type_Patch;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result             :    out Status.Type_Result_Status);

   procedure Calculate_Attack_Result
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             :    out Player.Type_Player_Id);

   procedure End_Perform_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             : in     Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result             :    out Status.Type_Result_Status);

   procedure Calculate_Ranged_Attack_Result
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             :    out Player.Type_Player_Id);

   procedure End_Perform_Ranged_Attack
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Winner             : in     Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result       :    out Status.Type_Result_Status);

   procedure End_Perform_Move
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Moving_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_End_Pos            : in     Hexagon.Type_Hexagon_Position;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name      : in     Effect.Type_Effect_Name);

   procedure End_Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name             : in     Effect.Type_Effect_Name;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name      : in     Effect.Type_Effect_Name);

   procedure End_Perform_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name             : in     Effect.Type_Effect_Name;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name      : in     Effect.Type_Effect_Name);

   procedure End_Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name             : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name      : in     Effect.Type_Effect_Name);

   procedure End_Perform_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name             : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Grant_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Result             : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Revoke_Piece_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_Result             : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Grant_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect             : in     Effect.Type_Effect;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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
      P_Result      :    out Status.Type_Result_Status);

   procedure End_Revoke_Patch_Effect
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece              : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area               : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name        : in     Effect.Type_Effect_Name;
      P_End_Status         : in     Status.Type_Status;
      P_Attempts_Remaining : in out Integer);

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

   Sentry_Piece      : constant Piece.Type_Piece_Type := 1;
   Knight_Piece      : constant Piece.Type_Piece_Type := 2;
   Bowman_Piece      : constant Piece.Type_Piece_Type := 3;
   Ship_Piece        : constant Piece.Type_Piece_Type := 4;
   Carrier_Piece     : constant Piece.Type_Piece_Type := 5;
   Farm_House        : constant Piece.Type_Piece_Type := 6;
   Lumberjack_House  : constant Piece.Type_Piece_Type := 7;
   Stonecutter_House : constant Piece.Type_Piece_Type := 8;
   Tower_House       : constant Piece.Type_Piece_Type := 9;
   --
   Landscape_Grass    : constant Landscape.Type_Landscape := 100;
   Landscape_Forest   : constant Landscape.Type_Landscape := 101;
   Landscape_Mountain : constant Landscape.Type_Landscape := 102;
   Landscape_Water    : constant Landscape.Type_Landscape := 103;

   Landscapes_Type_Info_List : Landscape.Server.Type_Landscape_Type_Info_List :=
     (Landscape_Grass =>
        Landscape.Server.Type_Landscape_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Grass"), Max_Pieces_Here => 6),
      Landscape_Forest =>
        Landscape.Server.Type_Landscape_Type_Info'
          (Type_Name       => Utilities.RemoteString.To_Unbounded_String ("Forest"),
           Max_Pieces_Here => 6),
      Landscape_Mountain =>
        Landscape.Server.Type_Landscape_Type_Info'
          (Type_Name       => Utilities.RemoteString.To_Unbounded_String ("Mountain"),
           Max_Pieces_Here => 6),
      Landscape_Water =>
        Landscape.Server.Type_Landscape_Type_Info'
          (Type_Name       => Utilities.RemoteString.To_Unbounded_String ("Water"),
           Max_Pieces_Here => 6));

   Effect_Action_Point : constant Effect.Type_Effect_Name := 1;
   Effect_Courage      : constant Effect.Type_Effect_Name := 2;
   Effect_Captain      : constant Effect.Type_Effect_Name := 3;
   Effect_Treasure     : constant Effect.Type_Effect_Name := 4;
   Effect_Stops        : constant Effect.Type_Effect_Name := 5;
   Effect_Load         : constant Effect.Type_Effect_Name := 6;
   Effect_Unload       : constant Effect.Type_Effect_Name := 7;
   Effect_Slot_1       : constant Effect.Type_Effect_Name := 8;
   Effect_Slot_2       : constant Effect.Type_Effect_Name := 9;
   Effect_Slot_3       : constant Effect.Type_Effect_Name := 10;
   Effect_Path         : constant Effect.Type_Effect_Name := 11;

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

   Effect_Type_Info_List : Effect.Server.Type_Effect_Type_Info_List :=
     (Effect_Action_Point =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Action Point")),
      Effect_Courage =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Courage")),
      Effect_Captain =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Captain")),
      Effect_Treasure =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Treasure")),
      Effect_Stops =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Stop")),
      Effect_Load =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Load")),
      Effect_Unload =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Unload")),
      Effect_Slot_1 =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Slot 1")),
      Effect_Slot_2 =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Slot 2")),
      Effect_Slot_3 =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Slot 3")),
      Effect_Path =>
        Effect.Server.Type_Effect_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Path")));

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
