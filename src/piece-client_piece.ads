--
--
--      Tubastga Game - A turn based strategy game.
--      Copyright (C) 2015-2017  Frank J Jorgensen
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
with Observation;
with Hexagon.Client_Map;
with Effect;
with Ada.Containers.Vectors;
with Construction;
with Action;
with Hexagon.Area;

package Piece.Client_Piece is

   type Type_Client_Piece is new Type_Piece with record
      Effects_On_Piece : Effect.Effect_List.Map;
   end record;

   type Type_Client_Piece_Access is access all Type_Client_Piece;
   type Type_Client_Piece_Class_Access is access all Type_Client_Piece'Class;

   procedure Init
     (P_Piece_Class : in Piece.Client_Piece.Type_Client_Piece'Class);

   procedure Create_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Piece_Type  : in     Type_Piece_Type;
      P_Category    : in     Type_Category;
      P_Patch       : in out Landscape.Type_Patch);

   procedure Put (P_Piece : in Type_Piece);

   procedure Put_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Patch       : in out Landscape.Type_Patch);

   procedure Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Patch       : in out Landscape.Type_Patch);

   procedure Perform_Attack
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece);

   procedure Perform_Ranged_Attack
     (P_Player_Id                         : in     Player.Type_Player_Id;
      P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece);

   function Validate_Executing_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;
   function Validate_Target_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure Perform_Move
     (P_Player_Id              : in     Player.Type_Player_Id;
      P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Type_Piece;
      P_To_Patch : in out Landscape.Type_Patch);

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames
        .Vector);

   procedure Grant_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Effect      : in     Effect.Type_Effect);

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Effect      : in     Effect.Type_Effect);

   procedure Grant_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A);

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A);

   procedure Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A);

   procedure Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Patch       : in     Landscape.Type_Patch;
      P_Effect      : in     Effect.Type_Effect);

   procedure Perform_Construction
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Construction_Piece : in     Type_Piece;
      P_Construction_Patch : in     Landscape.Type_Patch;
      P_Construction       : in     Construction.Type_Construction);

   procedure Perform_Demolition
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Demolition_Piece   : in     Type_Piece;
      P_Demolition_Patch   : in     Landscape.Type_Patch;
      P_Construction       : in     Construction.Type_Construction);

   function Find_Effect
     (P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect_Name :    Effect.Type_Effect_Name) return Natural;

   procedure Set_Reports_On_Pieces
     (P_Observed_Piece_Info : in Observation.Observation_Of_Pieces_Info
        .Changes_To_Pieces_Info
        .Vector;
      P_Effects : in Observation.Observation_Of_Pieces_Effects
        .Changes_To_Pieces_Effects
        .Vector);

   package Pieces_Client_List is new Ada.Containers.Vectors
     (Natural,
      Type_Client_Piece_Class_Access,
      Piece.Client_Piece."=");

   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Piece.Client_Piece.Pieces_Client_List.Cursor;
   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Type_Client_Piece_Class_Access;

   function Patch_Belongs_To_Player
     (P_Patch     : in Hexagon.Client_Map.Type_Client_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean;

   Client_Pieces_In_Game : Pieces_Client_List.Vector;

end Piece.Client_Piece;
