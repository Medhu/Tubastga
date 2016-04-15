--
--
--      Tubastga Game
--      Copyright (C) 2015  Frank J Jorgensen
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
with Construction;
with Effect;
with Goods;
with Ada.Containers.Ordered_Maps;
with Landscape.Server;
with Construction.Server;
with Effect.Server;
with Lua;
with Action;

package Tubastga_Piece.Server_Logic is
   Not_Implementet : exception;

   type Type_My_Tubastga_Piece is new Piece.Server.Fighting_Piece.Type_Piece with record
      Storage : Goods.Type_Storage_Access := null;
   end record;

   type Type_My_Tubastga_Piece_Access_Class is access all Type_My_Tubastga_Piece'Class;

   type Type_My_Tubastga_House is new Piece.Server.House_Piece.Type_House with record
      Storage : Goods.Type_Storage_Access := null;
   end record;

   type Type_My_Tubastga_House_Access_Class is access all Type_My_Tubastga_House'Class;

   procedure Init_Piece (P_Piece_Class : in out Type_My_Tubastga_Piece);

   procedure Init_Piece (P_Piece_Class : in out Type_My_Tubastga_House);

   function Create_Piece_Name
     (P_Piece : in Type_My_Tubastga_Piece) return Utilities.RemoteString.Type_String;

   function Create_Piece_Name
     (P_Piece : in Type_My_Tubastga_House) return Utilities.RemoteString.Type_String;

   procedure Spend_Resources_On_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_My_Tubastga_Piece;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Success     :    out Boolean);

   procedure Spend_Resources_On_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_My_Tubastga_House;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Success     :    out Boolean);

   function Movement_Capability
     (P_Piece : in Type_My_Tubastga_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   function Construction_Capability
     (P_Piece : in Type_My_Tubastga_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   function Create_Piece_Area
     (P_Piece : in Type_My_Tubastga_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;

   function Create_Piece_Area
     (P_Piece : in Type_My_Tubastga_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access_A;

   function Observation_Area
     (P_Piece : in Type_My_Tubastga_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   function Observation_Area
     (P_Piece : in Type_My_Tubastga_House)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   function Attack_Capability
     (P_Piece : in Type_My_Tubastga_Piece)
      return Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

   procedure Calculate_Attack_Result
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in     Type_My_Tubastga_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            :    out Player.Type_Player_Id);

   function Calculate_Attack_Action_Points
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_My_Tubastga_Piece;
      P_From_Patch, P_To_Patch            : in Landscape.Type_Patch;
      P_Player_Id                         : in Player.Type_Player_Id) return Integer;

   procedure After_Perform_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_My_Tubastga_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Winner                            : in     Player.Type_Player_Id;
      P_Player_Id                         : in     Player.Type_Player_Id);

   function Calculate_Ranged_Attack_Action_Points
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Type_My_Tubastga_Piece;
      P_From_Patch, P_To_Patch            : in Landscape.Type_Patch;
      P_Player_Id                         : in Player.Type_Player_Id) return Integer;

   procedure After_Perform_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_My_Tubastga_Piece;
      P_From_Patch, P_To_Patch            : in     Landscape.Type_Patch;
      P_Winner                            : in     Player.Type_Player_Id;
      P_Player_Id                         : in     Player.Type_Player_Id);

   function Calculate_Move_Action_Points
     (P_Action_Type            : in Action.Type_Action_Type;
      P_Moving_Piece           : in Type_My_Tubastga_Piece;
      P_From_Patch, P_To_Patch : in Landscape.Type_Patch;
      P_Player_Id              : in Player.Type_Player_Id) return Integer;

   procedure After_Perform_Move
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Type_My_Tubastga_Piece;
      P_From_Patch, P_To_Patch : in     Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id);

   procedure Before_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id);

   procedure Before_Remove_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id);

   function Calculate_Patch_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id) return Integer;

   function Calculate_Patch_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id) return Integer;

   function Calculate_Piece_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id) return Integer;

   function Calculate_Piece_Effect_Action_Points
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id) return Integer;

   procedure Calculate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id);

   procedure Calculate_Patch_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in Player.Type_Player_Id);

   procedure Calculate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id);

   procedure Calculate_Piece_Effect
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Effect      : in Effect.Type_Effect;
      P_Player_Id   : in Player.Type_Player_Id);

   function Validate_Create_Piece
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Create_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   function Validate_Create_Piece
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Create_Piece
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Pos                            : in     Hexagon.Type_Hexagon_Position;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   function Validate_Put_Piece
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Put_Piece
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_Piece;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id);

   procedure After_Put_Piece
     (P_Action_Type : in Action.Type_Action_Type;
      P_Piece       : in Type_My_Tubastga_House;
      P_Patch       : in Landscape.Type_Patch;
      P_Player_Id   : in Player.Type_Player_Id);

   function Validate_Remove_Piece
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Remove_Piece
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Attacking_Pos, P_Attacked_Pos     : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id    : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Path                              : in Hexagon.Path.Vector;
      P_Current_Player_Id, P_Player_Id    : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Ranged_Attack
     (P_Action_Type                       : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Attacking_Pos, P_Attacked_Pos     : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id    : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Move
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Moving_Piece                   : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Path                           : in Hexagon.Path.Vector;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Move
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Moving_Piece                   : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos             : in Hexagon.Type_Hexagon_Position;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Patch_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Effect                         : in Effect.Type_Effect;
      P_Area                           : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Patch_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Effect                         : in Effect.Type_Effect;
      P_Area                           : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Piece_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Perform_Piece_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Pos                            : in Hexagon.Type_Hexagon_Position;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Grant_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   procedure After_Grant_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   function Validate_Grant_Piece_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Grant_Piece_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Revoke_Piece_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Revoke_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   function Validate_Revoke_Piece_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Revoke_Piece_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   function Validate_Grant_Patch_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Patch                          : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Grant_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Patch                          : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   procedure After_Grant_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Patch                          : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   function Validate_Grant_Patch_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Patch                          : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Revoke_Patch_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Patch                          : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   function Validate_Revoke_Patch_Effect
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Piece                          : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Patch                          : in Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Revoke_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
      P_Patch                          : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   procedure After_Revoke_Patch_Effect
     (P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Patch                          : in     Hexagon.Server_Map.Type_Server_Patch;
      P_Effect                         : in     Effect.Type_Effect;
      P_Current_Player_Id, P_Player_Id : in     Player.Type_Player_Id);

   function Validate_Perform_Construction
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Constructing_Piece             : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Piece_Pos                      : in Hexagon.Type_Hexagon_Position;
      P_Construction_Pos               : in Hexagon.Type_Hexagon_Position;
      P_Construction                   : in Construction.Type_Construction;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure After_Perform_Construction
     (P_Action_Type        : in     Action.Type_Action_Type;
      P_Constructing_Piece : in out Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Piece_Patch        : in     Landscape.Type_Patch;
      P_Construction_Patch : in     Landscape.Type_Patch;
      P_Construction       : in     Construction.Type_Construction;
      P_Player_Id          : in     Player.Type_Player_Id);

   function Validate_Perform_Demolition
     (P_Action_Type                    : in Action.Type_Action_Type;
      P_Demolition_Piece               : in Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
      P_Piece_Pos                      : in Hexagon.Type_Hexagon_Position;
      P_Demolition_Pos                 : in Hexagon.Type_Hexagon_Position;
      P_Construction                   : in Construction.Type_Construction;
      P_Current_Player_Id, P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure Upkeep
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece             : in out Type_My_Tubastga_Piece);

   procedure Upkeep
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Type_My_Tubastga_House);

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
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Forest"), Max_Pieces_Here => 6),
      Landscape_Mountain =>
        Landscape.Server.Type_Landscape_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Mountain"), Max_Pieces_Here => 6),
      Landscape_Water =>
        Landscape.Server.Type_Landscape_Type_Info'
          (Type_Name => Utilities.RemoteString.To_Unbounded_String ("Water"), Max_Pieces_Here => 6));

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

   Construction_Wall1 : constant Construction.Type_Construction := 1;
   Construction_Wall2 : constant Construction.Type_Construction := 2;
   Construction_Wall3 : constant Construction.Type_Construction := 3;
   Construction_Wall4 : constant Construction.Type_Construction := 4;
   Construction_Wall5 : constant Construction.Type_Construction := 5;
   Construction_Wall6 : constant Construction.Type_Construction := 6;

   procedure Tubastga_Creating_Game
     (P_Map_Name      : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String);
   procedure Tubastga_Saving_Game
     (P_Map_Name      : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String);
   procedure Tubastga_Loading_Game
     (P_Map_Name      : in Utilities.RemoteString.Type_String;
      P_Scenario_Name : in Utilities.RemoteString.Type_String);

   procedure Tubastga_Joining_Game ;
   procedure Tubastga_Leaving_Game ;
   procedure Tubastga_Start_Game;
   procedure Tubastga_Upkeep_Game;
   procedure Tubastga_Start_Turn (P_Player_Id : in Player.Type_Player_Id);
   procedure Tubastga_End_Turn (P_Player_Id : in Player.Type_Player_Id);
   procedure Tubastga_End_Game (P_Game_Status : out Status.Type_Game_Status);

   Land_Piece_Move_Landscape_Array : constant Landscape.Server.Type_List_Landscape_Access :=
     new Landscape.Type_List_Landscape'(100 => True, 101 => True, 102 => True, 103 => False);
   Land_Piece_Attack_Landscape_Array : constant Landscape.Server.Type_List_Landscape_Access :=
     new Landscape.Type_List_Landscape'(100 => True, 101 => True, 102 => True, 103 => False);

   Sea_Piece_Move_Landscape_Array : constant Landscape.Server.Type_List_Landscape_Access :=
     new Landscape.Type_List_Landscape'(100 => False, 101 => False, 102 => False, 103 => True);
   Sea_Piece_Attack_Landscape_Array : constant Landscape.Server.Type_List_Landscape_Access :=
     new Landscape.Type_List_Landscape'(100 => False, 101 => False, 102 => False, 103 => True);

   House_Construct_Landscape_Array : constant Landscape.Server.Type_List_Landscape_Access :=
     new Landscape.Type_List_Landscape'(100 => True, 101 => True, 102 => True, 103 => False);
   House_Non_Construct_Landscape_Array : constant Landscape.Server.Type_List_Landscape_Access :=
     new Landscape.Type_List_Landscape'(100 => False, 101 => False, 102 => False, 103 => False);

   Pieces_Type_Info_List : Piece.Server.Fighting_Piece.Type_Piece_Type_Info_List :=
     (Sentry_Piece =>
        Piece.Server.Fighting_Piece.Type_Piece_Type_Info'
          (Utilities.RemoteString.To_Unbounded_String ("Sentry"),
           Piece.Fighting_Piece,
           Land_Piece_Move_Landscape_Array,
           Land_Piece_Attack_Landscape_Array),
      Knight_Piece =>
        Piece.Server.Fighting_Piece.Type_Piece_Type_Info'
          (Utilities.RemoteString.To_Unbounded_String ("Knight"),
           Piece.Fighting_Piece,
           Land_Piece_Move_Landscape_Array,
           Land_Piece_Attack_Landscape_Array),
      Bowman_Piece =>
        Piece.Server.Fighting_Piece.Type_Piece_Type_Info'
          (Utilities.RemoteString.To_Unbounded_String ("Bowman"),
           Piece.Fighting_Piece,
           Land_Piece_Move_Landscape_Array,
           Land_Piece_Attack_Landscape_Array),

      Ship_Piece =>
        Piece.Server.Fighting_Piece.Type_Piece_Type_Info'
          (Utilities.RemoteString.To_Unbounded_String ("Ship"),
           Piece.Fighting_Piece,
           Sea_Piece_Move_Landscape_Array,
           Sea_Piece_Attack_Landscape_Array),
      Carrier_Piece =>
        Piece.Server.Fighting_Piece.Type_Piece_Type_Info'
          (Utilities.RemoteString.To_Unbounded_String ("Carrier"),
           Piece.Fighting_Piece,
           Land_Piece_Move_Landscape_Array,
           Land_Piece_Attack_Landscape_Array));

   Houses_Type_Info_List : Piece.Server.House_Piece.Type_House_Type_Info_List :=
     (Farm_House =>
        Piece.Server.House_Piece.Type_House_Type_Info'
          (Type_Name           => Utilities.RemoteString.To_Unbounded_String ("Farm"),
           Category            => Piece.House_Piece,
           Construct_Landscape => House_Construct_Landscape_Array),
      Lumberjack_House =>
        Piece.Server.House_Piece.Type_House_Type_Info'
          (Type_Name           => Utilities.RemoteString.To_Unbounded_String ("Lumberjack"),
           Category            => Piece.House_Piece,
           Construct_Landscape => House_Construct_Landscape_Array),
      Stonecutter_House =>
        Piece.Server.House_Piece.Type_House_Type_Info'
          (Type_Name           => Utilities.RemoteString.To_Unbounded_String ("Stonecutter"),
           Category            => Piece.House_Piece,
           Construct_Landscape => House_Construct_Landscape_Array),
      Tower_House =>
        Piece.Server.House_Piece.Type_House_Type_Info'
          (Type_Name           => Utilities.RemoteString.To_Unbounded_String ("Tower"),
           Category            => Piece.House_Piece,
           Construct_Landscape => House_Construct_Landscape_Array));

   Construction_Type_Info_List : Construction.Server.Type_Construction_Type_Info_List :=
     (Construction.Server.Type_Construction_Type_Info'
        (Type_Name                 => Utilities.RemoteString.To_Unbounded_String ("Wall1"),
         Blocking_Neighbour_Number =>
           Construction.Server.Type_Way_List'(True, False, False, False, False, False)),
      Construction.Server.Type_Construction_Type_Info'
        (Type_Name                 => Utilities.RemoteString.To_Unbounded_String ("Wall2"),
         Blocking_Neighbour_Number =>
           Construction.Server.Type_Way_List'(False, True, False, False, False, False)),
      Construction.Server.Type_Construction_Type_Info'
        (Type_Name                 => Utilities.RemoteString.To_Unbounded_String ("Wall3"),
         Blocking_Neighbour_Number =>
           Construction.Server.Type_Way_List'(False, False, True, False, False, False)),
      Construction.Server.Type_Construction_Type_Info'
        (Type_Name                 => Utilities.RemoteString.To_Unbounded_String ("Wall4"),
         Blocking_Neighbour_Number =>
           Construction.Server.Type_Way_List'(False, False, False, True, False, False)),
      Construction.Server.Type_Construction_Type_Info'
        (Type_Name                 => Utilities.RemoteString.To_Unbounded_String ("Wall5"),
         Blocking_Neighbour_Number =>
           Construction.Server.Type_Way_List'(False, False, False, False, True, False)),
      Construction.Server.Type_Construction_Type_Info'
        (Type_Name                 => Utilities.RemoteString.To_Unbounded_String ("Wall6"),
         Blocking_Neighbour_Number =>
           Construction.Server.Type_Way_List'(False, False, False, False, False, True)));

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

   function Calculate_Construction_Action_Points
     (P_Action_Type        : in Action.Type_Action_Type;
      P_Constructing_Piece : in Type_My_Tubastga_House;
      P_Piece_Patch        : in Landscape.Type_Patch;
      P_Construction_Patch : in Landscape.Type_Patch;
      P_Construction       : in Construction.Type_Construction;
      P_Player_Id          : in Player.Type_Player_Id) return Integer;

   function Calculate_Demolition_Action_Points
     (P_Action_Type      : in Action.Type_Action_Type;
      P_Demolition_Piece : in Type_My_Tubastga_House;
      P_Piece_Patch      : in Landscape.Type_Patch;
      P_Demolition_Patch : in Landscape.Type_Patch;
      P_Construction     : in Construction.Type_Construction;
      P_Player_Id        : in Player.Type_Player_Id) return Integer;

   procedure After_Perform_Demolition
     (P_Action_Type      : in     Action.Type_Action_Type;
      P_Demolition_Piece : in out Type_My_Tubastga_House;
      P_Piece_Patch      : in     Landscape.Type_Patch;
      P_Demolition_Patch : in     Landscape.Type_Patch;
      P_Construction     : in     Construction.Type_Construction;
      P_Player_Id        : in     Player.Type_Player_Id);

   function Can_Load
     (P_Piece : in Type_My_Tubastga_Piece;
      P_Goods : in Goods.Type_Goods_Info) return Boolean;

   package Carrier_Paths_List is new Ada.Containers.Ordered_Maps
     (Piece.Type_Piece_Id,
      Hexagon.Type_Path,
      Piece."<",
      Hexagon."=");

   All_Paths : Carrier_Paths_List.Map;

   Lua_State : Lua.Lua_State;
end Tubastga_Piece.Server_Logic;
