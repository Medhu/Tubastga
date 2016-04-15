--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
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

with Observation;
with Hexagon.Area.Client_Area;
with Ada.Strings.Unbounded;
with Player;
with Landscape;
with Construction;
with Effect;

package Hexagon.Client_Map is

   Not_Existing_Patch : exception;
   Slot_Not_Found_For_Piece : exception;

   type Type_Draw_Action is (Unselected, Selected, Selected_Area, Reachable, Attackable);

   type Type_Client_Patch;
   type Type_Client_Patch_Adress is access all Type_Client_Patch;
   type Type_Neighbour_List is array (1 .. 6) of Type_Client_Patch_Adress;

   type Type_Client_Patch is new Landscape.Type_Patch with record
      Neighbours : Type_Neighbour_List;
      Visible              : Boolean;
      Draw_Action          : Type_Draw_Action;
   end record;

   type Type_Client_Patch_Area is array (Positive range <>) of Type_Client_Patch_Adress;
   type Type_Client_Patch_Area_Access is access all Type_Client_Patch_Area;

   type Type_Client_Map is array (1 .. 100, 1 .. 100) of Type_Client_Patch_Adress;
   type Type_Client_Map_Info is record
      Origo_Patch : Type_Client_Patch_Adress;
      Map         : Type_Client_Map;
   end record;

   type Type_Client_Map_Adress is access all Type_Client_Map;

   procedure Set_Origo_Patch
     (P_Client_Map : in out Type_Client_Map_Info;
      P_A, P_B     : in Type_Hexagon_Numbers);

   function Get_Absolute_X_From_AB (P_Patch : in Type_Client_Patch) return Integer;

   function Get_Absolute_Y_From_AB (P_Patch : in Type_Client_Patch) return Integer;

   -- Calculates X, Y for the patch in the client map array
   function Get_X_From_AB
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch)
      return         Integer;
   function Get_Y_From_AB
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch)
      return         Integer;

   -- Get adress for the patch in the client map array
   function Get_Patch_Adress_From_XY
     (P_Client_Map : in Type_Client_Map_Info;
      P_X, P_Y     : in Integer
     )
      return         Type_Client_Patch_Adress;

   -- Get adress for the patch in the client map array
   function Get_Patch_Adress_From_Absolute_XY
     (P_Client_Map : in Type_Client_Map_Info;
      P_X, P_Y     : in Integer
     )
      return         Type_Client_Patch_Adress;

   function Get_Patch_Adress_From_AB
     (P_Client_Map : in Type_Client_Map_Info;
      P_A, P_B     : in Type_Hexagon_Numbers)
      return         Type_Client_Patch_Adress;

   -- Get the map with terrain and positions
   procedure Get_Map
     (P_Player_Id  : in Player.Type_Player_Id;
      P_Client_Map : in out Type_Client_Map_Info);

   -- Used to be able to communicate with the drawing procedure which tile is
   --chosen and so on.
   procedure Select_Patch (P_Patch : in out Type_Client_Patch);
   procedure Select_Patch_Area (P_Patch : in out Type_Client_Patch);
   procedure Unselect_Patch (P_Patch : in out Type_Client_Patch);
   procedure Unselect_All_Patches (P_Client_Map : in out Type_Client_Map_Info);

   function Capability_To_Area
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch;
      P_Capability : in Hexagon.Area.Client_Area.Type_Action_Capabilities_Access)
      return         Type_Client_Patch_Area_Access;

   procedure Set_Reachable
     (P_Client_Map : in out Type_Client_Map_Info;
      P_Patch_Area : in out Type_Client_Patch_Area);
   procedure Set_Attackable
     (P_Client_Map : in out Type_Client_Map_Info;
      P_Patch_Area : in out Type_Client_Patch_Area);

   procedure Set_Reports_On_Map
     (P_Client_Map                  : in Type_Client_Map_Info;
      P_Player_Observations_List    : in
     Observation.Observation_Of_Patches.Changes_To_Patches.Vector;
      P_Player_Observed_Pieces_List : in Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;
      P_Player_Observed_Patches_Effects : in Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Vector;
      P_Player_Observed_Constructions : in Observation.Observation_Of_Construction.Changes_To_Construction.Vector);

   procedure Save_Map
     (P_Filename   : in Ada.Strings.Unbounded.Unbounded_String;
      P_Client_Map : in Type_Client_Map_Info);
   procedure Save_Scenario
     (P_Filename   : in Ada.Strings.Unbounded.Unbounded_String;
      P_Client_Map : in Type_Client_Map_Info);

   type Type_Visit_Procedure is access procedure
     (P_Client_Map : in out Type_Client_Map_Info;
      P_Patch      : in out Type_Client_Patch_Adress);
   type Type_Visit_Procedure_In is access procedure
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch_Adress);

   procedure Reset_Visit;
   procedure Reset_Visible (P_Client_Map : in out Type_Client_Map_Info);
   procedure Reset_Draw_Action (P_Client_Map : in out Type_Client_Map_Info);

   procedure Traverse
     (P_Client_Map : in out Type_Client_Map_Info;
      P_Patch      : in out Type_Client_Patch_Adress;
      P_Visit      : in Type_Visit_Procedure);

   procedure Traverse_In
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch_Adress;
      P_Visit      : in Type_Visit_Procedure_In);

   procedure Put (P_Patch : in Type_Client_Patch);

   Empty : constant Type_Client_Patch :=
     Type_Client_Patch'
     (True,
      Hexagon.Type_Hexagon_Position'(P_Valid => False),
      Landscape.Undefined_Landscape,
      Landscape.Pieces_Here_List.Empty_Vector,
      Construction.Construction_List.Empty_Set,
      Effect.Effect_List.Empty_Map,
      Neighbours           =>
     Type_Neighbour_List'(Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
          Type_Client_Patch_Adress'(null)),
      Visible => False,
      Draw_Action          => Unselected);

   Unknown : constant Type_Client_Patch :=
     Type_Client_Patch'
     (False,
      Hexagon.Type_Hexagon_Position'(P_Valid => False),
      Landscape.Undefined_Landscape,
      Neighbours           =>
     Type_Neighbour_List'(Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
                          Type_Client_Patch_Adress'(null),
          Type_Client_Patch_Adress'(null)),
      Visible => False,
      Draw_Action          => Unselected);

   procedure Init_Client_Map (P_Client_Map : in out Type_Client_Map_Info);

end Hexagon.Client_Map;
