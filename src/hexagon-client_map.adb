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

with Text_IO;
with Ada.Numerics.Generic_Elementary_Functions;
with Piece;
with Client.ClientRPC;
with Ada.Streams.Stream_IO;                     use Ada.Streams.Stream_IO;

package body Hexagon.Client_Map is
   Verbose : constant Boolean := False;

   type Type_Visited_Map is array (1 .. 100, 1 .. 100) of Boolean;
   Visited_Map : Type_Visited_Map;

   package Trig is new Ada.Numerics.Generic_Elementary_Functions (Float);

   Map_Buffer : Landscape.Type_Map;

   procedure Set_Origo_Patch
     (P_Client_Map : in out Type_Client_Map_Info;
      P_A, P_B     : in Type_Hexagon_Numbers)
   is
   begin
      P_Client_Map.Origo_Patch := Get_Patch_Adress_From_AB (P_Client_Map, P_A, P_B);
   end Set_Origo_Patch;

   procedure Put (P_Patch : in Type_Client_Patch) is
   begin
      if P_Patch.Pos.P_Valid then
         Landscape.Put (Landscape.Type_Patch (P_Patch));
         Text_IO.Put_Line ("Draw_Action=" & P_Patch.Draw_Action'Img);
         Landscape.Put_Pieces_Here (P_Patch.Pieces_Here);

      else
         Text_IO.Put_Line ("Not a valid position of the patch you try to print");
      end if;

   end Put;

   function Get_Patch_Adress_From_AB
     (P_Client_Map : in Type_Client_Map_Info;
      P_A, P_B     : in Type_Hexagon_Numbers)
      return         Type_Client_Patch_Adress
   is
      Next : Type_Client_Patch_Adress;
   begin

      Next := P_Client_Map.Map (1, 1);

      if P_A /= 1 then
         for Trav_A in 2 .. P_A loop
            Next := Type_Client_Patch_Adress (Next.all.Neighbours (1));
            if Next = null then
               raise Not_Existing_Patch;
            end if;
         end loop;
      end if;

      if P_B /= 1 then
         for Trav_B in 2 .. P_B loop
            Next := Type_Client_Patch_Adress (Next.all.Neighbours (6));
            if Next = null then
               raise Not_Existing_Patch;
            end if;
         end loop;
      end if;

      return Next;

   end Get_Patch_Adress_From_AB;

   -- Request all updates for the area according to subscription
   procedure Get_Map
     (P_Player_Id  : in Player.Type_Player_Id;
      P_Client_Map : in out Type_Client_Map_Info)
   is

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Get_Map - enter");
      end if;

      Client.ClientRPC.Get_Map (Map_Buffer);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Get_Map - retur");
      end if;

      for Trav_X in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_Y in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop

            P_Client_Map.Map (Trav_X, Trav_Y).Pos := Map_Buffer (Trav_X, Trav_Y).Pos;

            P_Client_Map.Map (Trav_X, Trav_Y).Landscape_Here :=
              Map_Buffer (Trav_X, Trav_Y).Landscape_Here;

         end loop;

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Get_Map - exit");
      end if;

   end Get_Map;

   procedure Reset_Visit is
   begin
      Visited_Map := (others => (others => False));
   end Reset_Visit;

   procedure Reset_Visible (P_Client_Map : in out Type_Client_Map_Info) is
   begin
      for ArrayX in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop -- Horisontal
         for ArrayY in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop -- Vertical
            P_Client_Map.Map (ArrayX, ArrayY).Visible := False ;
         end loop;
      end loop;
   end Reset_Visible;

   procedure Reset_Draw_Action (P_Client_Map : in out Type_Client_Map_Info) is
   begin
      for ArrayX in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop -- Horisontal
         for ArrayY in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop -- Vertical
            P_Client_Map.Map (ArrayX, ArrayY).Draw_Action          := Unselected;
         end loop;
      end loop;
   end Reset_Draw_Action;

   procedure Select_Patch (P_Patch : in out Type_Client_Patch) is
   begin
      P_Patch.Draw_Action := Selected;
   end Select_Patch;

   procedure Select_Patch_Area (P_Patch : in out Type_Client_Patch) is
   begin
      P_Patch.Draw_Action := Selected_Area;
   end Select_Patch_Area;

   procedure Unselect_Patch (P_Patch : in out Type_Client_Patch) is
   begin
      P_Patch.Draw_Action := Unselected;
   end Unselect_Patch;

   procedure Unselect_All_Patches (P_Client_Map : in out Type_Client_Map_Info) is
   begin
      for ArrayX in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop -- Horisontal
         for ArrayY in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop -- Vertical
            P_Client_Map.Map (ArrayX, ArrayY).Draw_Action := Unselected;
            --
         end loop;
      end loop;
   end Unselect_All_Patches;

   procedure Set_No_Pieces
     (P_Client_Map : in Type_Client_Map;
      P_Patch      : in out Type_Client_Patch)
   is
   begin
      Landscape.Pieces_Here_List.Clear (P_Patch.Pieces_Here);
   end Set_No_Pieces;

   procedure Set_Reports_On_Map
     (P_Client_Map                  : in Type_Client_Map_Info;
      P_Player_Observations_List    : in
        Observation.Observation_Of_Patches.Changes_To_Patches.Vector;
      P_Player_Observed_Pieces_List : in Observation.Observation_Of_Pieces.Changes_To_Pieces.Vector;
      P_Player_Observed_Patches_Effects : in Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Vector;
      P_Player_Observed_Constructions : in Observation.Observation_Of_Construction.Changes_To_Construction.Vector)
   is
      Trav_P_P               : Observation.Observation_Of_Patches.Changes_To_Patches.Cursor;
      Trav_P_Pieces          : Observation.Observation_Of_Pieces.Changes_To_Pieces.Cursor;
      Trav_Pieces_Here       : Landscape.Pieces_Here_List.Cursor;
      This_Observation       : Observation.Observation_Of_Patches.Type_Observed_Patch;
      This_Piece_Observation : Observation.Observation_Of_Pieces.Type_Observed_Piece;
      A_Patch                : Type_Client_Patch_Adress;

      Trav_Patches_Effect : Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Cursor;
      Trav_Construction : Observation.Observation_Of_Construction.Changes_To_Construction.Cursor;

      use Observation.Observation_Of_Patches.Changes_To_Patches;
      use Observation.Observation_Of_Pieces.Changes_To_Pieces;
      use Observation;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client.Map.Set_Reports_On_Map - enter");
      end if;

      -- First set and unset the patches that are visible or are not visible.
      Trav_P_P :=
         Observation.Observation_Of_Patches.Changes_To_Patches.First (P_Player_Observations_List);
      while Trav_P_P /= Observation.Observation_Of_Patches.Changes_To_Patches.No_Element loop

         This_Observation :=
            Observation.Observation_Of_Patches.Changes_To_Patches.Element (Trav_P_P);

         A_Patch :=
            Get_Patch_Adress_From_AB (P_Client_Map, This_Observation.A, This_Observation.B);

         Hexagon.Client_Map.Set_No_Pieces (P_Client_Map.Map, A_Patch.all);

         A_Patch.all.Visible := This_Observation.Visible;

         Observation.Observation_Of_Patches.Changes_To_Patches.Next (Trav_P_P);
      end loop;

      -- Set changes to pieces positions on map.
      Trav_P_Pieces :=
         Observation.Observation_Of_Pieces.Changes_To_Pieces.First (P_Player_Observed_Pieces_List);
      while Trav_P_Pieces /= Observation.Observation_Of_Pieces.Changes_To_Pieces.No_Element loop

         This_Piece_Observation :=
            Observation.Observation_Of_Pieces.Changes_To_Pieces.Element (Trav_P_Pieces);

         -- First remove the piece from any other patch where it might be
         --already.
         for Trav_A in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
            for Trav_B in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
               Trav_Pieces_Here :=
                  Landscape.Pieces_Here_List.First (P_Client_Map.Map (Trav_A, Trav_B).Pieces_Here);
               while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces_Here) loop
                  if Landscape.Pieces_Here_List.Element (Trav_Pieces_Here) =
                     This_Piece_Observation.Piece_Here_Id
                  then
                     -- Remove the piece from this patch
                     Landscape.Pieces_Here_List.Delete
                       (P_Client_Map.Map (Trav_A, Trav_B).Pieces_Here,
                        Landscape.Pieces_Here_List.To_Index (Trav_Pieces_Here));

                  end if;
                  Trav_Pieces_Here := Landscape.Pieces_Here_List.Next (Trav_Pieces_Here);
               end loop;
            end loop;
         end loop;

         -- If the new pieces' position is valid, it means it does exisit
         --somewhere on the map and
         -- we should place it there. If not, the piece has been removed from
         --the map totally (died
         --or destroyed).
         if This_Piece_Observation.Pos.P_Valid then
            A_Patch :=
               Hexagon.Client_Map.Get_Patch_Adress_From_AB
                 (P_Client_Map,
                  This_Piece_Observation.Pos.A,
                  This_Piece_Observation.Pos.B);

            Landscape.Pieces_Here_List.Append
              (A_Patch.all.Pieces_Here,
               This_Piece_Observation.Piece_Here_Id);
            Landscape.Pieces_Here_Sort.Sort (Landscape.Type_Patch (A_Patch.all).Pieces_Here);

         end if;

         Observation.Observation_Of_Pieces.Changes_To_Pieces.Next (Trav_P_Pieces);
      end loop;

      -- remove invisible
      Trav_Patches_Effect := Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.First (P_Player_Observed_Patches_Effects);

      while Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Has_Element (Trav_Patches_Effect) loop
         if not Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Valid then
            A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB(P_Client_Map,
                                                                   Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Pos.A,
                                                                   Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Pos.B);

            Effect.Effect_List.Exclude
              (A_Patch.all.Effects_Here,
               Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Effect_Info.Effect_Name);
         else
            A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB(P_Client_Map,
                                                                   Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Pos.A,
                                                                   Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Pos.B);

            Effect.Effect_List.Include
              (A_Patch.all.Effects_Here,
               Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Effect_Info.Effect_Name,
               Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Element (Trav_Patches_Effect).Effect_Info);
         end if;

         Trav_Patches_Effect := Observation.Observation_Of_Patches_Effects.Changes_To_Patches_Effects.Next (Trav_Patches_Effect);
      end loop;

      -- remove invisible
      Trav_Construction := Observation.Observation_Of_Construction.Changes_To_Construction.First (P_Player_Observed_Constructions);

      while Observation.Observation_Of_Construction.Changes_To_Construction.Has_Element (Trav_Construction) loop
         if not Observation.Observation_Of_Construction.Changes_To_Construction.Element (Trav_Construction).Valid then
            A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB(P_Client_Map,
                                                                   Observation.Observation_Of_Construction.Changes_To_Construction.Element (Trav_Construction).Pos.A,
                                                                   Observation.Observation_Of_Construction.Changes_To_Construction.Element (Trav_Construction).Pos.B);

            Construction.Construction_List.Exclude
              (A_Patch.all.Constructions_Here,
               Observation.Observation_Of_Construction.Changes_To_Construction.Element (Trav_Construction).Construction_Info);
         else
            A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB(P_Client_Map,
                                                                   Observation.Observation_Of_Construction.Changes_To_Construction.Element (Trav_Construction).Pos.A,
                                                                   Observation.Observation_Of_Construction.Changes_To_Construction.Element (Trav_Construction).Pos.B);

            Construction.Construction_List.Include
              (A_Patch.all.Constructions_Here,
               Observation.Observation_Of_Construction.Changes_To_Construction.Element (Trav_Construction).Construction_Info);
         end if;

         Trav_Construction := Observation.Observation_Of_Construction.Changes_To_Construction.Next (Trav_Construction);
      end loop;


      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client.Map.Set_Reports_On_Map - exit");
      end if;
   end Set_Reports_On_Map;


   procedure Traverse
     (P_Client_Map : in out Type_Client_Map_Info;
      P_Patch      : in out Type_Client_Patch_Adress;
      P_Visit      : in Type_Visit_Procedure)
   is
   begin

      for A in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for B in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
            P_Visit (P_Client_Map, P_Client_Map.Map (A, B));
         end loop;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Traverse - exit");
      end if;

   end Traverse;

   procedure Traverse_In
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch_Adress;
      P_Visit      : in Type_Visit_Procedure_In)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Hexagon.Client_Map.Traverse_In - enter " &
            P_Patch.Pos.A'Img &
            " " &
            P_Patch.Pos.B'Img);
      end if;

      for A in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for B in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
            P_Visit (P_Client_Map, P_Client_Map.Map (A, B));
         end loop;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Traverse_In - exit");
      end if;

   end Traverse_In;

   function Get_Absolute_X_From_AB (P_Patch : in Type_Client_Patch) return Integer is
   begin
      if P_Patch.Pos.P_Valid then
         return (Integer (Float (P_Patch.Pos.A - 1) * 0.866 * 42.0) + 20);
      else
         return 0;
      end if;
   end Get_Absolute_X_From_AB;

   function Get_Absolute_Y_From_AB (P_Patch : in Type_Client_Patch) return Integer is
   begin
      if P_Patch.Pos.P_Valid then
         return (Integer (Float (P_Patch.Pos.A - 1) * 0.5 * 42.0 +
                          Float (P_Patch.Pos.B - 1) * 42.0) +
                 18);
      else
         return 0;
      end if;
   end Get_Absolute_Y_From_AB;

   function Get_X_From_AB
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch)
      return         Integer
   is
   begin
      if P_Patch.Pos.P_Valid then
         return Get_Absolute_X_From_AB (P_Patch) -
                Get_Absolute_X_From_AB (P_Client_Map.Origo_Patch.all);
      else
         return 0;
      end if;
   end Get_X_From_AB;

   function Get_Y_From_AB
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch)
      return         Integer
   is
   begin
      if P_Patch.Pos.P_Valid then
         return Get_Absolute_Y_From_AB (P_Patch) -
                Get_Absolute_Y_From_AB (P_Client_Map.Origo_Patch.all);
      else
         return 0;
      end if;
   end Get_Y_From_AB;

   Clicked_X, Clicked_Y : Integer;
   Closest_Patch_Adress : Type_Client_Patch_Adress;
   Closest_Distance     : Float;

   procedure Find_Closest
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch_Adress)
   is
      Dist             : Float;
      Delta_X, Delta_Y : Integer;
   begin
      Delta_X := Get_X_From_AB (P_Client_Map, P_Patch.all) - Clicked_X;
      Delta_Y := Get_Y_From_AB (P_Client_Map, P_Patch.all) - Clicked_Y;

      Dist := Trig.Sqrt (Float (Delta_X * Delta_X + Delta_Y * Delta_Y));

      if Dist < Closest_Distance then
         Closest_Distance     := Dist;
         Closest_Patch_Adress := P_Patch;
      end if;
   end Find_Closest;

   procedure Find_Closest_Absolute
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch_Adress)
   is
      Dist             : Float;
      Delta_X, Delta_Y : Integer;
   begin

      Delta_X := Get_Absolute_X_From_AB (P_Patch.all) - Clicked_X;
      Delta_Y := Get_Absolute_Y_From_AB (P_Patch.all) - Clicked_Y;

      Dist := Trig.Sqrt (Float (Delta_X * Delta_X + Delta_Y * Delta_Y));

      if Dist < Closest_Distance then
         Closest_Distance     := Dist;
         Closest_Patch_Adress := P_Patch;
      end if;
   end Find_Closest_Absolute;

   function Get_Patch_Adress_From_XY
     (P_Client_Map : in Type_Client_Map_Info;
      P_X, P_Y     : in Integer  --; P_Origo_X, P_Origo_Y : in
                                 --Hexagon.Type_Hexagon_Numbers
        )
      return         Type_Client_Patch_Adress
   is
   begin
      Clicked_X            := P_X;
      Clicked_Y            := P_Y;
      Closest_Patch_Adress := null;
      Closest_Distance     := 100000.0;

      Reset_Visit;
      Traverse_In (P_Client_Map, P_Client_Map.Map (1, 1), Find_Closest'Access);

      return Closest_Patch_Adress;
   end Get_Patch_Adress_From_XY;

   function Get_Patch_Adress_From_Absolute_XY
     (P_Client_Map : in Type_Client_Map_Info;
      P_X, P_Y     : in Integer  --; P_Origo_X, P_Origo_Y : in
                                 --Hexagon.Type_Hexagon_Numbers
        )
      return         Type_Client_Patch_Adress
   is
   begin
      Clicked_X            := P_X;
      Clicked_Y            := P_Y;
      Closest_Patch_Adress := null;
      Closest_Distance     := 100000.0;

      Reset_Visit;
      Traverse_In (P_Client_Map, P_Client_Map.Map (1, 1), Find_Closest_Absolute'Access);

      return Closest_Patch_Adress;
   end Get_Patch_Adress_From_Absolute_XY;

   function Capability_To_Area
     (P_Client_Map : in Type_Client_Map_Info;
      P_Patch      : in Type_Client_Patch;
      P_Capability : in Hexagon.Area.Client_Area.Type_Action_Capabilities_Access)
      return         Type_Client_Patch_Area_Access
   is
      Area_Tiles : Type_Client_Patch_Area_Access;

      use Hexagon.Area.Client_Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Capability_To_Area - enter");
      end if;

      if P_Capability /= null then

         Area_Tiles := new Type_Client_Patch_Area (P_Capability'First .. P_Capability'Last);

         for Trav in P_Capability'First .. P_Capability'Last loop
            begin
               Area_Tiles.all (Trav) :=
                  Get_Patch_Adress_From_AB
                    (P_Client_Map,
                     Type_Hexagon_Numbers (Integer (P_Patch.Pos.A) +
                                           Integer (P_Capability (Trav).A)),
                     Type_Hexagon_Numbers (Integer (P_Patch.Pos.B) +
                                           Integer (P_Capability (Trav).B)));
            exception
               when others =>
                  Area_Tiles.all (Trav) := null; -- lat loesning for å loese
                  --det at nå-bare felter kan
                  --vaere utenfor kartet
                  --(e.g. i hjoernet)
            end;
         end loop;
      else
         Area_Tiles := null;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Capability_To_Area - enter");
      end if;

      return Area_Tiles;
   end Capability_To_Area;

   procedure Set_Reachable
     (P_Client_Map : in out Type_Client_Map_Info;
      P_Patch_Area : in out Type_Client_Patch_Area)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Set_Reachable - enter");
      end if;

      for Trav_X in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_Y in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
            if P_Client_Map.Map (Trav_X, Trav_Y).Draw_Action = Reachable then
               P_Client_Map.Map (Trav_X, Trav_Y).Draw_Action := Unselected;
            end if;
         end loop;
      end loop;

      for Trav in P_Patch_Area'First .. P_Patch_Area'Last loop

         if P_Patch_Area (Trav) /= null then
            P_Patch_Area (Trav).Draw_Action := Reachable;
         else
            if Verbose then
               Text_IO.Put_Line
                 ("Hexagon.Client_Map.Set_Reachable - Trav=" &
                  Trav'Img &
                  " probably  outside map");
            end if;
         end if;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Set_Reachable - exit");
      end if;
   end Set_Reachable;

   procedure Set_Attackable
     (P_Client_Map : in out Type_Client_Map_Info;
      P_Patch_Area : in out Type_Client_Patch_Area)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Set_Attackable - enter");
      end if;

      for Trav_X in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_Y in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
            if P_Client_Map.Map (Trav_X, Trav_Y).Draw_Action = Attackable then
               P_Client_Map.Map (Trav_X, Trav_Y).Draw_Action := Unselected;
            end if;
         end loop;
      end loop;

      for Trav in P_Patch_Area'First .. P_Patch_Area'Last loop

         if P_Patch_Area (Trav) /= null then
            P_Patch_Area (Trav).Draw_Action := Attackable;
         else
            if Verbose then
               Text_IO.Put_Line
                 ("Hexagon.Client_Map.Set_Attackable - Trav=" &
                  Trav'Img &
                  " probably  outside map");
            end if;
         end if;
      end loop;
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Set_Attackable - exit");
      end if;
   end Set_Attackable;

   procedure Save_Map
     (P_Filename   : in Ada.Strings.Unbounded.Unbounded_String;
      P_Client_Map : in Type_Client_Map_Info)
   is
      Write_File : Ada.Streams.Stream_IO.File_Type;
      Out_Stream : Stream_Access;

   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Save_Map - enter");
      end if;

      Create
        (Write_File,
         Ada.Streams.Stream_IO.Out_File,
         Ada.Strings.Unbounded.To_String (P_Filename));
      Out_Stream := Ada.Streams.Stream_IO.Stream (Write_File);

      for Trav_X in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         for Trav_Y in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop

            Landscape.Type_Landscape'Write
              (Ada.Streams.Stream_IO.Stream (Write_File),
               P_Client_Map.Map (Trav_X, Trav_Y).Landscape_Here);
         end loop;
      end loop;

      Close (Write_File);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Save_Map - exit");
      end if;
   end Save_Map;

   procedure Save_Scenario
     (P_Filename   : in Ada.Strings.Unbounded.Unbounded_String;
      P_Client_Map : in Type_Client_Map_Info)
   is
      Write_File : Ada.Streams.Stream_IO.File_Type;
      Out_Stream : Stream_Access;

      Trav : Landscape.Pieces_Here_List.Cursor;

      use Observation;
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Save_Scenario - enter");
      end if;

      Create
        (Write_File,
         Ada.Streams.Stream_IO.Out_File,
         Ada.Strings.Unbounded.To_String (P_Filename));
      Out_Stream := Ada.Streams.Stream_IO.Stream (Write_File);

      String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "<table border=""1""  >");

      for Trav_X in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop
         String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "  <tr>");
         for Trav_Y in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop
            String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "    <td");
            if P_Client_Map.Map (Trav_X, Trav_Y).Visible then
               String'Write (Ada.Streams.Stream_IO.Stream (Write_File), " BGCOLOR=""#ff0000"" >");
            else
               String'Write (Ada.Streams.Stream_IO.Stream (Write_File), " BGCOLOR=""#00ff00"" >");
            end if;

            String'Write
              (Ada.Streams.Stream_IO.Stream (Write_File),
               "(" &
               P_Client_Map.Map (Trav_X, Trav_Y).Pos.A'Img &
               "," &
               P_Client_Map.Map (Trav_X, Trav_Y).Pos.B'Img &
               ") " &
               P_Client_Map.Map (Trav_X, Trav_Y).all.Landscape_Here'Img);

            Trav :=
               Landscape.Pieces_Here_List.First (P_Client_Map.Map (Trav_X, Trav_Y).Pieces_Here);
            while Landscape.Pieces_Here_List.Has_Element (Trav) loop
               String'Write
                 (Ada.Streams.Stream_IO.Stream (Write_File),
                  "Id=" & Landscape.Pieces_Here_List.Element (Trav)'Img & "-");

               Trav := Landscape.Pieces_Here_List.Next (Trav);
            end loop;

            String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "    </td>");
         end loop;
         String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "  </tr>");
      end loop;

      String'Write (Ada.Streams.Stream_IO.Stream (Write_File), "</table>");
      Close (Write_File);

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map.Save_Scenario - exit");
      end if;
   end Save_Scenario;

   procedure Init_Client_Map (P_Client_Map : in out Type_Client_Map_Info) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map - enter");
      end if;

      -- Set up map
      --
      for ArrayX in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop -- Horisontal
         for ArrayY in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop -- Vertical

            P_Client_Map.Map (ArrayX, ArrayY) := new Type_Client_Patch'(Hexagon.Client_Map.Empty);
            Landscape.Pieces_Here_List.Clear (P_Client_Map.Map (ArrayX, ArrayY).Pieces_Here);
         end loop;
      end loop;

      for ArrayX in P_Client_Map.Map'First (1) .. P_Client_Map.Map'Last (1) loop -- Horisontal
         for ArrayY in P_Client_Map.Map'First (2) .. P_Client_Map.Map'Last (2) loop -- Vertical
         --
            begin
               P_Client_Map.Map (ArrayX, ArrayY).Neighbours (1) :=
                 (P_Client_Map.Map (ArrayX + 1, ArrayY + 0));
            exception
               when others =>
                  P_Client_Map.Map (ArrayX, ArrayY).Neighbours (1) := null;
            end;

            begin
               P_Client_Map.Map (ArrayX, ArrayY).Neighbours (2) :=
                 (P_Client_Map.Map (ArrayX + 1, ArrayY - 1));
            exception
               when others =>
                  P_Client_Map.Map (ArrayX, ArrayY).Neighbours (2) := null;
            end;

            begin
               P_Client_Map.Map (ArrayX, ArrayY).Neighbours (3) :=
                 (P_Client_Map.Map (ArrayX + 0, ArrayY - 1));
            exception
               when others =>
                  P_Client_Map.Map (ArrayX, ArrayY).Neighbours (3) := null;
            end;

            begin
               P_Client_Map.Map (ArrayX, ArrayY).Neighbours (4) :=
                 (P_Client_Map.Map (ArrayX - 1, ArrayY + 0));
            exception
               when others =>
                  P_Client_Map.Map (ArrayX, ArrayY).Neighbours (4) := null;
            end;

            begin
               P_Client_Map.Map (ArrayX, ArrayY).Neighbours (5) :=
                 (P_Client_Map.Map (ArrayX - 1, ArrayY + 1));
            exception
               when others =>
                  P_Client_Map.Map (ArrayX, ArrayY).Neighbours (5) := null;
            end;

            begin
               P_Client_Map.Map (ArrayX, ArrayY).Neighbours (6) :=
                 (P_Client_Map.Map (ArrayX + 0, ArrayY + 1));
            exception
               when others =>
                  P_Client_Map.Map (ArrayX, ArrayY).Neighbours (6) := null;
            end;

         end loop;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Hexagon.Client_Map - exit");
      end if;

   end Init_Client_Map;

end Hexagon.Client_Map;
