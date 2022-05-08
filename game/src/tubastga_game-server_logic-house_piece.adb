--
--
--      Tubastga Game
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

with Text_IO;
with Goods;

package body Tubastga_Game.Server_Logic.House_Piece is
   Verbose : constant Boolean := False;

   use Hexagon.Area;
   Farm_Production_Pattern : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access :=
     new Hexagon.Area.Type_Action_Capabilities'
       (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1), --13
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0), --14
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),--15
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),--16
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),--17
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1)--18
        );

   Lumberjack_Production_Pattern : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access :=
     new Hexagon.Area.Type_Action_Capabilities'
       (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1), --13
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0), --14
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),--15
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),--16
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),--17
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1)--18
        );

   Stonecutter_Production_Pattern : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access :=
     new Hexagon.Area.Type_Action_Capabilities'
       (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1), --13
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0), --14
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),--15
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),--16
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),--17
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1)--18
        );

   function Find_Tower_In_Radius
     (P_House_Piece_Pos       : in Hexagon.Type_Hexagon_Position;
      P_Distance              : in Positive) return Piece.Type_Piece_Id
   is
      Trav_Tower     : Piece.Server.Pieces_Server_List.Cursor;
      A_Pos : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(P_Valid => False);
      Found_Tower_Id : Piece.Type_Piece_Id           := Piece.Undefined_Piece_Id;
      A_House        : Server_Logic.Type_My_Tubastga_House;

      use Piece;
   begin

      Trav_Tower := Piece.Server.Pieces_Server_List.First (Piece.Server.All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav_Tower) and
        Found_Tower_Id = Piece.Undefined_Piece_Id
      loop

         if Piece.Server.Pieces_Server_List.Element (Trav_Tower).Actual_Piece.all.Type_Of_Piece =
           Tubastga_Game.Tower_House
         then
            A_House :=
              Server_Logic.Type_My_Tubastga_House
                (Piece.Server.Pieces_Server_List.Element (Trav_Tower).Actual_Piece.all);

            A_Pos := Piece.Server.Pieces_Server_List.Element (Trav_Tower).Actual_Pos;
            if A_Pos.P_Valid then

               if Hexagon.Server_Navigation.Hexagon_Distance
                   (A_Pos, P_House_Piece_Pos) <
                 P_Distance
               then
                  Found_Tower_Id := A_House.Id;
               end if;
            end if;
         end if;

         Trav_Tower := Piece.Server.Pieces_Server_List.Next (Trav_Tower);
      end loop;

      return Found_Tower_Id;
   end Find_Tower_In_Radius;

   function Count_Patches_With_Production
     (P_Pos                : in Hexagon.Type_Hexagon_Position;
      P_Landscape          : in Landscape.Type_Landscape;
      P_Production_Pattern : in Hexagon.Area.Server_Area.Type_Action_Capabilities_Access)
      return Natural
   is
      A_Patch                 : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Count_Producing_Patches : Natural;

      use Hexagon;
      use Landscape;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.House_Piece.Count_Patches_With_Production - enter");
      end if;

      Count_Producing_Patches := 0;

      for Trav in P_Production_Pattern'First .. P_Production_Pattern'Last loop
         A_Patch :=
           Hexagon.Server_Map.Get_Patch_Adress_From_AB
             (Hexagon.Type_Hexagon_Numbers
                (Integer (P_Pos.A) + Integer (P_Production_Pattern (Trav).A)),
              Hexagon.Type_Hexagon_Numbers
                (Integer (P_Pos.B) + Integer (P_Production_Pattern (Trav).B)));

         if A_Patch.all.Landscape_Here = P_Landscape then
            Count_Producing_Patches := Count_Producing_Patches + 1;
         end if;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.House_Piece.Count_Patches_With_Production - exit Ret=" &
            Count_Producing_Patches'Img);
      end if;

      return Count_Producing_Patches;
   end Count_Patches_With_Production;

   procedure Farm_House_Production
     (P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Server_Logic.Type_My_Tubastga_House)
   is
      A_House    : Server_Logic.Type_My_Tubastga_House;
      A_House_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;

      The_Goods               : Goods.Type_Goods_Info;
      Count_Producing_Patches : Natural;

      Ret : Boolean;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.House_Piece.Farm_House_Production - enter");
      end if;

      A_House_Id := Tubastga_Game.Server_Logic.House_Piece.Find_Tower_In_Radius (P_Patch.Pos, 4);

      if A_House_Id /= Piece.Undefined_Piece_Id then
         A_House :=
           Server_Logic.Type_My_Tubastga_House
             (Piece.Server.Find_Piece_In_List(A_House_Id)
                .Actual_Piece.all);

         Count_Producing_Patches :=
           Count_Patches_With_Production
             (P_Patch.Pos,
              Tubastga_Game.Landscape_Grass,
              Farm_Production_Pattern);
         The_Goods := Goods.Type_Goods_Info'(Goods.Food, Count_Producing_Patches);
         Goods.Into_Storage (A_House.Storage.all, The_Goods, Ret);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.House_Piece.Farm_House_Production - exit");
      end if;
   end Farm_House_Production;

   procedure Lumberjack_House_Production
     (P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Server_Logic.Type_My_Tubastga_House)
   is
      A_House                 : Server_Logic.Type_My_Tubastga_House;
      A_House_Id              : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      Count_Producing_Patches : Natural;

      The_Goods : Goods.Type_Goods_Info;

      Ret : Boolean;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.House_Piece.Lumberjack_House_Production - enter");
      end if;

      A_House_Id := Tubastga_Game.Server_Logic.House_Piece.Find_Tower_In_Radius (P_Patch.Pos, 4);

      if A_House_Id /= Piece.Undefined_Piece_Id then
         A_House :=
           Server_Logic.Type_My_Tubastga_House
             (Piece.Server.Find_Piece_In_List(A_House_Id)
                .Actual_Piece.all);

         Count_Producing_Patches :=
           Count_Patches_With_Production
             (P_Patch.Pos,
              Tubastga_Game.Landscape_Forest,
              Lumberjack_Production_Pattern);
         The_Goods := Goods.Type_Goods_Info'(Goods.Wood, Count_Producing_Patches);
         Goods.Into_Storage (A_House.Storage.all, The_Goods, Ret);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.House_Piece.Lumberjack_House_Production - exit");
      end if;
   end Lumberjack_House_Production;

   procedure Stonecutter_House_Production
     (P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Server_Logic.Type_My_Tubastga_House)
   is
      A_House                 : Server_Logic.Type_My_Tubastga_House;
      A_House_Id              : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
      Count_Producing_Patches : Natural;

      The_Goods : Goods.Type_Goods_Info;

      Ret : Boolean;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.House_Piece.Stonecutter_House_Production - enter");
      end if;

      A_House_Id := Tubastga_Game.Server_Logic.House_Piece.Find_Tower_In_Radius (P_Patch.Pos, 4);

      if A_House_Id /= Piece.Undefined_Piece_Id then
         A_House :=
           Server_Logic.Type_My_Tubastga_House
             (Piece.Server.Find_Piece_In_List(A_House_Id)
                .Actual_Piece.all);

         Count_Producing_Patches :=
           Count_Patches_With_Production
             (P_Patch.Pos,
              Tubastga_Game.Landscape_Mountain,
              Stonecutter_Production_Pattern);
         The_Goods := Goods.Type_Goods_Info'(Goods.Stone, Count_Producing_Patches);
         Goods.Into_Storage (A_House.Storage.all, The_Goods, Ret);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.House_Piece.Stonecutter_House_Production - exit");
      end if;
   end Stonecutter_House_Production;

end Tubastga_Game.Server_Logic.House_Piece;
