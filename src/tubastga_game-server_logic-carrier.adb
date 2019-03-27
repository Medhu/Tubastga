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
with Server.ServerAPI;

package body Tubastga_Game.Server_Logic.Carrier is
   Verbose : constant Boolean := False;

   function Get_Tower_Code
     (P_Tower_Id1, P_Tower_Id2, P_Tower_Id3 : in Piece.Type_Piece_Id) return Integer
   is
      Tmp              : String (1 .. 8);
      Tmp1, Tmp2, Tmp3 : String (1 .. 5);

      Tower_Id : Integer;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Tower_Code - enter " & P_Tower_Id1'Img & "-" & P_Tower_Id2'Img & "-" & P_Tower_Id3'Img & "-");
      end if;

      if P_Tower_Id1 /= Piece.Undefined_Piece_Id and P_Tower_Id1 in 1..99 then
         Tower_Id := Integer(P_Tower_Id1) + 1000;

         Tmp1 := Tower_Id'Img;
      else
         Tmp1 := "99999";
      end if;

      if P_Tower_Id2 /= Piece.Undefined_Piece_Id and P_Tower_Id2 in 1..99  then
         Tower_Id := Integer(P_Tower_Id2) + 1000;

         Tmp2 := Tower_Id'Img;
      else
         Tmp2 := "99999";
      end if;

      if P_Tower_Id3 /= Piece.Undefined_Piece_Id and P_Tower_Id3 in 1..99  then
         Tower_Id := Integer(P_Tower_Id3) + 1000;

         Tmp3 := Tower_Id'Img;
      else
         Tmp3 := "99999";
      end if;

      Tmp := " 1" & Tmp1 (4 .. 5) & Tmp2 (4 .. 5) & Tmp3 (4 .. 5);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Tower_Code - exit Tmp=" & Tmp);
      end if;
      return Integer'Value (Tmp);
   end Get_Tower_Code;

   function Get_Tower_Id (P_No : in Integer; P_Tower_Code : Integer) return Integer is
      Tmp : String (1 .. 8);
      Ret : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Tower_Id - enter P_Tower_Code=" & P_Tower_Code'Img);
      end if;

      Tmp := P_Tower_Code'Img;
      if P_No = 1 then
         Ret := Integer'Value (Tmp (3 .. 4));
      elsif P_No = 2 then
         Ret := Integer'Value (Tmp (5 .. 6));
      elsif P_No = 3 then
         Ret := Integer'Value (Tmp (7 .. 8));
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Tower_Id - enter Ret=" & Ret'Img);
      end if;

      return Ret;
   end Get_Tower_Id;

   function Get_Tower_Goods_Code
     (P_Goods_Id1, P_Goods_Id2, P_Goods_Id3 : in Goods.Type_Goods) return Integer
   is
      Tmp              : String (1 .. 8);
      Tmp1, Tmp2, Tmp3 : String (1 .. 3);
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Tower_Goods_Code - enter P_Goods_Id1=" & P_Goods_Id1'Img & " P_Goods_Id2=" & P_Goods_Id2'Img & " P_Goods_Id3=" & P_Goods_Id3'Img);
      end if;

      Tmp1 := Goods.Encode_Goods (P_Goods_Id1)'Img;
      Tmp2 := Goods.Encode_Goods (P_Goods_Id2)'Img;
      Tmp3 := Goods.Encode_Goods (P_Goods_Id3)'Img;

      Tmp := " 1" & Tmp1 (2 .. 3) & Tmp2 (2 .. 3) & Tmp3 (2 .. 3);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Tower_Goods_Code - enter Tmp=" & Tmp);
      end if;
      return Integer'Value (Tmp);
   end Get_Tower_Goods_Code;

   function Get_Tower_Goods_Id
     (P_No         : in Integer;
      P_Goods_Code :    Integer) return Goods.Type_Goods
   is
      Tmp : String (1 .. 8);
      Ret : Integer;
   begin
      Tmp := P_Goods_Code'Img;

      if P_No = 1 then
         Ret := Integer'Value (Tmp (3 .. 4));
      elsif P_No = 2 then
         Ret := Integer'Value (Tmp (5 .. 6));
      elsif P_No = 3 then
         Ret := Integer'Value (Tmp (7 .. 8));
      end if;

      return Goods.Decode_Goods (Ret);
   end Get_Tower_Goods_Id;

   function Carrier_Tower_Stops
     (P_Piece        : in Server_Logic.Type_My_Tubastga_Piece;
      P_Tower_Number : in Integer) return Integer
   is
      Cursor_Stops : Effect.Effect_List.Cursor;

      To_Tower_Id : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Stops - enter");
      end if;

      Cursor_Stops :=
        Effect.Effect_List.Find
          (P_Piece.Effects_On_Piece,
           Tubastga_Game.Effect_Stops);

      if Effect.Effect_List.Has_Element (Cursor_Stops) then
         To_Tower_Id :=
           Carrier.Get_Tower_Id (P_Tower_Number, Effect.Effect_List.Element (Cursor_Stops).Aux);
      else
         To_Tower_Id := 0;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Stops - exit To_Tower_Id=" & To_Tower_Id'Img);
      end if;

      return To_Tower_Id;
   end Carrier_Tower_Stops;

   function Carrier_Tower_Load
     (P_Piece        : in Server_Logic.Type_My_Tubastga_Piece;
      P_Tower_Number : in Integer) return Goods.Type_Goods
   is
      Cursor_Load : Effect.Effect_List.Cursor;

      To_Goods_Id : Goods.Type_Goods;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Load - enter");
      end if;

      Cursor_Load :=
        Effect.Effect_List.Find
          (P_Piece.Effects_On_Piece,
           Tubastga_Game.Effect_Load);

      if Effect.Effect_List.Has_Element (Cursor_Load) then
         To_Goods_Id :=
           Carrier.Get_Tower_Goods_Id
             (P_Tower_Number,
              Effect.Effect_List.Element (Cursor_Load).Aux);
      else
         To_Goods_Id := Goods.None;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Load - exit To_Goods_Id=" & To_Goods_Id'Img);
      end if;

      return To_Goods_Id;
   end Carrier_Tower_Load;

   function Carrier_Tower_Unload
     (P_Piece        : in Server_Logic.Type_My_Tubastga_Piece;
      P_Tower_Number : in Integer) return Goods.Type_Goods
   is
      Cursor_Unload : Effect.Effect_List.Cursor;

      To_Goods_Id : Goods.Type_Goods;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Unload - enter P_Tower_Number=" & P_Tower_Number'Img);
      end if;

      Cursor_Unload :=
        Effect.Effect_List.Find
          (P_Piece.Effects_On_Piece,
           Tubastga_Game.Effect_Unload);

      if Effect.Effect_List.Has_Element (Cursor_Unload) then
         To_Goods_Id :=
           Carrier.Get_Tower_Goods_Id
             (P_Tower_Number,
              Effect.Effect_List.Element (Cursor_Unload).Aux);
      else
         To_Goods_Id := Goods.None;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Unload - exit To_Goods_Id=" & To_Goods_Id'Img);
      end if;

      return To_Goods_Id;
   end Carrier_Tower_Unload;

   procedure Carrier_Move
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Patch     : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece     : in out Server_Logic.Type_My_Tubastga_Piece)
   is
      Current_Piece_Pos                     : Hexagon.Type_Hexagon_Position;
      Current_Carriers_Path                 : Hexagon.Navigation.Type_Path;
      Current_Path_Cursor, Next_Path_Cursor : Hexagon.Navigation.Path_Pkg.Cursor;
      Path_This_Piece                       : Server_Logic.Carrier_Paths_List.Cursor;
      Ret_Status                            : Status.Type_Status;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Carrier_Move- enter");
      end if;

      Current_Piece_Pos := P_Patch.Pos;

      Path_This_Piece := Server_Logic.Carrier_Paths_List.Find (Server_Logic.All_Paths, P_Piece.Id);

      if P_Piece.Player_Id = P_Player_Id and Server_Logic.Carrier_Paths_List.Has_Element (Path_This_Piece) then

            -- get the path that this piece is following at the moment.
            Current_Carriers_Path := Server_Logic.Carrier_Paths_List.Element (Server_Logic.All_Paths, P_Piece.Id);

            -- based on current position, find where on the path the piece is at the moment
            Current_Path_Cursor :=
           Hexagon.Navigation.Path_Pkg.Find (Current_Carriers_Path.This_Path,
                                             Hexagon.Navigation.Get_Navigation_Node_By_Position
                                               (Hexagon.Server_Map.A_Navigation, Current_Piece_Pos) );

            if Current_Carriers_Path.Direction = 1 then

               Next_Path_Cursor := Hexagon.Navigation.Path_Pkg.Next (Current_Path_Cursor);
            else

               Next_Path_Cursor := Hexagon.Navigation.Path_Pkg.Previous (Current_Path_Cursor);
            end if;

            if not Hexagon.Navigation.Path_Pkg.Has_Element (Next_Path_Cursor) then

               if Current_Carriers_Path.Direction = 1 then
                  Next_Path_Cursor := Hexagon.Navigation.Path_Pkg.Last (Current_Carriers_Path.This_Path);
                  Next_Path_Cursor                := Hexagon.Navigation.Path_Pkg.Previous (Next_Path_Cursor);
                  Current_Carriers_Path.Direction := -1;
               else
                  Next_Path_Cursor := Hexagon.Navigation.Path_Pkg.First (Current_Carriers_Path.This_Path);
                  Next_Path_Cursor                := Hexagon.Navigation.Path_Pkg.Next (Next_Path_Cursor);
                  Current_Carriers_Path.Direction := 1;
               end if;
               Server_Logic.Carrier_Paths_List.Replace_Element
                 (Server_Logic.All_Paths,
                  Path_This_Piece,
                  Current_Carriers_Path);
            end if;

            Server.ServerAPI.Perform_Move
              (P_Piece.Player_Id,
               Action.Type_Action_Type(1),
               P_Piece.Id,
               Hexagon.Navigation.Path_Pkg.Element (Next_Path_Cursor).all.Center,
               Ret_Status);

     end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Carrier_Move- exit");
      end if;
   end Carrier_Move;

   procedure Carrier_Tower_Transaction
     (P_Tower  : in out Server_Logic.Type_My_Tubastga_House;
      P_Piece  : in out Server_Logic.Type_My_Tubastga_Piece;
      P_Load   : in     Goods.Type_Goods;
      P_Unload : in     Goods.Type_Goods)
   is
      Ret_Goods     : Goods.Type_Goods_Info;
      Ret           : Boolean;
      Available_Qty : Integer;
      Ask_For_Qty   : Integer := 0;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Carrier_Tower_Transaction - enter Tower.Piece_Id=" & P_Tower.Id'Img);
      end if;

      --

      -- is there something carrier has, that should be unloaded to this tower?
      --        --is there something in this tower that this carrier should pick up?
      Available_Qty := Goods.Get_Qty (P_Piece.Storage.all, P_Unload);
      if Available_Qty >= Goods.Max_Quantities (P_Unload) then
         Ask_For_Qty := Goods.Max_Quantities (P_Unload);
      else
         Ask_For_Qty := Available_Qty;
      end if;

      Goods.From_Storage
        (P_Piece.Storage.all,
         Goods.Type_Goods_Info'(P_Unload, Ask_For_Qty),
         Ret_Goods);
      Goods.Into_Storage (P_Tower.Storage.all, Ret_Goods, Ret);

      -- is there something this tower has that carrier should pick up?
      Available_Qty := Goods.Get_Qty (P_Tower.Storage.all, P_Load);
      if Available_Qty >= Goods.Max_Quantities (P_Load) then
         Ask_For_Qty := Goods.Max_Quantities (P_Load);
      else
         Ask_For_Qty := Available_Qty;
      end if;

      Goods.From_Storage
        (P_Tower.Storage.all,
         Goods.Type_Goods_Info'(P_Load, Ask_For_Qty),
         Ret_Goods);
      Goods.Into_Storage (P_Piece.Storage.all, Ret_Goods, Ret);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Carrier_Tower_Transaction - exit");
      end if;
   end Carrier_Tower_Transaction;

   procedure Find_Path_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Patch     : in     Landscape.Type_Patch;
      P_Found     :    out Boolean;
      P_Piece_Id  :    out Piece.Type_Piece_Id)
   is
      Cursor_Path : Effect.Effect_List.Cursor;
      Player_Id   : Player.Type_Player_Id;

      Tmp : String (1 .. 8);

      use Player;
   begin
      Cursor_Path :=
        Effect.Effect_List.Find
          (P_Patch.Effects_Here,
           Tubastga_Game.Effect_Path);

      P_Found := Effect.Effect_List.Has_Element (Cursor_Path);
      if P_Found
        and then
        (Effect.Effect_List.Element (Cursor_Path).Aux > 1000000 and
         Effect.Effect_List.Element (Cursor_Path).Aux < 9999999)
      then
         Tmp := Effect.Effect_List.Element (Cursor_Path).Aux'Img;

         Player_Id := Player.Type_Player_Id (Integer'Value (Tmp (2 .. 2)));

         if P_Player_Id = Player_Id and Tmp (8 .. 8) = "0" then
            P_Piece_Id := Piece.Type_Piece_Id (Integer'Value (Tmp (3 .. 7)));
            P_Found    := True;
         else
            P_Piece_Id := Piece.Undefined_Piece_Id;
            P_Found    := False;
         end if;
      else
         P_Piece_Id := Piece.Undefined_Piece_Id;
         P_Found    := False;
      end if;

   end Find_Path_Effect;

   procedure Find_Un_Path_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Patch     : in     Landscape.Type_Patch;
      P_Found     :    out Boolean;
      P_Piece_Id  :    out Piece.Type_Piece_Id)
   is
      Cursor_Path : Effect.Effect_List.Cursor;
      Player_Id   : Player.Type_Player_Id;

      Tmp : String (1 .. 8);

      use Player;
   begin
      Cursor_Path :=
        Effect.Effect_List.Find
          (P_Patch.Effects_Here,
           Tubastga_Game.Effect_Path);

      P_Found := Effect.Effect_List.Has_Element (Cursor_Path);
      if P_Found
        and then
        (Effect.Effect_List.Element (Cursor_Path).Aux > 1000000 and
         Effect.Effect_List.Element (Cursor_Path).Aux < 9999999)
      then
         Tmp := Effect.Effect_List.Element (Cursor_Path).Aux'Img;

         Player_Id := Player.Type_Player_Id (Integer'Value (Tmp (2 .. 2)));

         if P_Player_Id = Player_Id and Tmp (8 .. 8) = "1" then
            P_Piece_Id := Piece.Type_Piece_Id (Integer'Value (Tmp (3 .. 7)));
            P_Found    := True;
         else
            P_Piece_Id := Piece.Undefined_Piece_Id;
            P_Found    := False;
         end if;
      else
         P_Piece_Id := Piece.Undefined_Piece_Id;
      end if;

   end Find_Un_Path_Effect;

   function Get_Create_Path_Patches
     (P_Player_Id : in Player.Type_Player_Id) return Type_Patch_List
   is
      A_Patch        : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Trav_A, Trav_B : Positive;
      Piece_Id       : Piece.Type_Piece_Id;
      Ret            : Boolean;
      Ret_List       : Type_Patch_List;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Path_Patches- enter");
      end if;

      Ret_List.Piece_Id := Piece.Undefined_Piece_Id;
      Patch_List.Clear (Ret_List.List_Of_Patches);

      Trav_A := Hexagon.Server_Map.A_Map'First (1);
      while Trav_A in Hexagon.Server_Map.A_Map'First (1) .. Hexagon.Server_Map.A_Map'Last (1) loop
         Trav_B := Hexagon.Server_Map.A_Map'First (2);
         while Trav_B in Hexagon.Server_Map.A_Map'First (2) .. Hexagon.Server_Map.A_Map'Last (2)
         loop
            A_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB
                (Hexagon.Type_Hexagon_Numbers (Trav_A),
                 Hexagon.Type_Hexagon_Numbers (Trav_B));

            Find_Path_Effect (P_Player_Id, Landscape.Type_Patch (A_Patch.all), Ret, Piece_Id);

            if Ret then
               Ret_List.Piece_Id := Piece_Id;
               Patch_List.Append (Ret_List.List_Of_Patches, A_Patch);
            end if;

            Trav_B := Trav_B + 1;
         end loop;
         Trav_A := Trav_A + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Path_Patches- exit");
      end if;

      return Ret_List;
   end Get_Create_Path_Patches;

   function Get_Remove_Path_From_Carrier
     (P_Player_Id : in Player.Type_Player_Id) return Piece.Type_Piece_Id
   is
      A_Patch        : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Trav_A, Trav_B : Positive;
      Piece_Id       : Piece.Type_Piece_Id;
      Ret            : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Get_Remove_Path_From_Carrier- enter");
      end if;

      Piece_Id := Piece.Undefined_Piece_Id;
      Ret      := False;

      Trav_A := Hexagon.Server_Map.A_Map'First (1);
      while Trav_A in Hexagon.Server_Map.A_Map'First (1) .. Hexagon.Server_Map.A_Map'Last (1) and
        not Ret
      loop
         Trav_B := Hexagon.Server_Map.A_Map'First (2);
         while Trav_B in Hexagon.Server_Map.A_Map'First (2) .. Hexagon.Server_Map.A_Map'Last (2) and
           not Ret
         loop
            A_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB
                (Hexagon.Type_Hexagon_Numbers (Trav_A),
                 Hexagon.Type_Hexagon_Numbers (Trav_B));

            Find_Un_Path_Effect (P_Player_Id, Landscape.Type_Patch (A_Patch.all), Ret, Piece_Id);

            Trav_B := Trav_B + 1;
         end loop;
         Trav_A := Trav_A + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Carrier.Get_Remove_Path_From_Carrier- exit Piece_Id=" & Piece_Id'Img);
      end if;

      return Piece_Id;
   end Get_Remove_Path_From_Carrier;

   function Create_From_Path_Patch_List
     (P_Player_Id  : in     Player.Type_Player_Id;
      P_Patch_List : in out Patch_List.Vector) return Hexagon.Navigation.Path_Pkg.Vector
   is

      type Type_Direction is (To_Left, To_Right);

      procedure Traverse
        (P_Patch      : in     Hexagon.Server_Map.Type_Server_Patch_Adress;
         P_Path       : in out Hexagon.Navigation.Path_Pkg.Vector;
         P_Patch_List : in out Patch_List.Vector;
         P_Direction  : in     Type_Direction)
      is
         Cursor_Check_With_Patch_List : Patch_List.Cursor;
         Patch_To_Check               : Hexagon.Server_Map.Type_Server_Patch_Adress;
         Neighbour_Found              : Boolean;
      begin
         Neighbour_Found              := False;
         Cursor_Check_With_Patch_List := Patch_List.First (P_Patch_List);
         while Patch_List.Has_Element (Cursor_Check_With_Patch_List) and not Neighbour_Found loop
            Patch_To_Check :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB
                (Patch_List.Element (Cursor_Check_With_Patch_List).all.Pos.A,
                 Patch_List.Element (Cursor_Check_With_Patch_List).all.Pos.B);

            if Hexagon.Server_Map.Are_Neighbours (P_Patch.all, Patch_To_Check.all) then
               Neighbour_Found := True;
            else
               Cursor_Check_With_Patch_List := Patch_List.Next (Cursor_Check_With_Patch_List);
            end if;
         end loop;

         if Neighbour_Found then
            if P_Direction = To_Left then
               Hexagon.Navigation.Path_Pkg.Prepend (P_Path,
                                                    Hexagon.Navigation.Get_Navigation_Node_By_Position
                                                      (Hexagon.Server_Map.A_Navigation,
                                                       Patch_To_Check.all.Pos) );
               Patch_List.Delete (P_Patch_List, Cursor_Check_With_Patch_List);
               Traverse (Patch_To_Check, P_Path, P_Patch_List, P_Direction);
            else
               Hexagon.Navigation.Path_Pkg.Append (P_Path,
                                                   Hexagon.Navigation.Get_Navigation_Node_By_Position
                                                     (Hexagon.Server_Map.A_Navigation,
                                                      Patch_To_Check.all.Pos) );
               Patch_List.Delete (P_Patch_List, Cursor_Check_With_Patch_List);
               Traverse (Patch_To_Check, P_Path, P_Patch_List, P_Direction);
            end if;

         end if;
      end Traverse;

      Patch_In_Progress : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Cursor_Any_Patch : Patch_List.Cursor;
      The_Path                                       : Hexagon.Navigation.Path_Pkg.Vector;

      use Hexagon.Server_Map;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Create_From_Path_Patch_List- enter");
      end if;

      -- start with whatever patch is first in the patch list
      Cursor_Any_Patch  := Patch_List.First (P_Patch_List);
      Patch_In_Progress :=
        Hexagon.Server_Map.Get_Patch_Adress_From_AB
          (Patch_List.Element (Cursor_Any_Patch).all.Pos.A,
           Patch_List.Element (Cursor_Any_Patch).all.Pos.B);
      Hexagon.Navigation.Path_Pkg.Append (The_Path,
                                          Hexagon.Navigation.Get_Navigation_Node_By_Position
                                            (Hexagon.Server_Map.A_Navigation, Patch_In_Progress.all.Pos) );

      Patch_List.Delete (P_Patch_List, Cursor_Any_Patch);

      Traverse (Patch_In_Progress, The_Path, P_Patch_List, To_Left);
      Traverse (Patch_In_Progress, The_Path, P_Patch_List, To_Right);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Create_From_Path_Patch_List- exit");
      end if;
      return The_Path;

   end Create_From_Path_Patch_List;

   procedure Clear_Path_Effects (P_Player_Id : in Player.Type_Player_Id) is
      A_Patch        : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Trav_A, Trav_B : Positive;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Clear_Path_Effects- enter");
      end if;

      Trav_A := Hexagon.Server_Map.A_Map'First (1);
      while Trav_A in Hexagon.Server_Map.A_Map'First (1) .. Hexagon.Server_Map.A_Map'Last (1) loop
         Trav_B := Hexagon.Server_Map.A_Map'First (2);
         while Trav_B in Hexagon.Server_Map.A_Map'First (2) .. Hexagon.Server_Map.A_Map'Last (2)
         loop
            A_Patch :=
              Hexagon.Server_Map.Get_Patch_Adress_From_AB
                (Hexagon.Type_Hexagon_Numbers (Trav_A),
                 Hexagon.Type_Hexagon_Numbers (Trav_B));

            Effect.Effect_List.Exclude
              (Landscape.Type_Patch (A_Patch.all).Effects_Here,
               Tubastga_Game.Effect_Path);

            Trav_B := Trav_B + 1;
         end loop;
         Trav_A := Trav_A + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Clear_Path_Effects- exit");
      end if;

   end Clear_Path_Effects;

   procedure Create_Workers_Path (P_Player_Id : in Player.Type_Player_Id) is
      New_Path, Prev_Path : Hexagon.Navigation.Path_Pkg.Vector;
      A_Patch_List        : Type_Patch_List;
      Cursor_Prev_Path    : Server_Logic.Carrier_Paths_List.Cursor;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Create_Workers_Path- enter");
      end if;

      A_Patch_List := Tubastga_Game.Server_Logic.Carrier.Get_Create_Path_Patches (P_Player_Id);

      if A_Patch_List.Piece_Id /= Piece.Undefined_Piece_Id then
         New_Path :=
           Tubastga_Game.Server_Logic.Carrier.Create_From_Path_Patch_List
             (P_Player_Id,
              A_Patch_List.List_Of_Patches);

         Cursor_Prev_Path := Server_Logic.Carrier_Paths_List.Find (Server_Logic.All_Paths, A_Patch_List.Piece_Id);
         if Server_Logic.Carrier_Paths_List.Has_Element (Cursor_Prev_Path) then
            Prev_Path := Server_Logic.Carrier_Paths_List.Element (Cursor_Prev_Path).This_Path;
            Hexagon.Navigation.Path_Pkg.Clear (Prev_Path);
            Server_Logic.Carrier_Paths_List.Exclude (Server_Logic.All_Paths, A_Patch_List.Piece_Id);
         end if;

         Server_Logic.Carrier_Paths_List.Include
           (Server_Logic.All_Paths,
            A_Patch_List.Piece_Id,
            Hexagon.Navigation.Type_Path'(New_Path, 1));
         Tubastga_Game.Server_Logic.Carrier.Clear_Path_Effects (P_Player_Id);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Create_Workers_Path- exit");
      end if;
   end Create_Workers_Path;

   procedure Remove_Workers_Path (P_Player_Id : in Player.Type_Player_Id) is
      Remove_Piece_Id : Piece.Type_Piece_Id;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Remove_Workers_Path- enter");
      end if;

      Remove_Piece_Id := Tubastga_Game.Server_Logic.Carrier.Get_Remove_Path_From_Carrier (P_Player_Id);

      if Remove_Piece_Id /= Piece.Undefined_Piece_Id then
         Server_Logic.Carrier_Paths_List.Exclude (Server_Logic.All_Paths, Remove_Piece_Id);
         Tubastga_Game.Server_Logic.Carrier.Clear_Path_Effects (P_Player_Id);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Carrier.Remove_Workers_Path- exit");
      end if;
   end Remove_Workers_Path;

end Tubastga_Game.Server_Logic.Carrier;


