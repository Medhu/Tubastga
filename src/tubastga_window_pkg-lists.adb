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

with Text_IO;

package body tubastga_window_pkg.lists is
   procedure Set_Last_Selected_Pos (P_Selected_List : in out Pos_List_Pkg.Vector;
      P_Pos : in     Hexagon.Type_Hexagon_Position; P_Shift_LR : in Boolean)
   is
      Does_Exist : Pos_List_Pkg.Cursor;

      use Piece.Client_Piece;
   begin
      if P_Shift_LR then
         Does_Exist := Pos_List_Pkg.Find (P_Selected_List, P_Pos);

         if Pos_List_Pkg.Has_Element (Does_Exist) then
            Pos_List_Pkg.Delete (P_Selected_List, Does_Exist);
         end if;
      else
         Pos_List_Pkg.Clear (P_Selected_List);
      end if;

      Pos_List_Pkg.Append (P_Selected_List, P_Pos);

   exception
      when others =>
         Text_IO.Put_Line ("exception: Set_Last_Selected_Pos");
   end Set_Last_Selected_Pos;

   function Get_Last_Selected_Pos
     (P_Selected_List : in Pos_List_Pkg.Vector) return Hexagon.Type_Hexagon_Position
   is
      Last_Focus  : Pos_List_Pkg.Cursor;
      A_Selection : Hexagon.Type_Hexagon_Position;

      use Piece.Client_Piece;
   begin
      Last_Focus := Pos_List_Pkg.Last (P_Selected_List);

      if Pos_List_Pkg.Has_Element (Last_Focus) then
         A_Selection := Pos_List_Pkg.Element (Last_Focus);
      end if;

      return A_Selection;

   exception
      when others =>
         Text_IO.Put_Line ("exception: Get_Last_Selected_Pos");
         return A_Selection;

   end Get_Last_Selected_Pos;

   function Get_Last_Selected_Piece
     (P_Selected_List : in Piece_List_Pkg.Vector) return Piece.Type_Piece_Id
   is
      Last_Focus       : Piece_List_Pkg.Cursor;
      Piece_Last_Focus : Piece.Type_Piece_Id;

      use Piece.Client_Piece;
   begin
      Last_Focus := Piece_List_Pkg.Last (P_Selected_List);

      if Piece_List_Pkg.Has_Element (Last_Focus) then
         Piece_Last_Focus := Piece_List_Pkg.Element (Last_Focus);
      else
         Piece_Last_Focus := Piece.Undefined_Piece_Id;
      end if;

      return Piece_Last_Focus;

   exception
      when others =>
         Text_IO.Put_Line ("exception: Get_Last_Selected_Piece");
         return Piece_Last_Focus;

   end Get_Last_Selected_Piece;

   procedure Set_Last_Selected_Piece (P_Selected_List : in out Piece_List_Pkg.Vector;
      P_Piece_Id : in     Piece.Type_Piece_Id; P_Shift_LR : in Boolean)
   is
      Does_Exist : Piece_List_Pkg.Cursor;

      use Piece.Client_Piece;
   begin
      if P_Shift_LR then
         Does_Exist := Piece_List_Pkg.Find (P_Selected_List, P_Piece_Id);

         if Piece_List_Pkg.Has_Element (Does_Exist) then
            Piece_List_Pkg.Delete (P_Selected_List, Does_Exist);
         end if;
      else
         Piece_List_Pkg.Clear (P_Selected_List);
      end if;

      Piece_List_Pkg.Append (P_Selected_List, P_Piece_Id);

   exception
      when others =>
         Text_IO.Put_Line ("exception: Set_Last_Selected_Piece");

   end Set_Last_Selected_Piece;

   function Find_Piece_In_All_Piece_List (P_List : in All_Pieces_List_Pkg.Vector;
      P_Piece_Id : in Piece.Type_Piece_Id) return All_Pieces_List_Pkg.Cursor
   is
      Trav       : All_Pieces_List_Pkg.Cursor;
      An_Element : Type_Piece_Position;
      Found      : Boolean := False;

      use Piece;
   begin
      Trav := All_Pieces_List_Pkg.First (P_List);
      while All_Pieces_List_Pkg.Has_Element (Trav) and not Found loop
         An_Element := All_Pieces_List_Pkg.Element (Trav);

         if An_Element.Actual_Piece_Id = P_Piece_Id then
            Found := True;
         else
            Trav := All_Pieces_List_Pkg.Next (Trav);
         end if;
      end loop;

      if Found then
         return Trav;
      else
         return All_Pieces_List_Pkg.No_Element;
      end if;

   end Find_Piece_In_All_Piece_List;

   procedure Set_All_Piece_In_List (P_List : in out All_Pieces_List_Pkg.Vector;
      P_Piece_Position                     : in     Type_Piece_Position)
   is
      Does_Exist : All_Pieces_List_Pkg.Cursor;

      use Piece.Client_Piece;
   begin
      Does_Exist := Find_Piece_In_All_Piece_List (P_List, P_Piece_Position.Actual_Piece_Id);

      if All_Pieces_List_Pkg.Has_Element (Does_Exist) then
         All_Pieces_List_Pkg.Delete (P_List, Does_Exist);
      end if;

      All_Pieces_List_Pkg.Append (P_List, P_Piece_Position);
   end Set_All_Piece_In_List;

   procedure Print_All_Piece_List (P_List : in All_Pieces_List_Pkg.Vector) is
      Trav       : All_Pieces_List_Pkg.Cursor;
      An_Element : Type_Piece_Position;

      use Piece;
   begin
      Trav := All_Pieces_List_Pkg.First (P_List);
      while All_Pieces_List_Pkg.Has_Element (Trav) loop
         An_Element := All_Pieces_List_Pkg.Element (Trav);

         Text_IO.Put_Line ("Print_All_Piece_List ID:" & An_Element.Actual_Piece_Id'Img);

         Trav := All_Pieces_List_Pkg.Next (Trav);
      end loop;
   end Print_All_Piece_List;

   function Less_Than (P_Left, P_Right : in Type_Piece_Position) return Boolean is

      use Piece;
   begin
      return P_Left.Actual_Piece_Id < P_Right.Actual_Piece_Id;
   end Less_Than;
end tubastga_window_pkg.lists;
