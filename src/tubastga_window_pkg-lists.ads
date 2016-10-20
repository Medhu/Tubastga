--
--
--      Tubastga Game - A turn based strategy game.
--      Copyright (C) 2015-2016  Frank J Jorgensen
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

with Ada.Containers.Vectors;

package Tubastga_Window_Pkg.Lists is
   package Pos_List_Pkg is new Ada.Containers.Vectors
     (Positive,
      Hexagon.Type_Hexagon_Position,
      Hexagon."=");
   package Piece_List_Pkg is new Ada.Containers.Vectors
     (Positive,
      Piece.Type_Piece_Id,
      Piece."=");
   package Piece_Sort_Pkg is new Piece_List_Pkg.Generic_Sorting(Piece."<");

   procedure Set_Last_Selected_Pos
     (P_Selected_List : in out Pos_List_Pkg.Vector;
      P_Pos           : in     Hexagon.Type_Hexagon_Position;
      P_Shift_LR      : in     Boolean);

   function Get_Last_Selected_Pos
     (P_Selected_List : in Pos_List_Pkg.Vector) return Hexagon.Type_Hexagon_Position;

   function Get_Last_Selected_Piece
     (P_Selected_List : in Piece_List_Pkg.Vector) return Piece.Type_Piece_Id;

   procedure Set_Last_Selected_Piece
     (P_Selected_List : in out Piece_List_Pkg.Vector;
      P_Piece_Id      : in     Piece.Type_Piece_Id;
      P_Shift_LR      : in     Boolean);

   type Type_Piece_Position is record
      Actual_Piece_Id : Piece.Type_Piece_Id;
      Actual_Pos      : Hexagon.Type_Hexagon_Position;
   end record;

   function Less_Than (P_Left, P_Right : in Type_Piece_Position) return Boolean;

   package All_Pieces_List_Pkg is new Ada.Containers.Vectors
     (Piece.Type_Piece_Id,
      Type_Piece_Position,
      "=");

   function Find_Piece_In_All_Piece_List
     (P_List     : in Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.Vector;
      P_Piece_Id : in Piece.Type_Piece_Id) return All_Pieces_List_Pkg.Cursor;

   procedure Set_All_Piece_In_List
     (P_List           : in out All_Pieces_List_Pkg.Vector;
      P_Piece_Position : in     Type_Piece_Position);

   procedure Print_All_Piece_List (P_List : in All_Pieces_List_Pkg.Vector);

   package All_Pieces_Sort_Pkg is new All_Pieces_List_Pkg.Generic_Sorting (Less_Than);

   All_Pieces_List                 : All_Pieces_List_Pkg.Vector;

end Tubastga_Window_Pkg.Lists;
