--
--
--      Tubastga Server is the core part of the server infrastructure of the Tubastga Game.
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

with Goods;
with Ada.Containers.Vectors;
with Hexagon.Server_Map;

package Tubastga_Piece.Carrier is

   package Patch_List is new Ada.Containers.Vectors
     (Positive,
      Hexagon.Server_Map.Type_Server_Patch_Adress,
      Hexagon.Server_Map."=");

   type Type_Patch_List is record
      List_Of_Patches : Patch_List.Vector;
      Piece_Id        : Piece.Type_Piece_Id;
   end record;

   function Get_Tower_Code (P_Tower_Id1, P_Tower_Id2, P_Tower_Id3 : in Piece.Type_Piece_Id) return Integer;

   function Get_Tower_Id (P_No : in Integer; P_Tower_Code : Integer) return Integer;

   function Get_Tower_Goods_Code
     (P_Goods_Id1, P_Goods_Id2, P_Goods_Id3 : in Goods.Type_Goods) return Integer;

   function Get_Tower_Goods_Id (P_No : in Integer; P_Goods_Code : Integer) return Goods.Type_Goods;

end Tubastga_Piece.Carrier;
