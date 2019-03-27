--
--
--      Tubastga Server is the core part of the server infrastructure of the Tubastga Game.
--      Copyright (C) 2015-2017-2016  Frank J Jorgensen
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
with Ada.Streams.Stream_IO;
with Tubastga_Game.Server_Logic;

package Tubastga_Game.Server_Logic.Carrier is

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

   function Carrier_Tower_Stops
     (P_Piece        : in Server_Logic.Type_My_Tubastga_Piece;
      P_Tower_Number : in Integer) return Integer;

   function Carrier_Tower_Load
     (P_Piece        : in Server_Logic.Type_My_Tubastga_Piece;
      P_Tower_Number : in Integer) return Goods.Type_Goods;

   function Carrier_Tower_Unload
     (P_Piece        : in Server_Logic.Type_My_Tubastga_Piece;
      P_Tower_Number : in Integer) return Goods.Type_Goods;

   procedure Carrier_Tower_Transaction
     (P_Tower  : in out Server_Logic.Type_My_Tubastga_House;
      P_Piece  : in out Server_Logic.Type_My_Tubastga_Piece;
      P_Load   : in     Goods.Type_Goods;
      P_Unload : in     Goods.Type_Goods);

   procedure Carrier_Move
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Patch     : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece     : in out Server_Logic.Type_My_Tubastga_Piece);

   function Get_Create_Path_Patches (P_Player_Id : in Player.Type_Player_Id) return Type_Patch_List;

   function Get_Remove_Path_From_Carrier (P_Player_Id : in Player.Type_Player_Id) return Piece.Type_Piece_Id;

   function Create_From_Path_Patch_List
     (P_Player_Id  : in     Player.Type_Player_Id;
      P_Patch_List : in out Patch_List.Vector) return Hexagon.Server_Navigation.Path_Pkg.Vector;

   procedure Clear_Path_Effects (P_Player_Id : in Player.Type_Player_Id);

   procedure Create_Workers_Path (P_Player_Id : in Player.Type_Player_Id);
   procedure Remove_Workers_Path (P_Player_Id : in Player.Type_Player_Id);

   procedure Save_Workers_Path (P_Stream : in out Ada.Streams.Stream_IO.Stream_Access);
   procedure Load_Workers_Path (P_Stream : in out Ada.Streams.Stream_IO.Stream_Access);
   procedure Print_Workers_Path;

end Tubastga_Game.Server_Logic.Carrier;
