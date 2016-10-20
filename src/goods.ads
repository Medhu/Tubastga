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

with Ada.Unchecked_Deallocation;

package Goods is

   type Type_Goods is (None, Stone, Wood, Food, Iron);
   type Type_Goods_Info is record
      The_Goods : Type_Goods := None;
      Quantity  : Integer    := 0;
   end record;

   type Type_Max_Quantities is array (Type_Goods) of Integer;
   Max_Quantities : constant Type_Max_Quantities := (0, 140, 180, 199, 150);

   function Encode_Goods (P_Goods : in Type_Goods) return Integer;
   function Decode_Goods (P_Aux : in Integer) return Type_Goods;

   type Type_Slots is array (Positive range <>) of Type_Goods_Info;

   type Type_Storage (P_Number_Of_Slots : Positive) is record
      Slots : Type_Slots (1 .. P_Number_Of_Slots);
   end record;

   type Type_Storage_Access is access all Type_Storage;
   procedure Free_Storage is new Ada.Unchecked_Deallocation
     (Object => Type_Storage,
      Name   => Type_Storage_Access);

   procedure Print_Storages (P_Storage : in Type_Storage);

   function Find_Available_Storage
     (P_Storage : in Type_Storage;
      P_Goods   : in Goods.Type_Goods) return Integer;

   function Get_Qty (P_Storage : in out Type_Storage; P_Goods : in Goods.Type_Goods) return Integer;

   procedure Into_Storage
     (P_Storage : in out Type_Storage;
      P_Goods   : in out Goods.Type_Goods_Info;
      P_Success :    out Boolean);

   procedure From_Storage
     (P_Storage       : in out Type_Storage;
      P_Request_Goods : in     Goods.Type_Goods_Info;
      P_Goods         : in out Goods.Type_Goods_Info);

   function Goods_Info_To_Aux (P_Goods_Info : in Goods.Type_Goods_Info) return Integer;

   function Aux_To_Goods_String (P_Aux : in Integer) return String;
   function Aux_To_Goods_Info (P_Aux : in Integer) return Type_Goods_Info;
end Goods;
