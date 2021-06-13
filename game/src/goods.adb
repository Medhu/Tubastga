--
--
--      Tubastga Server is the core part of the server infrastructure of the Tubastga Game.
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

package body Goods is

   Verbose : constant Boolean := False;

   function Decode_Goods (P_Aux : in Integer) return Type_Goods is
      Ret : Type_Goods;
   begin
      if Verbose then
         Text_IO.Put_Line ("Goods.Decode_Goods - enter P_Aux=" & P_Aux'Img);
      end if;

      case P_Aux is
         when 10 =>
            Ret := Goods.Stone;
         when 20 =>
            Ret := Goods.Wood;
         when 30 =>
            Ret := Goods.Food;
         when 40 =>
            Ret := Goods.Iron;
         when 99 =>
            Ret := Goods.None;
         when others =>
            Text_IO.Put_Line("Invalid P_Aux here");
      end case;

      if Verbose then
         Text_IO.Put_Line ("Goods.Decode_Goods - exit Ret=" & Ret'Img);
      end if;

      return Ret;
   end Decode_Goods;

   function Encode_Goods (P_Goods : in Type_Goods) return Integer is
      Ret : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line ("Goods.Encode_Goods - enter P_Aux=" & P_Goods'Img);
      end if;

      case P_Goods is
         when Goods.Stone =>
            Ret := 10;
         when Goods.Wood =>
            Ret := 20;
         when Goods.Food =>
            Ret := 30;
         when Goods.Iron =>
            Ret := 40;
         when Goods.None =>
            Ret := 99;
      end case;

      if Verbose then
         Text_IO.Put_Line ("Goods.Encode_Goods - exit Ret=" & Ret'Img);
      end if;

      return Ret;
   end Encode_Goods;

   procedure Print_Storages (P_Storage : in Type_Storage) is

   begin
      if Verbose then
         Text_IO.Put_Line ("Goods.Print_Storages - enter");
      end if;

      Text_IO.Put_Line ("Storage:");
      for Trav in P_Storage.Slots'First (1) .. P_Storage.Slots'Last (1) loop
         Text_IO.Put_Line
           (" P_Storage.Slots=" &
            P_Storage.Slots (Trav).The_Goods'Img &
            " " &
            P_Storage.Slots (Trav).Quantity'Img);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Goods.Print_Storages - exit");
      end if;
   end Print_Storages;

   function Find_In_Storage
     (P_Storage : in Type_Storage;
      P_Goods   : in Goods.Type_Goods) return Integer
   is
      Found     : Boolean := False;
      Ret, Trav : Integer;

      use Goods;
   begin
      if Verbose then
         Text_IO.Put_Line ("Goods.Find_In_Storage - enter P_Goods=" & P_Goods'Img);
      end if;

      Ret  := 0;
      Trav := 1;
      while Trav <= P_Storage.Slots'Last and not Found loop
         if P_Storage.Slots (Trav).The_Goods = P_Goods then
            Found := True;
            Ret   := Trav;
         else
            Trav := Trav + 1;
         end if;

      end loop;

      if Verbose then
         Text_IO.Put_Line ("Goods.Find_In_Storage - exit Ret =" & Ret'Img);
      end if;

      return Ret;
   end Find_In_Storage;

   function Find_Available_Storage
     (P_Storage : in Type_Storage;
      P_Goods   : in Goods.Type_Goods) return Integer
   is
      Found : Boolean := False;

      Ret  : Integer;
      Trav : Integer;
   begin
      -- find if any storage already contains this goods, then return this
      -- or if the above is not the case, then
      if Verbose then
         Text_IO.Put_Line ("Goods.Find_Available_Storage - enter");
      end if;

      Ret := Find_In_Storage (P_Storage, P_Goods);

      if Ret = 0 then
         Trav := 1;
         while Trav <= P_Storage.Slots'Last and not Found loop
            if P_Storage.Slots (Trav).Quantity = 0 then
               Found := True;
               Ret   := Trav;
            else
               Trav := Trav + 1;
            end if;

         end loop;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Goods.Find_Available_Storage - exit Ret =" & Ret'Img);
      end if;

      return Ret;
   end Find_Available_Storage;

   function Get_Qty
     (P_Storage : in out Type_Storage;
      P_Goods   : in     Goods.Type_Goods) return Integer
   is
      Current_Storage_No : Integer;
      Ret                : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line ("Goods.Get_Qty - enter P_Goods=" & P_Goods'Img);
      end if;

      Current_Storage_No := Find_In_Storage (P_Storage, P_Goods);
      if Current_Storage_No /= 0 then
         Ret := P_Storage.Slots (Current_Storage_No).Quantity;
      else
         Ret := 0;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Goods.Get_Qty - exit Ret=" & Ret'Img);
      end if;

      return Ret;
   end Get_Qty;

   procedure Into_Storage
     (P_Storage : in out Type_Storage;
      P_Goods   : in out Goods.Type_Goods_Info;
      P_Success :    out Boolean)
   is
      Current_Storage_No : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Goods.Into_Storage - enter P_Goods=" &
            P_Goods.The_Goods'Img &
            " P_Goods. =" &
            P_Goods.Quantity'Img);
      end if;

      P_Success := False;

      Current_Storage_No := Find_Available_Storage (P_Storage, P_Goods.The_Goods);

      if Current_Storage_No /= 0 then
         P_Storage.Slots (Current_Storage_No).The_Goods := P_Goods.The_Goods;
         if P_Storage.Slots (Current_Storage_No).Quantity + P_Goods.Quantity <=
           Max_Quantities (P_Goods.The_Goods)
         then
            P_Storage.Slots (Current_Storage_No).Quantity :=
              P_Storage.Slots (Current_Storage_No).Quantity + P_Goods.Quantity;
            P_Success := True;
         else
            P_Success := False;
         end if;
      else
         Text_IO.Put_Line ("Storage failes to take this (no available storage)");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Goods.Into_Storage - exit P_Success=" & P_Success'Img);
      end if;

   end Into_Storage;

   procedure From_Storage
     (P_Storage       : in out Type_Storage;
      P_Request_Goods : in     Type_Goods_Info;
      P_Goods         : in out Type_Goods_Info)
   is
      Current_Storage_No : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Goods.From_Storage - enter P_Request_Goods." &
            P_Request_Goods.The_Goods'Img &
            " P_Request_Goods.Quantity=" &
            P_Request_Goods.Quantity'Img);
      end if;

      P_Goods            := P_Request_Goods;
      Current_Storage_No := Find_In_Storage (P_Storage, P_Request_Goods.The_Goods);
      if Current_Storage_No /= 0 then
         if P_Storage.Slots (Current_Storage_No).Quantity >= P_Request_Goods.Quantity then
            P_Storage.Slots (Current_Storage_No).Quantity :=
              P_Storage.Slots (Current_Storage_No).Quantity - P_Request_Goods.Quantity;
            if P_Storage.Slots (Current_Storage_No).Quantity = 0 then
               P_Storage.Slots (Current_Storage_No).The_Goods := Goods.None;
            end if;
            P_Goods.Quantity := P_Request_Goods.Quantity;
         else
            P_Goods.Quantity := 0;
         end if;
      else
         P_Goods.Quantity := 0;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Goods.From_Storage - exit P_Goods.Quantity=" & P_Goods.Quantity'Img);
      end if;

   end From_Storage;

   function Goods_Info_To_Aux (P_Goods_Info : in Goods.Type_Goods_Info) return Integer is
   begin
      return Encode_Goods (P_Goods_Info.The_Goods) * 1000 + P_Goods_Info.Quantity;
   end Goods_Info_To_Aux;

   function Aux_To_Goods_String (P_Aux : in Integer) return String is
      Tmp       : String (1 .. 6);
      Goods_Ret : String (1 .. 8);
      Qty_Ret   : String (1 .. 3);
      Tmp_Goods : Goods.Type_Goods;
      Tmp2      : String (1 .. 11);
   begin
      if Verbose then
         Text_IO.Put_Line ("Goods.Aux_To_Goods_String - enter P_Aux=" & P_Aux'Img);
      end if;

      Tmp       := P_Aux'Img;
      Tmp_Goods := Decode_Goods (Integer'Value (Tmp (2 .. 3)));
      Qty_Ret   := Tmp (4 .. 6);
      case (Tmp_Goods) is
         when Goods.Stone =>
            Goods_Ret := "Stone   ";
         when Goods.Wood =>
            Goods_Ret := "Wood    ";
         when Goods.Food =>
            Goods_Ret := "Food    ";
         when Goods.Iron =>
            Goods_Ret := "Iron    ";
         when others =>
            Goods_Ret := "(ukjent)";
      end case;

      Tmp2 := Goods_Ret & Qty_Ret;

      if Verbose then
         Text_IO.Put_Line ("Goods.Aux_To_Goods_String - enter Tmp2=" & Tmp2);
      end if;

      return Tmp2;

   end Aux_To_Goods_String;

   function Aux_To_Goods_Info (P_Aux : in Integer) return Type_Goods_Info
   is
      Tmp       : String (1 .. 6);
      Qty_Ret   : String (1 .. 3);
      Tmp_Goods : Goods.Type_Goods;

      Ret : Type_Goods_Info;
   begin
      if Verbose then
         Text_IO.Put_Line ("Goods.Aux_To_Goods_Info - enter P_Aux=" & P_Aux'Img);
      end if;

      Tmp       := P_Aux'Img;
      Tmp_Goods := Decode_Goods (Integer'Value (Tmp (2 .. 3)));
      Qty_Ret   := Tmp (4 .. 6);
      Ret.Quantity := Integer'Value(Qty_Ret);
      case (Tmp_Goods) is
         when Goods.Stone =>
            Ret.The_Goods := Goods.Stone;
         when Goods.Wood =>
            Ret.The_Goods := Goods.Wood;
         when Goods.Food =>
            Ret.The_Goods := Goods.Food;
         when Goods.Iron =>
            Ret.The_Goods := Goods.Iron;
         when others =>
           Ret.The_Goods := Goods.None;
      end case;

      if Verbose then
         Text_IO.Put_Line ("Goods.Aux_To_Goods_Info - enter Ret=" & Ret.The_Goods'Img & " qty=" & Ret.Quantity'Img);
      end if;

      return Ret;

   end Aux_To_Goods_Info;

end Goods;
