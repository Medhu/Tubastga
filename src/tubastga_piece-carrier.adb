--
--
--      Tubastga Game
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

with Text_IO;
with Goods;

package body Tubastga_Piece.Carrier is
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
         Text_IO.Put_Line ("Tubastga_Piece.Carrier.Get_Tower_Code - enter " & P_Tower_Id1'Img & "-" & P_Tower_Id2'Img & "-" & P_Tower_Id3'Img & "-");
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
         Text_IO.Put_Line ("Tubastga_Piece.Carrier.Get_Tower_Code - exit Tmp=" & Tmp);
      end if;
      return Integer'Value (Tmp);
   end Get_Tower_Code;

   function Get_Tower_Id (P_No : in Integer; P_Tower_Code : Integer) return Integer is
      Tmp : String (1 .. 8);
      Ret : Integer;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Piece.Carrier.Get_Tower_Id - enter P_Tower_Code=" & P_Tower_Code'Img);
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
         Text_IO.Put_Line ("Tubastga_Piece.Carrier.Get_Tower_Id - enter Ret=" & Ret'Img);
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
         Text_IO.Put_Line ("Tubastga_Piece.Carrier.Get_Tower_Goods_Code - enter P_Goods_Id1=" & P_Goods_Id1'Img & " P_Goods_Id2=" & P_Goods_Id2'Img & " P_Goods_Id3=" & P_Goods_Id3'Img);
      end if;

      Tmp1 := Goods.Encode_Goods (P_Goods_Id1)'Img;
      Tmp2 := Goods.Encode_Goods (P_Goods_Id2)'Img;
      Tmp3 := Goods.Encode_Goods (P_Goods_Id3)'Img;

      Tmp := " 1" & Tmp1 (2 .. 3) & Tmp2 (2 .. 3) & Tmp3 (2 .. 3);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Piece.Carrier.Get_Tower_Goods_Code - enter Tmp=" & Tmp);
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


end Tubastga_Piece.Carrier;
