--
--
--      Tubastga - a turn based strategy game
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

with Tubastga_Piece;
with Utilities;

package body Tubastga_UI_Aux is
   function Convert_UI_State_To_Piece
     (P_UI_State : in Type_UI_State)
      return       Piece.Type_Piece_Type
   is
      Ret : Piece.Type_Piece_Type;
   begin
      case P_UI_State is
         when Place_Sentry =>
            Ret := Tubastga_Piece.Sentry_Piece;
         when Place_Knight =>
            Ret := Tubastga_Piece.Knight_Piece;
         when Place_Bowman =>
            Ret := Tubastga_Piece.Bowman_Piece;
         when Place_Carrier =>
            Ret := Tubastga_Piece.Carrier_Piece;
         when Place_Ship =>
            Ret := Tubastga_Piece.Ship_Piece;
         when Place_Tower =>
            Ret := Tubastga_Piece.Tower_House;
         when Place_Lumberjack =>
            Ret := Tubastga_Piece.Lumberjack_House;
         when Place_Farm =>
            Ret := Tubastga_Piece.Farm_House;
         when Place_Stonecutter =>
            Ret := Tubastga_Piece.Stonecutter_House;
         when others =>
            raise UI_Problem;
      end case;

      return Ret;
   end Convert_UI_State_To_Piece;

   function Convert_UI_State_To_Category
     (P_UI_State : in Type_UI_State)
      return       Piece.Type_Category
   is
      Ret : Piece.Type_Category;
   begin
      case P_UI_State is
         when Place_Sentry =>
            Ret := Piece.Fighting_Piece;
         when Place_Knight =>
            Ret := Piece.Fighting_Piece;
         when Place_Bowman =>
            Ret := Piece.Fighting_Piece;
         when Place_Carrier =>
            Ret := Piece.Fighting_Piece;
         when Place_Ship =>
            Ret := Piece.Fighting_Piece;
         when Place_Tower =>
            Ret := Piece.House_Piece;
         when Place_Lumberjack =>
            Ret := Piece.House_Piece;
         when Place_Farm =>
            Ret := Piece.House_Piece;
         when Place_Stonecutter =>
            Ret := Piece.House_Piece;
         when others =>
            raise UI_Problem;
      end case;

      return Ret;
   end Convert_UI_State_To_Category;

   My_Playername : Utilities.RemoteString.Type_String;
   Octet1, Octet2, Octet3, Octet4 : Natural;
   Server_Port : Natural;

   procedure Set_My_Player_Name(P_Playername : in Utilities.RemoteString.Type_String)
   is
   begin
      My_Playername := P_Playername;
   end Set_My_Player_Name;

   function Get_My_Player_Name return Utilities.RemoteString.Type_String
   is
   begin
      return My_Playername;
   end Get_My_Player_Name;

   procedure Set_TCPIP_Adress(P_Octet1, P_Octet2, P_Octet3, P_Octet4 : in Natural)
   is
   begin
      Octet1 := P_Octet1;
      Octet2 := P_Octet2;
      Octet3 := P_Octet3;
      Octet4 := P_Octet4;
   end Set_TCPIP_Adress;

   procedure Get_TCPIP_Adress(P_Octet1, P_Octet2, P_Octet3, P_Octet4 : out Natural)
   is
   begin
      P_Octet1 := Octet1;
      P_Octet2 := Octet2;
      P_Octet3 := Octet3;
      P_Octet4 := Octet4;
   end Get_TCPIP_Adress;

   procedure Set_Server_Port(P_Server_Port : in Natural)
   is
   begin
      Server_Port := P_Server_Port;
   end Set_Server_Port;

   function Get_Server_Port return Natural
   is
   begin
      return Server_Port;
   end Get_Server_Port;

end Tubastga_UI_Aux;
