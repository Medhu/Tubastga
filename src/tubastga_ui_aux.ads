--
--
--      Tubastga Game - A turn based strategy game
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

with Piece;
with Hexagon;
with Ada.Containers.Vectors;
with Utilities;

package Tubastga_UI_Aux is
   UI_Problem : exception;

   type Type_UI_State is (
--     Attack,
--     Move,
     Done,
     Scroll,
     Place_Sentry,
     Place_Knight,
     Place_Bowman,
     Place_Carrier,
     Place_Ship,
     Place_Tower,
     Place_Lumberjack,
     Place_Farm,
     Place_Stonecutter,
--     Promote,
--     Demote,
--     Search,
     Place_Wall1,
     Place_Wall2,
     Place_Wall3,
     Place_Wall4,
     Place_Wall5,
     Place_Wall6
     );

   UI_State : Type_UI_State;

   function Convert_UI_State_To_Piece
     (P_UI_State : in Type_UI_State)
      return       Piece.Type_Piece_Type;
   function Convert_UI_State_To_Category
     (P_UI_State : in Type_UI_State)
      return       Piece.Type_Category;

   type Type_Piece_Position is record
      Actual_Piece_Id : Piece.Type_Piece_Id;
      Actual_Pos      : Hexagon.Type_Hexagon_Position;
      Group_Id        : Natural;
   end record;

   package TAB_For_Pieces_List is new Ada.Containers.Vectors (
      Piece.Type_Piece_Id,
      Type_Piece_Position,
      "=");

   TAB_For_Pieces : TAB_For_Pieces_List.Vector;

   procedure Set_My_Player_Name(P_Playername : in Utilities.RemoteString.Type_String);
   function Get_My_Player_Name return Utilities.RemoteString.Type_String;

   procedure Set_Server_Port(P_Server_Port : in Natural);
   function Get_Server_Port return Natural;

   procedure Set_TCPIP_Adress(P_Octet1, P_Octet2, P_Octet3, P_Octet4 : in Natural);
   procedure Get_TCPIP_Adress(P_Octet1, P_Octet2, P_Octet3, P_Octet4 : out Natural);

end Tubastga_UI_Aux;
