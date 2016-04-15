--
--
--      Tubastga Game
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

with Text_IO;
with Goods;
with Ada.Strings.Bounded;
with Text_IO.Editing;
with Tubastga_Piece.Server_Logic;

package Tubastga_Piece.Server_Logic.House_Piece is

   procedure Farm_House_Production
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Server_Logic.Type_My_Tubastga_House);

   procedure Lumberjack_House_Production
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Server_Logic.Type_My_Tubastga_House);

   procedure Stonecutter_House_Production
     (P_Current_Player_Id : in     Player.Type_Player_Id;
      P_Patch             : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House             : in out Server_Logic.Type_My_Tubastga_House);

end Tubastga_Piece.Server_Logic.House_Piece;
