--
--
--      Tubastga Game - A turn based strategy game
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

with Gtk.Arguments;
with Gtk.Widget;
with Cairo;
with Tubastga_Window_Pkg.Lists;
with Utilities;
with Player;

package Tubastga_Window_Pkg.Callbacks is

   LB_Selected_Pos : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Vector;
   RB_Selected_Pos : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Vector;

   LB_Selected_Pieces : Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Vector;
   RB_Selected_Pieces : Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Vector;

   The_Window   : Type_Wnd_Main_Access;
   A_Client_Map : Hexagon.Client_Map.Type_Client_Map_Info;
   type Type_Player_Name_List is array (1 .. 3) of Utilities.RemoteString.Type_String;
   Player_Name_List : Type_Player_Name_List;
   Me_Player_Id     : Player.Type_Player_Id := 0;

end Tubastga_Window_Pkg.Callbacks;
