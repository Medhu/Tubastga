--
--
--      Tubastga Game - A turn based strategy game.
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
package Tubastga_Window_Pkg.Callbacks.Target_Patch is

   procedure On_Target_Patch_Tree_View (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class);

   procedure Set_Target_Patch_Window
     (P_Window : in out Type_Wnd_Target_Patch_Access;
      P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch_Adress);

end Tubastga_Window_Pkg.Callbacks.Target_Patch;
