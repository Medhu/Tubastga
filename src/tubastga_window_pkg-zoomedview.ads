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

with Hexagon.Client_Map;
with Glib;
with Tubastga_Window_Pkg;
with Tubastga_Window_Pkg.Callbacks;
with Tubastga_Window_Pkg.Callbacks.Main_Window;

package Tubastga_Window_Pkg.ZoomedView is

   procedure Set_Map_Scale (P_Map_Scale : in Glib.Gdouble);

   function Selected_Patch (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
                            P_Zoomedview_X,
                            P_Zoomedview_Y : Glib.Gdouble)
                            return Hexagon.Client_Map.Type_Client_Patch_Adress;

   procedure Scroll_Map
     (P_Client_Map       : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Scroll_Direction : in     Tubastga_Window_Pkg.Callbacks.Main_Window.Type_Scroll_Direction);

end Tubastga_Window_Pkg.ZoomedView;
