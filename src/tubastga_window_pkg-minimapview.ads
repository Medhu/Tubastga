--
--
--      Tubastga Game - A turn based strategy game.
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

with Hexagon.Client_Map;
with Gdk.Pixbuf;
with Tubastga_Window_Pkg;

package Tubastga_Window_Pkg.MinimapView is

   procedure Draw_Minimap
     (P_All_Images : in     Tubastga_Window_Pkg.Type_Images;
      P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Minimapview : in out Gdk.Pixbuf.Gdk_Pixbuf);

end Tubastga_Window_Pkg.MinimapView;
