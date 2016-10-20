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

with Tubastga_Window_Pkg.FullsizeView;
with Tubastga_Piece;
with Landscape;

package body Tubastga_Window_Pkg.MinimapView is

   Minimap_Scale   : constant Positive := 30;
   Minimap_Origo_Y : constant Integer  := 350;

   procedure Draw_Minimap
     (P_All_Images  : in     Tubastga_Window_Pkg.Type_Images;
      P_Client_Map  : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch       : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Minimapview : in out Gdk.Pixbuf.Gdk_Pixbuf)
   is
      Minimap_X, Minimap_Y : Glib.Gint;
      Landscape_Image      : Tubastga_Window_Pkg.Type_Image_Names;

      use Landscape;
   begin

      Minimap_X :=
        Glib.Gint (10 + Hexagon.Client_Map.Get_Absolute_X_From_AB (P_Patch) / Minimap_Scale);
      Minimap_Y :=
        Glib.Gint
          (Minimap_Origo_Y - Hexagon.Client_Map.Get_Absolute_Y_From_AB (P_Patch) / Minimap_Scale);

      -- minimap

      if P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Grass then
         Landscape_Image := Minimap_Grass;
      elsif P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Forest then
         Landscape_Image := Minimap_Forest;
      elsif P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Mountain then
         Landscape_Image := Minimap_Mountain;
      elsif P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Water then
         Landscape_Image := Minimap_Water;
      end if;

      if
        ((Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch) in
            0 .. 50 or
          Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch) in
            770 .. 820) and
         (Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch) in
            0 .. 1050)) or
         --
        ((Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch) in
            0 .. 50 or
          Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch) in
            1000 .. 1050) and
         (Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch) in
            0 .. 820))
      then
         Landscape_Image := Outside_View_Invisible_On_Minimap;
      end if;

     -- minimap
      Gdk.Pixbuf.Composite
        (P_All_Images (Landscape_Image).Image_Data,
         P_Minimapview,
         Glib.Gint (Minimap_X),
         Glib.Gint (Minimap_Y),
         5,
         5,
         Glib.Gdouble (Minimap_X),
         Glib.Gdouble (Minimap_Y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);
   end Draw_Minimap;

end Tubastga_Window_Pkg.MinimapView;
