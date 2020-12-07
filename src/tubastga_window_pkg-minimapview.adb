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

with Tubastga_Window_Pkg.FullsizeView;
with Tubastga_Game;
with Landscape;
with Text_IO;
with Glib;
with Tubastga_Window_Pkg.Images;

package body Tubastga_Window_Pkg.MinimapView is

   Verbose : constant Boolean := False;

   Minimap_Scale   : constant Positive := 30;
   Minimap_Origo_Y : constant Integer  := 370;

   procedure Draw_Minimap
     (
      P_Client_Map  : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch       : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Minimapview : in out Gdk.Pixbuf.Gdk_Pixbuf)
   is
      Minimap_X, Minimap_Y : Glib.Gint;
      Minimap_Landscape_Image      : Tubastga_Window_Pkg.Images.Type_Image_Access;

      use Landscape;
      use Tubastga_Window_Pkg.Images;
   begin
      if Verbose then
         Text_IO.Put_Line("Tubastga_Window_Pkg.Minimapview.Draw_Minimap - enter");
      end if;

      Minimap_X :=
        Glib.Gint (10 + Hexagon.Client_Map.Get_Absolute_X_From_AB (P_Patch) / Minimap_Scale);
      Minimap_Y :=
        Glib.Gint
          (Minimap_Origo_Y - Hexagon.Client_Map.Get_Absolute_Y_From_AB (P_Patch) / Minimap_Scale);

      -- minimap
      Minimap_Landscape_Image := Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images,
                                                              Tubastga_Window_Pkg.Images.Find_Minimap_Landscape_Image(P_Patch.Landscape_Here));
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
      Minimap_Landscape_Image := Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images,
                                                                      Tubastga_Window_Pkg.Images.Find_Other_Image("minimap_outside_view"));
      end if;

      -- minimap
      Gdk.Pixbuf.Composite
        (Minimap_Landscape_Image.all.Image_Data,
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

      if Verbose then
         Text_IO.Put_Line("Tubastga_Window_Pkg.Minimapview.Draw_Minimap - exit");
      end if;
   end Draw_Minimap;

end Tubastga_Window_Pkg.MinimapView;
