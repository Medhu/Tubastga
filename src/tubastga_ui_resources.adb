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

with Text_IO;
with Glib.Error;

package body Tubastga_UI_Resources is

   procedure Load_Images is
      Error : Glib.Error.GError;

      use Glib.Error;
   begin
      for Trav in All_Images'First .. All_Images'Last loop

         Text_IO.Put
           ("Searching for image: " &
            Trav'Img &
            ":" &
            Ada.Strings.Unbounded.To_String (All_Images (Trav).Filename));

         Gdk.Pixbuf.Gdk_New_From_File
           (All_Images (Trav).Image_Data,
            Ada.Strings.Unbounded.To_String (All_Images (Trav).Filename),
            Error);

         if Error = null then
            All_Images (Trav).Image_Width  := Gdk.Pixbuf.Get_Width (All_Images (Trav).Image_Data);
            All_Images (Trav).Image_Height := Gdk.Pixbuf.Get_Height (All_Images (Trav).Image_Data);
            --
            Text_IO.Put_Line
              (" Width:" &
               All_Images (Trav).Image_Width'Img &
               " Height:" &
               All_Images (Trav).Image_Height'Img);
         else
            Text_IO.Put_Line ("Error: " & Glib.Error.Get_Message (Error));
            Glib.Error.Error_Free (Error);
            All_Images (Trav).Image_Width  := 72;
            All_Images (Trav).Image_Height := 72;

            All_Images (Trav).Image_Data :=
              Gdk.Pixbuf.Gdk_New
                (Bits_Per_Sample => 24,
                 Width           => All_Images (Trav).Image_Width,
                 Height          => All_Images (Trav).Image_Height);

            Gdk.Pixbuf.Fill (All_Images (Trav).Image_Data, 16#0000FF00#);
         end if;
      end loop;
   end Load_Images;

   procedure Initialize is
   begin
      All_Images :=
        (Invisible =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\invisible_hexagon.png"),
              null,
              0,
              0),
         Selected_Patch_LB =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\selected_hexagon.png"),
              null,
              0,
              0),
         Selected_Patch_RB =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\selected_hexagon.png"),
              null,
              0,
              0),
         Player_1 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\player_red.png"), null, 0, 0),
         Player_2 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\player_green.png"), null, 0, 0),
         Player_3 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\player_blue.png"), null, 0, 0),
         Dry =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry.png"), null, 0, 0),
         Dry2 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry2.png"), null, 0, 0),
         Dry3 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry3.png"), null, 0, 0),
         Dry4 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry4.png"), null, 0, 0),
         Dry5 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry5.png"), null, 0, 0),
         Green =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green.png"), null, 0, 0),
         Green2 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green2.png"), null, 0, 0),
         Green3 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green3.png"), null, 0, 0),
         Green4 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green4.png"), null, 0, 0),
         Green5 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green5.png"), null, 0, 0),
         Green6 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green6.png"), null, 0, 0),
         Green7 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green7.png"), null, 0, 0),
         Green8 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green8.png"), null, 0, 0),
         Semi_Dry =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry.png"), null, 0, 0),
         Semi_Dry2 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry2.png"), null, 0, 0),
         Semi_Dry3 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry3.png"), null, 0, 0),
         Semi_Dry4 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry4.png"), null, 0, 0),
         Semi_Dry5 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry5.png"), null, 0, 0),
         Semi_Dry6 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry6.png"), null, 0, 0),
         Forested_Deciduous_Summer_Hills_Tile =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String
                ("resources\forested-deciduous-summer-hills-tile.png"),
              null,
              0,
              0),
         Hills_Variation =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\hills-variation.png"),
              null,
              0,
              0),
         Water =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\water.png"), null, 0, 0),
         Chest =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\box.png"), null, 0, 0),
         Wall1 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall1.png"),
              null,
              0,
              0),
         Wall2 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall2.png"),
              null,
              0,
              0),
         Wall3 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall3.png"),
              null,
              0,
              0),
         Wall4 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall4.png"),
              null,
              0,
              0),
         Wall5 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall5.png"),
              null,
              0,
              0),
         Wall6 =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall6.png"),
              null,
              0,
              0),

         None =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\empty_hexagon.png"),
              null,
              0,
              0),
         Fighter =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\fighter-idle-1.png"),
              null,
              0,
              0),
         Rider =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\rider.png"), null, 0, 0),
         Archer =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\archer.png"), null, 0, 0),
         Boat =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\boat.png"), null, 0, 0),
         Lumberjack =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\cobbles-keep.png"), null, 0, 0),
         Stonecutter =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\cobbles-keep.png"), null, 0, 0),
         Towerhouse =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\cobbles-keep.png"), null, 0, 0),

         Minimap_Outside_View =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String
                ("resources\outside_view_invisible_minimap_hexagon.png"),
              null,
              0,
              0),

         Minimap_Grass =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_grass_hexagon.png"),
              null,
              0,
              0),
         Minimap_Mountain =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_mountain_hexagon.png"),
              null,
              0,
              0),
         Minimap_Water =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_water_hexagon.png"),
              null,
              0,
              0),
         Minimap_Forest =>
           Type_Graphic_Data'
             (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_forest_hexagon.png"),
              null,
              0,
              0));

      Load_Images;
   end Initialize;

end Tubastga_UI_Resources;
