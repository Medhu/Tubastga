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

with Ada.Strings.Unbounded;
with Gdk.Pixbuf;
with Glib;

package Tubastga_UI_Resources is
   Verbose : constant Boolean := False;

   type Type_Graphic_Data is record
      Filename                  : Ada.Strings.Unbounded.Unbounded_String;
      Image_Data                : Gdk.Pixbuf.Gdk_Pixbuf;
      Image_Height, Image_Width : Glib.Gint;
   end record;

   type Type_Image_Names is
     (Invisible,
      Selected_Patch_LB,
      Selected_Patch_RB,
      Player_1,
      Player_2,
      Player_3,
      Dry,
      Dry2,
      Dry3,
      Dry4,
      Dry5,
      Green,
      Green2,
      Green3,
      Green4,
      Green5,
      Green6,
      Green7,
      Green8,
      Semi_Dry,
      Semi_Dry2,
      Semi_Dry3,
      Semi_Dry4,
      Semi_Dry5,
      Semi_Dry6,
      Forested_Deciduous_Summer_Hills_Tile,
      Hills_Variation,
      Water,
      Chest,
      Wall1,
      Wall2,
      Wall3,
      Wall4,
      Wall5,
      Wall6,
      None,
      Fighter,
      Rider,
      Archer,
      Boat,
      Lumberjack,
      Stonecutter,
      Towerhouse,
      Minimap_Outside_View,
      Minimap_Grass,
      Minimap_Mountain,
      Minimap_Water,
      Minimap_Forest
);

   type Type_Image_List is array (Type_Image_Names) of Type_Graphic_Data;

   All_Images : Type_Image_List;

   procedure Initialize;

end Tubastga_UI_Resources;
