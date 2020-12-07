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

with Gdk.Pixbuf;
with Glib;
with Ada.Strings.Unbounded;
with Ada.Containers.Hashed_Maps;
with Landscape;
with Player;

package Tubastga_Window_Pkg.Images is

   type Type_Image is record
      Image_Name                         : Ada.Strings.Unbounded.Unbounded_String;
      Image_Data                         : Gdk.Pixbuf.Gdk_Pixbuf;
      Image_Height, Image_Width          : Glib.Gint;
      Dest_X, Dest_Y, Offset_X, Offset_Y : Glib.Gint;
   end record;
   type Type_Image_Access is access all Type_Image;

   type Type_Image_Names is
     (Invisible, Selected_Patch_LB, Selected_Patch_RB, Player_1, Player_2, Player_3, Dry, Dry2,
      Dry3, Dry4, Dry5, Green, Green2, Green3, Green4, Green5, Green6, Green7, Green8, Semi_Dry,
      Semi_Dry2, Semi_Dry3, Semi_Dry4, Semi_Dry5, Semi_Dry6, Forested_Deciduous_Summer_Hills_Tile,
      Hills_Variation, Water, Chest, Wall1, Wall2, Wall3, Wall4, Wall5, Wall6, None, Fighter, Rider,
      Archer, Boat, Lumberjack, Stonecutter, Towerhouse, Minimap_Outside_View, Minimap_Grass,
      Minimap_Mountain, Minimap_Water, Minimap_Forest);

   function Image_Names_Hashed (P_Image_Name : in Type_Image_Names) return Ada.Containers.Hash_Type;

   package Images_List_Pkg is new Ada.Containers.Hashed_Maps (Type_Image_Names, Type_Image_Access,
      Image_Names_Hashed, "=", "=");

   function Find_Landscape_Image
     (P_Landscape : in Landscape.Type_Landscape) return Tubastga_Window_Pkg.Images.Type_Image_Names;

   function Find_Minimap_Landscape_Image
     (P_Landscape : in Landscape.Type_Landscape) return Tubastga_Window_Pkg.Images.Type_Image_Names;

   function Find_Piece_Image
     (P_Piece : in Tubastga_Window_Pkg.Type_Client_Piece) return Tubastga_Window_Pkg.Images
     .Type_Image_Names;

   function Find_Player_Image
     (P_Player_Id : in Player.Type_Player_Id) return Tubastga_Window_Pkg.Images.Type_Image_Names;

   function Find_House_Image
     (P_Piece : in Tubastga_Window_Pkg.Type_Client_Piece) return Tubastga_Window_Pkg.Images
     .Type_Image_Names;

   function Find_Other_Image
     (P_Type : in String) return Tubastga_Window_Pkg.Images.Type_Image_Names;

   procedure Initialize (P_Images : in out Images_List_Pkg.Map);

   procedure Load_Images (P_Images_List : in out Images_List_Pkg.Map);

   procedure Print_Images_List (P_Images_List : in Images_List_Pkg.Map);

   function Get_Image (P_Images_List : in out Images_List_Pkg.Map;
      P_Image_Name                   : in     Type_Image_Names) return Type_Image_Access;

   All_Images : Images_List_Pkg.Map;

   procedure Print_Image (P_Image : in Type_Image);

end Tubastga_Window_Pkg.Images;
