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
with Ada.Directories;
with Tubastga_Game;
with Landscape;

package body Tubastga_Window_Pkg.Images is

   Verbose : constant Boolean := False;

   function Image_Names_Hashed (P_Image_Name : in Type_Image_Names) return Ada.Containers.Hash_Type
   is
   begin
      return Type_Image_Names'Pos (P_Image_Name);
   end Image_Names_Hashed;

   function Find_Piece_Image
     (P_Piece : in Tubastga_Window_Pkg.Type_Client_Piece) return Tubastga_Window_Pkg.Images
     .Type_Image_Names
   is
      Piece_Image : Tubastga_Window_Pkg.Images.Type_Image_Names;

      use Piece;
   begin
      Piece_Image := Tubastga_Window_Pkg.Images.None;

      if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
         Piece_Image := Tubastga_Window_Pkg.Images.Fighter;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
         Piece_Image := Tubastga_Window_Pkg.Images.Rider;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
         Piece_Image := Tubastga_Window_Pkg.Images.Archer;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Ship_Piece then
         Piece_Image := Tubastga_Window_Pkg.Images.Boat;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
         Piece_Image := Tubastga_Window_Pkg.Images.Boat;
      end if;

      return Piece_Image;
   end Find_Piece_Image;

   function Find_Landscape_Image
     (P_Landscape : in Landscape.Type_Landscape) return Tubastga_Window_Pkg.Images.Type_Image_Names
   is
      Landscape_Image : Tubastga_Window_Pkg.Images.Type_Image_Names;

      use Landscape;
   begin
      Landscape_Image := Tubastga_Window_Pkg.Images.None;

      if P_Landscape = Tubastga_Game.Landscape_Grass then
         Landscape_Image := Tubastga_Window_Pkg.Images.Green;
      elsif P_Landscape = Tubastga_Game.Landscape_Forest then
         Landscape_Image := Tubastga_Window_Pkg.Images.Forested_Deciduous_Summer_Hills_Tile;
      elsif P_Landscape = Tubastga_Game.Landscape_Mountain then
         Landscape_Image := Tubastga_Window_Pkg.Images.Hills_Variation;
      elsif P_Landscape = Tubastga_Game.Landscape_Water then
         Landscape_Image := Tubastga_Window_Pkg.Images.Water;
      end if;

      return Landscape_Image;
   end Find_Landscape_Image;

   function Find_Minimap_Landscape_Image
     (P_Landscape : in Landscape.Type_Landscape) return Tubastga_Window_Pkg.Images.Type_Image_Names
   is
      Landscape_Image : Tubastga_Window_Pkg.Images.Type_Image_Names;

      use Landscape;
   begin
      --if Verbose then
      --    Text_IO.Put_Line("Tubastga_Window_Pkg.Images.Find_Minimap_Landscape_Image - enter");
      --end if;

      if P_Landscape = Tubastga_Game.Landscape_Grass then
         Landscape_Image := Tubastga_Window_Pkg.Images.Minimap_Grass;
      elsif P_Landscape = Tubastga_Game.Landscape_Forest then
         Landscape_Image := Tubastga_Window_Pkg.Images.Minimap_Forest;
      elsif P_Landscape = Tubastga_Game.Landscape_Mountain then
         Landscape_Image := Tubastga_Window_Pkg.Images.Minimap_Mountain;
      elsif P_Landscape = Tubastga_Game.Landscape_Water then
         Landscape_Image := Tubastga_Window_Pkg.Images.Minimap_Water;
      end if;

      --if Verbose then
      --   Text_IO.Put_Line("Tubastga_Window_Pkg.Images.Find_Minimap_Landscape_Image - exit");
      --end if;

      return Landscape_Image;
   end Find_Minimap_Landscape_Image;

   function Find_Player_Image
     (P_Player_Id : in Player.Type_Player_Id) return Tubastga_Window_Pkg.Images.Type_Image_Names
   is
      Player_Image_Name : Tubastga_Window_Pkg.Images.Type_Image_Names;

      use Player;
   begin
      Player_Image_Name := Tubastga_Window_Pkg.Images.None;

      if P_Player_Id = 1 then
         Player_Image_Name := Tubastga_Window_Pkg.Images.Player_1;
      elsif P_Player_Id = 2 then
         Player_Image_Name := Tubastga_Window_Pkg.Images.Player_2;
      else
         Player_Image_Name := Tubastga_Window_Pkg.Images.Player_3;
      end if;

      return Player_Image_Name;
   end Find_Player_Image;

   function Find_House_Image
     (P_Piece : in Tubastga_Window_Pkg.Type_Client_Piece) return Tubastga_Window_Pkg.Images
     .Type_Image_Names
   is
      House_Image_Name : Tubastga_Window_Pkg.Images.Type_Image_Names;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Find_House_Image - enter");
      end if;

      House_Image_Name := Tubastga_Window_Pkg.Images.None;

      if P_Piece.Type_Of_Piece = Tubastga_Game.Farm_House then
         if P_Piece.Player_Id = 1 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Boat;
         elsif P_Piece.Player_Id = 2 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Boat;
         else
            House_Image_Name := Tubastga_Window_Pkg.Images.Boat;
         end if;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Tower_House then
         if P_Piece.Player_Id = 1 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Towerhouse;
         elsif P_Piece.Player_Id = 2 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Towerhouse;
         else
            House_Image_Name := Tubastga_Window_Pkg.Images.Towerhouse;
         end if;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Lumberjack_House then
         if P_Piece.Player_Id = 1 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Lumberjack;
         elsif P_Piece.Player_Id = 2 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Lumberjack;
         else
            House_Image_Name := Tubastga_Window_Pkg.Images.Lumberjack;
         end if;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Stonecutter_House then
         if P_Piece.Player_Id = 1 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Stonecutter;
         elsif P_Piece.Player_Id = 2 then
            House_Image_Name := Tubastga_Window_Pkg.Images.Stonecutter;
         else
            House_Image_Name := Tubastga_Window_Pkg.Images.Stonecutter;
         end if;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Find_House_Image - exit");
      end if;

      return House_Image_Name;
   end Find_House_Image;

   function Find_Other_Image (P_Type : in String) return Tubastga_Window_Pkg.Images.Type_Image_Names
   is
      Other_Image : Tubastga_Window_Pkg.Images.Type_Image_Names;
   begin
--      if Verbose then
--         Text_IO.Put_Line("Tubastga_Window_Pkg.Images.Find_Minimap_Landscape_Image - enter");
--      end if;

      Other_Image := Tubastga_Window_Pkg.Images.None;

      if P_Type = "invisible" then
         Other_Image := Tubastga_Window_Pkg.Images.Invisible;
      elsif P_Type = "selected_patch_LB" then
         Other_Image := Tubastga_Window_Pkg.Images.Selected_Patch_LB;
      elsif P_Type = "selected_patch_RB" then
         Other_Image := Tubastga_Window_Pkg.Images.Selected_Patch_RB;
      elsif P_Type = "minimap_outside_view" then
         Other_Image := Tubastga_Window_Pkg.Images.Minimap_Outside_View;
      end if;

      text_io.Put_Line("Other_Image:" & Other_Image'Img);
--      if Verbose then
--         Text_IO.Put_Line("Tubastga_Window_Pkg.Images.Find_Minimap_Landscape_Image - exit");
--      end if;

      return Other_Image;
   end Find_Other_Image;

   procedure Initialize (P_Images : in out Images_List_Pkg.Map) is

      use Glib;
   begin

      Images_List_Pkg.Include
        (P_Images, Invisible,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\invisible_hexagon.png"), null, 0,
            0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Selected_Patch_LB,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\selected_hexagon.png"), null, 0,
            0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Selected_Patch_RB,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\selected_hexagon.png"), null, 0,
            0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Player_1,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\player_red.png"), null, 0, 0, 0,
            0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Player_2,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\player_green.png"), null, 0, 0, 0,
            0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Player_3,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\player_blue.png"), null, 0, 0, 0,
            0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Dry,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Dry2,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry2.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Dry3,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry3.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Dry4,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry4.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Dry5,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\dry5.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green2,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green2.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green3,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green3.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green4,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green4.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green5,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green5.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green6,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green6.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green7,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green7.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Green8,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green8.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Semi_Dry,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry.png"), null, 0, 0, 0, 0,
            0, 0));

      Images_List_Pkg.Include
        (P_Images, Semi_Dry2,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry2.png"), null, 0, 0, 0, 0,
            0, 0));

      Images_List_Pkg.Include
        (P_Images, Semi_Dry3,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry3.png"), null, 0, 0, 0, 0,
            0, 0));

      Images_List_Pkg.Include
        (P_Images, Semi_Dry4,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry4.png"), null, 0, 0, 0, 0,
            0, 0));

      Images_List_Pkg.Include
        (P_Images, Semi_Dry5,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry5.png"), null, 0, 0, 0, 0,
            0, 0));

      Images_List_Pkg.Include
        (P_Images, Semi_Dry6,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\semi-dry6.png"), null, 0, 0, 0, 0,
            0, 0));

      Images_List_Pkg.Include
        (P_Images, Forested_Deciduous_Summer_Hills_Tile,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String
              ("resources\forested-deciduous-summer-hills-tile.png"),
            null, 0, 0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Hills_Variation,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\hills-variation.png"), null, 0, 0,
            0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Water,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\water.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Chest,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\box.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Wall1,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall1.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Wall2,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall2.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Wall3,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall3.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Wall4,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall4.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Wall5,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall5.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Wall6,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall6.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, None,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\empty_hexagon.png"), null, 0, 0,
            0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Fighter,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\fighter-idle-1.png"), null, 0, 0,
            0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Rider,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\rider.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Archer,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\archer.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Boat,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\boat.png"), null, 0, 0, 0, 0, 0,
            0));

      Images_List_Pkg.Include
        (P_Images, Lumberjack,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\cobbles-keep.png"), null, 0, 0, 0,
            0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Stonecutter,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\cobbles-keep.png"), null, 0, 0, 0,
            0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Towerhouse,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\cobbles-keep.png"), null, 0, 0, 0,
            0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Minimap_Outside_View,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String
              ("resources\outside_view_invisible_minimap_hexagon.png"),
            null, 0, 0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Minimap_Grass,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_grass_hexagon.png"), null,
            0, 0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Minimap_Mountain,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_mountain_hexagon.png"),
            null, 0, 0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Minimap_Water,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_water_hexagon.png"), null,
            0, 0, 0, 0, 0, 0));

      Images_List_Pkg.Include
        (P_Images, Minimap_Forest,
         new Type_Image'
           (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_forest_hexagon.png"),
            null, 0, 0, 0, 0, 0, 0));


   end Initialize;

   procedure Load_Image (P_Image : in out Type_Image_Access) is
      Error : Glib.Error.GError;

      use Glib.Error;
   begin

      Text_IO.Put_Line ("Load_Image:" & Ada.Strings.Unbounded.To_String (P_Image.all.Image_Name));
      Gdk.Pixbuf.Gdk_New_From_File
        (P_Image.all.Image_Data, Ada.Strings.Unbounded.To_String (P_Image.all.Image_Name), Error);

      if Error = null then
         P_Image.all.Image_Width  := Gdk.Pixbuf.Get_Width (P_Image.all.Image_Data);
         P_Image.all.Image_Height := Gdk.Pixbuf.Get_Height (P_Image.all.Image_Data);
         --
         Text_IO.Put_Line
           (" Width:" & P_Image.all.Image_Width'Img & " Height:" & P_Image.all.Image_Height'Img);
      else
         Text_IO.Put_Line ("Error: " & Glib.Error.Get_Message (Error));
         Glib.Error.Error_Free (Error);
         P_Image.all.Image_Width  := 72;
         P_Image.all.Image_Height := 72;

         P_Image.all.Image_Data :=
           Gdk.Pixbuf.Gdk_New
             (Bits_Per_Sample => 24, Width => P_Image.all.Image_Width,
              Height          => P_Image.all.Image_Height);

         Gdk.Pixbuf.Fill (P_Image.all.Image_Data, 16#0000FF00#);
      end if;

   end Load_Image;

   procedure Load_Images (P_Images_List : in out Images_List_Pkg.Map)
   is
      An_Element  : Tubastga_Window_Pkg.Images.Type_Image_Access;

      Trav_Images : Images_List_Pkg.Cursor;
   begin
      Trav_Images := Images_List_Pkg.First (P_Images_List);
      while Tubastga_Window_Pkg.Images.Images_List_Pkg.Has_Element (Trav_Images) loop
         An_Element := Tubastga_Window_Pkg.Images.Images_List_Pkg.Element (Trav_Images);

         Tubastga_Window_Pkg.Images.Load_Image (An_Element);
         Trav_Images := Images_List_Pkg.Next (Trav_Images);
      end loop;

   end Load_Images;

   function Get_Image (P_Images_List : in out Images_List_Pkg.Map;
      P_Image_Name                   : in     Type_Image_Names) return Type_Image_Access
   is
      Ret : Tubastga_Window_Pkg.Images.Type_Image_Access;
   begin
--      if Verbose then
--         Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Get_Image - enter P_Image_Name=" & P_Image_Name'Img);
--      end if;

      Ret :=
        Tubastga_Window_Pkg.Images.Images_List_Pkg.Element
          (Tubastga_Window_Pkg.Images.All_Images, P_Image_Name);

      --     if Verbose then
      --        Text_IO.Put_Line ("Tubastga_Window_Pkg.Images.Get_Image - exit");
      --     end if;

      return Ret; --A_Creature.all.Frame;
   end Get_Image;

   procedure Print_Image (P_Image : in Type_Image) is
   begin
      Text_IO.Put_Line ("    " & Ada.Strings.Unbounded.To_String (P_Image.Image_Name));
   end Print_Image;

   procedure Print_Images_List (P_Images_List : in Images_List_Pkg.Map) is
      Trav_Images : Images_List_Pkg.Cursor;

      An_Image : Type_Image_Access;
   begin
      if Verbose then
         Text_IO.Put_Line("Tubastga_Window_Pkg.Images.Print_Images_List - enter");
      end if;

      Text_IO.Put_Line("Images List:");
      Trav_Images := Images_List_Pkg.First (P_Images_List);
      while Images_List_Pkg.Has_Element (Trav_Images) loop
         An_Image := Images_List_Pkg.Element (Trav_Images);
         Text_IO.Put ("Image: " & Images_List_Pkg.Key (Trav_Images)'Img & " ");
         Print_Image (An_Image.all);

         Trav_Images := Images_List_Pkg.Next (Trav_Images);
      end loop;

      if Verbose then
         Text_IO.Put_Line("Tubastga_Window_Pkg.Images.Print_Images_List - exit");
      end if;
   end Print_Images_List;

end Tubastga_Window_Pkg.Images;
