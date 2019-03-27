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

with Tubastga_Window_Pkg;
with Piece.Client_Piece;
with Tubastga_Game;
with Player;
with Text_IO;

package body Tubastga_Window_Pkg.FullsizeView is
   Verbose : constant Boolean := False;

   Game_Area_Origo_X : constant Integer   := 50;
   Game_Area_Origo_Y : constant Integer   := 1050;
   Png_Width         : constant Glib.Gint := 72;
   Png_Height        : constant Glib.Gint := 72;

   to_do : exception;
   use Glib;
   use Tubastga_UI_Resources;

   function Get_All_Pix_Patch_X_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint
   is
   begin
      return Glib.Gint
          (Game_Area_Origo_X + Hexagon.Client_Map.Get_X_From_AB (P_Client_Map, P_Patch) - 20);
   end Get_All_Pix_Patch_X_From_AB;

   function Get_All_Pix_Patch_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint
   is
   begin
      return Glib.Gint
          (Game_Area_Origo_Y - Hexagon.Client_Map.Get_Y_From_AB (P_Client_Map, P_Patch) - 22);
   end Get_All_Pix_Patch_Y_From_AB;

   function Get_All_Pix_Player_X_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch) + 15;
   end Get_All_Pix_Player_X_From_AB;

   function Get_All_Pix_Player_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch) + 2;
   end Get_All_Pix_Player_Y_From_AB;

   function Get_All_Pix_Piece_X_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch);-- -13;
   end Get_All_Pix_Piece_X_From_AB;

   function Get_All_Pix_Piece_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch); -- - 25;
   end Get_All_Pix_Piece_Y_From_AB;

   procedure Draw_Effects
     (P_Pixbuf      : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Effect_List : in     Effect.Effect_List.Map)
   is
      Trav : Effect.Effect_List.Cursor;

      use Effect;
   begin
      Trav := Effect.Effect_List.First (P_Effect_List);
      while Effect.Effect_List.Has_Element (Trav) loop
         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Game.Effect_Treasure then
            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Chest).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               Png_Width,
               Png_Height,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;
         Trav := Effect.Effect_List.Next (Trav);
      end loop;
   end Draw_Effects;

   procedure Draw_Constructions
     (P_Pixbuf            : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Construction_List : in     Effect.Effect_List.Map)
   is
      Trav : Effect.Effect_List.Cursor;

      use Effect;
   begin
      Trav := Effect.Effect_List.First (P_Construction_List);
      while Effect.Effect_List.Has_Element (Trav) loop
         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Game.Effect_Wall1 then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Wall1).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               Png_Width,
               Png_Height,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Game.Effect_Wall2 then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Wall2).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               Png_Width,
               Png_Height,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Game.Effect_Wall3 then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Wall3).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               Png_Width,
               Png_Height,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Game.Effect_Wall4 then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Wall4).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               Png_Width,
               Png_Height,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Game.Effect_Wall5 then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Wall5).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               Png_Width,
               Png_Height,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Game.Effect_Wall6 then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Wall6).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               Png_Width,
               Png_Height,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;
         Trav := Effect.Effect_List.Next (Trav);
      end loop;
   end Draw_Constructions;

   procedure Draw_Landscapes
     (P_Pixbuf    : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Landscape : in     Landscape.Type_Landscape)
   is
      Landscape_Image : Tubastga_UI_Resources.Type_Image_Names;

      use Landscape;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Landscapes - enter");
      end if;

      if P_Landscape = Tubastga_Game.Landscape_Grass then
         Landscape_Image := Tubastga_UI_Resources.Green;
      elsif P_Landscape = Tubastga_Game.Landscape_Forest then
         Landscape_Image := Tubastga_UI_Resources.Forested_Deciduous_Summer_Hills_Tile;
      elsif P_Landscape = Tubastga_Game.Landscape_Mountain then
         Landscape_Image := Tubastga_UI_Resources.Hills_Variation;
      elsif P_Landscape = Tubastga_Game.Landscape_Water then
         Landscape_Image := Tubastga_UI_Resources.Water;
      end if;

      Gdk.Pixbuf.Composite
        (Tubastga_UI_Resources.All_Images (Landscape_Image).Image_Data,
         P_Pixbuf,
         Glib.Gint (0),
         Glib.Gint (0),
         Png_Width,
         Png_Height,
         Glib.Gdouble (0),
         Glib.Gdouble (0),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Landscapes - exit");
      end if;
   end Draw_Landscapes;

   procedure Draw_All_Patch
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_All_Landscape_On_Patch,
      P_All_Constructions_On_Patch,
      P_All_Effects_On_Patch : in out Gdk.Pixbuf.Gdk_Pixbuf)

   is
      x, y : Glib.Gint;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_All_Patch - enter");
      end if;

      x := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch);
      y := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch);

      Gdk.Pixbuf.Composite
        (P_All_Landscape_On_Patch,
         P_Fullsizeview,
         Glib.Gint (x),
         Glib.Gint (y),
         Png_Width,
         Png_Height,
         Glib.Gdouble (x),
         Glib.Gdouble (y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);

      Gdk.Pixbuf.Composite
        (P_All_Constructions_On_Patch,
         P_Fullsizeview,
         Glib.Gint (x),
         Glib.Gint (y),
         Png_Width,
         Png_Height,
         Glib.Gdouble (x),
         Glib.Gdouble (y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);

      Gdk.Pixbuf.Composite
        (P_All_Effects_On_Patch,
         P_Fullsizeview,
         Glib.Gint (x),
         Glib.Gint (y),
         Png_Width,
         Png_Height,
         Glib.Gdouble (x),
         Glib.Gdouble (y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_All_Patch - exit");
      end if;
   end Draw_All_Patch;

   procedure Draw_Invisible
     (P_Client_Map : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Pixbuf     : in out Gdk.Pixbuf.Gdk_Pixbuf)
   is
      x, y : Glib.Gint;

      use Landscape;
   begin
      if Verbose and False then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Invisible - enter");
      end if;

      x := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch);
      y := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch);

      Gdk.Pixbuf.Composite
        (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Invisible).Image_Data,
         P_Pixbuf,
         Glib.Gint (x),
         Glib.Gint (y),
         Png_Width,
         Png_Height,
         Glib.Gdouble (x),
         Glib.Gdouble (y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);

      if Verbose and False then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Invisible - exit");
      end if;
   end Draw_Invisible;

   procedure Draw_Players
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector)
   is
      Trav_Pieces  : Landscape.Pieces_Here_List.Cursor;
      Player_Image : Tubastga_UI_Resources.Type_Image_Names;
      A_Piece      : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Players - enter");
      end if;

      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      if Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) then
         Player_Image := Tubastga_UI_Resources.None;

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         if A_Piece.Player_Id = 1 then
            Player_Image := Tubastga_UI_Resources.Player_1;
         elsif A_Piece.Player_Id = 2 then
            Player_Image := Tubastga_UI_Resources.Player_2;
         else
            Player_Image := Tubastga_UI_Resources.Player_3;
         end if;

         declare
            x, y : Glib.Gint;
            use Hexagon;
         begin
            x :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_X_From_AB (P_Client_Map, P_Patch);
            y :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_Y_From_AB (P_Client_Map, P_Patch);

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Player_Image).Image_Data,
               P_Fullsizeview,
               Glib.Gint (x),
               Glib.Gint (y),
               Png_Width,
               Png_Height,
               Glib.Gdouble (x),
               Glib.Gdouble (y),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end;

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Players - exit");
      end if;
   end Draw_Players;

   procedure Draw_Houses
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector)
   is
      Trav_Pieces : Landscape.Pieces_Here_List.Cursor;

      Piece_Image : Tubastga_UI_Resources.Type_Image_Names;
      A_Piece     : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Piece_No    : Integer;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Houses - enter");
      end if;

      Piece_No    := 1;
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop
         Piece_Image := Tubastga_UI_Resources.None;

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         -- Now houses
         if A_Piece.Type_Of_Piece = Tubastga_Game.Farm_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_UI_Resources.Boat;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_UI_Resources.Boat;
            else
               Piece_Image := Tubastga_UI_Resources.Boat;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Game.Tower_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_UI_Resources.Towerhouse;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_UI_Resources.Towerhouse;
            else
               Piece_Image := Tubastga_UI_Resources.Towerhouse;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Game.Lumberjack_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_UI_Resources.Lumberjack;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_UI_Resources.Lumberjack;
            else
               Piece_Image := Tubastga_UI_Resources.Lumberjack;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Game.Stonecutter_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_UI_Resources.Stonecutter;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_UI_Resources.Stonecutter;
            else
               Piece_Image := Tubastga_UI_Resources.Stonecutter;
            end if;
         end if;

         declare
            x, y : Glib.Gint;
            use Hexagon;
         begin
            x :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Piece_X_From_AB
                (P_Client_Map,
                 P_Patch,
                 Piece_No);
            y :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Piece_Y_From_AB
                (P_Client_Map,
                 P_Patch,
                 Piece_No);

            if Piece_Image /= Tubastga_UI_Resources.None then
               Gdk.Pixbuf.Composite
                 (Tubastga_UI_Resources.All_Images (Piece_Image).Image_Data,
                  P_Fullsizeview,
                  Glib.Gint (x),
                  Glib.Gint (y),
                  Png_Width,--  + 150,
                  Png_Height,-- + 150,
                  Glib.Gdouble (x),
                  Glib.Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);
            end if;

         end;
         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
         Piece_No    := Piece_No + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Houses - exit");
      end if;
   end Draw_Houses;

   function Find_Piece_Image
     (P_Piece : in Tubastga_Window_Pkg.Type_Client_Piece)
      return Tubastga_UI_Resources.Type_Image_Names
   is
      Piece_Image : Tubastga_UI_Resources.Type_Image_Names;

      use Piece;
   begin
      Piece_Image := Tubastga_UI_Resources.None;

      if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
         Piece_Image := Tubastga_UI_Resources.Fighter;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
         Piece_Image := Tubastga_UI_Resources.Rider;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
         Piece_Image := Tubastga_UI_Resources.Archer;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Ship_Piece then
         Piece_Image := Tubastga_UI_Resources.Boat;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
         Piece_Image := Tubastga_UI_Resources.Boat;
      end if;

      return Piece_Image;
   end Find_Piece_Image;

   procedure Draw_Pieces
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector)
   is
      Trav_Pieces : Landscape.Pieces_Here_List.Cursor;

      Piece_Image : Tubastga_UI_Resources.Type_Image_Names;
      A_Piece     : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Piece_No    : Integer;

      use Piece;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Pieces - enter");
      end if;

      Piece_No    := 1;
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         Piece_Image := Find_Piece_Image (Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));

         declare
            x, y : Glib.Gint;
            use Hexagon;
         begin
            x :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Piece_X_From_AB
                (P_Client_Map,
                 P_Patch,
                 Piece_No);
            y :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Piece_Y_From_AB
                (P_Client_Map,
                 P_Patch,
                 Piece_No);

            if Piece_Image /= Tubastga_UI_Resources.None then
               Gdk.Pixbuf.Composite
                 (Tubastga_UI_Resources.All_Images (Piece_Image).Image_Data,
                  P_Fullsizeview,
                  Glib.Gint (x),
                  Glib.Gint (y),
                  Png_Width,--  + 150,
                  Png_Height,-- + 150,
                  Glib.Gdouble (x),
                  Glib.Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);
            end if;

         end;
         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
         Piece_No    := Piece_No + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Pieces - exit");
      end if;
   end Draw_Pieces;

   procedure Draw_Patch_Selections
     (P_Pixbuf                             : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Patch                              : in     Hexagon.Client_Map.Type_Client_Patch;
      P_LB_Selected_Pos, P_RB_Selected_Pos :        Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Vector)
   is
      Trav  : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Cursor;
      A_Pos : Hexagon.Type_Hexagon_Position;

      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Patch_Selections - enter");
      end if;

      Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.First (P_LB_Selected_Pos);
      while Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Has_Element (Trav) loop
         A_Pos := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Element (Trav);

         if A_Pos = P_Patch.Pos then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Selected_Patch_LB)
                 .Image_Data,
               P_Pixbuf,
               Gint (0),
               Gint (0),
               Png_Width,
               Png_Height,
               Gdouble (0),
               Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Next (Trav);
      end loop;

      Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.First (P_RB_Selected_Pos);
      while Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Has_Element (Trav) loop
         A_Pos := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Element (Trav);

         if A_Pos = P_Patch.Pos then

            Gdk.Pixbuf.Composite
              (Tubastga_UI_Resources.All_Images (Tubastga_UI_Resources.Selected_Patch_RB)
                 .Image_Data,
               P_Pixbuf,
               Gint (0),
               Gint (0),
               Png_Width,
               Png_Height,
               Gdouble (0),
               Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Next (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Patch_Selections - exit");
      end if;
   end Draw_Patch_Selections;

   function Selected_Patch
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Fullsizeview_X,
      P_Fullsizeview_Y : Glib.Gdouble)
      return Hexagon.Client_Map.Type_Client_Patch_Adress
   is
      X, Y : Integer;
   begin
      X := Integer (P_Fullsizeview_X) - 55;
      Y := 1050 - Integer (P_Fullsizeview_Y);

      return Hexagon.Client_Map.Get_Patch_Adress_From_XY (P_Client_Map, X, Y);
   end Selected_Patch;

end Tubastga_Window_Pkg.FullsizeView;
