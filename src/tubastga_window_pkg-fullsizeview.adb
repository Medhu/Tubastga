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

with Tubastga_Window_Pkg;
with Piece.Client_Piece;
with Tubastga_Piece;
with Player;

package body Tubastga_Window_Pkg.FullsizeView is
   Game_Area_Origo_X : constant Integer := 50;
   Game_Area_Origo_Y : constant Integer := 1050;

   use Glib;

   type Type_P_Pos is record
      X, Y : Glib.Gint;
   end record;

   type Type_Piece_Pos is array (1 .. 7) of Type_P_Pos;

   Piece_Pos : Type_Piece_Pos :=
     ((20, 7), (31, 14), (31, 24), (20, 28), (10, 24), (9, 11), (20, 21) -- for the carrier
     );

   Player_Pos : Type_Piece_Pos :=
     ((15, 2), (23, 7), (23, 17), (15, 21), (7, 17), (7, 8), (15, 14) -- for the carrier
   );

   Select_Point : Type_Piece_Pos :=
     ((0, -9), (8, -4), (8, 4), (0, 9), (-8, 4), (-8, -4), (0, 0) -- for the carrier
   );

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
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch) +
        Glib.Gint (Player_Pos (P_Trav_Draw).X);
   end Get_All_Pix_Player_X_From_AB;

   function Get_All_Pix_Player_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch) +
        Glib.Gint (Player_Pos (P_Trav_Draw).Y);
   end Get_All_Pix_Player_Y_From_AB;

   function Get_All_Pix_Piece_X_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch) +
        Glib.Gint (Piece_Pos (P_Trav_Draw).X);
   end Get_All_Pix_Piece_X_From_AB;

   function Get_All_Pix_Piece_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint
   is
      use Glib;
   begin
      return Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch) +
        Glib.Gint (Piece_Pos (P_Trav_Draw).Y);
   end Get_All_Pix_Piece_Y_From_AB;

   function Selected_Piece_Position
     (P_Client_Map                       : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch                            : in Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview_X, P_Fullsizeview_Y : in Glib.Gint) return Integer
   is
      X, Y           : Glib.Gint;
      Dist_Min, Dist : Glib.Gint;
      Closest_Point  : Integer := 0;

      use Glib;
   begin
      Dist_Min := 1000;

      for Trav in Player_Pos'First .. Player_Pos'Last loop
         X := Glib.Gint (Float (Hexagon.Client_Map.Get_X_From_AB (P_Client_Map, P_Patch)) - 20.0);
         Y := Glib.Gint (Float (Hexagon.Client_Map.Get_Y_From_AB (P_Client_Map, P_Patch)) - 22.0);

         X := X + (Glib.Gint (Float (Player_Pos (Trav).X) * 1.5));
         Y := Y + (Glib.Gint (Float (Player_Pos (Trav).Y) * 1.5));

         Dist :=
           (X - P_Fullsizeview_X) * (X - P_Fullsizeview_X) +
           (Y - P_Fullsizeview_Y) * (Y - P_Fullsizeview_Y);

         if Dist < Dist_Min then
            Dist_Min      := Dist;
            Closest_Point := Trav;
         end if;
      end loop;

      return Closest_Point;
   end Selected_Piece_Position;

   procedure Draw_Effects
     (P_All_Images  : in     Tubastga_Window_Pkg.Type_Images;
      P_Pixbuf      : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Effect_List : in     Effect.Effect_List.Map)
   is
      Trav : Effect.Effect_List.Cursor;

      use Effect;
   begin
      Trav := Effect.Effect_List.First (P_Effect_List);
      while Effect.Effect_List.Has_Element (Trav) loop
         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Piece.Effect_Treasure then
            Gdk.Pixbuf.Composite
              (P_All_Images (Tubastga_Window_Pkg.Chest).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               50,
               44,
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
     (P_All_Images        : in     Tubastga_Window_Pkg.Type_Images;
      P_Pixbuf            : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Construction_List : in     Construction.Construction_List.Set)
   is
      Trav : Construction.Construction_List.Cursor;

      use Construction;
   begin
      Trav := Construction.Construction_List.First (P_Construction_List);
      while Construction.Construction_List.Has_Element (Trav) loop
         if Construction.Construction_List.Element (Trav) = Tubastga_Piece.Construction_Wall1 then

            Gdk.Pixbuf.Composite
              (P_All_Images (Tubastga_Window_Pkg.Wall1).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               50,
               44,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Construction.Construction_List.Element (Trav) = Tubastga_Piece.Construction_Wall2 then

            Gdk.Pixbuf.Composite
              (P_All_Images (Tubastga_Window_Pkg.Wall2).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               50,
               44,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Construction.Construction_List.Element (Trav) = Tubastga_Piece.Construction_Wall3 then

            Gdk.Pixbuf.Composite
              (P_All_Images (Tubastga_Window_Pkg.Wall3).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               50,
               44,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Construction.Construction_List.Element (Trav) = Tubastga_Piece.Construction_Wall4 then

            Gdk.Pixbuf.Composite
              (P_All_Images (Tubastga_Window_Pkg.Wall4).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               50,
               44,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Construction.Construction_List.Element (Trav) = Tubastga_Piece.Construction_Wall5 then

            Gdk.Pixbuf.Composite
              (P_All_Images (Tubastga_Window_Pkg.Wall5).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               50,
               44,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         if Construction.Construction_List.Element (Trav) = Tubastga_Piece.Construction_Wall6 then

            Gdk.Pixbuf.Composite
              (P_All_Images (Tubastga_Window_Pkg.Wall6).Image_Data,
               P_Pixbuf,
               Glib.Gint (0),
               Glib.Gint (0),
               50,
               44,
               Glib.Gdouble (0),
               Glib.Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;
         Trav := Construction.Construction_List.Next (Trav);
      end loop;
   end Draw_Constructions;

   procedure Draw_Landscapes
     (P_All_Images : in     Tubastga_Window_Pkg.Type_Images;
      P_Pixbuf     : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Landscape  : in     Landscape.Type_Landscape)
   is
      Landscape_Image : Tubastga_Window_Pkg.Type_Image_Names;
      use Landscape;
   begin

      if P_Landscape = Tubastga_Piece.Landscape_Grass then
         Landscape_Image := Tubastga_Window_Pkg.Grass;
      elsif P_Landscape = Tubastga_Piece.Landscape_Forest then
         Landscape_Image := Tubastga_Window_Pkg.Forest;
      elsif P_Landscape = Tubastga_Piece.Landscape_Mountain then
         Landscape_Image := Tubastga_Window_Pkg.Mountain;
      elsif P_Landscape = Tubastga_Piece.Landscape_Water then
         Landscape_Image := Tubastga_Window_Pkg.Water;
      end if;

      Gdk.Pixbuf.Composite
        (P_All_Images (Landscape_Image).Image_Data,
         P_Pixbuf,
         Glib.Gint (0),
         Glib.Gint (0),
         50,
         44,
         Glib.Gdouble (0),
         Glib.Gdouble (0),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);
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
      x := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch);
      y := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch);

      Gdk.Pixbuf.Composite
        (P_All_Landscape_On_Patch,
         P_Fullsizeview,
         Glib.Gint (x),
         Glib.Gint (y),
         50,
         44,
         Glib.Gdouble (x),
         Glib.Gdouble (y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);
      --
      Gdk.Pixbuf.Composite
        (P_All_Constructions_On_Patch,
         P_Fullsizeview,
         Glib.Gint (x),
         Glib.Gint (y),
         50,
         44,
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
         50,
         44,
         Glib.Gdouble (x),
         Glib.Gdouble (y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);
   end Draw_All_Patch;

   procedure Draw_Invisible
     (P_Client_Map : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in     Hexagon.Client_Map.Type_Client_Patch;
      P_All_Images : in     Tubastga_Window_Pkg.Type_Images;
      P_Pixbuf     : in out Gdk.Pixbuf.Gdk_Pixbuf)
   is
      x, y : Glib.Gint;

      use Landscape;
   begin
      x := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch);
      y := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch);

      Gdk.Pixbuf.Composite
        (P_All_Images (Invisible).Image_Data,
         P_Pixbuf,
         Glib.Gint (x),
         Glib.Gint (y),
         50,
         44,
         Glib.Gdouble (x),
         Glib.Gdouble (y),
         1.0,
         1.0,
         Gdk.Pixbuf.Interp_Nearest,
         255);
   end Draw_Invisible;

   procedure Draw_Players
     (P_All_Images   : in     Tubastga_Window_Pkg.Type_Images;
      P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector)
   is
      Trav_Pieces : Landscape.Pieces_Here_List.Cursor;

      Player_Image : Tubastga_Window_Pkg.Type_Image_Names;

      A_Piece  : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Piece_No : Integer;

      use Piece;
      use Player;
   begin

      Piece_No    := 1;
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop
         Player_Image := Tubastga_Window_Pkg.None;

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         -- Now houses
         if A_Piece.Type_Of_Piece = Tubastga_Piece.Farm_House then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Tower_House then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Lumberjack_House then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Stonecutter_House then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         end if;

         if A_Piece.Type_Of_Piece = Tubastga_Piece.Sentry_Piece then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Knight_Piece then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Bowman_Piece then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Ship_Piece then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Carrier_Piece then
            if A_Piece.Player_Id = 1 then
               Player_Image := Tubastga_Window_Pkg.Red;
            elsif A_Piece.Player_Id = 2 then
               Player_Image := Tubastga_Window_Pkg.Green;
            else
               Player_Image := Tubastga_Window_Pkg.Blue;
            end if;
         end if;

         declare
            x, y : Glib.Gint;
            use Hexagon;
         begin
            x :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_X_From_AB
                (P_Client_Map,
                 P_Patch,
                 Piece_No);
            y :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_Y_From_AB
                (P_Client_Map,
                 P_Patch,
                 Piece_No);

            Gdk.Pixbuf.Composite
              (P_All_Images (Player_Image).Image_Data,
               P_Fullsizeview,
               Glib.Gint (x),
               Glib.Gint (y),
               50,
               44,
               Glib.Gdouble (x),
               Glib.Gdouble (y),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end;

         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
         Piece_No    := Piece_No + 1;
      end loop;

   end Draw_Players;

   procedure Draw_Players_Selections
     (P_All_Images   : in     Tubastga_Window_Pkg.Type_Images;
      P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector;
      P_LB_Selected_Piece,
      P_RB_Selected_Piece : in Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Vector)
   is
      Trav_Pieces         : Landscape.Pieces_Here_List.Cursor;
      Trav_Selected_Piece : Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Cursor;

      Player_Image : Tubastga_Window_Pkg.Type_Image_Names;

      A_Piece  : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Piece_No : Integer;

      use Piece;
      use Player;
   begin

      Piece_No    := 1;
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         Player_Image        := Tubastga_Window_Pkg.None;
         Trav_Selected_Piece :=
           Tubastga_Window_Pkg.Lists.Piece_List_Pkg.First (P_LB_Selected_Piece);
         while Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Has_Element
             (Trav_Selected_Piece) and
           Player_Image = None
         loop

            if A_Piece.all.Id =
              Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Element (Trav_Selected_Piece)
            then
               Player_Image := Selected_Piece;
            end if;

            if Player_Image /= None then
               declare
                  x, y : Glib.Gint;
                  use Hexagon;
               begin
                  x :=
                    Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_X_From_AB
                      (P_Client_Map,
                       P_Patch,
                       Piece_No);
                  y :=
                    Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_Y_From_AB
                      (P_Client_Map,
                       P_Patch,
                       Piece_No);

                  Gdk.Pixbuf.Composite
                    (P_All_Images (Player_Image).Image_Data,
                     P_Fullsizeview,
                     Glib.Gint (x),
                     Glib.Gint (y),
                     50,
                     44,
                     Glib.Gdouble (x),
                     Glib.Gdouble (y),
                     1.0,
                     1.0,
                     Gdk.Pixbuf.Interp_Nearest,
                     255);
               end;
            end if;

            Trav_Selected_Piece :=
              Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Next (Trav_Selected_Piece);
         end loop;

         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
         Piece_No    := Piece_No + 1;
      end loop;

   end Draw_Players_Selections;

   procedure Draw_Pieces
     (P_All_Images   : in     Tubastga_Window_Pkg.Type_Images;
      P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector)
   is
      Trav_Pieces : Landscape.Pieces_Here_List.Cursor;

      Piece_Image : Tubastga_Window_Pkg.Type_Image_Names;
      A_Piece     : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Piece_No    : Integer;

      use Piece;
      use Player;
   begin

      Piece_No    := 1;
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop
         Piece_Image := Tubastga_Window_Pkg.None;

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         -- Now houses
         if A_Piece.Type_Of_Piece = Tubastga_Piece.Farm_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Farm_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Farm_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Farm_p3;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Tower_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Tower_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Tower_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Tower_p3;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Lumberjack_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Lumberjack_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Lumberjack_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Lumberjack_p3;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Stonecutter_House then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Stonecutter_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Stonecutter_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Stonecutter_p3;
            end if;
         end if;

         if A_Piece.Type_Of_Piece = Tubastga_Piece.Sentry_Piece then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Sentry_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Sentry_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Sentry_p3;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Knight_Piece then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Knight_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Knight_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Knight_p3;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Bowman_Piece then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Bowman_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Bowman_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Bowman_p3;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Ship_Piece then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Ship_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Ship_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Ship_p3;
            end if;
         elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Carrier_Piece then
            if A_Piece.Player_Id = 1 then
               Piece_Image := Tubastga_Window_Pkg.Carrier_p1;
            elsif A_Piece.Player_Id = 2 then
               Piece_Image := Tubastga_Window_Pkg.Carrier_p2;
            else
               Piece_Image := Tubastga_Window_Pkg.Carrier_p3;
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

            Gdk.Pixbuf.Composite
              (P_All_Images (Piece_Image).Image_Data,
               P_Fullsizeview,
               Glib.Gint (x),
               Glib.Gint (y),
               50,
               44,
               Glib.Gdouble (x),
               Glib.Gdouble (y),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);

         end;
         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
         Piece_No    := Piece_No + 1;
      end loop;

   end Draw_Pieces;

   procedure Draw_Patch_Selections
     (P_All_Images : in     Tubastga_Window_Pkg.Type_Images;
      P_Pixbuf     : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Patch      : in     Hexagon.Client_Map.Type_Client_Patch;
      P_LB_Selected_Pos,
      P_RB_Selected_Pos : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Vector)
   is
      Trav  : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Cursor;
      A_Pos : Hexagon.Type_Hexagon_Position;

      use Hexagon;
   begin

      Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.First (P_LB_Selected_Pos);
      while Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Has_Element (Trav) loop
         A_Pos := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Element (Trav);

         if A_Pos = P_Patch.Pos then

            Gdk.Pixbuf.Composite
              (P_All_Images (Selected_Patch_LB).Image_Data,
               P_Pixbuf,
               Gint (0),
               Gint (0),
               50,
               44,
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
              (P_All_Images (Selected_Patch_RB).Image_Data,
               P_Pixbuf,
               Gint (0),
               Gint (0),
               50,
               44,
               Gdouble (0),
               Gdouble (0),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end if;

         Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Next (Trav);
      end loop;

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

   function Selected_Piece
     (P_Client_Map                       : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Fullsizeview_X, P_Fullsizeview_Y :    Glib.Gdouble) return Integer
   is
      X, Y, Patch_X, Patch_Y : Glib.Gint;
      Dist_Min, Dist         : Glib.Gint;
      Closest_Point          : Integer := 0;

      A_Patch          : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Piece_X, Piece_Y : Glib.Gint;

      use Glib;
   begin
      Dist_Min := 1000;

      A_Patch :=
        Selected_Patch
          (P_Client_Map,
           Glib.Gdouble (P_Fullsizeview_X),
           Glib.Gdouble (P_Fullsizeview_Y));

      X := Glib.Gint (P_Fullsizeview_X) - Glib.Gint (55);
      Y := Glib.Gint (1050) - Glib.Gint (P_Fullsizeview_Y);

      for Trav in Select_Point'First .. Select_Point'Last loop
         Patch_X := Glib.Gint (Hexagon.Client_Map.Get_X_From_AB (P_Client_Map, A_Patch.all)); --
         Patch_Y := Glib.Gint (Hexagon.Client_Map.Get_Y_From_AB (P_Client_Map, A_Patch.all));-- -

         Piece_X := Patch_X + Glib.Gint (Float (Select_Point (Trav).X));
         Piece_Y := Patch_Y - Glib.Gint (Float (Select_Point (Trav).Y));

         Dist := (X - Piece_X) * (X - Piece_X) + (Y - Piece_Y) * (Y - Piece_Y);

         if Dist < Dist_Min then
            Dist_Min      := Dist;
            Closest_Point := Trav;
         end if;
      end loop;

      return Closest_Point;
   end Selected_Piece;

end Tubastga_Window_Pkg.FullsizeView;
