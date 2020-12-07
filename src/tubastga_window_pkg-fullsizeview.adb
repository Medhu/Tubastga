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
with Tubastga_Window_Pkg.Images;


package body Tubastga_Window_Pkg.FullsizeView is
   Verbose : constant Boolean := False;

   Game_Area_Origo_X : constant Integer   := 50;
   Game_Area_Origo_Y : constant Integer   := 1050;
   Png_Width         : constant Glib.Gint := 72;
   Png_Height        : constant Glib.Gint := 72;

   use Glib;

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

   procedure Draw_Arrow
   is
   begin
      null;
--private static void drawArrow(int tipX, int tailX, int tipY, int tailY, Graphics2D g)
--{
--    int arrowLength = 7; //can be adjusted
--    int dx = tipX - tailX;
--    int dy = tipY - tailY;

--    double theta = Math.atan2(dy, dx);

--    double rad = Math.toRadians(35); //35 angle, can be adjusted
--    double x = tipX - arrowLength * Math.cos(theta + rad);
--    double y = tipY - arrowLength * Math.sin(theta + rad);

--    double phi2 = Math.toRadians(-35);//-35 angle, can be adjusted
--    double x2 = tipX - arrowLength * Math.cos(theta + phi2);
--    double y2 = tipY - arrowLength * Math.sin(theta + phi2);

--    int[] arrowYs = new int[3];
--    arrowYs[0] = tipY;
--    arrowYs[1] = (int) y;
--    arrowYs[2] = (int) y2;

--    int[] arrowXs = new int[3];
--    arrowXs[0] = tipX;
--    arrowXs[1] = (int) x;
--    arrowXs[2] = (int) x2;

--    g.fillPolygon(arrowXs, arrowYs, 3);
--}
   end Draw_Arrow;

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
              (Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images, Tubastga_Window_Pkg.Images.Chest).Image_Data,
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
              (Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images, Tubastga_Window_Pkg.Images.Wall1).Image_Data,
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
              (Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images, Tubastga_Window_Pkg.Images.Wall2).Image_Data,
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
              (Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images, Tubastga_Window_Pkg.Images.Wall3).Image_Data,
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
              (Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images, Tubastga_Window_Pkg.Images.Wall4).Image_Data,
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
              (Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images, Tubastga_Window_Pkg.Images.Wall5).Image_Data,
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
              (Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images, Tubastga_Window_Pkg.Images.Wall6).Image_Data,
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
      Landscape_Image : Tubastga_Window_Pkg.Images.Type_Image_Access;

      use Landscape;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Landscapes - enter");
      end if;

      Landscape_Image := Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images,
                                                              Tubastga_Window_Pkg.Images.Find_Landscape_Image(P_Landscape));
      Gdk.Pixbuf.Composite
        (Landscape_Image.all.Image_Data,
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
      Other_Image : Tubastga_Window_Pkg.Images.Type_Image_Access;

      use Landscape;
   begin
      if Verbose and False then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Invisible - enter");
      end if;

      x := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (P_Client_Map, P_Patch);
      y := Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (P_Client_Map, P_Patch);

      Other_Image := Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images,
                                                          Tubastga_Window_Pkg.Images.Find_Other_Image("invisible") );

      Gdk.Pixbuf.Composite
        (Other_Image.all.Image_Data,
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
      Player_Image : Tubastga_Window_Pkg.Images.Type_Image_Access;
      A_Piece      : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Players - enter");
      end if;

      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      if Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) then

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         Player_Image :=
           Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images,
                                              Tubastga_Window_Pkg.Images.Find_Player_Image(A_Piece.all.Player_Id));

         declare
            x, y : Glib.Gint;
            use Hexagon;
         begin
            x :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_X_From_AB (P_Client_Map, P_Patch);
            y :=
              Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Player_Y_From_AB (P_Client_Map, P_Patch);

            Gdk.Pixbuf.Composite
              (Player_Image.all.Image_Data,
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

      Piece_Image_Name : Tubastga_Window_Pkg.Images.Type_Image_Names;
      Piece_Image : Tubastga_Window_Pkg.Images.Type_Image_Access;
      A_Piece     : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Piece_No    : Integer;

      use Piece;
      use Player;
      use Tubastga_Window_Pkg.Images;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Houses - enter");
      end if;

      Piece_No    := 1;
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         Piece_Image_Name := Tubastga_Window_Pkg.Images.Find_House_Image(Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));
         Piece_Image := Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images,
                                              Piece_Image_Name);
         -- Now houses

         if Piece_Image_Name /= Tubastga_Window_Pkg.Images.None then
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
                 (Piece_Image.all.Image_Data,
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

            end;
         end if;

         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
         Piece_No    := Piece_No + 1;
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Houses - exit");
      end if;
   end Draw_Houses;

   procedure Draw_Pieces
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector)
   is
      Trav_Pieces : Landscape.Pieces_Here_List.Cursor;

      Piece_Image_Name : Tubastga_Window_Pkg.Images.Type_Image_Names;
      Piece_Image : Tubastga_Window_Pkg.Images.Type_Image_Access;
      A_Piece     : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      Piece_No    : Integer;

      use Piece;
      use Player;
      use Tubastga_Window_Pkg.Images;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.FullsizeView.Draw_Pieces - enter");
      end if;

      Piece_No    := 1;
      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop

         A_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Landscape.Pieces_Here_List.Element (Trav_Pieces));

         Piece_Image_Name := Tubastga_Window_Pkg.Images.Find_Piece_Image (Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));
         Piece_Image := Tubastga_Window_Pkg.Images.Get_Image(Tubastga_Window_Pkg.Images.All_Images,
                                                            Piece_Image_Name);

         if Piece_Image_Name /= Tubastga_Window_Pkg.Images.None then

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
                 (Piece_Image.all.Image_Data,
                  P_Fullsizeview,
                  Glib.Gint (x + Piece_Image.all.Dest_X),
                  Glib.Gint (y + Piece_Image.all.Dest_Y),
                  Piece_Image.all.Image_Width,--  + 150,
                  Piece_Image.all.Image_Height,-- + 150,
                  Glib.Gdouble (x + Piece_Image.all.Offset_X),
                  Glib.Gdouble (y + Piece_Image.all.Offset_Y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);

            end;
         end if;

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
      Other_Image : Tubastga_Window_Pkg.Images.Type_Image_Access;
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

            Other_Image := Tubastga_Window_Pkg.Images.Get_Image
              (Tubastga_Window_Pkg.Images.All_Images,
               Tubastga_Window_Pkg.Images.Find_Other_Image("selected_patch_LB"));

            Gdk.Pixbuf.Composite
              (Other_Image.all.Image_Data,
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

            Other_Image := Tubastga_Window_Pkg.Images.Get_Image
              (Tubastga_Window_Pkg.Images.All_Images,
               Tubastga_Window_Pkg.Images.Find_Other_Image("selected_patch_RB"));

            Gdk.Pixbuf.Composite
              (Other_Image.all.Image_Data,
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
