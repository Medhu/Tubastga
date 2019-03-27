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

with Hexagon.Client_Map;
with Glib;
with Gdk.Pixbuf;
with Tubastga_Window_Pkg;
with Effect;
--with Construction;
with Landscape;
with Tubastga_Window_Pkg.Lists;

package Tubastga_Window_Pkg.FullsizeView is
   Patch_Zoom_Width  : constant Glib.Gint := Glib.Gint (300);
   Patch_Zoom_Height : constant Glib.Gint := Glib.Gint (150);

   function Get_All_Pix_Patch_X_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint;

   function Get_All_Pix_Patch_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint;

   function Get_All_Pix_Player_X_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint;

   function Get_All_Pix_Player_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Glib.Gint;

   function Get_All_Pix_Piece_X_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint;

   function Get_All_Pix_Piece_Y_From_AB
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
      P_Trav_Draw  : in Natural) return Glib.Gint;

   procedure Draw_Effects
     (P_Pixbuf      : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Effect_List : in     Effect.Effect_List.Map);

   procedure Draw_Constructions
     (P_Pixbuf            : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Construction_List : in     Effect.Effect_List.Map);

   procedure Draw_Landscapes
     (P_Pixbuf    : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Landscape : in     Landscape.Type_Landscape);

   procedure Draw_All_Patch
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_All_Landscape_On_Patch,
      P_All_Constructions_On_Patch,
      P_All_Effects_On_Patch : in out Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Draw_Players
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector);

   procedure Draw_Houses
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector);

   function Find_Piece_Image
     (P_Piece : in Tubastga_Window_Pkg.Type_Client_Piece)
      return Tubastga_UI_Resources.Type_Image_Names;

   procedure Draw_Pieces
     (P_Client_Map   : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Fullsizeview : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Pieces_Here  : in     Landscape.Pieces_Here_List.Vector);

   procedure Draw_Invisible
     (P_Client_Map : in     Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in     Hexagon.Client_Map.Type_Client_Patch;
      P_Pixbuf     : in out Gdk.Pixbuf.Gdk_Pixbuf);

   procedure Draw_Patch_Selections
     (P_Pixbuf                             : in out Gdk.Pixbuf.Gdk_Pixbuf;
      P_Patch                              : in     Hexagon.Client_Map.Type_Client_Patch;
      P_LB_Selected_Pos, P_RB_Selected_Pos :        Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Vector);

   function Selected_Patch
     (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
      P_Fullsizeview_X,
      P_Fullsizeview_Y : Glib.Gdouble)
      return Hexagon.Client_Map.Type_Client_Patch_Adress;

end Tubastga_Window_Pkg.FullsizeView;
