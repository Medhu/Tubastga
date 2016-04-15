--
--
--      Tubastga Game - A turn based strategy game.
--      Copyright (C) 2015  Frank J Jorgensen
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

with System.RPC;
with System;     use System;
with Glib;       use Glib;
with Gdk.Event;  use Gdk.Event;
with Gdk.Types;  use Gdk.Types;
with Gtk.Enums;  use Gtk.Enums;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Label;  use Gtk.Label;
with Text_IO;
with Gdk.Pixbuf;
with Glib.Error;
use type Glib.Error.GError;
with Gdk.Pixbuf;
with Gdk.Window;        use Gdk.Window;
with Tubastga_UI_Aux;
with Gtk.Main;          use Gtk.Main;
with Tubastga_Window_Pkg.Sounds;
with Cairo;
with Gdk.Cairo;
with Ada.Strings.Fixed;
with Landscape;
with Hexagon.Client_Map;
with Hexagon.Area;
with Piece;
with Piece.Client_Piece;
with Tubastga_Piece;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;
with Utilities;
with Gtk.Text_Buffer;   use Gtk.Text_Buffer;
with Gtk.Text_Iter;     use Gtk.Text_Iter;
with Player;
with Status;
with Hexagon.Area.Client_Area;
with Observation;
with Effect;
with Client.Server_Adm;
with GNAT.Calendar.Time_IO;
with Ada.Calendar;
with Ada.Real_Time;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Construction;
with Ada.Containers.Ordered_Maps;
with Goods;
with Tubastga_Window_Pkg.Effects;
with Action;
with Server;

package body Tubastga_Window_Pkg.Callbacks is
   Verbose : constant Boolean := True;

   type Type_Scroll_Direction is (Up, Down, Left, Right);

   type Type_Anim_State is
     (Anim_Idle,
      Anim_Show_Frame_Pause,
      Anim_Get_Pieces_Report,
      Anim_Get_Pieces_Report_Pause,
      Anim_Show_Frame);

   type Type_Selected_Area_Cycle is (Reachable, Attackable);
   Selected_Area_Cycle : Type_Selected_Area_Cycle := Reachable;

   Anim_State : Type_Anim_State := Anim_Idle;
   Frame_Cursor : Observation.Frames.Piece_Visibility_Frames.Cursor;
   Anim_Show_Frame_Pause_Counter, Anim_Get_Pieces_Report_Pause_Counter : Integer;

   Max_Show_Frame_Pause_Counter        : constant Integer := 10;
   Max_Get_Pieces_Report_Pause_Counter : constant Integer := 30;
   use Gtk.Arguments;

   Draw : Cairo.Cairo_Context;

   Left_Mouse_Button  : constant Guint := 1;
   Right_Mouse_Button : constant Guint := 3;

   Max_Activity_Text : constant Integer := 50;

   Game_Area_Origo_X : constant Integer := 50;
   Game_Area_Origo_Y : constant Integer := 1050;
   Map_Scale         : Float            := 0.50;

   Minimap_Origo_Y : constant Integer  := 350;
   Minimap_Scale   : constant Positive := 30;

   Button_Event : Boolean := False;

   Pieces_Menu_Y : constant Integer := 850;

   type Type_Draw_Piece_Action is (None, Selected, Target);
   type Type_GUI_Piece is record
      Piece_Index_In_Patch : Positive;
      Draw_Action          : Type_Draw_Piece_Action;
   end record;

   package Pieces_GUI_List is new Ada.Containers.Ordered_Maps
     (Piece.Type_Piece_Id,
      Type_GUI_Piece,
      Piece."<");

   function Get_Selected_Piece
     (P_Pieces_GUI_List : in Pieces_GUI_List.Map) return Piece.Type_Piece_Id
   is
      Trav    : Pieces_GUI_List.Cursor;
      A_Piece : Type_GUI_Piece;
      A_Key   : Piece.Type_Piece_Id;
      Ret     : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
   begin
      Trav := Pieces_GUI_List.First (P_Pieces_GUI_List);
      while Pieces_GUI_List.Has_Element (Trav) loop
         A_Piece := Pieces_GUI_List.Element (Trav);
         A_Key   := Pieces_GUI_List.Key (Trav);
         if A_Piece.Draw_Action = Selected then
            Ret := A_Key;
         end if;
         Trav := Pieces_GUI_List.Next (Trav);
      end loop;

      return Ret;
   end Get_Selected_Piece;

   Pieces_GUI_Positions            : Pieces_GUI_List.Map;
   Trav_TAB_Pieces                 : Landscape.Pieces_Here_List.Cursor;
   Player_Pieces_Visibility_Frames : Observation.Frames.Piece_Visibility_Frames.Vector;
   System_Messages                 : Observation.Activity.Activity_Report.Vector;

   Current_Piece_Id   : Integer := 0;
   Current_TAB_Cursor : Tubastga_UI_Aux.TAB_For_Pieces_List.Cursor;

   Button_Pressed_Patch, Space_Pressed_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;
   TAB_Selected_Patch, Mouse_Hover_Patch     : Hexagon.Client_Map.Type_Client_Patch_Adress := null;

   A_Client_Map : Hexagon.Client_Map.Type_Client_Map_Info;

   Button_Pressed_X, Button_Pressed_Y, Space_Pressed_X, Space_Pressed_Y : Integer;
   Mouse_X, Mouse_Y, tmp_X, tmp_Y                                       : Integer;

   type Type_Player_Name_List is array (1 .. 3) of Utilities.RemoteString.Type_String;
   Player_Name_List : Type_Player_Name_List;

   Iter                                                   : Gtk_Text_Iter;
   The_Window                                             : Wnd_Main_Access;
   All_Pix, Scale_Pix, All_Minimap_Pix, Scale_Minimap_Pix : Gdk.Pixbuf.Gdk_Pixbuf;
   All_Constructions_On_Patch,
   All_Landscape_On_Patch,
   All_Surprise_On_Patch,
   All_Selections_On_Patch : Gdk.Pixbuf.Gdk_Pixbuf;

   Current_Player_Id             : Player.Type_Player_Id := 1;
   Me_Player_Id                  : Player.Type_Player_Id := 0;
   Now_Countdown, Last_Countdown : Integer               := -1;
   Game_State                    : Status.Type_Game_Status;
   --

   Last_Updates_Summary, Now : Ada.Real_Time.Time;

   type Type_P_Pos is record
      X, Y       : Gint;
      Image_Here : Type_Image_Names;
   end record;

   type Type_Piece_Pos is array (1 .. 7) of Type_P_Pos;

   Piece_Pos : Type_Piece_Pos :=
     ((20, 7, None),
      (31, 14, None),
      (31, 24, None),
      (20, 28, None),
      (10, 24, None),
      (9, 11, None),
      (20, 21, None) -- for the carrier
      );

   Player_Pos : Type_Piece_Pos :=
     ((15, 2, None),
      (23, 7, None),
      (23, 17, None),
      (15, 21, None),
      (7, 17, None),
      (7, 8, None),
      (15, 14, None) -- for the carrier
      );

   procedure Reset_Pieces_GUI_List is
      An_Element : Type_GUI_Piece;
      A_Key      : Piece.Type_Piece_Id;
      Trav       : Pieces_GUI_List.Cursor;
   begin
      Trav := Pieces_GUI_List.First (Pieces_GUI_Positions);
      while Pieces_GUI_List.Has_Element (Trav) loop
         An_Element             := Pieces_GUI_List.Element (Trav);
         A_Key                  := Pieces_GUI_List.Key (Trav);
         An_Element.Draw_Action := None;

         Pieces_GUI_List.Include (Pieces_GUI_Positions, A_Key, An_Element);

         Trav := Pieces_GUI_List.Next (Trav);
      end loop;
   end Reset_Pieces_GUI_List;

   function Is_Treasure_Here (P_Patch : in Hexagon.Client_Map.Type_Client_Patch) return Boolean is
      Trav : Effect.Effect_List.Cursor;

      Found : Boolean;

      use Effect;
      use Hexagon;
   begin
      Found := False;

      Trav := Effect.Effect_List.First (P_Patch.Effects_Here);
      while Effect.Effect_List.Has_Element (Trav) and not Found loop
         if Effect.Effect_List.Element (Trav).Effect_Name = Tubastga_Piece.Effect_Treasure then
            Found := True;
         end if;

         Trav := Effect.Effect_List.Next (Trav);
      end loop;

      return Found;
   end Is_Treasure_Here;

   function Is_Construction_Here
     (P_Patch        : in Hexagon.Client_Map.Type_Client_Patch;
      P_Construction : in Construction.Type_Construction) return Boolean
   is
      Found : Boolean;
   begin
      Found :=
        Construction.Construction_List.Has_Element
          (Construction.Construction_List.Find (P_Patch.Constructions_Here, P_Construction));

      return Found;
   end Is_Construction_Here;

   function GUI_X_To_Game_X (P_GUI_X : in Gdouble) return Integer is
   begin
      return Integer ((P_GUI_X - Gdouble (55.0 * Map_Scale)) / Gdouble (Map_Scale));
   end GUI_X_To_Game_X;

   function GUI_Y_To_Game_Y (P_GUI_Y : in Gdouble) return Integer is
   begin
      return Integer ((Gdouble (1050.0 * Map_Scale) - P_GUI_Y) / Gdouble (Map_Scale));
   end GUI_Y_To_Game_Y;

   function GUI_X_To_Minimap_X (P_GUI_X : in Gdouble) return Integer is
   begin
      return Integer (P_GUI_X - 510.0) * Minimap_Scale;
   end GUI_X_To_Minimap_X;

   function GUI_Y_To_Minimap_Y (P_GUI_Y : in Gdouble) return Integer is
   begin
      return Integer (600.0 - P_GUI_Y) * Minimap_Scale;
   end GUI_Y_To_Minimap_Y;

   function Pieces_Here_Index
     (P_Patch          : in Hexagon.Client_Map.Type_Client_Patch;
      P_Position_Index : in Integer) return Integer
   is
      Trav_Pieces_Here : Landscape.Pieces_Here_List.Cursor;
      Piece_Id         : Piece.Type_Piece_Id;
      Ret              : Integer;
      Found            : Boolean;
   begin
      Found            := False;
      Ret              := 1;
      Trav_Pieces_Here := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces_Here) and not Found loop
         Piece_Id := Landscape.Pieces_Here_List.Element (Trav_Pieces_Here);

         -- Is it this piece that is on 'P_Position_Index'?
         if Pieces_GUI_List.Element (Pieces_GUI_Positions, Piece_Id).Piece_Index_In_Patch =
           P_Position_Index
         then
            Found := True;
         else
            Ret := Ret + 1;
         end if;
         Trav_Pieces_Here := Landscape.Pieces_Here_List.Next (Trav_Pieces_Here);
      end loop;
--
      if not Found then
         Ret := 0;
      end if;

      return Ret;
   end Pieces_Here_Index;

   function Pieces_In_GUI_Index
     (P_Patch             : in Hexagon.Client_Map.Type_Client_Patch;
      P_Pieces_Here_Index : in Integer) return Integer
   is
      A_Piece_Id     : Piece.Type_Piece_Id;
      Position_Index : Integer;
   begin
      begin
         A_Piece_Id :=
           Landscape.Pieces_Here_List.Element (P_Patch.Pieces_Here, P_Pieces_Here_Index);
         Text_IO.Put_Line
           ("Pieces_In_GUI_Index A_Piece_Id =" &
            A_Piece_Id'Img &
            " based on P_Pieces_Here_Index=" &
            P_Pieces_Here_Index'Img);
         Position_Index :=
           Pieces_GUI_List.Element (Pieces_GUI_Positions, A_Piece_Id).Piece_Index_In_Patch;
      exception
         when others =>
            Position_Index := 0;
      end;

      return Position_Index;
   end Pieces_In_GUI_Index;

   function Selected_Position
     (P_Patch          : in Hexagon.Client_Map.Type_Client_Patch;
      P_GUI_X, P_GUI_Y : in Gint) return Integer
   is
      X, Y           : Gint;
      tx, ty         : Gint;
      Dist_Min, Dist : Gint;
      Closest_Point  : Integer := 0;

   begin
      Dist_Min := 1000;

      for Trav in Player_Pos'First .. Player_Pos'Last loop
         X :=
           Gint
             (Float (Game_Area_Origo_X) +
              Float (Hexagon.Client_Map.Get_X_From_AB (A_Client_Map, P_Patch)) -
              20.0);
         Y :=
           Gint
             (Float (Game_Area_Origo_Y) -
              Float (Hexagon.Client_Map.Get_Y_From_AB (A_Client_Map, P_Patch)) -
              22.0);

         X := X + (Gint (Float (Player_Pos (Trav).X) * 1.5));
         Y := Y + (Gint (Float (Player_Pos (Trav).Y) * 1.5));

         X := Gint (Float (X) * Map_Scale);
         Y := Gint (Float (Y) * Map_Scale);

         tx := Gint (tmp_X);
         ty := Gint (tmp_Y);

         Dist := (X - tx) * (X - tx) + (Y - ty) * (Y - ty);

         if Dist < Dist_Min then
            Dist_Min      := Dist;
            Closest_Point := Trav;
         end if;
      end loop;

      return Closest_Point;
   end Selected_Position;

   function Selected_Piece
     (P_Patch    : in Hexagon.Client_Map.Type_Client_Patch;
      P_Piece_Id :    Piece.Type_Piece_Id) return Integer
   is
      Find_Piece            : Landscape.Pieces_Here_List.Cursor;
      Find_Index, Ret_Index : Positive;

      use Piece;
   begin
      Find_Piece := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      Find_Index := 1;
      while Landscape.Pieces_Here_List.Has_Element (Find_Piece) loop
         if Landscape.Pieces_Here_List.Element (Find_Piece) = Piece.Type_Piece_Id (P_Piece_Id) then
            Ret_Index := Find_Index;
         end if;

         Find_Index := Find_Index + 1;
         Find_Piece := Landscape.Pieces_Here_List.Next (Find_Piece);
      end loop;

      return Ret_Index;
   end Selected_Piece;

   procedure Update_Activity_Report_Buffer
     (P_Window        : in Wnd_Main_Access;
      P_Activity_List : in Observation.Activity.Activity_Report.Vector)
   is
      Data            : Observation.Activity.Type_Activity_Report;
      Activity_Cursor : Observation.Activity.Activity_Report.Cursor;
      Iter, Iter2     : Gtk.Text_Iter.Gtk_Text_Iter;
   begin
      while Get_Line_Count (P_Window.Buffer_Activity_Report) > Gint (Max_Activity_Text) loop
         Get_Start_Iter (P_Window.Buffer_Activity_Report, Iter);
         Get_Iter_At_Line (P_Window.Buffer_Activity_Report, Iter2, 1);
         Delete (P_Window.Buffer_Activity_Report, Iter, Iter2);
      end loop;

      --
      Activity_Cursor := Observation.Activity.Activity_Report.First (P_Activity_List);
      while Observation.Activity.Activity_Report.Has_Element (Activity_Cursor) loop
         Data := Observation.Activity.Activity_Report.Element (Activity_Cursor);

         Get_End_Iter (P_Window.Buffer_Activity_Report, Iter);
         Insert
           (P_Window.Buffer_Activity_Report,
            Iter,
            Utilities.RemoteString.To_String (Data.Activity_Description) & ASCII.LF);
         Observation.Activity.Activity_Report.Next (Activity_Cursor);
      end loop;
      --

      Get_Start_Iter (P_Window.Buffer_Resources_Info, Iter);
      Get_End_Iter (P_Window.Buffer_Resources_Info, Iter2);
      Delete (P_Window.Buffer_Resources_Info, Iter, Iter2);
      Get_End_Iter (P_Window.Buffer_Resources_Info, Iter);

      declare
         Trav_Towers : Piece.Client_Piece.Pieces_Client_List.Cursor;
         A_Piece     : Tubastga_Window_Pkg.Type_Client_Access_Class;

         use Piece;
         use Player;
      begin
         Trav_Towers :=
           Piece.Client_Piece.Pieces_Client_List.First (Piece.Client_Piece.Client_Pieces_In_Game);
         while Piece.Client_Piece.Pieces_Client_List.Has_Element (Trav_Towers) loop
            A_Piece :=
              Tubastga_Window_Pkg.Type_Client_Access_Class
                (Piece.Client_Piece.Pieces_Client_List.Element (Trav_Towers));

            if A_Piece.all.Type_Of_Piece = Tubastga_Piece.Tower_House then

               Get_End_Iter (P_Window.Buffer_Resources_Info, Iter);
               Insert
                 (P_Window.Buffer_Resources_Info,
                  Iter,
                  "Tower:" &
                  A_Piece.all.Id'Img &
                  " " &
                  Utilities.RemoteString.To_String (A_Piece.all.Name) &
                  " belonging to " &
                  Utilities.RemoteString.To_String
                    (Player_Name_List (Integer (A_Piece.all.Player_Id))) &
                  ASCII.LF);

               for Trav_Slot in A_Piece.all.Storage.Slots'First .. A_Piece.all.Storage.Slots'Last
               loop
                  Get_End_Iter (P_Window.Buffer_Resources_Info, Iter);
                  Insert
                    (P_Window.Buffer_Resources_Info,
                     Iter,
                     "Resources:" &
                     A_Piece.all.Storage.Slots (Trav_Slot).The_Goods'Img &
                     " qty " &
                     A_Piece.all.Storage.Slots (Trav_Slot).Quantity'Img &
                     ASCII.LF);
               end loop;
            end if;
            Trav_Towers := Piece.Client_Piece.Pieces_Client_List.Next (Trav_Towers);
         end loop;
      end;

   end Update_Activity_Report_Buffer;

   function Periodic_Updates_Summary return Boolean is
      use Ada.Real_Time;
      use Player;
   begin
      if Me_Player_Id /= 0 then
         Now := Ada.Real_Time.Clock;

         if Now > Last_Updates_Summary + Ada.Real_Time.Milliseconds (500) then
            Observation.Activity.Activity_Report.Clear (System_Messages);

            Client.Server_Adm.Get_Updates_Summary
              (Me_Player_Id,
               Current_Player_Id,
               Now_Countdown,
               Game_State,
               System_Messages);

            Last_Updates_Summary := Ada.Real_Time.Clock;
         end if;
      end if;

      Update_Activity_Report_Buffer (The_Window, System_Messages);
      Observation.Activity.Activity_Report.Clear (System_Messages);

      Queue_Draw (The_Window);

      if Now_Countdown /= Last_Countdown then
         Last_Countdown := Now_Countdown;

      end if;

      return True;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-callbacks.Periodic_Updates_Summary - Server.Server.Game_Engine_Doesnt_Exists");
         return True;
      when System.RPC.Communication_Error =>
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Periodic_Updates_Summary - System.RPC.Communication_Error");
         return True;
   end Periodic_Updates_Summary;

   function Format_Landscape (P_Landscape : in Landscape.Type_Landscape) return String is
   begin
      return Utilities.RemoteString.To_String
          (Tubastga_Piece.Landscapes_Type_Info_List (P_Landscape).Type_Name);
   end Format_Landscape;

   procedure Set_Performing_Piece_Window
     (P_Piece_Window : in out Wnd_Piece_Access;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch_Adress;
      P_Piece        : in     Tubastga_Window_Pkg.Type_Client_Piece)
   is
      Trav_Patch_Constructions : Construction.Construction_List.Cursor;

      Start_Iter, End_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Set_Performing_Piece_Window - enter");
      end if;

      P_Piece_Window.all.Selected_Patch := P_Patch;

      if P_Piece.Id /= Piece.Undefined_Piece_Id then
         P_Piece_Window.all.Selected_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece.Id));

         Gtk.GEntry.Set_Text
           (P_Piece_Window.all.En_Piece_Name,
            Utilities.RemoteString.To_String (P_Piece_Window.all.Selected_Piece.Name));

         Tubastga_Window_Pkg.Sounds.Play_Set_Performing_Piece_Window;

      end if;

      Gtk.Text_Buffer.Get_Start_Iter
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         Start_Iter);
      Gtk.Text_Buffer.Get_End_Iter
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         End_Iter);
      Gtk.Text_Buffer.Delete
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         Start_Iter,
         End_Iter);

      Gtk.Text_Buffer.Set_Text
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         "Information for patch (" & P_Patch.Pos.A'Img & ", " & P_Patch.Pos.B'Img & ")" & ASCII.LF);

      Gtk.Text_Buffer.Get_End_Iter
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         End_Iter);
      Gtk.Text_Buffer.Insert
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         End_Iter,
         "Observed landscape: " & Format_Landscape (P_Patch.Landscape_Here) & ASCII.LF);

      Gtk.Text_Buffer.Get_End_Iter
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         End_Iter);
      Gtk.Text_Buffer.Insert
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         End_Iter,
         "Patch Effects: " & ASCII.LF);

      Tubastga_Window_Pkg.Effects.Format_Patch_Effects
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         P_Patch.all);

      Gtk.Text_Buffer.Get_End_Iter
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         End_Iter);
      Gtk.Text_Buffer.Insert
        (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
         End_Iter,
         "Constructions: " & ASCII.LF);

      Trav_Patch_Constructions := Construction.Construction_List.First (P_Patch.Constructions_Here);
      while Construction.Construction_List.Has_Element (Trav_Patch_Constructions) loop

         Gtk.Text_Buffer.Get_End_Iter
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter,
            Construction.Construction_List.Element (Trav_Patch_Constructions)'Img & ASCII.LF);

         Trav_Patch_Constructions := Construction.Construction_List.Next (Trav_Patch_Constructions);
      end loop;

      if P_Piece.Id /= Piece.Undefined_Piece_Id then
         Get_End_Iter
           (The_Window.all.Wnd_Performing_Piece.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter,
            "Information regarding piece" & ASCII.LF);

         Tubastga_Window_Pkg.Effects.Format_Piece_Effects
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            P_Piece);
         --
         if Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece.Id)).Category =
           Piece.Fighting_Piece
         then
            Gtk.Label.Set_Label
              (P_Piece_Window.all.Lbl_Piece_Type,
               Utilities.RemoteString.To_String
                 (Tubastga_Piece.Pieces_Type_Info_List
                    (Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece.Id))
                       .Type_Of_Piece)
                    .Type_Name));

            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall1, False);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall2, False);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall3, False);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall4, False);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall5, False);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall6, False);

            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Move, True);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Attack, True);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Search, True);

         elsif Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece.Id)).Category =
           Piece.House_Piece
         then
            Gtk.Label.Set_Label
              (P_Piece_Window.all.Lbl_Piece_Type,
               Utilities.RemoteString.To_String
                 (Tubastga_Piece.Houses_Type_Info_List
                    (Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece.Id))
                       .Type_Of_Piece)
                    .Type_Name));

            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall1, True);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall2, True);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall3, True);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall4, True);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall5, True);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Wall6, True);

            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Move, False);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Attack, False);
            Gtk.Button.Set_Sensitive (P_Piece_Window.all.Btn_Search, False);

         end if;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Set_Performing_Piece_Window - exit");
      end if;
   end Set_Performing_Piece_Window;

   procedure Set_Target_Piece_Window
     (P_Piece_Window : in out Wnd_Target_Access;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch_Adress;
      P_Piece_Id     : in     Integer)
   is
      A_Piece : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      Trav_Patch_Constructions : Construction.Construction_List.Cursor;

      Start_Iter, End_Iter : Gtk.Text_Iter.Gtk_Text_Iter;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Set_Target_Piece_Window - enter");
      end if;

      P_Piece_Window.all.Selected_Patch := P_Patch;

      if P_Piece_Id /= 0 then

         P_Piece_Window.all.Selected_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id));

         Gtk.GEntry.Set_Text
           (P_Piece_Window.all.En_Piece_Name,
            Utilities.RemoteString.To_String
              (Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id)).Name));

         Tubastga_Window_Pkg.Sounds.Play_Set_Target_Piece_Window;

         --
         A_Piece := Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id));
      end if;

      Gtk.Text_Buffer.Get_Start_Iter
        (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
         Start_Iter);
      Gtk.Text_Buffer.Get_End_Iter (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info, End_Iter);
      Gtk.Text_Buffer.Delete
        (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
         Start_Iter,
         End_Iter);

      Gtk.Text_Buffer.Set_Text
        (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
         "Information for patch (" & P_Patch.Pos.A'Img & ", " & P_Patch.Pos.B'Img & ")" & ASCII.LF);

      Gtk.Text_Buffer.Get_End_Iter (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info, End_Iter);
      Gtk.Text_Buffer.Insert
        (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
         End_Iter,
         "Observed landscape: " & Format_Landscape (P_Patch.Landscape_Here) & ASCII.LF);

      Gtk.Text_Buffer.Get_End_Iter (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info, End_Iter);
      Gtk.Text_Buffer.Insert
        (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
         End_Iter,
         "Patch Effects: " & ASCII.LF);

      Tubastga_Window_Pkg.Effects.Format_Patch_Effects
        (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
         P_Patch.all);

      Gtk.Text_Buffer.Get_End_Iter (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info, End_Iter);
      Gtk.Text_Buffer.Insert
        (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
         End_Iter,
         "Constructions: " & ASCII.LF);

      Trav_Patch_Constructions := Construction.Construction_List.First (P_Patch.Constructions_Here);
      while Construction.Construction_List.Has_Element (Trav_Patch_Constructions) loop

         Gtk.Text_Buffer.Get_End_Iter (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info, End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
            End_Iter,
            Construction.Construction_List.Element (Trav_Patch_Constructions)'Img & ASCII.LF);

         Trav_Patch_Constructions := Construction.Construction_List.Next (Trav_Patch_Constructions);
      end loop;

      if P_Piece_Id /= 0 then

         Get_End_Iter (The_Window.all.Wnd_Target.all.Buffer_Target_Piece_Patch_Info, End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
            End_Iter,
            "Information regarding piece" & ASCII.LF);

         Tubastga_Window_Pkg.Effects.Format_Piece_Effects
           (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
            Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));

         --
         if Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id)).Category =
           Piece.Fighting_Piece
         then
            Gtk.Label.Set_Label
              (P_Piece_Window.all.Lbl_Piece_Type,
               Utilities.RemoteString.To_String
                 (Tubastga_Piece.Pieces_Type_Info_List
                    (Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id))
                       .Type_Of_Piece)
                    .Type_Name));

         elsif Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id)).Category =
           Piece.House_Piece
         then
            Gtk.Label.Set_Label
              (P_Piece_Window.all.Lbl_Piece_Type,
               Utilities.RemoteString.To_String
                 (Tubastga_Piece.Houses_Type_Info_List
                    (Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id))
                       .Type_Of_Piece)
                    .Type_Name));

         end if;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Set_Target_Piece_Window - exit");
      end if;
   end Set_Target_Piece_Window;

   procedure DlgGame_Populate
     (P_Server_Info : in out Utilities.RemoteString_List.Vector;
      P_Connect     :    out Boolean;
      P_Create      :    out Boolean;
      P_Save        :    out Boolean;
      P_Load        :    out Boolean;
      P_Join        :    out Boolean;
      P_Leave       :    out Boolean;
      P_Disconnect  :    out Boolean)
   is

      Trav       : Utilities.RemoteString_List.Cursor;
      An_Element : Utilities.RemoteString.Type_String;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg-callbacks.DlgGame_Populate - enter");
      end if;

      P_Connect    := False;
      P_Create     := False;
      P_Save       := False;
      P_Load       := False;
      P_Join       := False;
      P_Leave      := False;
      P_Disconnect := False;

      --    declare
      --     Chosen_Game_Map : UTF8_String :=
   --     Gtk.Combo_Box_Text.Get_Active_Text(The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map);

      --Ret : Boolean;
--      begin
--         Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map);
--         Gtk.Combo_Box_Text.Append_Text
--           (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map,
--            Chosen_Game_Map);
--         Gtk.Combo_Box_Text.Append_Text
      --         (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map,
      --        "Chose map...");

      --Ret := Gtk.Combo_Box.Set_Active_Id(Gtk.Combo_Box.Gtk_Combo_Box(The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map), Chosen_Game_Map);
      --      end;

      -- reset
--      Combo_Box_Text_Entry_List.Find(Cmb_Create_Game_Chose_Map
      -- what map is selected ?

      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map);
      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name);
      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name);

      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map,
         "Chose map...");
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name,
         "Load game...");
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name,
         "Join as...");

      P_Join  := True;
      P_Leave := False;
      Trav    := Utilities.RemoteString_List.First (P_Server_Info);
      while Utilities.RemoteString_List.Has_Element (Trav) loop
         An_Element := Utilities.RemoteString_List.Element (Trav);

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 10) =
           "Player " & Utilities.Number_To_Fixed_String (Natural (Me_Player_Id), 2) & ":"
         then

            P_Join  := False;
            P_Leave := True;
         end if;

         Text_IO.Put_Line
           (".---." &
            Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 9) &
            ".");
         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 9) =
           "Connected"
         then
            P_Connect    := False;
            P_Disconnect := True;
         end if;

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 12) =
           "Disconnected"
         then
            P_Connect    := True;
            P_Disconnect := False;
         end if;

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 4) = "Run:" then
            if Ada.Strings.Fixed.Tail
                (Utilities.RemoteString.To_String (An_Element),
                 Utilities.RemoteString.To_String (An_Element)'Last - 4) =
              "STARTING"
            then
               P_Connect    := False;
               P_Create     := True;
               P_Save       := False;
               P_Load       := True;
               P_Disconnect := True;
            end if;

            if Ada.Strings.Fixed.Tail
                (Utilities.RemoteString.To_String (An_Element),
                 Utilities.RemoteString.To_String (An_Element)'Last - 4) =
              "ONGOING"
            then
               P_Connect    := False;
               P_Create     := False;
               P_Save       := True;
               P_Load       := False;
               P_Disconnect := True;
            end if;

         end if;

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 12) =
           "Description:"
         then

            Gtk.Label.Set_Text
              (The_Window.all.dlgMainMenu.Lbl_Join_Game,
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 12));
         end if;
         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 4) = "Map:" then

            Gtk.Combo_Box_Text.Insert
              (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map,
               100,
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 4),
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 4));
         end if;
         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 6) =
           "Saved:"
         then
            Gtk.Combo_Box_Text.Append_Text
              (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name,
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 6));
         end if;

         for I in Player.Type_Player_Id'First .. Player.Type_Player_Id'Last loop
            if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 20) =
              "Registered Player" & I'Img & ":"
            then
               Gtk.Combo_Box_Text.Append_Text
                 (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name,
                  Ada.Strings.Fixed.Tail
                    (Utilities.RemoteString.To_String (An_Element),
                     Utilities.RemoteString.To_String (An_Element)'Last - 20));
            end if;
         end loop;

         Trav := Utilities.RemoteString_List.Next (Trav);
      end loop;

      Utilities.RemoteString_List.Clear (P_Server_Info);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-callbacks.DlgGame_Populate.Game_Status=" & Game_State'Img);
      end if;

   end DlgGame_Populate;

   procedure On_Map_Area_Show (Object : access Gtk_Drawing_Area_Record'Class) is
      use Utilities.RemoteString;
      use Ada.Strings.Unbounded;

      use Gdk.Pixbuf;
   begin
      if Verbose then
         Text_IO.Put_Line ("On_Map_Area_Show - enter ");
      end if;

      The_Window := Tubastga_Window_Pkg.Wnd_Main_Access (Get_Toplevel (Object));

      All_Pix :=
        Gdk.Pixbuf.Gdk_New
          (Has_Alpha => True,
           Width     => 850,
           Height    => Gint (Game_Area_Origo_Y) + 50);
      Scale_Pix       := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 700, Height => 730);
      All_Minimap_Pix :=
        Gdk.Pixbuf.Gdk_New
          (Has_Alpha => True,
           Width     => Gint (500),
           Height    => Gint (Minimap_Origo_Y) + Gint (10));
      Scale_Minimap_Pix := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 700, Height => 730);
      All_Constructions_On_Patch :=
        Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 55, Height => 44);
      All_Landscape_On_Patch  := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 55, Height => 44);
      All_Surprise_On_Patch   := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 55, Height => 44);
      All_Selections_On_Patch := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 55, Height => 44);

      if Verbose then
         Text_IO.Put_Line ("On_Map_Area_Show - exit");
      end if;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Tubastga_Window_Pkg-callbacks.On_Map_Area_Show - exception");

   end On_Map_Area_Show;

   procedure On_Player_Timer_Area_Show (Object : access Gtk_Drawing_Area_Record'Class) is
      use Utilities.RemoteString;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Player_Timer_Area_Show - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Player_Timer_Area_Show - exit");
      end if;

   end On_Player_Timer_Area_Show;

   procedure Gtk_Do_It
     (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in out Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
      use Piece;
      use Landscape;
      use Hexagon.Client_Map;
      use Player;

      Landscape_Image,
      Surprise_Image,
      Piece_Image,
      UI_Aid_Image,
      Player_Image,
      Construction_Image : Type_Image_Names;
      Trav_Pieces        : Landscape.Pieces_Here_List.Cursor;
      Piece_No           : Integer;
      A_Piece            : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      --
      use Hexagon;

      function Get_All_Pix_Patch_X_From_AB
        (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Gint
      is
      begin
         return Gint
             (Game_Area_Origo_X + Hexagon.Client_Map.Get_X_From_AB (P_Client_Map, P_Patch) - 20);
      end Get_All_Pix_Patch_X_From_AB;

      function Get_All_Pix_Patch_Y_From_AB
        (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in Hexagon.Client_Map.Type_Client_Patch) return Gint
      is
      begin
         return Gint
             (Game_Area_Origo_Y - Hexagon.Client_Map.Get_Y_From_AB (P_Client_Map, P_Patch) - 22);
      end Get_All_Pix_Patch_Y_From_AB;

      function Get_All_Pix_Player_X_From_AB
        (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
         P_Trav_Draw  : in Natural) return Gint
      is
      begin
         return Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch) +
           Gint (Player_Pos (P_Trav_Draw).X);
      end Get_All_Pix_Player_X_From_AB;

      function Get_All_Pix_Player_Y_From_AB
        (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
         P_Trav_Draw  : in Natural) return Gint
      is
      begin
         return Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch) +
           Gint (Player_Pos (P_Trav_Draw).Y);
      end Get_All_Pix_Player_Y_From_AB;

      function Get_All_Pix_Piece_X_From_AB
        (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
         P_Trav_Draw  : in Natural) return Gint
      is
      begin
         return Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch) +
           Gint (Piece_Pos (P_Trav_Draw).X);
      end Get_All_Pix_Piece_X_From_AB;

      function Get_All_Pix_Piece_Y_From_AB
        (P_Client_Map : in Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in Hexagon.Client_Map.Type_Client_Patch;
         P_Trav_Draw  : in Natural) return Gint
      is
      begin
         return Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch) +
           Gint (Piece_Pos (P_Trav_Draw).Y);
      end Get_All_Pix_Piece_Y_From_AB;

      procedure Draw_Constructions
        (P_Pixbuf : in out Gdk.Pixbuf.Gdk_Pixbuf;
         P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch)
      is
         Construction_Image : Type_Image_Names;
      begin
         if Is_Construction_Here (P_Patch, Tubastga_Piece.Construction_Wall1) then
            Construction_Image := Wall1;

            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Construction_Image).Image_Data,
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

         if Is_Construction_Here (P_Patch, Tubastga_Piece.Construction_Wall2) then
            Construction_Image := Wall2;

            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Construction_Image).Image_Data,
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

         if Is_Construction_Here (P_Patch, Tubastga_Piece.Construction_Wall3) then
            Construction_Image := Wall3;

            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Construction_Image).Image_Data,
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

         if Is_Construction_Here (P_Patch, Tubastga_Piece.Construction_Wall4) then
            Construction_Image := Wall4;

            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Construction_Image).Image_Data,
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

         if Is_Construction_Here (P_Patch, Tubastga_Piece.Construction_Wall5) then
            Construction_Image := Wall5;

            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Construction_Image).Image_Data,
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

         if Is_Construction_Here (P_Patch, Tubastga_Piece.Construction_Wall6) then
            Construction_Image := Wall6;

            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Construction_Image).Image_Data,
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
      end Draw_Constructions;

      procedure Draw_Landscapes
        (P_Pixbuf : in out Gdk.Pixbuf.Gdk_Pixbuf;
         P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch)
      is
      begin
         if P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Grass then
            Landscape_Image := Grass;
         elsif P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Forest then
            Landscape_Image := Forest;
         elsif P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Mountain then
            Landscape_Image := Mountain;
         elsif P_Patch.Landscape_Here = Tubastga_Piece.Landscape_Water then
            Landscape_Image := Water;
         end if;

         Gdk.Pixbuf.Composite
           (The_Window.All_Images (Landscape_Image).Image_Data,
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
      end Draw_Landscapes;

      procedure Draw_Surprises
        (P_Pixbuf : in out Gdk.Pixbuf.Gdk_Pixbuf;
         P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch)
      is
      begin
         if Is_Treasure_Here (P_Patch) then
            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Chest).Image_Data,
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
      end Draw_Surprises;

      procedure Draw_Selections
        (P_Pixbuf : in out Gdk.Pixbuf.Gdk_Pixbuf;
         P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch)
      is
         UI_Aid_Image : Type_Image_Names := None;
      begin
         -- Now UI Aid Selection
         if P_Patch.Draw_Action = Reachable then
            UI_Aid_Image := Reachable;
         elsif P_Patch.Draw_Action = Attackable then
            UI_Aid_Image := Attackable;
         elsif P_Patch.Draw_Action = Selected then
            UI_Aid_Image := Selected_Patch;
         elsif P_Patch.Draw_Action = Selected_Area then
            UI_Aid_Image := Selected_Area;
         end if;

         if UI_Aid_Image /= None then

            Gdk.Pixbuf.Composite
              (The_Window.All_Images (UI_Aid_Image).Image_Data,
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

      end Draw_Selections;

   begin

      declare
         Minimap_X, Minimap_Y : Gint;
      begin

         Minimap_X :=
           Gint (10 + Hexagon.Client_Map.Get_Absolute_X_From_AB (P_Patch.all) / Minimap_Scale);
         Minimap_Y :=
           Gint
             (Minimap_Origo_Y -
              Hexagon.Client_Map.Get_Absolute_Y_From_AB (P_Patch.all) / Minimap_Scale);

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
           ((Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all) in 0 .. 50 or
             Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all) in 770 .. 820) and
            (Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all) in 0 .. 1050)) or
            --
           ((Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all) in 0 .. 50 or
             Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all) in 1000 .. 1050) and
            (Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all) in 0 .. 820))
         then
            Landscape_Image := Outside_View_Invisible_On_Minimap;
         end if;

         declare
            Minimap_X, Minimap_Y : Gint;
         begin

            Minimap_X :=
              Gint (10 + Hexagon.Client_Map.Get_Absolute_X_From_AB (P_Patch.all) / Minimap_Scale);
            Minimap_Y :=
              Gint
                (Minimap_Origo_Y -
                 Hexagon.Client_Map.Get_Absolute_Y_From_AB (P_Patch.all) / Minimap_Scale);

            -- minimap
            Gdk.Pixbuf.Composite
              (The_Window.All_Images (Landscape_Image).Image_Data,
               All_Minimap_Pix,
               Gint (Minimap_X),
               Gint (Minimap_Y),
               5,
               5,
               Gdouble (Minimap_X),
               Gdouble (Minimap_Y),
               1.0,
               1.0,
               Gdk.Pixbuf.Interp_Nearest,
               255);
         end;

         Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.all.Pieces_Here);
         while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop
            declare
               n          : Type_GUI_Piece;
               A_Piece_Id : Piece.Type_Piece_Id;
            begin
               A_Piece_Id := Landscape.Pieces_Here_List.Element (Trav_Pieces);
               if Pieces_GUI_List.Has_Element
                   (Pieces_GUI_List.Find (Pieces_GUI_Positions, A_Piece_Id))
               then
                  n := Pieces_GUI_List.Element (Pieces_GUI_Positions, A_Piece_Id);
               else
                  n := Type_GUI_Piece'(1, None);
                  Pieces_GUI_List.Include
                    (Pieces_GUI_Positions,
                     A_Piece_Id,
                     Type_GUI_Piece'(n.Piece_Index_In_Patch, n.Draw_Action));
               end if;
            end;

            Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
         end loop;

      end;

      if Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all) in 0 .. 820 and
        Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all) in 0 .. 1050
      then

         if P_Patch.Visible then
            Landscape_Image    := None;
            Surprise_Image     := None;
            Construction_Image := None;
            Player_Image       := None;
            Piece_Image        := None;
            UI_Aid_Image       := None;

            Gdk.Pixbuf.Fill (All_Surprise_On_Patch, Guint32 (0));
            Draw_Surprises (All_Surprise_On_Patch, P_Patch.all);

            Gdk.Pixbuf.Fill (All_Landscape_On_Patch, Guint32 (0));
            Draw_Landscapes (All_Landscape_On_Patch, P_Patch.all);

            Gdk.Pixbuf.Fill (All_Constructions_On_Patch, Guint32 (0));
            Draw_Constructions (All_Constructions_On_Patch, P_Patch.all);

            Gdk.Pixbuf.Fill (All_Selections_On_Patch, Guint32 (0));
            Draw_Constructions (All_Selections_On_Patch, P_Patch.all);

            declare
               x, y : Gint;
            begin
               x := Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all);
               y := Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all);

               Gdk.Pixbuf.Composite
                 (All_Landscape_On_Patch,
                  All_Pix,
                  Gint (x),
                  Gint (y),
                  50,
                  44,
                  Gdouble (x),
                  Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);
               --
               Gdk.Pixbuf.Composite
                 (All_Constructions_On_Patch,
                  All_Pix,
                  Gint (x),
                  Gint (y),
                  50,
                  44,
                  Gdouble (x),
                  Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);

               Gdk.Pixbuf.Composite
                 (All_Surprise_On_Patch,
                  All_Pix,
                  Gint (x),
                  Gint (y),
                  50,
                  44,
                  Gdouble (x),
                  Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);
            end;

            for Trav_Init in Player_Pos'First .. Player_Pos'Last loop
               Player_Pos (Trav_Init).Image_Here := None;
               Piece_Pos (Trav_Init).Image_Here  := None;
            end loop;

            Piece_No    := 1;
            Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
            while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop
               Player_Image := None;
               Piece_Image  := None;

               A_Piece :=
                 Piece.Client_Piece.Find_Piece_In_List
                   (Landscape.Pieces_Here_List.Element (Trav_Pieces));

               -- Now houses
               if A_Piece.Type_Of_Piece = Tubastga_Piece.Farm_House then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Farm_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Farm_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Farm_p3;
                     Player_Image := Blue;
                  end if;
               elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Tower_House then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Tower_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Tower_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Tower_p3;
                     Player_Image := Blue;
                  end if;
               elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Lumberjack_House then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Lumberjack_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Lumberjack_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Lumberjack_p3;
                     Player_Image := Blue;
                  end if;
               elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Stonecutter_House then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Stonecutter_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Stonecutter_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Stonecutter_p3;
                     Player_Image := Blue;
                  end if;
               end if;

               if A_Piece.Type_Of_Piece = Tubastga_Piece.Sentry_Piece then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Sentry_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Sentry_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Sentry_p3;
                     Player_Image := Blue;
                  end if;
               elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Knight_Piece then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Knight_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Knight_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Knight_p3;
                     Player_Image := Blue;
                  end if;
               elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Bowman_Piece then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Bowman_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Bowman_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Bowman_p3;
                     Player_Image := Blue;
                  end if;
               elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Ship_Piece then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Ship_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Ship_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Ship_p3;
                     Player_Image := Blue;
                  end if;
               elsif A_Piece.Type_Of_Piece = Tubastga_Piece.Carrier_Piece then
                  if A_Piece.Player_Id = 1 then
                     Piece_Image  := Carrier_p1;
                     Player_Image := Red;
                  elsif A_Piece.Player_Id = 2 then
                     Piece_Image  := Carrier_p2;
                     Player_Image := Green;
                  else
                     Piece_Image  := Carrier_p3;
                     Player_Image := Blue;
                  end if;
               end if;

               declare
                  n : Type_GUI_Piece;
               begin
                  n :=
                    Pieces_GUI_List.Element
                      (Pieces_GUI_List.Find (Pieces_GUI_Positions, A_Piece.Id));

                  if A_Piece.Type_Of_Piece = Tubastga_Piece.Carrier_Piece then

                     if Pieces_GUI_List.Element (Pieces_GUI_Positions, A_Piece.Id).Draw_Action =
                       Selected
                     then
                        Player_Pos (7).Image_Here := Selected_Piece;
                     else
                        Player_Pos (7).Image_Here := Player_Image;

                     end if;

                     Piece_Pos (7).Image_Here := Piece_Image;
                     Pieces_GUI_List.Include
                       (Pieces_GUI_Positions,
                        A_Piece.Id,
                        Type_GUI_Piece'(7, n.Draw_Action));

                  else
                     if Pieces_GUI_List.Element (Pieces_GUI_Positions, A_Piece.Id).Draw_Action =
                       Selected
                     then
                        Player_Pos (Piece_No).Image_Here := Selected_Piece;
                     else
                        Player_Pos (Piece_No).Image_Here := Player_Image;

                     end if;

                     Piece_Pos (Piece_No).Image_Here := Piece_Image;
                     Pieces_GUI_List.Include
                       (Pieces_GUI_Positions,
                        A_Piece.Id,
                        Type_GUI_Piece'(Piece_No, n.Draw_Action));

                     Piece_No := Piece_No + 1;
                  end if;

               end;

               Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);

            end loop;

            for Trav_Draw in Player_Pos'First .. Player_Pos'Last loop
               if Player_Pos (Trav_Draw).Image_Here /= None then

                  declare
                     x, y : Gint;
                  begin
                     x := Get_All_Pix_Player_X_From_AB (A_Client_Map, P_Patch.all, Trav_Draw);
                     y := Get_All_Pix_Player_Y_From_AB (A_Client_Map, P_Patch.all, Trav_Draw);

                     Gdk.Pixbuf.Composite
                       (The_Window.All_Images (Player_Pos (Trav_Draw).Image_Here).Image_Data,
                        All_Pix,
                        Gint (x),
                        Gint (y),
                        50,
                        44,
                        Gdouble (x),
                        Gdouble (y),
                        1.0,
                        1.0,
                        Gdk.Pixbuf.Interp_Nearest,
                        255);

                  end;
               end if;

            end loop;

            for Trav_Draw in Piece_Pos'First .. Piece_Pos'Last loop

               if Piece_Pos (Trav_Draw).Image_Here /= None then
                  declare
                     x, y : Gint;
                  begin
                     x := Get_All_Pix_Piece_X_From_AB (A_Client_Map, P_Patch.all, Trav_Draw);
                     y := Get_All_Pix_Piece_Y_From_AB (A_Client_Map, P_Patch.all, Trav_Draw);

                     Gdk.Pixbuf.Composite
                       (The_Window.All_Images (Piece_Pos (Trav_Draw).Image_Here).Image_Data,
                        All_Pix,
                        Gint (x),
                        Gint (y),
                        50,
                        44,
                        Gdouble (x),
                        Gdouble (y),
                        1.0,
                        1.0,
                        Gdk.Pixbuf.Interp_Nearest,
                        255);
                  end;
               end if;
            end loop;

            --
            --
            -- Now UI Aid Selection

            Draw_Selections (All_Selections_On_Patch, P_Patch.all);
            declare
               x, y : Gint;
            begin
               x := Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all);
               y := Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all);

               Gdk.Pixbuf.Composite
                 (All_Selections_On_Patch,
                  All_Pix,
                  Gint (x),
                  Gint (y),
                  50,
                  44,
                  Gdouble (x),
                  Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);
            end;

         else
            declare
               x, y : Gint;
            begin
               x := Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all);
               y := Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all);

               Gdk.Pixbuf.Composite
                 (The_Window.All_Images (Invisible).Image_Data,
                  All_Pix,
                  Gint (x),
                  Gint (y),
                  50,
                  44,
                  Gdouble (x),
                  Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);
            end;
         end if;
      end if;

   end Gtk_Do_It;

   procedure On_Player_Timer_Area_Expose_Event
     (Object      :    access Gtk_Drawing_Area_Record'Class;
      P_Draw      :    Cairo.Cairo_Context;
      P_Player_Id : in Player.Type_Player_Id)
   is

      use Player;
   begin

      if P_Player_Id = Me_Player_Id then
         Cairo.Set_Source_Rgb (P_Draw, 0.50, 0.40, 0.30);
         Cairo.Rectangle (P_Draw, Gdouble (0), Gdouble (0), Gdouble (125), Gdouble (40));
      end if;

      if P_Player_Id = Current_Player_Id then
         Cairo.Set_Source_Rgb (P_Draw, 0.0, 0.0, 0.0);
         Cairo.Arc (P_Draw, Gdouble (95), Gdouble (20), Gdouble (15), Gdouble (0), Gdouble (6.28));
         Cairo.Fill (P_Draw);
      end if;

      if P_Player_Id = 1 then
         Cairo.Set_Source_Rgb (P_Draw, 1.0, 0.0, 0.0);
      elsif P_Player_Id = 2 then
         Cairo.Set_Source_Rgb (P_Draw, 0.0, 1.0, 0.0);
      elsif P_Player_Id = 3 then
         Cairo.Set_Source_Rgb (P_Draw, 0.0, 0.0, 1.0);
      end if;

      Cairo.Rectangle (P_Draw, Gdouble (5), Gdouble (5), Gdouble (60), Gdouble (30));
      Cairo.Fill (P_Draw);

      if P_Player_Id = Current_Player_Id then
         Cairo.Arc
           (P_Draw,
            Gdouble (95),
            Gdouble (20),
            Gdouble (12),
            Gdouble (0),
            Gdouble (2 * 6.28) * Gdouble (Now_Countdown));

         Cairo.Fill (P_Draw);
      end if;

      Cairo.Stroke (P_Draw);
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Player_Timer_Area_Expose_Event - exit");
      end if;
   end On_Player_Timer_Area_Expose_Event;

   function On_Player_1_Timer_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Player_1_Timer_Area_Expose_Event - enter");
      end if;

      On_Player_Timer_Area_Expose_Event (Object, P_Draw, Player.Type_Player_Id (1));

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Player_1_Timer_Area_Expose_Event - exit");
      end if;

      return True;
   end On_Player_1_Timer_Area_Expose_Event;

   function On_Player_2_Timer_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Player_2_Timer_Area_Expose_Event - enter");
      end if;

      On_Player_Timer_Area_Expose_Event (Object, P_Draw, Player.Type_Player_Id (2));

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Player_2_Timer_Area_Expose_Event - exit");
      end if;

      return True;
   end On_Player_2_Timer_Area_Expose_Event;

   function On_Player_3_Timer_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Player_3_Timer_Area_Expose_Event - enter");
      end if;

      On_Player_Timer_Area_Expose_Event (Object, P_Draw, Player.Type_Player_Id (3));

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Player_3_Timer_Area_Expose_Event - exit");
      end if;

      return True;
   end On_Player_3_Timer_Area_Expose_Event;

   function On_Map_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
      Target_X, Target_Y : Integer;
      Curr_Patch         : Hexagon.Client_Map.Type_Client_Patch_Adress;
      Adm_Status         : Status.Type_Adm_Status;

      Piece_Selected_Index : Integer := 0;

      use Utilities.RemoteString;
      use Ada.Containers;
      use Piece;
      use Hexagon.Client_Map;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-callbacks.On_Map_Area_Expose_Event - enter " &
            GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%s - %i") &
            " anim status=" &
            Anim_State'Img &
            " Me_Player_Id=" &
            Me_Player_Id'Img);
      end if;

      if Me_Player_Id = 0 then
         return True;
      end if;

      for Trav_Players in Player_Name_List'First .. Player_Name_List'Last loop
         if Player_Name_List (Trav_Players) = "" then
            Player_Name_List (Trav_Players) :=
              Client.Server_Adm.Get_Player_Name (Player.Type_Player_Id (Trav_Players), Adm_Status);
         end if;
      end loop;

      Set_Text
        (The_Window.all.Lbl_Player_1_Name,
         Utilities.RemoteString.To_String (Player_Name_List (1)));
      Set_Text
        (The_Window.all.Lbl_Player_2_Name,
         Utilities.RemoteString.To_String (Player_Name_List (2)));
      Set_Text
        (The_Window.all.Lbl_Player_3_Name,
         Utilities.RemoteString.To_String (Player_Name_List (3)));

      Draw := P_Draw;

      case Anim_State is
         when Anim_Idle =>
            Anim_State := Anim_Get_Pieces_Report;

         when Anim_Show_Frame_Pause =>
            Anim_Show_Frame_Pause_Counter := Anim_Show_Frame_Pause_Counter + 1;

            if Anim_Show_Frame_Pause_Counter > Max_Show_Frame_Pause_Counter then
               Anim_State := Anim_Show_Frame;
            else
               Anim_State := Anim_Show_Frame_Pause;
            end if;

         when Anim_Get_Pieces_Report_Pause =>
            Anim_Get_Pieces_Report_Pause_Counter := Anim_Get_Pieces_Report_Pause_Counter + 1;

            if Anim_Get_Pieces_Report_Pause_Counter > Max_Get_Pieces_Report_Pause_Counter then
               Anim_State := Anim_Get_Pieces_Report;
            else
               Anim_State := Anim_Get_Pieces_Report_Pause;
            end if;

         when Anim_Get_Pieces_Report =>

            -- Clear all data from previous run:
            Observation.Frames.Clear_Frames (Player_Pieces_Visibility_Frames);
            Piece.Client_Piece.Get_Pieces_Report (Me_Player_Id, Player_Pieces_Visibility_Frames);

            Frame_Cursor :=
              Observation.Frames.Piece_Visibility_Frames.First (Player_Pieces_Visibility_Frames);
            if Observation.Frames.Piece_Visibility_Frames.Has_Element (Frame_Cursor) then
               Anim_State := Anim_Show_Frame;
            else
               Anim_Get_Pieces_Report_Pause_Counter := 0;
               Anim_State                           := Anim_Get_Pieces_Report_Pause;
            end if;

         when Anim_Show_Frame =>

            -- remove picture from previous frame
            Gdk.Pixbuf.Fill (All_Pix, Guint32 (0));

            Piece.Client_Piece.Set_Reports_On_Pieces
              (Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Pieces_Info,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor)
                 .Pieces_Effects_Info);
            --Populate_Piece_Window(The_Window.all.wndPiece);

            -- Set new info on map
            Hexagon.Client_Map.Set_Reports_On_Map
              (A_Client_Map,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Patches,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Pieces,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor)
                 .Patches_Effects_Info,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor)
                 .Constructions_Info);

            Tubastga_Window_Pkg.Effects.Update_Client_Piece;

            Update_Activity_Report_Buffer
              (The_Window,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Activities_Info);

            Hexagon.Client_Map.Reset_Visit;

            Hexagon.Client_Map.Traverse (A_Client_Map, A_Client_Map.Origo_Patch, Gtk_Do_It'Access);

            Hexagon.Client_Map.Reset_Visit;

            Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.Next (Frame_Cursor);
            if Observation.Frames.Piece_Visibility_Frames.Has_Element (Frame_Cursor) then
               Anim_Show_Frame_Pause_Counter := 0;
               Anim_State                    := Anim_Show_Frame_Pause;
            else
               Anim_Get_Pieces_Report_Pause_Counter := 0;
               Tubastga_UI_Aux.TAB_For_Pieces_List.Clear (Tubastga_UI_Aux.TAB_For_Pieces);
               for Trav_X in A_Client_Map.Map'First (1) .. A_Client_Map.Map'Last (1) loop
                  for Trav_Y in A_Client_Map.Map'First (2) .. A_Client_Map.Map'Last (2) loop

                     Trav_TAB_Pieces :=
                       Landscape.Pieces_Here_List.First
                         (A_Client_Map.Map (Trav_X, Trav_Y).Pieces_Here);
                     while Landscape.Pieces_Here_List.Has_Element (Trav_TAB_Pieces) loop
                        Tubastga_UI_Aux.TAB_For_Pieces_List.Append
                          (Tubastga_UI_Aux.TAB_For_Pieces,
                           Tubastga_UI_Aux.Type_Piece_Position'
                             (Landscape.Pieces_Here_List.Element (Trav_TAB_Pieces),
                              Hexagon.Type_Hexagon_Position'
                                (True,
                                 A_Client_Map.Map (Trav_X, Trav_Y).Pos.A,
                                 A_Client_Map.Map (Trav_X, Trav_Y).Pos.B),
                              0));

                        Trav_TAB_Pieces := Landscape.Pieces_Here_List.Next (Trav_TAB_Pieces);
                     end loop;
                  end loop;
               end loop;

               declare
                  A_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
                  A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;

                  use Piece.Client_Piece;
               begin

                  -- TODO: The list of Pieces_GUI_Positions and Tab's needs to be
                  -- maintained when pieces are killed or they disappear from
                  -- view.
                  --
                  A_Piece_Id := Get_Selected_Piece (Pieces_GUI_Positions);
                  if A_Piece_Id /= Piece.Undefined_Piece_Id then
                     A_Piece := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

                     if A_Piece /= null then
                        Set_Performing_Piece_Window
                          (The_Window.all.Wnd_Performing_Piece,
                           Button_Pressed_Patch,
                           Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));
                     end if;
                  end if;

               end;

               Anim_State := Anim_Get_Pieces_Report_Pause;
            end if;

      end case;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-callbacks.On_Map_Area_Expose_Event - after setting on map " &
            GNAT.Calendar.Time_IO.Image (Ada.Calendar.Clock, "%s - %i"));

      end if;

      if Button_Event then

         Hexagon.Client_Map.Reset_Visit;

         Gdk.Pixbuf.Fill (All_Pix, Guint32 (0));

         Hexagon.Client_Map.Traverse (A_Client_Map, A_Client_Map.Origo_Patch, -- current origo for
         --this client.
         Gtk_Do_It'Access);

         Hexagon.Client_Map.Reset_Visit;

         Button_Event := False;
      end if;

      if TAB_Selected_Patch /= null then
         Curr_Patch := TAB_Selected_Patch;
      elsif Mouse_Hover_Patch /= null then
         Curr_Patch := Mouse_Hover_Patch;
      end if;

      if Curr_Patch /= null then

         Target_X :=
           Integer (Gdouble (Hexagon.Client_Map.Get_X_From_AB (A_Client_Map, Curr_Patch.all)));
         Target_Y :=
           Integer (Gdouble (Hexagon.Client_Map.Get_Y_From_AB (A_Client_Map, Curr_Patch.all)));

         declare
            use Utilities;
            Trav_Pieces : Landscape.Pieces_Here_List.Cursor;
            Curr_Piece  : Piece.Client_Piece.Type_Client_Piece_Class_Access;
            Curr_Name   : Utilities.RemoteString.Type_String;
         begin
            if Curr_Patch.Pos.P_Valid then
               Set_Text
                 (The_Window.Buffer_Hover_Info,
                  "Information for patch (" &
                  Curr_Patch.Pos.A'Img &
                  ", " &
                  Curr_Patch.Pos.B'Img &
                  ")" &
                  ASCII.LF);

               Trav_Pieces := Landscape.Pieces_Here_List.First (Curr_Patch.Pieces_Here);
               while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop
                  Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
                  Curr_Piece :=
                    Piece.Client_Piece.Find_Piece_In_List (Curr_Patch.Pieces_Here (Trav_Pieces));
                  Curr_Name :=
                    Piece.Get_Name
                      (Piece.Type_Piece (Tubastga_Window_Pkg.Type_Client_Piece (Curr_Piece.all)));

                  Insert
                    (The_Window.Buffer_Hover_Info,
                     Iter,
                     "Player: " &
                     Curr_Piece.Player_Id'Img &
                     " Observed piece: " &
                     Curr_Piece.Type_Of_Piece'Img &
                     " Piece Id:" &
                     Curr_Piece.Id'Img &
                     " " &
                     Utilities.RemoteString.To_String (Curr_Name) &
                     ASCII.LF);
                  Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
               end loop;

               Piece_Selected_Index :=
                 Pieces_Here_Index
                   (Curr_Patch.all,
                    Selected_Position
                      (Curr_Patch.all,
                       (Gint (Float (Mouse_X) / 0.65) - 25),
                       (Gint (Float (Mouse_Y) / 0.65) - 25)));

               if Piece_Selected_Index /= 0 then
                  Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
                  Curr_Piece :=
                    Piece.Client_Piece.Find_Piece_In_List
                      (Landscape.Pieces_Here_List.Element
                         (Curr_Patch.all.Pieces_Here,
                          Piece_Selected_Index));

                  Insert
                    (The_Window.Buffer_Hover_Info,
                     Iter,
                     "Currently selected Piece :" & Curr_Piece.Id'Img & ASCII.LF);

               end if;

               Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
               Insert
                 (The_Window.Buffer_Hover_Info,
                  Iter,
                  "Observed landscape: " & Format_Landscape (Curr_Patch.Landscape_Here) & ASCII.LF);

               Gtk.Text_Buffer.Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
               Gtk.Text_Buffer.Insert
                 (The_Window.Buffer_Hover_Info,
                  Iter,
                  "Patch Effects: " & ASCII.LF);

               Tubastga_Window_Pkg.Effects.Format_Patch_Effects
                 (The_Window.Buffer_Hover_Info,
                  Curr_Patch.all);
            end if;
         end;
      end if; -- Curr_Patch /= null

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-callbacks.On_Map_Area_Expose_Event - Map_Scale=" & Map_Scale'Img);
      end if;

      Gdk.Pixbuf.Fill (Scale_Pix, Guint32 (0));
      Gdk.Pixbuf.Composite
        (All_Pix,
         Scale_Pix,
         Gint (0.0),
         Gint (0.0),
         Gint (700.0),
         Gint (730.0),
         Gdouble (0),
         Gdouble (0),
         Gdouble (Map_Scale),
         Gdouble (Map_Scale),
         Gdk.Pixbuf.Interp_Nearest,
         255);

      Gdk.Cairo.Set_Source_Pixbuf (P_Draw, Scale_Pix, Gdouble (0), Gdouble (0));
      Cairo.Paint (P_Draw);

      Gdk.Pixbuf.Composite
        (All_Minimap_Pix,
         Scale_Minimap_Pix,
         Gint (0),
         Gint (0),
         140,
         360,
         Gdouble (0),
         Gdouble (0),
         Gdouble (1.0),
         Gdouble (1.0),
         Gdk.Pixbuf.Interp_Nearest,
         255);

      Gdk.Cairo.Set_Source_Pixbuf (P_Draw, Scale_Minimap_Pix, Gdouble (500), Gdouble (250));

      Cairo.Paint (P_Draw);

      --  Bold and a bigger font
      Cairo.Set_Source_Rgb (P_Draw, 0.0, 0.0, 1.0);
      Cairo.Move_To (P_Draw, 430.0, 20.0);
      Cairo.Select_Font_Face
        (P_Draw,
         "courier",
         Cairo.Cairo_Font_Slant_Normal,
         Cairo.Cairo_Font_Weight_Bold);
      Cairo.Set_Font_Size (P_Draw, 20.0);
      Cairo.Show_Text (P_Draw, "Tubast'ga");
      --
      -- Helper lines....
      --        Cairo.Move_To (P_Draw, Gdouble (36.0 / 0.65 * Map_Scale), Gdouble (0));
      --        Cairo.Line_To (P_Draw, Gdouble (36.0 / 0.65 * Map_Scale), Gdouble (700));
      --        Cairo.Move_To (P_Draw, Gdouble (0), Gdouble (682.0 / 0.65 * Map_Scale));
      --        Cairo.Line_To (P_Draw, Gdouble (700), Gdouble (682.0 / 0.65 * Map_Scale));
      -- minimap
      --        Cairo.Move_To (P_Draw, Gdouble (510), Gdouble (0));
      --        Cairo.Line_To (P_Draw, Gdouble (510), Gdouble (700));
      --        Cairo.Move_To (P_Draw, Gdouble (0), Gdouble (600));
      --        Cairo.Line_To (P_Draw, Gdouble (700), Gdouble (600));

      Cairo.Stroke (P_Draw);

      return True;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Tubastga_Window_Pkg-callbacks.On_Map_Area_Expose_Event - exception");
         return True;
   end On_Map_Area_Expose_Event;

   procedure Scroll_Ongoing (P_Scroll_Direction : in Type_Scroll_Direction) is

      A, B : Hexagon.Type_Hexagon_Numbers;

      use Piece;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-Callbacks.Scroll_Ongoing - enter Button_Pressed_X=" &
            Button_Pressed_X'Img &
            " Button_Pressed_Y=" &
            Button_Pressed_Y'Img &
            " Mouse_X=" &
            Mouse_X'Img &
            " Mouse_Y=" &
            Mouse_Y'Img &
            " - Origo_Patch.Pos.A=" &
            A_Client_Map.Origo_Patch.Pos.A'Img &
            " Origo_Patch.Pos.B=" &
            A_Client_Map.Origo_Patch.Pos.B'Img);
      end if;

      A := A_Client_Map.Origo_Patch.Pos.A;
      B := A_Client_Map.Origo_Patch.Pos.B;
      --
      if P_Scroll_Direction = Up and A_Client_Map.Origo_Patch.Pos.B < 100 then
         B := A_Client_Map.Origo_Patch.Pos.B + 1;
      elsif P_Scroll_Direction = Down and A_Client_Map.Origo_Patch.Pos.B > 1 then
         B := A_Client_Map.Origo_Patch.Pos.B - 1;
      elsif P_Scroll_Direction = Right and A_Client_Map.Origo_Patch.Pos.A < 100 then
         A := A_Client_Map.Origo_Patch.Pos.A + 1;
      elsif P_Scroll_Direction = Left and A_Client_Map.Origo_Patch.Pos.A > 1 then
         A := A_Client_Map.Origo_Patch.Pos.A - 1;
      end if;

      Hexagon.Client_Map.Set_Origo_Patch (A_Client_Map, A, B);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg-Callbacks.Scroll_Ongoing - exit");
      end if;
   end Scroll_Ongoing;

   procedure Place_Piece is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;

      use Ada.Containers;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-Callbacks.Place_Piece - enter Tubastga_UI_Aux.UI_State=" &
            Tubastga_UI_Aux.UI_State'Img);
      end if;

      -- and user clicks a tile
      -- now try to place the piece
      --
      Hexagon.Client_Map.Unselect_All_Patches (A_Client_Map);
      Hexagon.Client_Map.Select_Patch (Button_Pressed_Patch.all);

      if Landscape.Pieces_Here_List.Length (Button_Pressed_Patch.all.Pieces_Here) = 0 then
         Piece.Client_Piece.Create_Piece
           (Action.Type_Action_Type (1),
            A_Piece,
            Tubastga_UI_Aux.Convert_UI_State_To_Piece (Tubastga_UI_Aux.UI_State),
            Tubastga_UI_Aux.Convert_UI_State_To_Category (Tubastga_UI_Aux.UI_State),
            Landscape.Type_Patch (Button_Pressed_Patch.all),
            Me_Player_Id,
            Ret_Status);
      end if;

      if Ret_Status = Status.Ok then
         Tubastga_Window_Pkg.Sounds.Play_Placed_Piece;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg-Callbacks.Place_Piece - exit");
      end if;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Tubastga_Window_Pkg-callbacks.Place_Piece - exception");
      when System.RPC.Communication_Error =>
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-callbacks.Place_Piece - System.RPC.Communication_Error");

   end Place_Piece;

   function On_Map_Area_Motion_Notify_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);
      X    : Gdouble;
      Y    : Gdouble;

      use Tubastga_UI_Aux;
      use Hexagon.Client_Map;
   begin
      Get_Coords (Arg1, X, Y);
      Mouse_X := GUI_X_To_Game_X (X);
      Mouse_Y := GUI_Y_To_Game_Y (Y);
      tmp_X   := Integer (X);
      tmp_Y   := Integer (Y);

      Mouse_Hover_Patch :=
        Hexagon.Client_Map.Get_Patch_Adress_From_XY
          (A_Client_Map,
           Integer (Mouse_X),
           Integer (Mouse_Y));
      TAB_Selected_Patch := null;

      return True;
   end On_Map_Area_Motion_Notify_Event;

   function On_Map_Area_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1       : Gdk_Event := To_Event (Params, 1);
      X          : Gdouble;
      Y          : Gdouble;
      A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      A_Piece_Id : Piece.Type_Piece_Id;

      Piece_Selected_Index : Integer := 0;

      function Next_Select_Area_Cycle
        (P_Current_Select_Area : in Type_Selected_Area_Cycle) return Type_Selected_Area_Cycle
      is
         Selected_Area_Cycle : Type_Selected_Area_Cycle;
      begin
         if P_Current_Select_Area = Reachable then
            Selected_Area_Cycle := Attackable;
         else
            Selected_Area_Cycle := Reachable;
         end if;

         return Selected_Area_Cycle;
      end Next_Select_Area_Cycle;

      procedure Set_Piece_Selected
        (P_Piece_Selected_Index : in     Integer;
         P_Piece_Id             :    out Piece.Type_Piece_Id)
      is
         Pieces_GUI_Cursor : Pieces_GUI_List.Cursor;
         GUI_Piece         : Type_GUI_Piece;
      begin
         if P_Piece_Selected_Index /= 0 then
            P_Piece_Id :=
              Landscape.Pieces_Here_List.Element
                (Button_Pressed_Patch.all.Pieces_Here,
                 P_Piece_Selected_Index);

            Pieces_GUI_Cursor     := Pieces_GUI_List.Find (Pieces_GUI_Positions, P_Piece_Id);
            GUI_Piece             := Pieces_GUI_List.Element (Pieces_GUI_Cursor);
            GUI_Piece.Draw_Action := Selected;
            Pieces_GUI_List.Include (Pieces_GUI_Positions, P_Piece_Id, GUI_Piece);

         else
            P_Piece_Id := Piece.Undefined_Piece_Id;
         end if;
      end Set_Piece_Selected;

      procedure Set_Patches_Reachable
        (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in     Hexagon.Client_Map.Type_Client_Patch;
         P_Piece      : in     Tubastga_Window_Pkg.Type_Client_Piece)
      is
         Patch_Area            : Hexagon.Client_Map.Type_Client_Patch_Area_Access;
         Movement_Capabilities : Hexagon.Area.Client_Area.Type_Action_Capabilities_Access;

         use Hexagon.Client_Map;
      begin
         Tubastga_Window_Pkg.Sounds.Play_Set_Patches_Reachable;

         Movement_Capabilities :=
           Piece.Client_Piece.Movement_Capability (Piece.Type_Piece (P_Piece));
         Patch_Area :=
           Hexagon.Client_Map.Capability_To_Area (P_Client_Map, P_Patch, Movement_Capabilities);
         if Patch_Area /= null then
            Hexagon.Client_Map.Set_Reachable (P_Client_Map, Patch_Area.all);
         end if;
      end Set_Patches_Reachable;

      procedure Set_Patches_Attackable
        (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
         P_Patch      : in     Hexagon.Client_Map.Type_Client_Patch;
         P_Piece      : in     Tubastga_Window_Pkg.Type_Client_Piece)
      is
         Patch_Area          : Hexagon.Client_Map.Type_Client_Patch_Area_Access;
         Attack_Capabilities : Hexagon.Area.Client_Area.Type_Action_Capabilities_Access;

         use Hexagon.Client_Map;
      begin
         Tubastga_Window_Pkg.Sounds.Play_Set_Patches_Attackable;

         Attack_Capabilities := Piece.Client_Piece.Attack_Capability (Piece.Type_Piece (P_Piece));
         Patch_Area          :=
           Hexagon.Client_Map.Capability_To_Area (P_Client_Map, P_Patch, Attack_Capabilities);
         if Patch_Area /= null then
            Hexagon.Client_Map.Set_Attackable (P_Client_Map, Patch_Area.all);
         end if;
      end Set_Patches_Attackable;

      --
      procedure Draw_Minimap
        (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
         P_X, P_Y     : in     Gdouble)

      is
         Minimap_X, Minimap_Y : Integer;
         Minimap_Patch        : Hexagon.Client_Map.Type_Client_Patch_Adress;
      begin
         Minimap_X := Integer (Float (GUI_X_To_Minimap_X (P_X)));
         Minimap_Y := Integer (Float (GUI_Y_To_Minimap_Y (P_Y)));

         Minimap_Patch :=
           Hexagon.Client_Map.Get_Patch_Adress_From_Absolute_XY
             (P_Client_Map,
              Minimap_X,
              Minimap_Y);

         if Verbose then
            Text_IO.Put_Line
              ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Button_Press_Event - minimap Minimap_X=" &
               Minimap_X'Img &
               " Minimap_Y=" &
               Minimap_Y'Img &
               " A=" &
               Minimap_Patch.all.Pos.A'Img &
               " B=" &
               Minimap_Patch.all.Pos.B'Img);
         end if;

         Hexagon.Client_Map.Set_Origo_Patch
           (P_Client_Map,
            Hexagon.Type_Hexagon_Numbers (Minimap_Patch.all.Pos.A),
            Hexagon.Type_Hexagon_Numbers (Minimap_Patch.all.Pos.B));

      end Draw_Minimap;
      --

      use Piece;
      use Tubastga_UI_Aux;
      use Hexagon.Client_Map;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Button_Press_Event - enter");
      end if;

      if Me_Player_Id = 0 then
         return True;
      end if;

      Get_Coords (Arg1, X, Y);

      if Gdouble (X) > Gdouble (500) and Gdouble (Y) > Gdouble (340) then
         Draw_Minimap (A_Client_Map, X, Y);
         Button_Event := True;
      else
         if Verbose then
            Text_IO.Put_Line
              ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Button_Press_Event - Mainmap");
         end if;

         Hexagon.Client_Map.Reset_Draw_Action (A_Client_Map);
         Reset_Pieces_GUI_List;

         if Y < Gdouble (Pieces_Menu_Y) then
            -- try to find selected tiles in game board

            Button_Pressed_X := GUI_X_To_Game_X (X);
            Button_Pressed_Y := GUI_Y_To_Game_Y (Y);

            Button_Pressed_Patch :=
              Hexagon.Client_Map.Get_Patch_Adress_From_XY
                (A_Client_Map,
                 Button_Pressed_X,
                 Button_Pressed_Y);

            Piece_Selected_Index :=
              Pieces_Here_Index
                (Button_Pressed_Patch.all,
                 Selected_Position
                   (Button_Pressed_Patch.all,
                    (Gint (Float (X) / 0.65) - 25),
                    (Gint (Float (Y) / 0.65) - 25)));

            if Verbose then
               Text_IO.Put_Line
                 ("Tubastga_Window_Pkg.Callbacks.On_Map_Area_Button_Press_Event - Piece_Selected_Index=" &
                  Piece_Selected_Index'Img);
            end if;
            --
            Set_Piece_Selected (Piece_Selected_Index, A_Piece_Id);
            --

            if Get_Button (Arg1) = Left_Mouse_Button then

               if A_Piece_Id /= Piece.Undefined_Piece_Id then
                  A_Piece := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

                  Set_Performing_Piece_Window
                    (The_Window.all.Wnd_Performing_Piece,
                     Button_Pressed_Patch,
                     Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));

                  Selected_Area_Cycle := Next_Select_Area_Cycle (Selected_Area_Cycle);

                  if Selected_Area_Cycle = Reachable then
                     Set_Patches_Reachable
                       (A_Client_Map,
                        Button_Pressed_Patch.all,
                        Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));
                  end if;

                  if Selected_Area_Cycle = Attackable then
                     Set_Patches_Attackable
                       (A_Client_Map,
                        Button_Pressed_Patch.all,
                        Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));
                  end if;

               -- mark the reachable tiles so that they will be marked by the
               --redrawing
               else
                  Set_Performing_Piece_Window
                    (The_Window.all.Wnd_Performing_Piece,
                     Button_Pressed_Patch,
                     Tubastga_Window_Pkg.Type_Client_Piece'
                       (Piece.Undefined_Piece_Id,
                        Piece.Undefined_Piece_Type,
                        Piece.Fighting_Piece,
                        Utilities.RemoteString.To_Unbounded_String (""),
                        0,
                        Effect.Effect_List.Empty_Map,
                        0,
                        Goods.Type_Storage'
                          (P_Number_Of_Slots => 3,
                           Slots             =>
                             (1 => Goods.Type_Goods_Info'(Goods.None, 0),
                              2 => Goods.Type_Goods_Info'(Goods.None, 0),
                              3 => Goods.Type_Goods_Info'(Goods.None, 0))),
                        Stops   => (others => Piece.Undefined_Piece_Id),
                        Load    => (others => Goods.None),
                        Unload  => (others => Goods.None),
                        Captain => False));
               end if;

            elsif Get_Button (Arg1) = Right_Mouse_Button then

               if A_Piece_Id /= Piece.Undefined_Piece_Id then
                  Set_Target_Piece_Window
                    (The_Window.all.Wnd_Target,
                     Button_Pressed_Patch,
                     Integer
                       (Landscape.Pieces_Here_List.Element
                          (Button_Pressed_Patch.all.Pieces_Here,
                           Piece_Selected_Index)));
               else
                  Set_Target_Piece_Window (The_Window.all.Wnd_Target, Button_Pressed_Patch, 0);
               end if;

            end if;

            -- are we trying to place a piece?
            if Tubastga_UI_Aux.UI_State in
                Tubastga_UI_Aux.Place_Sentry .. Tubastga_UI_Aux.Place_Stonecutter
            then
               Place_Piece;
               Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Done;
            end if;

            Button_Event := True;
         end if;

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Button_Press_Event - exit");
      end if;

      return True;
   end On_Map_Area_Button_Press_Event;

   function On_Map_Area_Scroll_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);

      use Tubastga_UI_Aux;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Scroll_Event - enter");
      end if;

      if Arg1.Scroll.Direction = Scroll_Up then
         Map_Scale := Map_Scale + 0.025;
      end if;
      if Arg1.Scroll.Direction = Scroll_Down then
         Map_Scale := Map_Scale - 0.025;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Scroll_Event - exit");
      end if;

      return False;
   end On_Map_Area_Scroll_Event;

   procedure On_Button_End_Turn (Object : access Gtk_Tool_Button_Record'Class) is
      Ret : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_End_Turn - clicked");
      end if;

      Ret := Client.Server_Adm.End_Turn (Me_Player_Id);

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Tubastga_Window_Pkg-callbacks.On_Button_End_Turn - exception");
      when System.RPC.Communication_Error =>
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-callbacks.On_Button_End_Turn - System.RPC.Communication_Error");

   end On_Button_End_Turn;

   procedure On_Button_Place_Sentry (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Place_Sentry - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Sentry;

   end On_Button_Place_Sentry;

   procedure On_Button_Place_Bowman (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Place_Bowman - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Bowman;

   end On_Button_Place_Bowman;

   procedure On_Button_Place_Carrier (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Place_Carrier - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Carrier;

   end On_Button_Place_Carrier;

   procedure On_Button_Place_Ship (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Place_Ship - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Ship;
   end On_Button_Place_Ship;

   procedure On_Button_Place_Farm (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Place_Farm - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Farm;
   end On_Button_Place_Farm;

   procedure On_Button_Place_Tower (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Tower - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Tower;
   end On_Button_Place_Tower;

   procedure On_Button_Place_Lumberjack (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Lumberjack - clicked");
      end if;
      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Lumberjack;

   end On_Button_Place_Lumberjack;

   procedure On_Button_Place_Knight (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Knight - clicked");
      end if;
      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Knight;

   end On_Button_Place_Knight;

   procedure On_Button_Place_Stonecutter (Object : access Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Place_Stonecutter - clicked");
      end if;
      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Stonecutter;
   end On_Button_Place_Stonecutter;

   procedure On_Button_Wall1 (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Button_Wall1 - clicked piece_id=" &
            The_Window.all.Wnd_Performing_Piece.Selected_Piece.Id'Img);
      end if;

      Piece.Client_Piece.Perform_Construction
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall1,
         Current_Player_Id,
         Ret_Status);
      Text_IO.Put_Line ("Building Wall1 Ret_Status=" & Ret_Status'Img);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall1;
   end On_Button_Wall1;

   procedure On_Button_Wall2 (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall2 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall2,
         Current_Player_Id,
         Ret_Status);
      Text_IO.Put_Line ("Building Wall1 Ret_Status=" & Ret_Status'Img);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall2;
   end On_Button_Wall2;

   procedure On_Button_Wall3 (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall3 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall3,
         Current_Player_Id,
         Ret_Status);
      Text_IO.Put_Line ("Building Wall1 Ret_Status=" & Ret_Status'Img);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall3;
   end On_Button_Wall3;

   procedure On_Button_Wall4 (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall4 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall4,
         Current_Player_Id,
         Ret_Status);
      Text_IO.Put_Line ("Building Wall1 Ret_Status=" & Ret_Status'Img);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall4;
   end On_Button_Wall4;

   procedure On_Button_Wall5 (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall5 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall5,
         Current_Player_Id,
         Ret_Status);
      Text_IO.Put_Line ("Building Wall1 Ret_Status=" & Ret_Status'Img);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall5;
   end On_Button_Wall5;

   procedure On_Button_Wall6 (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall6 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall6,
         Current_Player_Id,
         Ret_Status);
      Text_IO.Put_Line ("Building Wall1 Ret_Status=" & Ret_Status'Img);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall6;
   end On_Button_Wall6;

   procedure On_Button_Move (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;

      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Move - clicked");
      end if;

      Piece.Client_Piece.Perform_Move
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.all.Selected_Patch.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.all.Selected_Patch.all),
         Me_Player_Id,
         Ret_Status);

   end On_Button_Move;

   procedure On_Button_Attack (Object : access Gtk_Button_Record'Class) is
      Winner     : Player.Type_Player_Id := 0;
      Ret_Status : Status.Type_Status;

      use Piece;
      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Attack - clicked");
      end if;

      if The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.Type_Of_Piece /=
        Tubastga_Piece.Bowman_Piece
      then
         Piece.Client_Piece.Perform_Attack
           (Action.Type_Action_Type (1),
            Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
            Piece.Type_Piece (The_Window.all.Wnd_Target.all.Selected_Piece.all),
            Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.all.Selected_Patch.all),
            Landscape.Type_Patch (The_Window.all.Wnd_Target.all.Selected_Patch.all),
            Me_Player_Id,
            Winner,
            Ret_Status);
      else
         Piece.Client_Piece.Perform_Ranged_Attack
           (Action.Type_Action_Type (1),
            Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
            Piece.Type_Piece (The_Window.all.Wnd_Target.all.Selected_Piece.all),
            Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.all.Selected_Patch.all),
            Landscape.Type_Patch (The_Window.all.Wnd_Target.all.Selected_Patch.all),
            Me_Player_Id,
            Winner,
            Ret_Status);
      end if;

   end On_Button_Attack;

   procedure On_Button_Promote (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Promote - clicked");
      end if;

      Piece.Client_Piece.Grant_Piece_Effect
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Effect.Type_Effect'(Tubastga_Piece.Effect_Captain, 1),
         Me_Player_Id,
         Ret_Status);

   end On_Button_Promote;

   procedure On_Button_Demote (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Demote - clicked");
      end if;

      Piece.Client_Piece.Revoke_Piece_Effect
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Effect.Type_Effect'(Tubastga_Piece.Effect_Captain, 1),
         Me_Player_Id,
         Ret_Status);

   end On_Button_Demote;

   procedure On_Button_Search (Object : access Gtk_Button_Record'Class) is
      Ret_Status    : Status.Type_Status;
      Effect_Cursor : Effect.Effect_List.Cursor;
      An_Effect     : Effect.Type_Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Search - clicked");
      end if;

      Effect_Cursor :=
        Effect.Effect_List.Find
          (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all.Effects_Here,
           Tubastga_Piece.Effect_Treasure);
      An_Effect := Effect.Effect_List.Element (Effect_Cursor);
      Text_IO.Put_Line ("Effect=" & An_Effect.Effect_Name'Img & " Aux=" & An_Effect.Aux'Img);

      Piece.Client_Piece.Perform_Patch_Effect
        (Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
         An_Effect,
         Hexagon.Area.Type_Action_Capabilities_A'
           (1 => The_Window.all.Wnd_Performing_Piece.Selected_Patch.all.Pos),
         Me_Player_Id,
         Ret_Status);

   end On_Button_Search;

   procedure On_Button_Create_Path (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;

      Area_Count : Positive;

      use Hexagon.Client_Map;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Create_Path - clicked");
      end if;

      -- find all patches that are selected on map:
      Area_Count := 1;
      for Trav_A in A_Client_Map.Map'First (1) .. A_Client_Map.Map'Last (1) loop
         for Trav_B in A_Client_Map.Map'First (2) .. A_Client_Map.Map'Last (2) loop

            if A_Client_Map.Map (Trav_A, Trav_B).all.Draw_Action = Selected_Area then
               Area_Count := Area_Count + 1;
            end if;
         end loop;
      end loop;

      declare
         The_Area   : Hexagon.Area.Type_Action_Capabilities_A (1 .. Area_Count);
         Area_Index : Integer;
      begin
         -- find all patches that are selected on map:
         Area_Index := 1;
         for Trav_A in A_Client_Map.Map'First (1) .. A_Client_Map.Map'Last (1) loop
            for Trav_B in A_Client_Map.Map'First (2) .. A_Client_Map.Map'Last (2) loop
               if A_Client_Map.Map (Trav_A, Trav_B).all.Draw_Action = Selected_Area then
                  The_Area (Area_Index) :=
                    Hexagon.Type_Hexagon_Position'
                      (True,
                       A_Client_Map.Map (Trav_A, Trav_B).Pos.A,
                       A_Client_Map.Map (Trav_A, Trav_B).Pos.B);
                  Area_Index := Area_Index + 1;
               end if;
            end loop;
         end loop;

         Piece.Client_Piece.Grant_Patch_Effect
           (Action.Type_Action_Type (1),
            Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
            Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
            Effect.Type_Effect'
              (Tubastga_Piece.Effect_Path,
               Integer (Me_Player_Id) * 1000000 +
               Integer (The_Window.all.Wnd_Performing_Piece.Selected_Piece.Id) * 10 +
               0),
            The_Area,
            Me_Player_Id,
            Ret_Status);
      end;

   end On_Button_Create_Path;

   procedure On_Button_Remove_Path (Object : access Gtk_Button_Record'Class) is
      Ret_Status : Status.Type_Status;

      use Hexagon.Client_Map;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Remove_Path - clicked");
      end if;

      declare
         The_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1);
      begin
         The_Area (1) :=
           Hexagon.Type_Hexagon_Position'
             (True,
              The_Window.all.Wnd_Performing_Piece.Selected_Patch.all.Pos.A,
              The_Window.all.Wnd_Performing_Piece.Selected_Patch.all.Pos.B);

         Text_IO.Put_Line ("Grant patch effect=");
         Piece.Client_Piece.Grant_Patch_Effect
           (Action.Type_Action_Type (1),
            Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
            Landscape.Type_Patch (The_Window.all.Wnd_Performing_Piece.Selected_Patch.all),
            Effect.Type_Effect'
              (Tubastga_Piece.Effect_Path,
               (Integer (Me_Player_Id) * 1000000 +
                Integer (The_Window.all.Wnd_Performing_Piece.Selected_Piece.Id) * 10 +
                1)),
            The_Area,
            Me_Player_Id,
            Ret_Status);
      end;

   end On_Button_Remove_Path;

   procedure On_Button_Game (Object : access Gtk_Tool_Button_Record'Class) is
      GameOption : Gtk.Dialog.Gtk_Response_Type;
      use Gtk.Dialog;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Game - clicked");
      end if;

      GameOption := Gtk.Dialog.Run (Gtk.Dialog.Gtk_Dialog (The_Window.all.dlgMainMenu));
      Gtk.Dialog.Hide (Gtk.Dialog.Gtk_Dialog (The_Window.all.dlgMainMenu));

   end On_Button_Game;

   function On_Keyboard_Key_Press
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);

      use Piece.Client_Piece.Pieces_Client_List;
      use Piece;
   begin

      case Get_Key_Val (Arg1) is
         when GDK_Tab =>
            -- very first "tab"- start at beginning
            if Current_Piece_Id = 0 then
               Current_TAB_Cursor :=
                 Tubastga_UI_Aux.TAB_For_Pieces_List.First (Tubastga_UI_Aux.TAB_For_Pieces);
            else
               Current_TAB_Cursor := Tubastga_UI_Aux.TAB_For_Pieces_List.Next (Current_TAB_Cursor);
            end if;

            if not Tubastga_UI_Aux.TAB_For_Pieces_List.Has_Element (Current_TAB_Cursor) then
               Current_TAB_Cursor :=
                 Tubastga_UI_Aux.TAB_For_Pieces_List.First (Tubastga_UI_Aux.TAB_For_Pieces);
            end if;

            Current_Piece_Id :=
              Integer
                (Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Piece_Id);

            Hexagon.Client_Map.Reset_Draw_Action (A_Client_Map);
            Reset_Pieces_GUI_List;

            Button_Pressed_Patch :=
              Hexagon.Client_Map.Get_Patch_Adress_From_AB
                (A_Client_Map,
                 Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.A,
                 Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.B);
            Mouse_Hover_Patch  := null;
            TAB_Selected_Patch := Button_Pressed_Patch;

            declare
               TAB_Cursor : Pieces_GUI_List.Cursor;

               N : Type_GUI_Piece;
            begin
               TAB_Cursor :=
                 Pieces_GUI_List.Find
                   (Pieces_GUI_Positions,
                    Piece.Type_Piece_Id (Current_Piece_Id));

               N := Pieces_GUI_List.Element (TAB_Cursor);

               N.Draw_Action := Selected;

               Pieces_GUI_List.Include
                 (Pieces_GUI_Positions,
                  Piece.Type_Piece_Id (Current_Piece_Id),
                  N);
            end;

            -- Crude scroll functinality to get the selected piece into view.
            declare
               --
               A_Min, B_Min, A_Max, B_Max, Origo_A, Origo_B, A, B : Integer;
            begin
               A :=
                 Integer
                   (Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.A);
               B :=
                 Integer
                   (Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.B);

               A_Min :=
                 Integer
                   (Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.A) -
                 3;

               B_Min :=
                 Integer
                   (Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.B) -
                 3;
               A_Max :=
                 Integer
                   (Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.A) +
                 3;
               B_Max :=
                 Integer
                   (Tubastga_UI_Aux.TAB_For_Pieces_List.Element (Current_TAB_Cursor).Actual_Pos.B) +
                 3;

               Origo_A := Integer (A_Client_Map.Origo_Patch.all.Pos.A);
               Origo_B := Integer (A_Client_Map.Origo_Patch.all.Pos.B);

               if A_Min < Integer (A_Client_Map.Origo_Patch.all.Pos.A) then
                  Origo_A := A_Min;
               end if;

               if B_Min < Integer (A_Client_Map.Origo_Patch.all.Pos.B) then
                  Origo_B := B_Min;
               end if;

               if A_Max > Integer (A_Client_Map.Origo_Patch.all.Pos.A) + 20 then
                  Origo_A := Integer (A_Client_Map.Origo_Patch.all.Pos.A) + A - 3;
               end if;

               if B_Max > Integer (A_Client_Map.Origo_Patch.all.Pos.B) + 15 then
                  Origo_B := Integer (A_Client_Map.Origo_Patch.all.Pos.B) + B - 3;
               end if;

               if Origo_A < 1 then
                  Origo_A := 1;
               end if;
               if Origo_B < 1 then
                  Origo_B := 1;
               end if;

               Hexagon.Client_Map.Set_Origo_Patch
                 (A_Client_Map,
                  Hexagon.Type_Hexagon_Numbers (Origo_A),
                  Hexagon.Type_Hexagon_Numbers (Origo_B));
            end;

         when GDK_Up =>
            Scroll_Ongoing (Up);

         when GDK_Down =>
            Scroll_Ongoing (Down);

         when GDK_Left =>
            Scroll_Ongoing (Left);

         when GDK_Right =>
            Scroll_Ongoing (Right);

         when GDK_space =>
            begin
               Space_Pressed_X := Mouse_X;
               Space_Pressed_Y := Mouse_Y;

               Space_Pressed_Patch :=
                 Hexagon.Client_Map.Get_Patch_Adress_From_XY
                   (A_Client_Map,
                    Space_Pressed_X,
                    Space_Pressed_Y);
               Hexagon.Client_Map.Select_Patch_Area (Space_Pressed_Patch.all);
            end;

         when GDK_Home =>
            null;

         when GDK_Page_Up =>
            null;

         when GDK_End =>
            null;

         when GDK_Page_Down =>
            null;

         when GDK_uparrow =>
            Scroll_Ongoing (Up);

         when GDK_downarrow =>
            Scroll_Ongoing (Down);

         when GDK_leftarrow =>
            Scroll_Ongoing (Left);

         when GDK_rightarrow =>
            Scroll_Ongoing (Right);

         when GDK_Escape =>
            Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Done;

         when others =>
            null;
      end case;

      Button_Event := True;

      return True;
   end On_Keyboard_Key_Press;

   procedure Refresh_Server_Configuration is
      Adm_Status                                           : Status.Type_Adm_Status;
      Server_Info                                          : Utilities.RemoteString_List.Vector;
      Connect, Create, Save, Load, Join, Leave, Disconnect : Boolean;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.Refresh_Server_Configuration - enter");
      end if;

      Utilities.RemoteString_List.Clear (Server_Info);
      Client.Server_Adm.Get_Server_Info (Server_Info, Adm_Status);
      if Adm_Status = Status.Adm_Ok then
         DlgGame_Populate (Server_Info, Connect, Create, Save, Load, Join, Leave, Disconnect);
         Gtk.Combo_Box_Text.Set_Active (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map, 0);
         Gtk.Combo_Box_Text.Set_Active (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name, 0);
         Gtk.Combo_Box_Text.Set_Active (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name, 0);

         Gtk.Button.Set_Sensitive (The_Window.all.dlgMainMenu.Btn_Connect_Server, Connect);
         Gtk.Button.Set_Sensitive (The_Window.all.dlgMainMenu.Btn_Disconnect_Server, Disconnect);
         Gtk.Button.Set_Sensitive (The_Window.all.dlgMainMenu.Btn_Create_Game, Create);
         Gtk.Button.Set_Sensitive (The_Window.all.dlgMainMenu.Btn_Join_Game, Join);
         Gtk.Button.Set_Sensitive (The_Window.all.dlgMainMenu.Btn_Leave_Game, Leave);
         Gtk.Button.Set_Sensitive (The_Window.all.dlgMainMenu.Btn_Save_Game, Save);
         Gtk.Button.Set_Sensitive (The_Window.all.dlgMainMenu.Btn_Load_Game, Load);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.Refresh_Server_Configuration - exit");
      end if;

   end Refresh_Server_Configuration;

   procedure On_Spin_Button_Change (Object : access Gtk_Spin_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Spin_Button_Change - clicked");
      end if;
      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map);
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map, 0);

      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name);
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name, 0);

      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name);
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name, 0);

      Refresh_Server_Configuration;
   end On_Spin_Button_Change;

   procedure On_Button_Connect (Object : access Gtk_Button_Record'Class) is
      TCPIP_Octet1, TCPIP_Octet2, TCPIP_Octet3, TCPIP_Octet4 : Natural;
      Server_Connection : Utilities.RemoteString.Type_String;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Connect - clicked");
      end if;
      TCPIP_Octet1 := Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet1));
      TCPIP_Octet2 := Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet2));
      TCPIP_Octet3 := Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet3));
      TCPIP_Octet4 := Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet4));

      Server_Connection := Utilities.RemoteString.To_Unbounded_String
          (Ada.Strings.Fixed.Trim (TCPIP_Octet1'Img, Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim (TCPIP_Octet2'Img, Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim (TCPIP_Octet3'Img, Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim (TCPIP_Octet4'Img, Ada.Strings.Left));

      Client.Server_Adm.Connect
        (Server_Connection,
         Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPPort)));

      Refresh_Server_Configuration;
   end On_Button_Connect;

   procedure On_Button_Disconnect (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Disconnect - clicked");
      end if;

      Client.Server_Adm.Client_Stopped (Me_Player_Id);
      Client.Server_Adm.Disconnect;

      Refresh_Server_Configuration;
   end On_Button_Disconnect;

   procedure On_Button_Refresh (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Refresh - clicked");
      end if;

      Refresh_Server_Configuration;

   end On_Button_Refresh;

   procedure On_Button_Create_Game (Object : access Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Create_Game - clicked");
      end if;

      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.dlgMainMenu.En_Create_Game_Player_Name_1)));
      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.dlgMainMenu.En_Create_Game_Player_Name_2)));
      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.dlgMainMenu.En_Create_Game_Player_Name_3)));
      Client.Server_Adm.Create_Game
        (Utilities.RemoteString.To_Unbounded_String
           (Gtk.Combo_Box_Text.Get_Active_Text
              (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Map)),
         Player_Name_List,
         Adm_Status);

      Refresh_Server_Configuration;
   end On_Button_Create_Game;

   procedure On_Button_Save_Game (Object : access Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;

      use Utilities.RemoteString;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Save_Game - clicked");
      end if;

      Client.Server_Adm.Save_Game
        (Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.dlgMainMenu.En_Save_Game_Name)),
         Adm_Status);

      Refresh_Server_Configuration;
   end On_Button_Save_Game;

   procedure On_Button_Load_Game (Object : access Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;

      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Load_Game - clicked");
      end if;

      Client.Server_Adm.Load_Game
        (Utilities.RemoteString.To_Unbounded_String
           (Gtk.Combo_Box_Text.Get_Active_Text (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name)),
         Adm_Status);

      Refresh_Server_Configuration;
   end On_Button_Load_Game;

   procedure On_Button_Join_Game (Object : access Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;
      My_Name    : Utilities.RemoteString.Type_String;

      use Status;
      use Utilities.RemoteString;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Join_Game - clicked - enter");
      end if;

      My_Name :=
        Utilities.RemoteString.To_Unbounded_String
          (Gtk.Combo_Box_Text.Get_Active_Text (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name));

      Me_Player_Id := Client.Server_Adm.Join_Game (My_Name, Adm_Status);
      if Adm_Status = Status.Adm_Ok then
         Hexagon.Client_Map.Get_Map (Me_Player_Id, A_Client_Map);
      end if;
      if Me_Player_Id = Player.Type_Player_Id (1) then
         Set_Text (The_Window.all.Lbl_Player_1_Name, Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (2) then
         Set_Text (The_Window.all.Lbl_Player_2_Name, Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (3) then
         Set_Text (The_Window.all.Lbl_Player_3_Name, Utilities.RemoteString.To_String (My_Name));
      end if;

      Set_Title
        (The_Window,
         UTF8_String'("Tubast'ga - " & Utilities.RemoteString.To_String (My_Name)));
      Set_Title
        (The_Window.Wnd_Performing_Piece,
         UTF8_String'
           ("Tubast'ga - Performing Piece - " & Utilities.RemoteString.To_String (My_Name)));
      Set_Title
        (The_Window.Wnd_Target,
         UTF8_String'("Tubast'ga - Target - " & Utilities.RemoteString.To_String (My_Name)));

      Refresh_Server_Configuration;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Button_Join_Game - clicked - exit Me_Player_Id=" &
            Me_Player_Id'Img);
      end if;
   end On_Button_Join_Game;

   procedure On_Button_Leave_Game (Object : access Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;
      My_Name    : Utilities.RemoteString.Type_String;

      use Status;
      use Utilities.RemoteString;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Leave_Game - clicked - enter");
      end if;

      My_Name :=
        Utilities.RemoteString.To_Unbounded_String
          (Utilities.RemoteString.To_String (Player_Name_List (Integer (Me_Player_Id))));

      Client.Server_Adm.Leave_Game (Me_Player_Id, My_Name, Adm_Status);
      if Adm_Status = Status.Adm_Ok then
         Hexagon.Client_Map.Get_Map (Me_Player_Id, A_Client_Map);
      end if;
      if Me_Player_Id = Player.Type_Player_Id (1) then
         Set_Text (The_Window.all.Lbl_Player_1_Name, Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (2) then
         Set_Text (The_Window.all.Lbl_Player_2_Name, Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (3) then
         Set_Text (The_Window.all.Lbl_Player_3_Name, Utilities.RemoteString.To_String (My_Name));
      end if;

      Set_Title (The_Window, UTF8_String'("Tubast'ga"));
      Set_Title (The_Window.Wnd_Performing_Piece, UTF8_String'("Tubast'ga - Performing Piece"));
      Set_Title (The_Window.Wnd_Target, UTF8_String'("Tubast'ga - Target"));

      Refresh_Server_Configuration;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Button_Leave_Game - clicked - exit Me_Player_Id=" &
            Me_Player_Id'Img);
      end if;
   end On_Button_Leave_Game;

   procedure On_Button_Close (Object : access Gtk_Button_Record'Class) is
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Close - clicked");
      end if;
      Gtk.Dialog.Response
        (Gtk.Dialog.Gtk_Dialog (The_Window.all.dlgMainMenu),
         Gtk.Dialog.Gtk_Response_OK);

   end On_Button_Close;

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class) is
   begin
      Client.Server_Adm.Client_Stopped (Me_Player_Id);
      Client.Server_Adm.Disconnect;
      Destroy (Object);
      Gtk.Main.Main_Quit;
   end Exit_Main;

begin
   Last_Updates_Summary := Ada.Real_Time.Clock;

   Hexagon.Client_Map.Init_Client_Map (A_Client_Map);
   Hexagon.Client_Map.Set_Origo_Patch (A_Client_Map, 1, 1);

end Tubastga_Window_Pkg.Callbacks;
