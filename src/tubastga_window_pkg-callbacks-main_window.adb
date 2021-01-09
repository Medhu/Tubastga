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

with System.RPC;
with Glib;
with Gdk.Event;
with Gtk.Widget;
with Gtk.Label;
with Text_IO;
with Gdk.Pixbuf;
with Glib.Error;
use type Glib.Error.GError;
with Gdk.Pixbuf;
with Tubastga_UI_Aux;
with Gtk.Main;
with Tubastga_Window_Pkg.Sounds;
with Tubastga_Window_Pkg.FullsizeView;
with Tubastga_Window_Pkg.ScrolledView;
with Tubastga_Window_Pkg.ZoomedView;
with Tubastga_Window_Pkg.MinimapView;
with Cairo;
with Gdk.Cairo;
with Landscape;
with Hexagon.Client_Map;
with Hexagon.Area;
with Piece;
with Piece.Client_Piece;
with Tubastga_Game;
with Gdk.Types.Keysyms;
with Utilities;
with Gtk.Text_Buffer;
with Gtk.Text_Iter;
with Player;
with Status;
with Observation;
with Effect;
with Client.Server_Adm;
with Ada.Real_Time;
with Tubastga_Window_Pkg.Effects;
with Action;
with Server;
with Tubastga_Window_Pkg.Lists;
with Tubastga_Window_Pkg.Callbacks.Performing_Patch;
with Tubastga_Window_Pkg.Callbacks.Target_Patch;
with Tubastga_Window_Pkg.Images;

package body Tubastga_Window_Pkg.Callbacks.Main_Window is
   Verbose : constant Boolean := False;

   type Type_Anim_State is
     (Anim_Idle,
      Anim_Show_Frame_Pause,
      Anim_Get_Pieces_Report,
      Anim_Get_Pieces_Report_Pause,
      Anim_Show_Frame);

   Anim_State : Type_Anim_State := Anim_Idle;
   Frame_Cursor : Observation.Frames.Piece_Visibility_Frames.Cursor;
   Anim_Show_Frame_Pause_Counter, Anim_Get_Pieces_Report_Pause_Counter : Integer;

   Max_Show_Frame_Pause_Counter        : constant Integer := 10;
   Max_Get_Pieces_Report_Pause_Counter : constant Integer := 30;
   use Gtk.Arguments;

   Draw : Cairo.Cairo_Context;

   Left_Mouse_Button  : constant Glib.Guint := 1;
   Right_Mouse_Button : constant Glib.Guint := 3;

   Max_Activity_Text : constant Integer := 50;

   Game_Area_Origo_Y : constant Integer := 1050;
   Map_Scale         : Float            := 0.5;

   Minimap_Origo_Y : constant Integer := 400;

   Button_Event : Boolean := False;

   Pieces_Menu_Y : constant Integer := 850;

   Shift_LR_Pressed : Boolean := False;

   Player_Pieces_Visibility_Frames : Observation.Frames.Piece_Visibility_Frames.Vector;
   System_Messages                 : Observation.Activity.Activity_Report.Vector;

   Curr_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress := null;

   Iter : Gtk.Text_Iter.Gtk_Text_Iter;

   Pieces_Pix : Gdk.Pixbuf.Gdk_Pixbuf;
   All_Pix, Scale_Pix, All_Minimap_Pix, Scale_Minimap_Pix, Patch_Zoom_Pix : Gdk.Pixbuf.Gdk_Pixbuf;

   All_Constructions_On_Patch,
   All_Landscape_On_Patch,
   All_Effects_On_Patch,
   All_Selections_On_Patch : Gdk.Pixbuf.Gdk_Pixbuf;

   Now_Countdown, Last_Countdown : Integer := -1;
   Game_State                    : Status.Type_Game_Status;
   --

   Last_Updates_Summary, Now : Ada.Real_Time.Time;

   procedure Update_Activity_Report_Buffer
     (P_Window        : in Type_Wnd_Main_Access;
      P_Activity_List : in Observation.Activity.Activity_Report.Vector)
   is
      Data            : Observation.Activity.Type_Activity_Report;
      Activity_Cursor : Observation.Activity.Activity_Report.Cursor;
      Iter, Iter2     : Gtk.Text_Iter.Gtk_Text_Iter;

      use Glib;
   begin
      while Gtk.Text_Buffer.Get_Line_Count (P_Window.Buffer_Activity_Report) >
        Glib.Gint (Max_Activity_Text)
      loop
         Gtk.Text_Buffer.Get_Start_Iter (P_Window.Buffer_Activity_Report, Iter);
         Gtk.Text_Buffer.Get_Iter_At_Line (P_Window.Buffer_Activity_Report, Iter2, 1);
         Gtk.Text_Buffer.Delete (P_Window.Buffer_Activity_Report, Iter, Iter2);
      end loop;

      --
      Activity_Cursor := Observation.Activity.Activity_Report.First (P_Activity_List);
      while Observation.Activity.Activity_Report.Has_Element (Activity_Cursor) loop
         Data := Observation.Activity.Activity_Report.Element (Activity_Cursor);

         Gtk.Text_Buffer.Get_End_Iter (P_Window.Buffer_Activity_Report, Iter);
         Gtk.Text_Buffer.Insert
           (P_Window.Buffer_Activity_Report,
            Iter,
            Utilities.RemoteString.To_String (Data.Activity_Description) & ASCII.LF);
         Observation.Activity.Activity_Report.Next (Activity_Cursor);
      end loop;
      --

      Gtk.Text_Buffer.Get_Start_Iter (P_Window.Buffer_Resources_Info, Iter);
      Gtk.Text_Buffer.Get_End_Iter (P_Window.Buffer_Resources_Info, Iter2);
      Gtk.Text_Buffer.Delete (P_Window.Buffer_Resources_Info, Iter, Iter2);
      Gtk.Text_Buffer.Get_End_Iter (P_Window.Buffer_Resources_Info, Iter);

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

            if A_Piece.all.Type_Of_Piece = Tubastga_Game.Tower_House then

               Gtk.Text_Buffer.Get_End_Iter (P_Window.Buffer_Resources_Info, Iter);
               Gtk.Text_Buffer.Insert
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
                  Gtk.Text_Buffer.Get_End_Iter (P_Window.Buffer_Resources_Info, Iter);
                  Gtk.Text_Buffer.Insert
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
          (Tubastga_Game.Landscapes_Type_Info_List (P_Landscape).Type_Name);
   end Format_Landscape;

   procedure On_Map_Area_Show (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class) is

      use Utilities.RemoteString;
      use Glib;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Show - enter ");
      end if;

      The_Window :=
        Tubastga_Window_Pkg.Type_Wnd_Main_Access (Gtk.Drawing_Area.Get_Toplevel (Object));

      All_Pix :=
        Gdk.Pixbuf.Gdk_New
          (Has_Alpha => True,
           Width     => 1400,
           Height    => Glib.Gint (Game_Area_Origo_Y) + 80);
      Pieces_Pix :=
        Gdk.Pixbuf.Gdk_New
          (Has_Alpha => True,
           Width     => 1400,
           Height    => Glib.Gint (Game_Area_Origo_Y) + 80);
      Scale_Pix       := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 700, Height => 730);
      All_Minimap_Pix :=
        Gdk.Pixbuf.Gdk_New
          (Has_Alpha => True,
           Width     => Glib.Gint (500),
           Height    => Glib.Gint (Minimap_Origo_Y) + Glib.Gint (10));
      Scale_Minimap_Pix := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 700, Height => 730);

      Patch_Zoom_Pix :=
        Gdk.Pixbuf.Gdk_New
          (Has_Alpha => True,
           Width     => Glib.Gint (Tubastga_Window_Pkg.FullsizeView.Patch_Zoom_Width),
           Height    => Glib.Gint (Tubastga_Window_Pkg.FullsizeView.Patch_Zoom_Height));

      All_Constructions_On_Patch :=
        Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);
      All_Landscape_On_Patch  := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);
      All_Effects_On_Patch    := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);
      All_Selections_On_Patch := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 72, Height => 72);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Show - exit");
      end if;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Show - exception");

   end On_Map_Area_Show;

   procedure On_Player_Timer_Area_Show
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class)
   is
      use Utilities.RemoteString;

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_Timer_Area_Show - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_Timer_Area_Show - exit");
      end if;

   end On_Player_Timer_Area_Show;

   procedure All_Pieces_On_Map
     (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in out Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
      Trav_Pieces      : Landscape.Pieces_Here_List.Cursor;
      A_Piece_Id       : Piece.Type_Piece_Id;
      A_Piece_Position : Tubastga_Window_Pkg.Lists.Type_Piece_Position;

   begin

      Trav_Pieces := Landscape.Pieces_Here_List.First (P_Patch.all.Pieces_Here);
      while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop

         A_Piece_Id       := Landscape.Pieces_Here_List.Element (Trav_Pieces);
         A_Piece_Position :=
           Tubastga_Window_Pkg.Lists.Type_Piece_Position'(A_Piece_Id, P_Patch.all.Pos);
         Tubastga_Window_Pkg.Lists.Set_All_Piece_In_List
           (Tubastga_Window_Pkg.Lists.All_Pieces_List,
            A_Piece_Position);

         Trav_Pieces := Landscape.Pieces_Here_List.Next (Trav_Pieces);
      end loop;

      Tubastga_Window_Pkg.Lists.All_Pieces_Sort_Pkg.Sort
        (Tubastga_Window_Pkg.Lists.All_Pieces_List);

   end All_Pieces_On_Map;

   procedure Draw_Map
     (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in out Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
   begin
      All_Pieces_On_Map (A_Client_Map, P_Patch);

      Tubastga_Window_Pkg.MinimapView.Draw_Minimap (A_Client_Map, P_Patch.all, All_Minimap_Pix);

      if Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all) in
          0 .. 820 and
        Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all) in
          0 .. 1050
      then

         if P_Patch.Visible then

            Gdk.Pixbuf.Fill (All_Effects_On_Patch, Glib.Guint32 (0));
            Gdk.Pixbuf.Fill (All_Landscape_On_Patch, Glib.Guint32 (0));
            Gdk.Pixbuf.Fill (All_Constructions_On_Patch, Glib.Guint32 (0));
            Gdk.Pixbuf.Fill (All_Selections_On_Patch, Glib.Guint32 (0));

            Tubastga_Window_Pkg.FullsizeView.Draw_Effects
              (All_Effects_On_Patch,
               P_Patch.all.Effects_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_Landscapes
              (All_Landscape_On_Patch,
               P_Patch.all.Landscape_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_Constructions
              (All_Constructions_On_Patch,
               P_Patch.all.Effects_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_All_Patch
              (A_Client_Map,
               P_Patch.all,
               All_Pix,
               All_Landscape_On_Patch,
               All_Constructions_On_Patch,
               All_Effects_On_Patch);

            Tubastga_Window_Pkg.FullsizeView.Draw_Houses
              (A_Client_Map,
               P_Patch.all,
               All_Pix,
               P_Patch.all.Pieces_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_Players
              (A_Client_Map,
               P_Patch.all,
               All_Pix,
               P_Patch.all.Pieces_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_Pieces
              (A_Client_Map,
               P_Patch.all,
               Pieces_Pix,
               P_Patch.all.Pieces_Here);
            --
            --
            -- Now UI Aid Selection

            Tubastga_Window_Pkg.FullsizeView.Draw_Patch_Selections
              (All_Selections_On_Patch,
               P_Patch.all,
               LB_Selected_Pos,
               RB_Selected_Pos);

            declare
               x, y : Glib.Gint;
            begin
               x :=
                 Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB
                   (A_Client_Map,
                    P_Patch.all);
               y :=
                 Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB
                   (A_Client_Map,
                    P_Patch.all);

               Gdk.Pixbuf.Composite
                 (All_Selections_On_Patch,
                  All_Pix,
                  Glib.Gint (x),
                  Glib.Gint (y),
                  72,
                  72,
                  Glib.Gdouble (x),
                  Glib.Gdouble (y),
                  1.0,
                  1.0,
                  Gdk.Pixbuf.Interp_Nearest,
                  255);
            end;

         else
            Tubastga_Window_Pkg.FullsizeView.Draw_Invisible (A_Client_Map, P_Patch.all, All_Pix);
         end if;

      end if;

   end Draw_Map;

   procedure On_Player_Timer_Area_Expose_Event
     (Object      :    access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw      :    Cairo.Cairo_Context;
      P_Player_Id : in Player.Type_Player_Id)
   is

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_Timer_Area_Expose_Event - enter");
      end if;

      if P_Player_Id = Me_Player_Id then
         Cairo.Set_Source_Rgb (P_Draw, 0.50, 0.40, 0.30);
         Cairo.Rectangle
           (P_Draw,
            Glib.Gdouble (0),
            Glib.Gdouble (0),
            Glib.Gdouble (125),
            Glib.Gdouble (40));
      end if;

      if P_Player_Id = 1 then
         Cairo.Set_Source_Rgb (P_Draw, 1.0, 0.0, 0.0);
      elsif P_Player_Id = 2 then
         Cairo.Set_Source_Rgb (P_Draw, 0.0, 1.0, 0.0);
      elsif P_Player_Id = 3 then
         Cairo.Set_Source_Rgb (P_Draw, 0.0, 0.0, 1.0);
      end if;

      Cairo.Rectangle
        (P_Draw,
         Glib.Gdouble (5),
         Glib.Gdouble (5),
         Glib.Gdouble (60),
         Glib.Gdouble (30));
      Cairo.Fill (P_Draw);

      Cairo.Stroke (P_Draw);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_Timer_Area_Expose_Event - exit");
      end if;
   end On_Player_Timer_Area_Expose_Event;

   function On_Player_1_Timer_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_1_Timer_Area_Expose_Event - enter");
      end if;

      On_Player_Timer_Area_Expose_Event (Object, P_Draw, Player.Type_Player_Id (1));

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_1_Timer_Area_Expose_Event - exit");
      end if;

      return True;
   end On_Player_1_Timer_Area_Expose_Event;

   function On_Player_2_Timer_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_2_Timer_Area_Expose_Event - enter");
      end if;

      On_Player_Timer_Area_Expose_Event (Object, P_Draw, Player.Type_Player_Id (2));

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_2_Timer_Area_Expose_Event - exit");
      end if;

      return True;
   end On_Player_2_Timer_Area_Expose_Event;

   function On_Player_3_Timer_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_3_Timer_Area_Expose_Event - enter");
      end if;

      On_Player_Timer_Area_Expose_Event (Object, P_Draw, Player.Type_Player_Id (3));

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_3_Timer_Area_Expose_Event - exit");
      end if;

      return True;
   end On_Player_3_Timer_Area_Expose_Event;

   function On_Map_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean
   is
--      Target_X, Target_Y : Integer;
      Adm_Status : Status.Type_Adm_Status;

      use Utilities.RemoteString;
      use Ada.Containers;
      use Piece;
      use Hexagon.Client_Map;
      use Player;
      use Glib;
   begin
      if Me_Player_Id = 0 then
         return True;
      end if;

      --if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Expose_Event - enter ");
      --end if;



      for Trav_Players in Player_Name_List'First .. Player_Name_List'Last loop
         if Player_Name_List (Trav_Players) = "" then
            Player_Name_List (Trav_Players) :=
              Client.Server_Adm.Get_Player_Name (Player.Type_Player_Id (Trav_Players), Adm_Status);
         end if;
      end loop;

      Gtk.Label.Set_Text
        (The_Window.all.Lbl_Player_1_Name,
         Utilities.RemoteString.To_String (Player_Name_List (1)));
      Gtk.Label.Set_Text
        (The_Window.all.Lbl_Player_2_Name,
         Utilities.RemoteString.To_String (Player_Name_List (2)));
      Gtk.Label.Set_Text
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
            Gdk.Pixbuf.Fill (All_Pix, Glib.Guint32 (0));
            Gdk.Pixbuf.Fill (Pieces_Pix, Glib.Guint32 (0));

            Piece.Client_Piece.Set_Reports_On_Pieces
              (Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Pieces_Info,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor)
                 .Pieces_Effects_Info);

            -- Set new info on map
            Hexagon.Client_Map.Set_Reports_On_Map
              (A_Client_Map,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Patches,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Observed_Pieces,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor)
                 .Patches_Effects_Info);

            Tubastga_Window_Pkg.Effects.Update_Client_Piece;

            Update_Activity_Report_Buffer
              (The_Window,
               Observation.Frames.Piece_Visibility_Frames.Element (Frame_Cursor).Activities_Info);

            Hexagon.Client_Map.Reset_Visit;

            Hexagon.Client_Map.Traverse (A_Client_Map, A_Client_Map.Origo_Patch, Draw_Map'Access);

            Hexagon.Client_Map.Reset_Visit;

            Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.Next (Frame_Cursor);
            if Observation.Frames.Piece_Visibility_Frames.Has_Element (Frame_Cursor) then
               Anim_Show_Frame_Pause_Counter := 0;
               Anim_State                    := Anim_Show_Frame_Pause;
            else
               Anim_Get_Pieces_Report_Pause_Counter := 0;

               declare
                  A_Pos   : Hexagon.Type_Hexagon_Position;
                  A_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

                  use Piece.Client_Piece;
               begin

                  -- TODO: The list of Pieces_GUI_Positions and Tab's needs to be
                  -- maintained when pieces are killed or they disappear from
                  -- view.
                  --
                  A_Pos := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
                  if A_Pos.P_Valid then
                     A_Patch :=
                       Hexagon.Client_Map.Get_Patch_Adress_From_AB (A_Client_Map, A_Pos.A, A_Pos.B);

                     Tubastga_Window_Pkg.Callbacks.Performing_Patch.Set_Selected_Patch_Window
                       (The_Window.all.Wnd_Performing_Patch,
                        A_Patch);
                  else
                     A_Patch := null;
                  end if;

               end;

               Anim_State := Anim_Get_Pieces_Report_Pause;
            end if;

      end case;

      if Button_Event then

         Hexagon.Client_Map.Reset_Visit;

         Gdk.Pixbuf.Fill (All_Pix, Glib.Guint32 (0));
         Gdk.Pixbuf.Fill (Pieces_Pix, Glib.Guint32(0));

         Hexagon.Client_Map.Traverse (A_Client_Map, A_Client_Map.Origo_Patch, -- current origo for
         --this client.
         Draw_Map'Access);

         Hexagon.Client_Map.Reset_Visit;

         Button_Event := False;
      end if;

      if Curr_Patch /= null then

--         Tubastga_Window_Pkg.FullsizeView.Draw_Arrow(A_Client_Map, Curr_Patch.all, All_Pix);

         declare
            use Utilities;
            Trav_Pieces  : Landscape.Pieces_Here_List.Cursor;
            Curr_Piece   : Piece.Client_Piece.Type_Client_Piece_Class_Access;
            Curr_Name    : Utilities.RemoteString.Type_String;
            Patch_Zoom_x : Glib.Gint := 0;
            Patch_Zoom_y : Glib.Gint := 0;
            Selected_Pos : Hexagon.Type_Hexagon_Position;

            use Piece.Client_Piece;
         begin
            if Curr_Patch.Pos.P_Valid then
               Gtk.Text_Buffer.Set_Text
                 (The_Window.all.Buffer_Hover_Info,
                  "Information for patch (" &
                  Curr_Patch.Pos.A'Img &
                  ", " &
                  Curr_Patch.Pos.B'Img &
                  ")" &
                  ASCII.LF);

               Trav_Pieces := Landscape.Pieces_Here_List.First (Curr_Patch.Pieces_Here);
               while Landscape.Pieces_Here_List.Has_Element (Trav_Pieces) loop
                  Gtk.Text_Buffer.Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
                  Curr_Piece :=
                    Piece.Client_Piece.Find_Piece_In_List (Curr_Patch.Pieces_Here (Trav_Pieces));
                  Curr_Name :=
                    Piece.Get_Name
                      (Piece.Type_Piece (Tubastga_Window_Pkg.Type_Client_Piece (Curr_Piece.all)));

                  Gtk.Text_Buffer.Insert
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

               Gtk.Text_Buffer.Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
               Curr_Piece :=
                 Piece.Client_Piece.Find_Piece_In_List
                   (Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces));

               if Curr_Piece /= null then
                  Gtk.Text_Buffer.Insert
                    (The_Window.Buffer_Hover_Info,
                     Iter,
                     "Currently selected Piece :" & Curr_Piece.Id'Img & ASCII.LF);

               end if;

               Gtk.Text_Buffer.Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
               Gtk.Text_Buffer.Insert
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
               --
               --
               --

            end if;
         end;
      end if; -- Curr_Patch /= null

      Gdk.Pixbuf.Fill (Scale_Pix, Glib.Guint32 (0));
      Gdk.Pixbuf.Composite
        (All_Pix,
         Scale_Pix,
         Glib.Gint (0.0),
         Glib.Gint (0.0),
         Glib.Gint (700.0),
         Glib.Gint (730.0),
         Glib.Gdouble (0),
         Glib.Gdouble (0),
         Glib.Gdouble (Map_Scale),
         Glib.Gdouble (Map_Scale),
         Gdk.Pixbuf.Interp_Nearest,
         255);
      Gdk.Pixbuf.Composite
        (Pieces_Pix,
         Scale_Pix,
         Glib.Gint (0.0),
         Glib.Gint (0.0),
         Glib.Gint (700.0),
         Glib.Gint (730.0),
         Glib.Gdouble (0),
         Glib.Gdouble (0),
         Glib.Gdouble (Map_Scale),
         Glib.Gdouble (Map_Scale),
         Gdk.Pixbuf.Interp_Nearest,
         255);
      Gdk.Cairo.Set_Source_Pixbuf (P_Draw, Scale_Pix, Glib.Gdouble (0), Glib.Gdouble (0));
      Cairo.Paint (P_Draw);

      Gdk.Pixbuf.Composite
        (All_Minimap_Pix,
         Scale_Minimap_Pix,
         Glib.Gint (0),
         Glib.Gint (0),
         140,
         360,
         Glib.Gdouble (0),
         Glib.Gdouble (0),
         Glib.Gdouble (1.0),
         Glib.Gdouble (1.0),
         Gdk.Pixbuf.Interp_Nearest,
         255);
      Gdk.Cairo.Set_Source_Pixbuf
        (P_Draw,
         Scale_Minimap_Pix,
         Glib.Gdouble (500),
         Glib.Gdouble (250));

      declare
         Trav : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Cursor;
         Target_Pos : Hexagon.Type_Hexagon_Position;
         Performing_Pos : Hexagon.Type_Hexagon_Position;
      begin
         Text_IO.Put_Line("A - START");
         Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.First (LB_Selected_Pos);
         Performing_Pos := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Element (Trav);

         Trav := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.First (RB_Selected_Pos);
         Target_Pos := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Element (Trav);

         Tubastga_Window_Pkg.FullsizeView.Draw_Arrow
           (A_Client_Map,
            Hexagon.Client_Map.Get_Patch_Adress_From_AB(A_Client_Map, Performing_Pos.A, Performing_Pos.B).all,
            Hexagon.Client_Map.Get_Patch_Adress_From_AB(A_Client_Map, Target_Pos.A, Target_Pos.B).all,
            All_Pix);

         Text_IO.Put_Line("A - SLUTT");
      end;

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
--        Cairo.Move_To (P_Draw, Gdouble (0), Gdouble (0));
  --      Cairo.Line_To (P_Draw, Gdouble (100.0), Gdouble (1000));
    --    Cairo.Move_To (P_Draw, Gdouble (0), Gdouble (682.0 / 0.65 * Map_Scale));
      --  Cairo.Line_To (P_Draw, Gdouble (700), Gdouble (682.0 / 0.65 * Map_Scale));

      --Player Pos
--      Cairo.Move_To (P_Draw, Gdouble (36.0 / 0.65 * Map_Scale), Gdouble (682.0 / 0.65 * Map_Scale));
--      Cairo.Line_To (P_Draw, Gdouble (36.0 / 0.65 * Map_Scale) +gdouble(15), Gdouble (682.0 / 0.65 * Map_Scale) + gdouble(2));

--        declare
--           pa_xx, pa_yy : Gint;
--
--           pi_xx1, pi_yy1 : Gint;
--        begin
--           pa_xx := Gint (27.0);
--           pa_yy := Gint (525.0);
--
--           --         (20, 7), (31, 14), (31, 24), (20, 28), (10, 24), (9, 11), (20, 21)
--           --opp
--           pi_xx1 := Gint (27.0 + 0.0);
--           pi_yy1 := Gint (525.0 - 9.0);
--           Cairo.Move_To (P_Draw, Gdouble (pa_xx), Gdouble (pa_yy));
--           Cairo.Line_To (P_Draw, Gdouble (pi_xx1), Gdouble (pi_yy1));
--
--           --n-oest
--           pi_xx1 := Gint (27.0 + 8.0);
--           pi_yy1 := Gint (525.0 - 4.0);
--           Cairo.Move_To (P_Draw, Gdouble (pa_xx), Gdouble (pa_yy));
--           Cairo.Line_To (P_Draw, Gdouble (pi_xx1), Gdouble (pi_yy1));
--
--           --s-oest
--           pi_xx1 := Gint (27.0 + 8.0);
--           pi_yy1 := Gint (525.0 + 4.0);
--           Cairo.Move_To (P_Draw, Gdouble (pa_xx), Gdouble (pa_yy));
--           Cairo.Line_To (P_Draw, Gdouble (pi_xx1), Gdouble (pi_yy1));
--
--           --ned
--           pi_xx1 := Gint (27.0 + 0.0);
--           pi_yy1 := Gint (525.0 + 9.0);
--           Cairo.Move_To (P_Draw, Gdouble (pa_xx), Gdouble (pa_yy));
--           Cairo.Line_To (P_Draw, Gdouble (pi_xx1), Gdouble (pi_yy1));
--
--           --s-vest
--           pi_xx1 := Gint (27.0 - 8.0);
--           pi_yy1 := Gint (525.0 + 4.0);
--           Cairo.Move_To (P_Draw, Gdouble (pa_xx), Gdouble (pa_yy));
--           Cairo.Line_To (P_Draw, Gdouble (pi_xx1), Gdouble (pi_yy1));
--
--           --n-vest
--           pi_xx1 := Gint (27.0 - 8.0);
--           pi_yy1 := Gint (525.0 - 4.0);
--           Cairo.Move_To (P_Draw, Gdouble (pa_xx), Gdouble (pa_yy));
--           Cairo.Line_To (P_Draw, Gdouble (pi_xx1), Gdouble (pi_yy1));
--
--        end;

      -- minimap
      --        Cairo.Move_To (P_Draw, Gdouble (510), Gdouble (0));
      --        Cairo.Line_To (P_Draw, Gdouble (510), Gdouble (700));
      --        Cairo.Move_To (P_Draw, Gdouble (0), Gdouble (600));
      --        Cairo.Line_To (P_Draw, Gdouble (700), Gdouble (600));

      Cairo.Stroke (P_Draw);

      --if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Expose_Event - exit");
      --end if;

      return True;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Expose_Event - exception");
         return True;
   end On_Map_Area_Expose_Event;

   procedure Place_Piece is
      A_Piece : Piece.Type_Piece;

      A_Pos   : Hexagon.Type_Hexagon_Position;
      A_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress;

      use Ada.Containers;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.Place_Piece - enter");
      end if;

      -- and user clicks a tile
      -- now try to place the piece
      --

      A_Pos   := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
      A_Patch := Hexagon.Client_Map.Get_Patch_Adress_From_AB (A_Client_Map, A_Pos.A, A_Pos.B);
      Piece.Client_Piece.Create_Piece
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         A_Piece,
         Tubastga_UI_Aux.Convert_UI_State_To_Piece (Tubastga_UI_Aux.UI_State),
         Tubastga_UI_Aux.Convert_UI_State_To_Category (Tubastga_UI_Aux.UI_State),
         Landscape.Type_Patch (A_Patch.all));

      Tubastga_Window_Pkg.Sounds.Play_Placed_Piece;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.Place_Piece - exit");
      end if;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.Place_Piece - exception");
      when System.RPC.Communication_Error =>
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.Place_Piece - System.RPC.Communication_Error");

   end Place_Piece;

   function On_Map_Area_Motion_Notify_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk.Event.Gdk_Event := To_Event (Params, 1);
      X    : Glib.Gdouble;
      Y    : Glib.Gdouble;

      use Tubastga_UI_Aux;
      use Hexagon.Client_Map;
   begin
      Gdk.Event.Get_Coords (Arg1, X, Y);

      Curr_Patch := Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);

      return True;
   end On_Map_Area_Motion_Notify_Event;

   function On_Map_Area_Button_Press_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1    : Gdk.Event.Gdk_Event                         := To_Event (Params, 1);
      X       : Glib.Gdouble;
      Y       : Glib.Gdouble;
      A_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress := null;

      use Tubastga_UI_Aux;
      use Hexagon.Client_Map;
      use Player;
      use Glib;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Button_Press_Event - enter");
      end if;

      if Me_Player_Id = 0 then
         return True;
      end if;

      Gdk.Event.Get_Coords (Arg1, X, Y);

      if Glib.Gdouble (X) > Glib.Gdouble (500) and Glib.Gdouble (Y) > Glib.Gdouble (340) then
         Button_Event := True;
      else

         if Y < Gdouble (Pieces_Menu_Y) then

            if Gdk.Event.Get_Button (Arg1) = Left_Mouse_Button then

               A_Patch := Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);
               Tubastga_Window_Pkg.Lists.Set_Last_Selected_Pos
                 (LB_Selected_Pos,
                  A_Patch.all.Pos,
                  Shift_LR_Pressed);

               Tubastga_Window_Pkg.Callbacks.Performing_Patch.Set_Selected_Patch_Window
                 (The_Window.all.Wnd_Performing_Patch,
                  A_Patch);

            elsif Gdk.Event.Get_Button (Arg1) = Right_Mouse_Button then

               A_Patch := Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);
               Tubastga_Window_Pkg.Lists.Set_Last_Selected_Pos
                 (RB_Selected_Pos,
                  A_Patch.all.Pos,
                  Shift_LR_Pressed);

               Tubastga_Window_Pkg.Callbacks.Target_Patch.Set_Target_Patch_Window
                 (The_Window.all.Wnd_Target_Patch,
                  A_Patch);

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
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Button_Press_Event - exit");
      end if;

      return True;
   end On_Map_Area_Button_Press_Event;

   function On_Map_Area_Scroll_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk.Event.Gdk_Event := To_Event (Params, 1);

      use Gdk.Event;
      use Tubastga_UI_Aux;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Scroll_Event - enter");
      end if;

      if Arg1.Scroll.Direction = Gdk.Event.Scroll_Up then
         Map_Scale := Map_Scale + 0.025;
         Tubastga_Window_Pkg.ZoomedView.Set_Map_Scale (Glib.Gdouble (Map_Scale));
      end if;
      if Arg1.Scroll.Direction = Scroll_Down then
         Map_Scale := Map_Scale - 0.025;
         Tubastga_Window_Pkg.ZoomedView.Set_Map_Scale (Glib.Gdouble (Map_Scale));
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Scroll_Event - exit");
      end if;

      return False;
   end On_Map_Area_Scroll_Event;

   procedure On_Button_Place_Sentry
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Sentry - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Sentry;

   end On_Button_Place_Sentry;

   procedure On_Button_Place_Bowman
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Bowman - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Bowman;

   end On_Button_Place_Bowman;

   procedure On_Button_Place_Carrier
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Carrier - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Carrier;

   end On_Button_Place_Carrier;

   procedure On_Button_Place_Ship (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Ship - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Ship;
   end On_Button_Place_Ship;

   procedure On_Button_Place_Farm (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Farm - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Farm;
   end On_Button_Place_Farm;

   procedure On_Button_Place_Tower (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Tower - clicked");
      end if;

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Tower;
   end On_Button_Place_Tower;

   procedure On_Button_Place_Lumberjack
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Lumberjack - clicked");
      end if;
      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Lumberjack;

   end On_Button_Place_Lumberjack;

   procedure On_Button_Place_Knight
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Knight - clicked");
      end if;
      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Knight;

   end On_Button_Place_Knight;

   procedure On_Button_Place_Stonecutter
     (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Stonecutter - clicked");
      end if;
      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Stonecutter;
   end On_Button_Place_Stonecutter;

   function On_Keyboard_Key_Press
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk.Event.Gdk_Event := To_Event (Params, 1);

      use Piece.Client_Piece.Pieces_Client_List;
      use Piece;
   begin

      case Gdk.Event.Get_Key_Val (Arg1) is
         when Gdk.Types.Keysyms.GDK_Tab =>
            declare
               TAB_Cursor           : Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.Cursor;
               Current_TAB_Piece_Id : Piece.Type_Piece_Id;

               A_Piece_Position : Tubastga_Window_Pkg.Lists.Type_Piece_Position;

               use Piece;
            begin
               Current_TAB_Piece_Id :=
                 Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
               Tubastga_Window_Pkg.Lists.Print_All_Piece_List
                 (Tubastga_Window_Pkg.Lists.All_Pieces_List);

               if Current_TAB_Piece_Id = Piece.Undefined_Piece_Id then
                  TAB_Cursor :=
                    Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.First
                      (Tubastga_Window_Pkg.Lists.All_Pieces_List);
               else
                  TAB_Cursor :=
                    Tubastga_Window_Pkg.Lists.Find_Piece_In_All_Piece_List
                      (Tubastga_Window_Pkg.Lists.All_Pieces_List,
                       Current_TAB_Piece_Id);
               end if;

               TAB_Cursor := Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.Next (TAB_Cursor);

               if not Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.Has_Element (TAB_Cursor) then
                  TAB_Cursor :=
                    Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.First
                      (Tubastga_Window_Pkg.Lists.All_Pieces_List);
               end if;

               if Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.Has_Element (TAB_Cursor) then
                  A_Piece_Position :=
                    Tubastga_Window_Pkg.Lists.All_Pieces_List_Pkg.Element (TAB_Cursor);

                  Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece
                    (LB_Selected_Pieces,
                     A_Piece_Position.Actual_Piece_Id,
                     Shift_LR_Pressed);
                  Tubastga_Window_Pkg.ScrolledView.Scroll_To_Patch
                    (A_Client_Map,
                     A_Piece_Position.Actual_Pos);
               end if;
            end;

         when Gdk.Types.Keysyms.GDK_Up =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Up);

         when Gdk.Types.Keysyms.GDK_Down =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Down);

         when Gdk.Types.Keysyms.GDK_Left =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Left);

         when Gdk.Types.Keysyms.GDK_Right =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Right);

         when Gdk.Types.Keysyms.GDK_space =>
            null;

         when Gdk.Types.Keysyms.GDK_Home =>
            null;

         when Gdk.Types.Keysyms.GDK_Page_Up =>
            null;

         when Gdk.Types.Keysyms.GDK_End =>
            null;

         when Gdk.Types.Keysyms.GDK_Page_Down =>
            null;

         when Gdk.Types.Keysyms.GDK_uparrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Up);

         when Gdk.Types.Keysyms.GDK_downarrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Down);

         when Gdk.Types.Keysyms.GDK_leftarrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Left);

         when Gdk.Types.Keysyms.GDK_rightarrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Right);

         when Gdk.Types.Keysyms.GDK_Escape =>
            Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Done;

         when Gdk.Types.Keysyms.GDK_Shift_L =>
            Shift_LR_Pressed := True;

         when Gdk.Types.Keysyms.GDK_Shift_R =>
            Shift_LR_Pressed := True;

         when Gdk.Types.Keysyms.GDK_Control_L =>
            null;

         when others =>
            null;
      end case;

      Button_Event := True;

      return True;
   end On_Keyboard_Key_Press;

   function On_Keyboard_Key_Release
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk.Event.Gdk_Event := To_Event (Params, 1);

   begin

      case Gdk.Event.Get_Key_Val (Arg1) is
         when Gdk.Types.Keysyms.GDK_Shift_L =>
            --            Shift_LR_Pressed := False;
            null;

         when Gdk.Types.Keysyms.GDK_Shift_R =>
            --            Shift_LR_Pressed := False;
            null;

         when others =>
            null;
      end case;

      return True;
   end On_Keyboard_Key_Release;

   procedure On_Button_Game (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class) is
      GameOption : Gtk.Dialog.Gtk_Response_Type;
      use Gtk.Dialog;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Game - clicked");
      end if;

      GameOption := Gtk.Dialog.Run (Gtk.Dialog.Gtk_Dialog (The_Window.all.Dlg_Main_Menu));
      Gtk.Dialog.Hide (Gtk.Dialog.Gtk_Dialog (The_Window.all.Dlg_Main_Menu));

   end On_Button_Game;

   procedure Exit_Main (Object : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Client.Server_Adm.Client_Stopped (Me_Player_Id);
      Client.Server_Adm.Disconnect;
      Gtk.Widget.Destroy (Object);
      Gtk.Main.Main_Quit;
   end Exit_Main;

begin
   Last_Updates_Summary := Ada.Real_Time.Clock;

   Hexagon.Client_Map.Init_Client_Map (A_Client_Map);
   Hexagon.Client_Map.Set_Origo_Patch (A_Client_Map, 1, 1);

end Tubastga_Window_Pkg.Callbacks.Main_Window;
