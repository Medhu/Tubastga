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
with Tubastga_Window_Pkg.FullsizeView;
with Tubastga_Window_Pkg.ScrolledView;
with Tubastga_Window_Pkg.ZoomedView;
with Tubastga_Window_Pkg.MinimapView;
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
with Observation;
with Effect;
with Client.Server_Adm;
with GNAT.Calendar.Time_IO;
with Ada.Calendar;
with Ada.Real_Time;
with Gtk.Spin_Button;   use Gtk.Spin_Button;
with Construction;
with Goods;
with Tubastga_Window_Pkg.Effects;
with Action;
with Server;
with Tubastga_Window_Pkg.Lists;

package body Tubastga_Window_Pkg.Callbacks is
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

   Left_Mouse_Button  : constant Guint := 1;
   Right_Mouse_Button : constant Guint := 3;

   Max_Activity_Text : constant Integer := 50;

   Game_Area_Origo_Y : constant Integer := 1050;
   Map_Scale         : Float            := 0.5;

   Minimap_Origo_Y : constant Integer := 350;

   Button_Event : Boolean := False;

   Pieces_Menu_Y : constant Integer := 850;

   LB_Selected_Pos : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Vector;
   RB_Selected_Pos : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Vector;

   LB_Selected_Pieces : Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Vector;
   RB_Selected_Pieces : Tubastga_Window_Pkg.Lists.Piece_List_Pkg.Vector;

   Shift_LR_Pressed : Boolean := False;

   Player_Pieces_Visibility_Frames : Observation.Frames.Piece_Visibility_Frames.Vector;
   System_Messages                 : Observation.Activity.Activity_Report.Vector;

   Curr_Patch : Hexagon.Client_Map.Type_Client_Patch_Adress := null;

   A_Client_Map : Hexagon.Client_Map.Type_Client_Map_Info;

   type Type_Player_Name_List is array (1 .. 3) of Utilities.RemoteString.Type_String;
   Player_Name_List : Type_Player_Name_List;

   Iter                                                   : Gtk_Text_Iter;
   The_Window                                             : Wnd_Main_Access;
   All_Pix, Scale_Pix, All_Minimap_Pix, Scale_Minimap_Pix : Gdk.Pixbuf.Gdk_Pixbuf;
   All_Constructions_On_Patch,
   All_Landscape_On_Patch,
   All_Effects_On_Patch,
   All_Selections_On_Patch : Gdk.Pixbuf.Gdk_Pixbuf;

   Me_Player_Id                  : Player.Type_Player_Id := 0;
   Now_Countdown, Last_Countdown : Integer               := -1;
   Game_State                    : Status.Type_Game_Status;
   --

   Last_Updates_Summary, Now : Ada.Real_Time.Time;

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

      use Piece.Client_Piece;
      use Piece;
      use Hexagon.Client_Map;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Set_Performing_Piece_Window - enter");
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

      P_Piece_Window.all.Selected_Patch := P_Patch;
      if P_Piece.Id /= Piece.Undefined_Piece_Id then
         if P_Piece_Window.all.Selected_Piece /= null then
            if P_Piece.Id /= P_Piece_Window.all.Selected_Piece.all.Id then
               Tubastga_Window_Pkg.Sounds.Play_Set_Performing_Piece_Window;
            end if;
         else
            Tubastga_Window_Pkg.Sounds.Play_Set_Performing_Piece_Window;
         end if;

         P_Piece_Window.all.Selected_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece.Id));

         Gtk.GEntry.Set_Text
           (P_Piece_Window.all.En_Piece_Name,
            Utilities.RemoteString.To_String (P_Piece_Window.all.Selected_Piece.Name));

         Get_End_Iter
           (The_Window.all.Wnd_Performing_Piece.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter,
            "Information about piece" & ASCII.LF);

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

      if P_Patch /= null then

         Gtk.Text_Buffer.Get_End_Iter
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter,
            ASCII.LF &
            "Information about patch (" &
            P_Patch.all.Pos.A'Img &
            ", " &
            P_Patch.all.Pos.B'Img &
            ")" &
            ASCII.LF);

         Gtk.Text_Buffer.Get_End_Iter
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
            End_Iter,
            "Observed landscape: " & Format_Landscape (P_Patch.all.Landscape_Here) & ASCII.LF);

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

         Trav_Patch_Constructions :=
           Construction.Construction_List.First (P_Patch.all.Constructions_Here);
         while Construction.Construction_List.Has_Element (Trav_Patch_Constructions) loop

            Gtk.Text_Buffer.Get_End_Iter
              (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
               End_Iter);
            Gtk.Text_Buffer.Insert
              (P_Piece_Window.all.Buffer_Performing_Piece_Patch_Info,
               End_Iter,
               Construction.Construction_List.Element (Trav_Patch_Constructions)'Img & ASCII.LF);

            Trav_Patch_Constructions :=
              Construction.Construction_List.Next (Trav_Patch_Constructions);
         end loop;

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Set_Performing_Piece_Window - exit");
      end if;
   end Set_Performing_Piece_Window;

   procedure Set_Target_Piece_Window
     (P_Piece_Window : in out Wnd_Target_Access;
      P_Patch        : in     Hexagon.Client_Map.Type_Client_Patch_Adress;
      P_Piece_Id     : in     Piece.Type_Piece_Id)
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

      if P_Piece_Id /= Piece.Undefined_Piece_Id then

         P_Piece_Window.all.Selected_Piece :=
           Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id));

         Gtk.GEntry.Set_Text
           (P_Piece_Window.all.En_Piece_Name,
            Utilities.RemoteString.To_String
              (Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id)).Name));

         Tubastga_Window_Pkg.Sounds.Play_Set_Target_Piece_Window;

         --
         A_Piece := Piece.Client_Piece.Find_Piece_In_List (Piece.Type_Piece_Id (P_Piece_Id));

         Get_End_Iter (The_Window.all.Wnd_Target.all.Buffer_Target_Piece_Patch_Info, End_Iter);
         Gtk.Text_Buffer.Insert
           (P_Piece_Window.all.Buffer_Target_Piece_Patch_Info,
            End_Iter,
            ASCII.LF & "Information regarding piece" & ASCII.LF);

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

      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario);
      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Load_Game_Name);
      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Join_Player_Name);

      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario,
         "Chose scenario...");
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
              (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario,
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
      All_Effects_On_Patch    := Gdk.Pixbuf.Gdk_New (Has_Alpha => True, Width => 55, Height => 44);
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

   procedure All_Pieces_On_Map
     (P_Client_Map : in out Hexagon.Client_Map.Type_Client_Map_Info;
      P_Patch      : in out Hexagon.Client_Map.Type_Client_Patch_Adress)
   is
      Trav_Pieces      : Landscape.Pieces_Here_List.Cursor;
      A_Piece_Id       : Piece.Type_Piece_Id;
      A_Piece_Position : Tubastga_Window_Pkg.Lists.Type_Piece_Position;

      use Piece;
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

      --
      use Hexagon;

   begin
      All_Pieces_On_Map (A_Client_Map, P_Patch);

      Tubastga_Window_Pkg.MinimapView.Draw_Minimap
        (The_Window.all.All_Images,
         A_Client_Map,
         P_Patch.all,
         All_Minimap_Pix);

      if Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_X_From_AB (A_Client_Map, P_Patch.all) in
          0 .. 820 and
        Tubastga_Window_Pkg.FullsizeView.Get_All_Pix_Patch_Y_From_AB (A_Client_Map, P_Patch.all) in
          0 .. 1050
      then

         if P_Patch.Visible then
            Landscape_Image    := None;
            Surprise_Image     := None;
            Construction_Image := None;
            Player_Image       := None;
            Piece_Image        := None;
            UI_Aid_Image       := None;

            Gdk.Pixbuf.Fill (All_Effects_On_Patch, Glib.Guint32 (0));
            Gdk.Pixbuf.Fill (All_Landscape_On_Patch, Glib.Guint32 (0));
            Gdk.Pixbuf.Fill (All_Constructions_On_Patch, Glib.Guint32 (0));
            Gdk.Pixbuf.Fill (All_Selections_On_Patch, Glib.Guint32 (0));

            Tubastga_Window_Pkg.FullsizeView.Draw_Effects
              (The_Window.all.All_Images,
               All_Effects_On_Patch,
               P_Patch.all.Effects_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_Landscapes
              (The_Window.all.All_Images,
               All_Landscape_On_Patch,
               P_Patch.all.Landscape_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_Constructions
              (The_Window.all.All_Images,
               All_Constructions_On_Patch,
               P_Patch.all.Constructions_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_All_Patch
              (A_Client_Map,
               P_Patch.all,
               All_Pix,
               All_Landscape_On_Patch,
               All_Constructions_On_Patch,
               All_Effects_On_Patch);

            Tubastga_Window_Pkg.FullsizeView.Draw_Players
              (The_Window.all.All_Images,
               A_Client_Map,
               P_Patch.all,
               All_Pix,
               P_Patch.all.Pieces_Here);

            Tubastga_Window_Pkg.FullsizeView.Draw_Players_Selections
              (The_Window.all.All_Images,
               A_Client_Map,
               P_Patch.all,
               All_Pix,
               P_Patch.all.Pieces_Here,
               LB_Selected_Pieces,
               RB_Selected_Pieces);

            Tubastga_Window_Pkg.FullsizeView.Draw_Pieces
              (The_Window.all.All_Images,
               A_Client_Map,
               P_Patch.all,
               All_Pix,
               P_Patch.all.Pieces_Here);

            --
            --
            -- Now UI Aid Selection

            Tubastga_Window_Pkg.FullsizeView.Draw_Patch_Selections
              (The_Window.all.All_Images,
               All_Selections_On_Patch,
               P_Patch.all,
               LB_Selected_Pos,
               RB_Selected_Pos);
            declare
               x, y : Gint;
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
            Tubastga_Window_Pkg.FullsizeView.Draw_Invisible
              (A_Client_Map,
               P_Patch.all,
               The_Window.all.All_Images,
               All_Pix);
         end if;
      end if;

   end Draw_Map;

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

      if P_Player_Id = 1 then
         Cairo.Set_Source_Rgb (P_Draw, 1.0, 0.0, 0.0);
      elsif P_Player_Id = 2 then
         Cairo.Set_Source_Rgb (P_Draw, 0.0, 1.0, 0.0);
      elsif P_Player_Id = 3 then
         Cairo.Set_Source_Rgb (P_Draw, 0.0, 0.0, 1.0);
      end if;

      Cairo.Rectangle (P_Draw, Gdouble (5), Gdouble (5), Gdouble (60), Gdouble (30));
      Cairo.Fill (P_Draw);

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
      Adm_Status         : Status.Type_Adm_Status;

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

            Hexagon.Client_Map.Traverse (A_Client_Map, A_Client_Map.Origo_Patch, Draw_Map'Access);

            Hexagon.Client_Map.Reset_Visit;

            Frame_Cursor := Observation.Frames.Piece_Visibility_Frames.Next (Frame_Cursor);
            if Observation.Frames.Piece_Visibility_Frames.Has_Element (Frame_Cursor) then
               Anim_Show_Frame_Pause_Counter := 0;
               Anim_State                    := Anim_Show_Frame_Pause;
            else
               Anim_Get_Pieces_Report_Pause_Counter := 0;

               declare
                  A_Piece_Id : Piece.Type_Piece_Id := Piece.Undefined_Piece_Id;
                  A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
                  A_Pos      : Hexagon.Type_Hexagon_Position;
                  A_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress;

                  use Piece.Client_Piece;
               begin

                  -- TODO: The list of Pieces_GUI_Positions and Tab's needs to be
                  -- maintained when pieces are killed or they disappear from
                  -- view.
                  --
                  A_Piece_Id :=
                    Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);
                  A_Pos := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Pos (LB_Selected_Pos);
                  if A_Pos.P_Valid then
                     A_Patch :=
                       Hexagon.Client_Map.Get_Patch_Adress_From_AB (A_Client_Map, A_Pos.A, A_Pos.B);
                  else
                     A_Patch := null;
                  end if;

                  if A_Piece_Id /= Piece.Undefined_Piece_Id then
                     A_Piece := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

                     if A_Piece /= null then
                        Set_Performing_Piece_Window
                          (The_Window.all.Wnd_Performing_Piece,
                           A_Patch,
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
         Draw_Map'Access);

         Hexagon.Client_Map.Reset_Visit;

         Button_Event := False;
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

            use Piece.Client_Piece;
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

               Get_End_Iter (The_Window.Buffer_Hover_Info, Iter);
               Curr_Piece :=
                 Piece.Client_Piece.Find_Piece_In_List
                   (Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces));

               if Curr_Piece /= null then
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

--      Cairo.Stroke (P_Draw);

      return True;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Tubastga_Window_Pkg-callbacks.On_Map_Area_Expose_Event - exception");
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
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg-Callbacks.Place_Piece - enter Tubastga_UI_Aux.UI_State=" &
            Tubastga_UI_Aux.UI_State'Img);
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

      Curr_Patch := Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);

      return True;
   end On_Map_Area_Motion_Notify_Event;

   function On_Map_Area_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1       : Gdk_Event                                   := To_Event (Params, 1);
      X          : Gdouble;
      Y          : Gdouble;
      A_Patch    : Hexagon.Client_Map.Type_Client_Patch_Adress := null;
      A_Piece    : Piece.Client_Piece.Type_Client_Piece_Class_Access;
      A_Piece_Id : Piece.Type_Piece_Id;

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
         Button_Event := True;
      else
         if Verbose then
            Text_IO.Put_Line
              ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Button_Press_Event - Mainmap");
         end if;

         if Y < Gdouble (Pieces_Menu_Y) then

            if Get_Button (Arg1) = Left_Mouse_Button then

               A_Patch := Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);
               Tubastga_Window_Pkg.Lists.Set_Last_Selected_Pos
                 (LB_Selected_Pos,
                  A_Patch.all.Pos,
                  Shift_LR_Pressed);

               declare
                  n  : Integer;
                  Id : Piece.Type_Piece_Id;
               begin
                  n  := Tubastga_Window_Pkg.ZoomedView.Selected_Piece (A_Client_Map, X, Y);
                  Id := Landscape.Pieces_Here_List.Element (A_Patch.all.Pieces_Here, n);
                  Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece
                    (LB_Selected_Pieces,
                     Id,
                     Shift_LR_Pressed);
               exception
                  when others =>
                     null;

               end;

               A_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (LB_Selected_Pieces);

               if A_Piece_Id /= Piece.Undefined_Piece_Id then
                  A_Piece := Piece.Client_Piece.Find_Piece_In_List (A_Piece_Id);

                  Set_Performing_Piece_Window
                    (The_Window.all.Wnd_Performing_Piece,
                     A_Patch,
                     Tubastga_Window_Pkg.Type_Client_Piece (A_Piece.all));

               else
                  Set_Performing_Piece_Window
                    (The_Window.all.Wnd_Performing_Piece,
                     A_Patch,
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

               A_Patch := Tubastga_Window_Pkg.ZoomedView.Selected_Patch (A_Client_Map, X, Y);
               Tubastga_Window_Pkg.Lists.Set_Last_Selected_Pos
                 (RB_Selected_Pos,
                  A_Patch.all.Pos,
                  Shift_LR_Pressed);

               declare
                  n  : Integer;
                  Id : Piece.Type_Piece_Id;
               begin
                  n  := Tubastga_Window_Pkg.ZoomedView.Selected_Piece (A_Client_Map, X, Y);
                  Id := Landscape.Pieces_Here_List.Element (A_Patch.all.Pieces_Here, n);
                  Tubastga_Window_Pkg.Lists.Set_Last_Selected_Piece
                    (RB_Selected_Pieces,
                     Id,
                     Shift_LR_Pressed);
               exception
                  when others =>
                     null;

               end;

               A_Piece_Id := Tubastga_Window_Pkg.Lists.Get_Last_Selected_Piece (RB_Selected_Pieces);

               if A_Piece_Id /= Piece.Undefined_Piece_Id then

                  Set_Target_Piece_Window (The_Window.all.Wnd_Target, A_Patch, A_Piece_Id);
               else
                  Set_Target_Piece_Window
                    (The_Window.all.Wnd_Target,
                     A_Patch,
                     Piece.Undefined_Piece_Id);
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
         Tubastga_Window_Pkg.ZoomedView.Set_Map_Scale (Glib.Gdouble (Map_Scale));
      end if;
      if Arg1.Scroll.Direction = Scroll_Down then
         Map_Scale := Map_Scale - 0.025;
         Tubastga_Window_Pkg.ZoomedView.Set_Map_Scale (Glib.Gdouble (Map_Scale));
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Map_Area_Scroll_Event - exit");
      end if;

      return False;
   end On_Map_Area_Scroll_Event;

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
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.callbacks.On_Button_Wall1 - clicked piece_id=" &
            The_Window.all.Wnd_Performing_Piece.Selected_Piece.Id'Img);
      end if;

      Piece.Client_Piece.Perform_Construction
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall1);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall1;
   end On_Button_Wall1;

   procedure On_Button_Wall2 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall2 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall2);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall2;
   end On_Button_Wall2;

   procedure On_Button_Wall3 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall3 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall3);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall3;
   end On_Button_Wall3;

   procedure On_Button_Wall4 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall4 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall4);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall4;
   end On_Button_Wall4;

   procedure On_Button_Wall5 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall5 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall5);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall5;
   end On_Button_Wall5;

   procedure On_Button_Wall6 (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Wall6 - clicked");
      end if;

      Piece.Client_Piece.Perform_Construction
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.Selected_Patch.all),
         Tubastga_Piece.Construction_Wall6);

      Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Place_Wall6;
   end On_Button_Wall6;

   procedure On_Button_Move (Object : access Gtk_Button_Record'Class) is

      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Move - clicked");
      end if;

      Piece.Client_Piece.Perform_Move
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Landscape.Type_Patch (The_Window.all.Wnd_Target.all.Selected_Patch.all));

   end On_Button_Move;

   procedure On_Button_Attack (Object : access Gtk_Button_Record'Class) is

      use Piece;
      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Attack - clicked");
      end if;

      Piece.Client_Piece.Perform_Attack
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Piece.Type_Piece (The_Window.all.Wnd_Target.all.Selected_Piece.all));

   end On_Button_Attack;

   procedure On_Button_Ranged_Attack (Object : access Gtk_Button_Record'Class) is

      use Piece;
      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Ranged_Attack - clicked");
      end if;

      Piece.Client_Piece.Perform_Ranged_Attack
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Piece.Type_Piece (The_Window.all.Wnd_Target.all.Selected_Piece.all));

   end On_Button_Ranged_Attack;

   procedure On_Button_Promote (Object : access Gtk_Button_Record'Class) is

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Promote - clicked");
      end if;

      Piece.Client_Piece.Grant_Piece_Effect
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Effect.Type_Effect'(Tubastga_Piece.Effect_Captain, 1));

   end On_Button_Promote;

   procedure On_Button_Demote (Object : access Gtk_Button_Record'Class) is

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Demote - clicked");
      end if;

      Piece.Client_Piece.Revoke_Piece_Effect
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Effect.Type_Effect'(Tubastga_Piece.Effect_Captain, 1));

   end On_Button_Demote;

   procedure On_Button_Search (Object : access Gtk_Button_Record'Class) is
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
      if Effect.Effect_List.Has_Element (Effect_Cursor) then
         An_Effect := Effect.Effect_List.Element (Effect_Cursor);

         Piece.Client_Piece.Perform_Patch_Effect
           (Me_Player_Id,
            Action.Type_Action_Type (1),
            Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.Selected_Piece.all),
            An_Effect,
            Hexagon.Area.Type_Action_Capabilities_A'
              (1 => The_Window.all.Wnd_Performing_Piece.Selected_Patch.all.Pos));
      end if;

   end On_Button_Search;

   procedure On_Button_Create_Path (Object : access Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Create_Path - clicked");
      end if;

      declare
         The_Area : Hexagon.Area
           .Type_Action_Capabilities_A
         (1 .. Integer (Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Length (LB_Selected_Pos)));
         Trav_Pos   : Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Cursor;
         Area_Index : Integer;
         A_Pos      : Hexagon.Type_Hexagon_Position;
      begin

         Area_Index := 1;
         Trav_Pos   := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.First (LB_Selected_Pos);
         while Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Has_Element (Trav_Pos) loop
            A_Pos                 := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Element (Trav_Pos);
            The_Area (Area_Index) := Hexagon.Type_Hexagon_Position'(True, A_Pos.A, A_Pos.B);
            Trav_Pos              := Tubastga_Window_Pkg.Lists.Pos_List_Pkg.Next (Trav_Pos);
            Area_Index            := Area_Index + 1;
         end loop;
--
         Piece.Client_Piece.Grant_Patch_Effect
           (Me_Player_Id,
            Action.Type_Action_Type (1),
            Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
            Effect.Type_Effect'
              (Tubastga_Piece.Effect_Path,
               Integer (Me_Player_Id) * 1000000 +
               Integer (The_Window.all.Wnd_Performing_Piece.Selected_Piece.Id) * 10 +
               0),
            The_Area);
      end;

   end On_Button_Create_Path;

   procedure On_Button_Remove_Path (Object : access Gtk_Button_Record'Class) is

      The_Area : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1);

      use Hexagon.Client_Map;
      use Hexagon;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Remove_Path - clicked");
      end if;

      The_Area (1) :=
        Hexagon.Type_Hexagon_Position'
          (True,
           The_Window.all.Wnd_Performing_Piece.Selected_Patch.all.Pos.A,
           The_Window.all.Wnd_Performing_Piece.Selected_Patch.all.Pos.B);

      Piece.Client_Piece.Grant_Patch_Effect
        (Me_Player_Id,
         Action.Type_Action_Type (1),
         Piece.Type_Piece (The_Window.all.Wnd_Performing_Piece.all.Selected_Piece.all),
         Effect.Type_Effect'
           (Tubastga_Piece.Effect_Path,
            (Integer (Me_Player_Id) * 1000000 +
             Integer (The_Window.all.Wnd_Performing_Piece.Selected_Piece.Id) * 10 +
             1)),
         The_Area);

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

         when GDK_Up =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Up);

         when GDK_Down =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Down);

         when GDK_Left =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Left);

         when GDK_Right =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Right);

         when GDK_space =>
            null;

         when GDK_Home =>
            null;

         when GDK_Page_Up =>
            null;

         when GDK_End =>
            null;

         when GDK_Page_Down =>
            null;

         when GDK_uparrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Up);

         when GDK_downarrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Down);

         when GDK_leftarrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Left);

         when GDK_rightarrow =>
            Tubastga_Window_Pkg.ZoomedView.Scroll_Map (A_Client_Map, Right);

         when GDK_Escape =>
            Tubastga_UI_Aux.UI_State := Tubastga_UI_Aux.Done;

         when GDK_Shift_L =>
            Shift_LR_Pressed := True;

         when GDK_Shift_R =>
            Shift_LR_Pressed := True;

         when others =>
            null;
      end case;

      Button_Event := True;

      return True;
   end On_Keyboard_Key_Press;

   function On_Keyboard_Key_Release
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      Arg1 : Gdk_Event := To_Event (Params, 1);

   begin

      case Get_Key_Val (Arg1) is
         when GDK_Shift_L =>
            Shift_LR_Pressed := False;

         when GDK_Shift_R =>
            Shift_LR_Pressed := False;

         when others =>
            null;
      end case;

      return True;
   end On_Keyboard_Key_Release;

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
         Gtk.Combo_Box_Text.Set_Active
           (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario,
            0);
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
      Gtk.Combo_Box_Text.Remove_All (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario);
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario, 0);

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
      Server_Connection                                      : Utilities.RemoteString.Type_String;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.callbacks.On_Button_Connect - clicked");
      end if;
      TCPIP_Octet1 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet1));
      TCPIP_Octet2 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet2));
      TCPIP_Octet3 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet3));
      TCPIP_Octet4 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.dlgMainMenu.all.Adj_TCPIPOctet4));

      Server_Connection :=
        Utilities.RemoteString.To_Unbounded_String
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
              (The_Window.all.dlgMainMenu.Cmb_Create_Game_Chose_Scenario)),
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
