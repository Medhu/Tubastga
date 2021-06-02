--
--
--      Tubastga - Scenario Editor
--      Copyright (C) 2021  Frank J Jorgensen
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
with Glib;                       use Glib;
with Gtk;                        use Gtk;
with Gtk.Widget;                 use Gtk.Widget;
with Gtk.Enums;                  use Gtk.Enums;
with Gtkada.Handlers;            use Gtkada.Handlers;
with Callbacks_tubastga;         use Callbacks_tubastga;
with TubastgaEditor_Window_Intl; use TubastgaEditor_Window_Intl;
with TubastgaEditor_Window_Pkg.Callbacks;
use TubastgaEditor_Window_Pkg.Callbacks;
with Tubastga_Window_Pkg.Images;
with Gdk.Event; use Gdk.Event;
with Text_IO;
with Tubastga_Game;
--
--
with Gdk.Event;  use Gdk.Event;
with Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
with Gtk.Box;
with Gtk.File_Chooser;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Ada.Strings;
with Ada.Streams;
with Ada.Streams.Stream_IO;
with Gtk.Combo_Box;
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Layout;

--
package body TubastgaEditor_Window_Pkg is

   Verbose : constant Boolean := True;

   procedure Gtk_New (Window1 : out Window1_Access) is
   begin
      if Verbose then
         Text_IO.Put_Line ("TubastgaEditor_Window_Pkg.Gtk_New Window1- enter");
      end if;

      Window1 := new Window1_Record;
      TubastgaEditor_Window_Pkg.Initialize (Window1);

      if Verbose then
         Text_IO.Put_Line ("TubastgaEditor_Window_Pkg.Gtk_New Window1- exit");
      end if;
   end Gtk_New;

   procedure Initialize (Window1 : access Window1_Record'Class) is
      pragma Suppress (All_Checks);

   begin
      Gtk.Window.Initialize (Window1, Window_Toplevel);
      Set_Title (Window1, -"Tubastga Scenario Editor");
      Set_Position (Window1, Win_Pos_None);
      Set_Modal (Window1, False);
      Set_Resizable (Window1, True);
      Set_Default_Size (Window1, 1300, 00);

      Gtk_New (Window1.Table1, 14, 21, False);
      Set_Row_Spacings (Window1.Table1, 0);
      Set_Col_Spacings (Window1.Table1, 0);

      Gtk_New (Window1.Map_Area);
      Set_Size_Request (Window1.Map_Area, 700, 600);

      Attach
        (Window1.Table1, Window1.Map_Area, Left_Attach => 2,
         Right_Attach => 10, Top_Attach => 0, Bottom_Attach => 19,
         Xpadding                                      => 0, Ypadding => 0);

      Gtk_New (Window1.all.btnLandscapeGrass, "Grass");
      Set_Relief (Window1.all.btnLandscapeGrass, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnLandscapeGrass, Left_Attach => 0,
         Right_Attach => 1, Top_Attach => 0, Bottom_Attach => 1,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.all.btnLandscapeForest, "Forest");
      Set_Relief (Window1.all.btnLandscapeForest, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnLandscapeForest, Left_Attach => 1,
         Right_Attach => 2, Top_Attach => 0, Bottom_Attach => 1,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.all.btnLandscapeWater, "Water");
      Set_Relief (Window1.all.btnLandscapeWater, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnLandscapeWater, Left_Attach => 0,
         Right_Attach => 1, Top_Attach => 1, Bottom_Attach => 2,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.all.btnLandscapeMountain, "Mountain");
      Set_Relief (Window1.all.btnLandscapeMountain, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnLandscapeMountain, Left_Attach => 1,
         Right_Attach => 2, Top_Attach => 1, Bottom_Attach => 2,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.btnWidth1, -"Width 1");
      Set_Relief (Window1.btnWidth1, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnWidth1, Left_Attach => 0,
         Right_Attach => 1, Top_Attach => 9, Bottom_Attach => 10,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.btnWidth2, -"Width 2");
      Set_Relief (Window1.btnWidth2, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnWidth2, Left_Attach => 1,
         Right_Attach => 2, Top_Attach => 9, Bottom_Attach => 10,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.btnWidth3, -"Width 3");
      Set_Relief (Window1.btnWidth3, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnWidth3, Left_Attach => 0,
         Right_Attach => 1, Top_Attach => 10, Bottom_Attach => 11,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.btnFillAll, -"Fill");
      Set_Relief (Window1.btnFillAll, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnFillAll, Left_Attach => 1,
         Right_Attach => 2, Top_Attach => 10, Bottom_Attach => 11,
         Xoptions => Fill, Xpadding => 0, Ypadding => 0);

      Gtk_New (Window1.btnSave, -"Save");
      Set_Relief (Window1.btnSave, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnSave, Left_Attach => 0, Right_Attach => 1,
         Top_Attach => 19, Bottom_Attach => 20, Xoptions => Fill,
         Xpadding                                     => 0, Ypadding => 0);

      Gtk_New (Window1.btnLoad, -"Load");
      Set_Relief (Window1.btnLoad, Relief_Normal);

      Attach
        (Window1.Table1, Window1.btnLoad, Left_Attach => 0, Right_Attach => 1,
         Top_Attach => 20, Bottom_Attach => 21, Xoptions => Fill,
         Xpadding                                     => 0, Ypadding => 0);

      Add (Window1, Window1.Table1);
      --

      --
      Gtk.Drawing_Area.Set_Events
        (Window1.Map_Area,
         Exposure_Mask or Button_Press_Mask or Pointer_Motion_Mask or
         Pointer_Motion_Hint_Mask);

      --  Connect signals

      Drawing_Area_Callback.Connect
        (Window1.Map_Area, "show",
         Drawing_Area_Callback.To_Marshaller (On_Map_Area_Show'Access), False);

      Event_Cb.Connect
        (Window1.Map_Area, Signal_Draw,
         Event_Cb.To_Marshaller
           (On_Map_Area_Expose_Event'Unrestricted_Access));

      Return_Callback.Connect
        (Window1.Map_Area, "button_press_event",
         On_Map_Area_Button_Press_Event'Access, False);

      Return_Callback.Connect
        (Window1.Map_Area, "motion_notify_event",
         On_Map_Area_Motion_Notify_Event'Access, False);

      Button_Cb.Connect
        (Window1.btnLandscapeGrass, "pressed",
         Button_Cb.To_Marshaller (On_Button_Landscape_Grass'Access), False);

      Button_Cb.Connect
        (Window1.btnLandscapeForest, "pressed",
         Button_Cb.To_Marshaller (On_Button_Landscape_Forest'Access), False);

      Button_Cb.Connect
        (Window1.btnLandscapeWater, "pressed",
         Button_Cb.To_Marshaller (On_Button_Landscape_Water'Access), False);

      Button_Cb.Connect
        (Window1.btnLandscapeMountain, "pressed",
         Button_Cb.To_Marshaller (On_Button_Landscape_Mountain'Access), False);

      Button_Cb.Connect
        (Window1.all.btnFillAll, "pressed",
         Button_Cb.To_Marshaller (On_Button_FillAllLandscape'Access), False);

      Button_Cb.Connect
        (Window1.btnWidth1, "pressed",
         Button_Cb.To_Marshaller (On_Button_Width1'Access), False);

      Button_Cb.Connect
        (Window1.btnWidth2, "pressed",
         Button_Cb.To_Marshaller (On_Button_Width2'Access), False);

      Button_Cb.Connect
        (Window1.btnWidth3, "pressed",
         Button_Cb.To_Marshaller (On_Button_Width3'Access), False);
      --

      Button_Cb.Connect
        (Window1.btnSave, "pressed",
         Button_Cb.To_Marshaller (On_Button_Save'Access), False);

      Button_Cb.Connect
        (Window1.btnLoad, "pressed",
         Button_Cb.To_Marshaller (On_Button_Load'Access), False);

      Window_Cb.Connect
        (Window1, "destroy", Window_Cb.To_Marshaller (Exit_Main'Access));

      Return_Callback.Connect
        (Window1, "key_press_event", On_Keyboard_Key_Press'Access);

      Return_Callback.Connect
        (Window1, "key_release_event", On_Keyboard_Key_Released'Access);

      Add_Events
        (Window1.Map_Area,
         Button_Press_Mask or Button_Release_Mask or Button_Motion_Mask or
         Key_Press_Mask);

      Tubastga_Window_Pkg.Images.Initialize
        (Tubastga_Window_Pkg.Images.All_Images);

      Tubastga_Window_Pkg.Images.Load_Images
        (Tubastga_Window_Pkg.Images.All_Images);

      Tubastga_Window_Pkg.Images.Print_Images_List
        (Tubastga_Window_Pkg.Images.All_Images);

      Show_All (Window1);
   end Initialize;

   procedure FileOpen_Dialog
     (dlg_FileOpen :    out Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog;
      Parent       : in out Window1_Access)
   is
      Dummy : Gtk.Widget.Gtk_Widget;
   begin
      Gtk.File_Chooser_Dialog.Gtk_New
        (dlg_FileOpen, "File Open", Parent, Gtk.File_Chooser.Action_Open);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileOpen), "Ok",
           Gtk.Dialog.Gtk_Response_OK);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileOpen), "Cancel",
           Gtk.Dialog.Gtk_Response_Cancel);
   end FileOpen_Dialog;

   procedure FileSave_Dialog
     (dlg_FileSave :    out Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog;
      Parent       : in out Window1_Access)
   is
      Dummy : Gtk.Widget.Gtk_Widget;
   begin
      Gtk.File_Chooser_Dialog.Gtk_New
        (dlg_FileSave, "File Save", Parent, Gtk.File_Chooser.Action_Save);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileSave), "Ok",
           Gtk.Dialog.Gtk_Response_OK);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileSave), "Cancel",
           Gtk.Dialog.Gtk_Response_Cancel);
   end FileSave_Dialog;

end TubastgaEditor_Window_Pkg;
