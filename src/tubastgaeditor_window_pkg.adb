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

   function Hash_Map (P_Map : Landscape.Type_Landscape) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type (P_Map);
   end Hash_Map;

   function Equivalent_Keys (Left, Right : Landscape.Type_Landscape) return Boolean is
      use Landscape;
   begin
      return Left = Right;
   end Equivalent_Keys;

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

      Trav_Landscape                        : Landscape_Info_Pkg.Cursor;
      Top_Border_Attach, Left_Border_Attach : Integer;
      A_Landscape                           : Type_Landscape_Info;
      A_Key                                 : Landscape.Type_Landscape;

   begin
      Gtk.Window.Initialize (Window1, Window_Toplevel);
      Set_Title (Window1, -"Scenario Editor");
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
        (Window1.Table1,
         Window1.Map_Area,
         Left_Attach   => 2,
         Right_Attach  => 10,
         Top_Attach    => 0,
         Bottom_Attach => 19,
         Xpadding      => 0,
         Ypadding      => 0);

      TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Insert
        (Window1.all.Landscape_Info,
         Tubastga_Game.Landscape_Grass,
         TubastgaEditor_Window_Pkg.Type_Landscape_Info'
           (null,
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\grass_hexagon.png"),
               null,
               0,
               0),
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\minimap_grass_hexagon.png"),
               null,
               0,
               0),
            Ada.Strings.Unbounded.To_Unbounded_String ("Grass")));

      TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Insert
        (Window1.all.Landscape_Info,
         101,
         TubastgaEditor_Window_Pkg.Type_Landscape_Info'
           (null,
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\forest_hexagon.png"),
               null,
               0,
               0),
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\minimap_forest_hexagon.png"),
               null,
               0,
               0),
            Ada.Strings.Unbounded.To_Unbounded_String ("Forest")));
      TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Insert
        (Window1.all.Landscape_Info,
         102,
         TubastgaEditor_Window_Pkg.Type_Landscape_Info'
           (null,
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\water_hexagon.png"),
               null,
               0,
               0),
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\minimap_water_hexagon.png"),
               null,
               0,
               0),
            Ada.Strings.Unbounded.To_Unbounded_String ("Water")));
      TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Insert
        (Window1.all.Landscape_Info,
         103,
         TubastgaEditor_Window_Pkg.Type_Landscape_Info'
           (null,
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\mountain_hexagon.png"),
               null,
               0,
               0),
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\minimap_mountain_hexagon.png"),
               null,
               0,
               0),
            Ada.Strings.Unbounded.To_Unbounded_String ("Mountain")));
      TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Insert
        (Window1.all.Landscape_Info,
         104,
         TubastgaEditor_Window_Pkg.Type_Landscape_Info'
           (null,
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\solid_rock_hexagon.png"),
               null,
               0,
               0),
            Type_Graphic_Data'
              (Ada.Strings.Unbounded.To_Unbounded_String
                 ("resources\minimap_solid_rock_hexagon.png"),
               null,
               0,
               0),
            Ada.Strings.Unbounded.To_Unbounded_String ("Solid Rock")));

      --
      --  Buttons
      Top_Border_Attach  := 0;
      Left_Border_Attach := 0;
      Trav_Landscape := Landscape_Info_Pkg.First (Window1.all.Landscape_Info);
      while Landscape_Info_Pkg.Has_Element (Trav_Landscape) loop
         A_Landscape :=
           TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Element
             (Trav_Landscape);
         A_Key :=
           TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Key (Trav_Landscape);
         Gtk_New
           (A_Landscape.btnLandscape,
            Ada.Strings.Unbounded.To_String
              (TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Element
                 (Trav_Landscape)
                 .Button_Text));
         Set_Relief (A_Landscape.btnLandscape, Relief_Normal);

         if Top_Border_Attach = 4 then
            Top_Border_Attach  := 0;
            Left_Border_Attach := 1;
         end if;

         Attach
           (Window1.Table1,
            A_Landscape.btnLandscape,
            Left_Attach   => Guint (Left_Border_Attach),
            Right_Attach  => Guint (Left_Border_Attach + 1),
            Top_Attach    => Guint (Top_Border_Attach),
            Bottom_Attach => Guint (Top_Border_Attach + 1),
            Xoptions      => Fill,
            Xpadding      => 0,
            Ypadding      => 0);

         TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Include
           (Window1.all.Landscape_Info,
            A_Key,
            A_Landscape);

         Top_Border_Attach := Top_Border_Attach + 1;
         Trav_Landscape    := Landscape_Info_Pkg.Next (Trav_Landscape);
      end loop;

      Gtk_New (Window1.btnWidth1, -"Width 1");
      Set_Relief (Window1.btnWidth1, Relief_Normal);

      Attach
        (Window1.Table1,
         Window1.btnWidth1,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 9,
         Bottom_Attach => 10,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (Window1.btnWidth2, -"Width 2");
      Set_Relief (Window1.btnWidth2, Relief_Normal);

      Attach
        (Window1.Table1,
         Window1.btnWidth2,
         Left_Attach   => 1,
         Right_Attach  => 2,
         Top_Attach    => 9,
         Bottom_Attach => 10,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (Window1.btnWidth3, -"Width 3");
      Set_Relief (Window1.btnWidth3, Relief_Normal);

      Attach
        (Window1.Table1,
         Window1.btnWidth3,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 10,
         Bottom_Attach => 11,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (Window1.btnFillAll, -"Fill");
      Set_Relief (Window1.btnFillAll, Relief_Normal);

      Attach
        (Window1.Table1,
         Window1.btnFillAll,
         Left_Attach   => 1,
         Right_Attach  => 2,
         Top_Attach    => 10,
         Bottom_Attach => 11,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      --
      Gtk_New (Window1.btnScenarioConfig, -"Scenario Config");
      Set_Relief (Window1.btnScenarioConfig, Relief_Normal);

      Attach
        (Window1.Table1,
         Window1.btnScenarioConfig,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 18,
         Bottom_Attach => 19,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (Window1.btnSave, -"Save");
      Set_Relief (Window1.btnSave, Relief_Normal);

      Attach
        (Window1.Table1,
         Window1.btnSave,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 19,
         Bottom_Attach => 20,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (Window1.btnLoad, -"Load");
      Set_Relief (Window1.btnLoad, Relief_Normal);

      Attach
        (Window1.Table1,
         Window1.btnLoad,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 20,
         Bottom_Attach => 21,
         Xoptions      => Fill,
         Xpadding      => 0,
         Ypadding      => 0);

      Add (Window1, Window1.Table1);
      --

      --
      Gtk.Drawing_Area.Set_Events
        (Window1.Map_Area,
         Exposure_Mask or
         Button_Press_Mask or
         Pointer_Motion_Mask or
         Pointer_Motion_Hint_Mask);

      --  Connect signals

      Drawing_Area_Callback.Connect
        (Window1.Map_Area,
         "show",
         Drawing_Area_Callback.To_Marshaller
           (On_Map_Area_Show'Access),
         False);

      Event_Cb.Connect
        (Window1.Map_Area,
         Signal_Draw,
         Event_Cb.To_Marshaller
           (On_Map_Area_Expose_Event'Unrestricted_Access));

      Return_Callback.Connect
        (Window1.Map_Area,
         "button_press_event",
         On_Map_Area_Button_Press_Event'Access,
         False);

      Return_Callback.Connect
        (Window1.Map_Area,
         "motion_notify_event",
         On_Map_Area_Motion_Notify_Event'Access,
         False);

      Trav_Landscape := Landscape_Info_Pkg.First (Window1.all.Landscape_Info);
      while Landscape_Info_Pkg.Has_Element (Trav_Landscape) loop
         A_Landscape :=
           TubastgaEditor_Window_Pkg.Landscape_Info_Pkg.Element
             (Trav_Landscape);
         Text_IO.Put_Line
           ("A_Landscape=" &
            Ada.Strings.Unbounded.To_String (A_Landscape.Button_Text));
         Button_Cb.Connect
           (A_Landscape.btnLandscape,
            "pressed",
            Button_Cb.To_Marshaller
              (On_Button_Landscape'Access),
            False);

         Landscape_Info_Pkg.Replace_Element
           (Window1.all.Landscape_Info,
            Trav_Landscape,
            A_Landscape);
         Trav_Landscape := Landscape_Info_Pkg.Next (Trav_Landscape);
      end loop;
      --

      Button_Cb.Connect
        (Window1.all.btnFillAll,
         "pressed",
         Button_Cb.To_Marshaller
           (On_Button_FillAllLandscape'Access),
         False);

      Button_Cb.Connect
        (Window1.btnWidth1,
         "pressed",
         Button_Cb.To_Marshaller (On_Button_Width1'Access),
         False);

      Button_Cb.Connect
        (Window1.btnWidth2,
         "pressed",
         Button_Cb.To_Marshaller (On_Button_Width2'Access),
         False);

      Button_Cb.Connect
        (Window1.btnWidth3,
         "pressed",
         Button_Cb.To_Marshaller (On_Button_Width3'Access),
         False);
      --


      Button_Cb.Connect
        (Window1.btnSave,
         "pressed",
         Button_Cb.To_Marshaller (On_Button_Save'Access),
         False);

      Button_Cb.Connect
        (Window1.btnLoad,
         "pressed",
         Button_Cb.To_Marshaller (On_Button_Load'Access),
         False);

      Window_Cb.Connect
        (Window1,
         "destroy",
         Window_Cb.To_Marshaller (Exit_Main'Access));

      Return_Callback.Connect
        (Window1,
         "key_press_event",
         On_Keyboard_Key_Press'Access);

      Return_Callback.Connect
        (Window1,
         "key_release_event",
         On_Keyboard_Key_Released'Access);

      Add_Events
        (Window1.Map_Area,
         Button_Press_Mask or
         Button_Release_Mask or
         Button_Motion_Mask or
         Key_Press_Mask);

      Tubastga_Window_Pkg.Images.Initialize (Tubastga_Window_Pkg.Images.All_Images);

      Tubastga_Window_Pkg.Images.Load_Images (Tubastga_Window_Pkg.Images.All_Images);

      Tubastga_Window_Pkg.Images.Print_Images_List (Tubastga_Window_Pkg.Images.All_Images);

      Show_All (Window1);
   end Initialize;

   procedure FileOpen_Dialog
     (dlg_FileOpen :    out Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog;
      Parent       : in out Window1_Access)
   is
      Dummy : Gtk.Widget.Gtk_Widget;
   begin
      Gtk.File_Chooser_Dialog.Gtk_New
        (dlg_FileOpen,
         "File Open",
         Parent,
         Gtk.File_Chooser.Action_Open);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileOpen),
           "Ok",
           Gtk.Dialog.Gtk_Response_OK);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileOpen),
           "Cancel",
           Gtk.Dialog.Gtk_Response_Cancel);
   end FileOpen_Dialog;

   procedure FileSave_Dialog
     (dlg_FileSave :    out Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog;
      Parent       : in out Window1_Access)
   is
      Dummy : Gtk.Widget.Gtk_Widget;
   begin
      Gtk.File_Chooser_Dialog.Gtk_New
        (dlg_FileSave,
         "File Save",
         Parent,
         Gtk.File_Chooser.Action_Save);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileSave),
           "Ok",
           Gtk.Dialog.Gtk_Response_OK);
      Dummy :=
        Gtk.Dialog.Add_Button
          (Gtk.Dialog.Gtk_Dialog (dlg_FileSave),
           "Cancel",
           Gtk.Dialog.Gtk_Response_Cancel);
   end FileSave_Dialog;

end TubastgaEditor_Window_Pkg;
