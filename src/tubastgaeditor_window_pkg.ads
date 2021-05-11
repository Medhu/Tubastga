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

with Gtk.Window;       use Gtk.Window;
with Gtk.Table;        use Gtk.Table;
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Button;       use Gtk.Button;
with Ada;
with Gdk;
with Ada.Strings.Unbounded;
with Gdk.Pixbuf;
with Glib;
with Gtk.Dialog;
with Gtk.Label;        use Gtk.Label;
with Gtk.GEntry;
with Gtk.Combo_Box_Text;
with Gtk.List_Store;
with Gtk.File_Chooser_Dialog;
with Ada.Containers;
with Ada.Containers.Hashed_Maps;
with Gtk.Check_Button;
with Player;           use Player;
with Utilities;
with Gtk.Text_View;    use Gtk.Text_View;
with Gtk.Text_Buffer;  use Gtk.Text_Buffer;
with Gtk.Combo_Box;
with Gtk.Cell_Renderer_Text;
with Landscape;

package TubastgaEditor_Window_Pkg is

   type Type_Graphic_Data is record
      Filename                  : Ada.Strings.Unbounded.Unbounded_String;
      Image_Data                : Gdk.Pixbuf.Gdk_Pixbuf;
      Image_Height, Image_Width : Glib.Gint;
   end record;

   type Type_Landscape_Info is record
      btnLandscape         : Gtk_Button;
      Graphic_Data         : Type_Graphic_Data;
      Minimap_Graphic_Data : Type_Graphic_Data;
      Button_Text          : Ada.Strings.Unbounded.Unbounded_String;
   end record;

   function Hash_Map (P_Map : Positive) return Ada.Containers.Hash_Type;

   function Equivalent_Keys (Left, Right : Positive) return Boolean;

   package Landscape_Info_Pkg is new Ada.Containers.Hashed_Maps
     (Positive,
      Type_Landscape_Info,
      Hash_Map,
      Equivalent_Keys);

   type Window1_Record is new Gtk_Window_Record with record
      Table1   : Gtk_Table;
      Map_Area : Gtk_Drawing_Area;

      Landscape_Info : Landscape_Info_Pkg.Map;

      btnFillAll : Gtk_Button;

      btnWidth1 : Gtk_Button;
      btnWidth2 : Gtk_Button;
      btnWidth3 : Gtk_Button;

      --
      btnScenarioConfig : Gtk_Button;
      btnSave           : Gtk_Button;
      btnLoad           : Gtk_Button;

      dlgFileOpen,
      dlgFileSave : Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog;
   end record;

   type Window1_Access is access all Window1_Record'Class;

   procedure Gtk_New (Window1 : out Window1_Access);
   procedure Initialize (Window1 : access Window1_Record'Class);

   procedure FileOpen_Dialog
     (dlg_FileOpen :    out Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog;
      Parent       : in out Window1_Access);
   procedure FileSave_Dialog
     (dlg_FileSave :    out Gtk.File_Chooser_Dialog.Gtk_File_Chooser_Dialog;
      Parent       : in out Window1_Access);

end TubastgaEditor_Window_Pkg;
