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
with Cairo;
with Gtk.Arguments;
with Gtk.Widget;         use Gtk.Widget;
with Gtk.GEntry;         use Gtk.GEntry;
with Gtk.Combo_Box_Text; use Gtk.Combo_Box_Text;
with Gtk.Combo_Box;      use Gtk.Combo_Box;
with Ada.Containers.Vectors;
with Hexagon;            use Hexagon;

package TubastgaEditor_Window_Pkg.Callbacks is

   procedure On_Button_Save (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Load (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Check_Path (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Check_Navigation (Object : access Gtk_Button_Record'Class);

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class);

   procedure On_Button_Landscape_Grass
     (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Landscape_Forest
     (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Landscape_Water
     (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Landscape_Mountain
     (Object : access Gtk_Button_Record'Class);

   procedure On_Button_FillAllLandscape
     (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Width1 (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Width2 (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Width3 (Object : access Gtk_Button_Record'Class);

   procedure On_Map_Area_Show (Object : access Gtk_Drawing_Area_Record'Class);

   function On_Map_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Map_Area_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Map_Area_Motion_Notify_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Keyboard_Key_Press (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Keyboard_Key_Released (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

end TubastgaEditor_Window_Pkg.Callbacks;
