--
--
--      Tubastga Game - A turn based strategy game
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

with Gtk.Arguments;
with Gtk.Widget;      use Gtk.Widget;
with Cairo;
with Gtk.Spin_Button; use Gtk.Spin_Button;

package Tubastga_Window_Pkg.Callbacks is
   function Periodic_Updates_Summary return Boolean;

   procedure On_Map_Area_Show (Object : access Gtk_Drawing_Area_Record'Class);

   procedure On_Player_Timer_Area_Show (Object : access Gtk_Drawing_Area_Record'Class);

   function On_Map_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Player_1_Timer_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Player_2_Timer_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Player_3_Timer_Area_Expose_Event
     (Object : access Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Map_Area_Button_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Map_Area_Scroll_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Map_Area_Motion_Notify_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Button_End_Turn (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Sentry (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Bowman (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Carrier (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Ship (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Farm (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Tower (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Lumberjack (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Knight (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Stonecutter (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Button_Wall1 (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Wall2 (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Wall3 (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Wall4 (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Wall5 (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Wall6 (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Move (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Attack (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Search (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Promote (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Demote (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Create_Path (Object : access Gtk_Button_Record'Class);
   procedure On_Button_Remove_Path (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Game (Object : access Gtk_Tool_Button_Record'Class);

   procedure On_Spin_Button_Change (Object : access Gtk_Spin_Button_Record'Class);

   procedure On_Button_Connect (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Disconnect (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Refresh (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Create_Game (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Join_Game (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Leave_Game (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Load_Game (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Save_Game (Object : access Gtk_Button_Record'Class);

   procedure On_Button_Close (Object : access Gtk_Button_Record'Class);

   function On_Keyboard_Key_Press
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure Exit_Main (Object : access Gtk_Widget_Record'Class);

end Tubastga_Window_Pkg.Callbacks;
