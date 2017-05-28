
with Gtk.Arguments;
with Gtk.Widget;
with Cairo;
with Tubastga_Window_Pkg.Lists;
with Utilities;
with Player;
package Tubastga_Window_Pkg.Callbacks.Main_Window is
   type Type_Scroll_Direction is (Up, Down, Left, Right);

   function Periodic_Updates_Summary return Boolean;

   procedure On_Map_Area_Show (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class);

   procedure On_Player_Timer_Area_Show (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class);

   function On_Map_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Player_1_Timer_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Player_2_Timer_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Player_3_Timer_Area_Expose_Event
     (Object : access Gtk.Drawing_Area.Gtk_Drawing_Area_Record'Class;
      P_Draw : Cairo.Cairo_Context) return Boolean;

   function On_Map_Area_Button_Press_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Map_Area_Scroll_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Map_Area_Motion_Notify_Event
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Button_Place_Sentry (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Bowman (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Carrier (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Ship (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Farm (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Tower (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Lumberjack (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Knight (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure On_Button_Place_Stonecutter (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   function On_Keyboard_Key_Press
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   function On_Keyboard_Key_Release
     (Object : access Gtk.Widget.Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;

   procedure On_Button_Game (Object : access Gtk.Tool_Button.Gtk_Tool_Button_Record'Class);

   procedure Exit_Main (Object : access Gtk.Widget.Gtk_Widget_Record'Class);

end Tubastga_Window_Pkg.Callbacks.Main_Window;
