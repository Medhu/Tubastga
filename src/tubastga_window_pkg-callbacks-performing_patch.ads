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
--
package Tubastga_Window_Pkg.Callbacks.Performing_Patch is

   procedure On_Button_Wall1 (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Wall2 (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Wall3 (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Wall4 (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Wall5 (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Wall6 (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Move (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Attack (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Ranged_Attack (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Search (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Promote (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Demote (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Create_Path (Object : access Gtk.Button.Gtk_Button_Record'Class);
   procedure On_Button_Remove_Path (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Performing_Patch_Tree_View
     (Object : access Gtk.Tree_View.Gtk_Tree_View_Record'Class);

   procedure Activate_Action_Buttons
     (P_Window        : in out Type_Wnd_Performing_Patch_Access;
      P_Type_Category : in     Piece.Type_Category);

   procedure Set_Selected_Patch_Window
     (P_Window : in out Type_Wnd_Performing_Patch_Access;
      P_Patch  : in     Hexagon.Client_Map.Type_Client_Patch_Adress);

end Tubastga_Window_Pkg.Callbacks.Performing_Patch;
