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
package Tubastga_Window_Pkg.Callbacks.Main_Menu is

   procedure On_Spin_Button_Change (Object : access Gtk.Spin_Button.Gtk_Spin_Button_Record'Class);

   procedure On_Button_Connect (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Disconnect (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Refresh (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Create_Game (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Join_Game (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Leave_Game (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Load_Game (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Save_Game (Object : access Gtk.Button.Gtk_Button_Record'Class);

   procedure On_Button_Close (Object : access Gtk.Button.Gtk_Button_Record'Class);

end Tubastga_Window_Pkg.Callbacks.Main_Menu;
