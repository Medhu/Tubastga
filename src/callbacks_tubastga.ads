--
--
--      Tubastga Game - A turn based strategy game
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
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Drawing_Area;
with Gtk.Tool_Button;
with Gtk.Widget;
with Gtk.Button;
with Gtk.Spin_Button;
with Gtk.Box;
with Gtk.Tree_View;

package Callbacks_tubastga is

   package Drawing_Area_Callback is new Gtk.Handlers.Callback
     (Gtk.Drawing_Area.Gtk_Drawing_Area_Record);
   package Event_Cb is new Gtk.Handlers.Return_Callback
     (Gtk.Drawing_Area.Gtk_Drawing_Area_Record,
      Boolean);
   package Tool_Button_Callback is new Gtk.Handlers.Callback
     (Gtk.Tool_Button.Gtk_Tool_Button_Record);
   package Window_Cb is new Gtk.Handlers.Callback (Gtk.Widget.Gtk_Widget_Record);
   package Button_Cb is new Gtk.Handlers.Callback (Gtk.Button.Gtk_Button_Record);
   package Spin_Button_Cb is new Gtk.Handlers.Callback (Gtk.Spin_Button.Gtk_Spin_Button_Record);
   package Box_Cb is new Gtk.Handlers.Callback (Gtk.Box.Gtk_Hbox_Record);
   package Tree_View_Cb is new Gtk.Handlers.Callback (Gtk.Tree_View.Gtk_Tree_View_Record);

end Callbacks_tubastga;
