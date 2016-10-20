--
--
--      Tubastga Game - A turn based strategy game
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
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Drawing_Area; use Gtk.Drawing_Area;
with Gtk.Tool_Button;  use Gtk.Tool_Button;
with Gtk.Widget;       use Gtk.Widget;
with Gtk.Button;       use Gtk.Button;
with Gtk.Spin_Button;  use Gtk.Spin_Button;

package Callbacks_tubastga is

   package Drawing_Area_Callback is new Gtk.Handlers.Callback (Gtk_Drawing_Area_Record);
   package Event_Cb is new Gtk.Handlers.Return_Callback (Gtk_Drawing_Area_Record, Boolean);
   package Tool_Button_Callback is new Gtk.Handlers.Callback (Gtk_Tool_Button_Record);
   package Window_Cb is new Gtk.Handlers.Callback (Gtk_Widget_Record);
   package Button_Cb is new Gtk.Handlers.Callback (Gtk_Button_Record);
   package Spin_Button_Cb is new Gtk.Handlers.Callback (Gtk_Spin_Button_Record);

end Callbacks_tubastga;


