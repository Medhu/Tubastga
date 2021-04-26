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
with Gtk;               use Gtk;
with Gtk;               use Gtk;
with Gtk.Main;
with Gtk.Widget;        use Gtk.Widget;
with TubastgaEditor_Window_Pkg; use TubastgaEditor_Window_Pkg;


procedure TubastgaEditor_Window is
   TubastgaEditor_Window : Window1_Access;
begin
   Gtk.Main.Init;

   Gtk_New (TubastgaEditor_Window);

   Show_All (TubastgaEditor_Window);

   TubastgaEditor_Window_Pkg.FileOpen_Dialog(TubastgaEditor_Window.all.dlgFileOpen, TubastgaEditor_Window);
   TubastgaEditor_Window_Pkg.FileSave_Dialog(TubastgaEditor_Window.all.dlgFileSave, TubastgaEditor_Window);
   Gtk.Main.Main;
end TubastgaEditor_Window;
