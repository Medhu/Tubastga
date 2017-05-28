--
--
--      Tubastga a Turn based strategy game.
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
with Gtk.Main;
with Tubastga_Window_Pkg.Callbacks.Main_Window; use Tubastga_Window_Pkg.Callbacks;
with Tubastga_Window_Pkg;           use Tubastga_Window_Pkg;
with Client.Server_Adm;
with Glib.Main;
with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Tubastga_UI_Aux;
with Utilities;
with Piece;
with Piece.Client_Piece;

procedure Tubastga_Window is
   Tubastga_Window : Type_Wnd_Main_Access;
   Id1        : Glib.Main.G_Source_Id;

   My_Playername : Utilities.RemoteString.Type_String;

   Tubastga_Client_Class : Tubastga_Window_Pkg.Type_Client_Access_Class;

begin
   Tubastga_UI_Aux.Set_My_Player_Name (Utilities.RemoteString.To_Unbounded_String ("A Player"));
   Tubastga_UI_Aux.Set_Server_Port (4001);
   Tubastga_UI_Aux.Set_TCPIP_Adress (10, 0, 1, 7);
   for Arguments_Index in 1 .. Ada.Command_Line.Argument_Count loop

      if Ada.Command_Line.Argument (Arguments_Index)'Length > 7
        and then Ada.Strings.Fixed.Translate
                    (Ada.Command_Line.Argument (Arguments_Index) (1 .. 7),
                     Ada.Strings.Maps.Constants.Lower_Case_Map) =
                 "player:"
      then
         My_Playername :=
            Utilities.RemoteString.To_Unbounded_String
              (Ada.Command_Line.Argument (Arguments_Index) (
           8 .. Ada.Command_Line.Argument (Arguments_Index)'Length));

         Tubastga_UI_Aux.Set_My_Player_Name (My_Playername);
      end if;

   end loop;

   Client.Server_Adm.Init;

   Tubastga_Client_Class := new Tubastga_Window_Pkg.Type_Client_Piece;

   Piece.Client_Piece.Init(Tubastga_Client_Class.all);

   Gtk.Main.Init;
   Id1 := Glib.Main.Idle_Add (Tubastga_Window_Pkg.Callbacks.Main_Window.Periodic_Updates_Summary'Access);

   Tubastga_Window_Pkg.Gtk_New (Tubastga_Window);
   Tubastga_Window_Pkg.Show_All (Tubastga_Window);

   Gtk_New (Tubastga_Window.all.Dlg_Main_Menu);

   Gtk.Main.Main;
end Tubastga_Window;
