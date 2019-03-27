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
with Glib;
with Gtk.Widget;
with Gtk.Enums;
with Gtkada.Handlers;
with Callbacks_tubastga;
with Tubastga_Window_Intl; use Tubastga_Window_Intl;
with Tubastga_Window_Pkg.Callbacks;
with Tubastga_Window_Pkg.Callbacks.Main_Window;
with Tubastga_Window_Pkg.Callbacks.Performing_Patch;
with Tubastga_Window_Pkg.Callbacks.Target_Patch;
with Tubastga_Window_Pkg.Callbacks.Main_Menu;
with Gdk.Color;
with Text_IO;
with Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;

--
--
with Gdk.Event;
with Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
with Gtk.Image;
with Gtk.Adjustment;
with Tubastga_UI_Aux;
with Utilities;
with Gtk.Tree_View_Column;
with Tubastga_UI_Resources;

package body Tubastga_Window_Pkg is

   Verbose : constant Boolean := False;

   procedure Gtk_New (P_Dlg_Main_Menu : out Type_Dlg_Main_Menu_Access) is
   begin
      P_Dlg_Main_Menu := new Type_Dlg_Main_Menu_Record;
      Tubastga_Window_Pkg.Initialize (P_Dlg_Main_Menu);
   end Gtk_New;

   procedure Initialize (P_Dlg_Main_Menu : access Type_Dlg_Main_Menu_Record'Class) is
      TCPIPOctet1, TCPIPOctet2, TCPIPOctet3, TCPIPOctet4 : Natural;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Dlg_Main_Menu) - enter");
      end if;

      Gtk.Dialog.Initialize
        (Gtk.Dialog.Gtk_Dialog (P_Dlg_Main_Menu),
         "Main Menu",
         Flags => Gtk.Dialog.Destroy_With_Parent);
      Gtk.Table.Gtk_New (P_Dlg_Main_Menu.all.Layout_Table, 9, 2, False);

      Gtk.Box.Gtk_New (P_Dlg_Main_Menu.all.Box_Connect, Gtk.Enums.Orientation_Horizontal, Glib.Gint (0));
      Tubastga_UI_Aux.Get_TCPIP_Adress (TCPIPOctet1, TCPIPOctet2, TCPIPOctet3, TCPIPOctet4);
      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.TCPIPLabel, "Server TCP/IP:");
      Gtk.Adjustment.Gtk_New
        (P_Dlg_Main_Menu.all.Adj_TCPIPOctet1,
         Glib.Gdouble (TCPIPOctet1),
         Glib.Gdouble (0),
         Glib.Gdouble (255),
         Glib.Gdouble (1),
         Glib.Gdouble (1),
         Glib.Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (P_Dlg_Main_Menu.all.Adj_TCPIPOctet2,
         Glib.Gdouble (TCPIPOctet2),
         Glib.Gdouble (0),
         Glib.Gdouble (255),
         Glib.Gdouble (1),
         Glib.Gdouble (1),
         Glib.Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (P_Dlg_Main_Menu.all.Adj_TCPIPOctet3,
         Glib.Gdouble (TCPIPOctet3),
         Glib.Gdouble (0),
         Glib.Gdouble (255),
         Glib.Gdouble (1),
         Glib.Gdouble (1),
         Glib.Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (P_Dlg_Main_Menu.all.Adj_TCPIPOctet4,
         Glib.Gdouble (TCPIPOctet4),
         Glib.Gdouble (0),
         Glib.Gdouble (255),
         Glib.Gdouble (1),
         Glib.Gdouble (1),
         Glib.Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (P_Dlg_Main_Menu.all.Adj_TCPIPPort,
         Glib.Gdouble (0),
         Glib.Gdouble (1),
         Glib.Gdouble (65535),
         Glib.Gdouble (1),
         Glib.Gdouble (1),
         Glib.Gdouble (1));

      Gtk.Spin_Button.Gtk_New
        (P_Dlg_Main_Menu.all.Spn_TCPIPOctet1,
         P_Dlg_Main_Menu.all.Adj_TCPIPOctet1,
         Glib.Gdouble (1),
         0);
      Gtk.Spin_Button.Gtk_New
        (P_Dlg_Main_Menu.all.Spn_TCPIPOctet2,
         P_Dlg_Main_Menu.all.Adj_TCPIPOctet2,
         Glib.Gdouble (1),
         0);
      Gtk.Spin_Button.Gtk_New
        (P_Dlg_Main_Menu.all.Spn_TCPIPOctet3,
         P_Dlg_Main_Menu.all.Adj_TCPIPOctet3,
         Glib.Gdouble (1),
         0);
      Gtk.Spin_Button.Gtk_New
        (P_Dlg_Main_Menu.all.Spn_TCPIPOctet4,
         P_Dlg_Main_Menu.all.Adj_TCPIPOctet4,
         Glib.Gdouble (1),
         0);
      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.TCPIPPortLabel, "Port:");

      Gtk.Spin_Button.Gtk_New
        (P_Dlg_Main_Menu.all.Spn_TCPIPPort,
         P_Dlg_Main_Menu.all.Adj_TCPIPPort,
         Glib.Gdouble (1),
         0);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Connect,
         P_Dlg_Main_Menu.all.TCPIPLabel,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Connect,
         P_Dlg_Main_Menu.all.Spn_TCPIPOctet1,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Connect,
         P_Dlg_Main_Menu.all.Spn_TCPIPOctet2,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Connect,
         P_Dlg_Main_Menu.all.Spn_TCPIPOctet3,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Connect,
         P_Dlg_Main_Menu.all.Spn_TCPIPOctet4,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Connect,
         P_Dlg_Main_Menu.all.TCPIPPortLabel,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Connect,
         P_Dlg_Main_Menu.all.Spn_TCPIPPort,
         False,
         False,
         2);

      Gtk.Spin_Button.Set_Value
        (Gtk.Spin_Button.Gtk_Spin_Button (P_Dlg_Main_Menu.all.Spn_TCPIPPort),
         Glib.Gdouble (Tubastga_UI_Aux.Get_Server_Port));

      --
      --      Player info
      Gtk.Box.Gtk_New
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         Gtk.Enums.Orientation_Horizontal,
         Glib.Gint (0));
      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_1, "Player 1 Name:");
      Gtk.GEntry.Gtk_New (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_1);
      Gtk.GEntry.Set_Max_Length (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_1, Glib.Gint (10));
      Gtk.GEntry.Set_Width_Chars (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_1, Glib.Gint (11));

      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_2, "Player 2 Name:");
      Gtk.GEntry.Gtk_New (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_2);
      Gtk.GEntry.Set_Max_Length (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_2, Glib.Gint (10));
      Gtk.GEntry.Set_Width_Chars (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_2, Glib.Gint (11));

      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_3, "Player 3 Name:");
      Gtk.GEntry.Gtk_New (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_3);
      Gtk.GEntry.Set_Max_Length (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_3, Glib.Gint (10));
      Gtk.GEntry.Set_Width_Chars (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_3, Glib.Gint (11));

      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Create_Game_Chose_Scenario, "Chose scenario:");
      Gtk.Combo_Box_Text.Gtk_New (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario);
      Gtk.Combo_Box_Text.Append_Text
        (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario, 0);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_1,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_1,
         False,
         False,
         2);

      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_2,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_2,
         False,
         False,
         2);

      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_3,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_3,
         False,
         False,
         2);

      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.Lbl_Create_Game_Chose_Scenario,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Create_Game,
         P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario,
         False,
         False,
         2);
      --
      Gtk.Box.Show (P_Dlg_Main_Menu.all.Box_Create_Game);
      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_1);
      Gtk.GEntry.Show (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_1);

      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_2);
      Gtk.GEntry.Show (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_2);

      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Create_Game_Player_Name_3);
      Gtk.GEntry.Show (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_3);

      Gtk.Combo_Box_Text.Show (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario);
      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Create_Game_Chose_Scenario);

      Gtk.GEntry.Set_Text
        (P_Dlg_Main_Menu.all.En_Create_Game_Player_Name_1,
         Utilities.RemoteString.To_String (Tubastga_UI_Aux.Get_My_Player_Name));

      --
      Gtk.Box.Gtk_New
        (P_Dlg_Main_Menu.all.Box_Load_Game,
         Gtk.Enums.Orientation_Horizontal,
         Glib.Gint (0));
      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Load_Game_Name, "Chose game to load:");
      Gtk.Combo_Box_Text.Gtk_New (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name);
      Gtk.Combo_Box_Text.Append_Text
        (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name, 0);

      Gtk.Combo_Box_Text.Show (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name);
      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Load_Game_Name);
      Gtk.Box.Show (P_Dlg_Main_Menu.all.Box_Load_Game);

      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Load_Game,
         P_Dlg_Main_Menu.all.Lbl_Load_Game_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Load_Game,
         P_Dlg_Main_Menu.all.Cmb_Load_Game_Name,
         False,
         False,
         2);
      --
      --
      Gtk.Box.Gtk_New
        (P_Dlg_Main_Menu.all.Box_Save_Game,
         Gtk.Enums.Orientation_Horizontal,
         Glib.Gint (0));
      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Save_Game_Name, "Save game as... :");
      Gtk.GEntry.Gtk_New (P_Dlg_Main_Menu.all.En_Save_Game_Name);
      Gtk.GEntry.Set_Max_Length (P_Dlg_Main_Menu.all.En_Save_Game_Name, 20);
      Gtk.GEntry.Set_Width_Chars (P_Dlg_Main_Menu.all.En_Save_Game_Name, 21);

      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Save_Game_Name);
      Gtk.GEntry.Show (P_Dlg_Main_Menu.all.En_Save_Game_Name);
      Gtk.Box.Show (P_Dlg_Main_Menu.all.Box_Save_Game);

      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Save_Game,
         P_Dlg_Main_Menu.all.Lbl_Save_Game_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Save_Game,
         P_Dlg_Main_Menu.all.En_Save_Game_Name,
         False,
         False,
         2);
      --
      Gtk.Box.Gtk_New
        (P_Dlg_Main_Menu.all.Box_Join_Game,
         Gtk.Enums.Orientation_Horizontal,
         Glib.Gint (0));
      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Join_Player_Name, "Player Name");
      Gtk.Combo_Box_Text.Gtk_New (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name);
      Gtk.Combo_Box_Text.Append_Text
        (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name, 0);
      Gtk.Label.Gtk_New (P_Dlg_Main_Menu.all.Lbl_Join_Game, "description of connection");
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Join_Game,
         P_Dlg_Main_Menu.all.Lbl_Join_Player_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Join_Game,
         P_Dlg_Main_Menu.all.Cmb_Join_Player_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (P_Dlg_Main_Menu.all.Box_Join_Game,
         P_Dlg_Main_Menu.all.Lbl_Join_Game,
         False,
         False,
         2);
      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Join_Player_Name);
      Gtk.Combo_Box_Text.Show (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name);
      Gtk.Label.Show (P_Dlg_Main_Menu.all.Lbl_Join_Game);
      Gtk.Box.Show (P_Dlg_Main_Menu.all.Box_Join_Game);
      --
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Box_Connect,
         1,
         2,
         0,
         1);
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Box_Create_Game,
         1,
         2,
         2,
         3);
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Box_Load_Game,
         1,
         2,
         3,
         4);
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Box_Save_Game,
         1,
         2,
         4,
         5);
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Box_Join_Game,
         1,
         2,
         5,
         6);

      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Connect_Server, "Connect Server");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Connect_Server,
         0,
         1,
         0,
         1);

      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Refresh, "Refresh");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Refresh,
         0,
         1,
         1,
         2);

      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Create_Game, "Create Game");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Create_Game,
         0,
         1,
         2,
         3);
      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Load_Game, "Load Game");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Load_Game,
         0,
         1,
         3,
         4);
      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Save_Game, "Save Game");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Save_Game,
         0,
         1,
         4,
         5);
      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Join_Game, "Join Game");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Join_Game,
         0,
         1,
         5,
         6);
      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Leave_Game, "Leave Game");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Leave_Game,
         0,
         1,
         6,
         7);
      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Disconnect_Server, "Disconnect Server");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Disconnect_Server,
         0,
         1,
         7,
         8);
      Gtk.Button.Gtk_New (P_Dlg_Main_Menu.all.Btn_Close, "Close");
      Gtk.Table.Attach
        (P_Dlg_Main_Menu.all.Layout_Table,
         P_Dlg_Main_Menu.all.Btn_Close,
         0,
         1,
         8,
         9);

      Gtk.Box.Pack_Start (Get_Action_Area (P_Dlg_Main_Menu), P_Dlg_Main_Menu.all.Layout_Table);

      Gtk.Table.Show (P_Dlg_Main_Menu.all.Layout_Table);
      Gtk.Box.Show (P_Dlg_Main_Menu.all.Box_Connect);
      Gtk.Label.Show (P_Dlg_Main_Menu.all.TCPIPLabel);
      Gtk.Spin_Button.Show (P_Dlg_Main_Menu.all.Spn_TCPIPOctet1);
      Gtk.Spin_Button.Show (P_Dlg_Main_Menu.all.Spn_TCPIPOctet2);
      Gtk.Spin_Button.Show (P_Dlg_Main_Menu.all.Spn_TCPIPOctet3);
      Gtk.Spin_Button.Show (P_Dlg_Main_Menu.all.Spn_TCPIPOctet4);
      Gtk.Spin_Button.Show (P_Dlg_Main_Menu.all.Spn_TCPIPPort);
      Gtk.Label.Show (P_Dlg_Main_Menu.all.TCPIPPortLabel);

      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Connect_Server);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Refresh);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Create_Game);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Load_Game);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Save_Game);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Join_Game);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Leave_Game);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Disconnect_Server);
      Gtk.Button.Show (P_Dlg_Main_Menu.all.Btn_Close);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Connect_Server,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Connect'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Refresh,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Refresh'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Create_Game,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Create_Game'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Join_Game,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Join_Game'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Leave_Game,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Leave_Game'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Load_Game,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Load_Game'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Save_Game,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Save_Game'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Disconnect_Server,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Disconnect'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Dlg_Main_Menu.Btn_Close,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Close'Access),
         False);

      Callbacks_Tubastga.Spin_Button_Cb.Connect
        (P_Dlg_Main_Menu.Spn_TCPIPOctet1,
         "value-changed",
         Callbacks_Tubastga.Spin_Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Spin_Button_Change'Access),
         False);
      Callbacks_Tubastga.Spin_Button_Cb.Connect
        (P_Dlg_Main_Menu.Spn_TCPIPOctet2,
         "value-changed",
         Callbacks_Tubastga.Spin_Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Spin_Button_Change'Access),
         False);
      Callbacks_Tubastga.Spin_Button_Cb.Connect
        (P_Dlg_Main_Menu.Spn_TCPIPOctet3,
         "value-changed",
         Callbacks_Tubastga.Spin_Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Spin_Button_Change'Access),
         False);
      Callbacks_Tubastga.Spin_Button_Cb.Connect
        (P_Dlg_Main_Menu.Spn_TCPIPOctet4,
         "value-changed",
         Callbacks_Tubastga.Spin_Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Spin_Button_Change'Access),
         False);
      Callbacks_Tubastga.Spin_Button_Cb.Connect
        (P_Dlg_Main_Menu.Spn_TCPIPPort,
         "value-changed",
         Callbacks_Tubastga.Spin_Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Spin_Button_Change'Access),
         False);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Dlg_Main_Menu) - exit");
      end if;

   end Initialize;

   procedure Gtk_New (P_Wnd_Main : out Type_Wnd_Main_Access) is
   begin
      P_Wnd_Main := new Type_Wnd_Main_Record;
      Tubastga_Window_Pkg.Initialize (P_Wnd_Main);
   end Gtk_New;

   procedure Initialize (P_Wnd_Main : access Type_Wnd_Main_Record'Class) is
      pragma Suppress (All_Checks);

      Error : Glib.Error.GError;

      Blue_Color,
      Red_Color,
      Grey_Color,
      Green_Color,
      Orange_Color,
      Yellow_Color : Gdk.Color.Gdk_Color;

      use Gdk.Event;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Wnd_Main) - enter");
      end if;

      Gtk.Window.Initialize (P_Wnd_Main, Gtk.Enums.Window_Toplevel);
      Tubastga_Window_Pkg.Set_Title (P_Wnd_Main, -"Tubast'ga");
      Tubastga_Window_Pkg.Set_Position (P_Wnd_Main, Gtk.Enums.Win_Pos_None);
      Tubastga_Window_Pkg.Set_Modal (P_Wnd_Main, False);
      Tubastga_Window_Pkg.Set_Resizable (P_Wnd_Main, True);

      Gtk.Toolbar.Gtk_New (P_Wnd_Main.Toolbar_Main);
      Gtk.Toolbar.Set_Orientation (P_Wnd_Main.Toolbar_Main, Gtk.Enums.Orientation_Horizontal);
      Gtk.Toolbar.Set_Style (P_Wnd_Main.Toolbar_Main, Gtk.Enums.Toolbar_Icons);

      Gtk.Box.Gtk_New (P_Wnd_Main.Box_Toolbar_Game, Gtk.Enums.Orientation_Vertical, Glib.Gint (2));
      Tubastga_Window_Pkg.Add (P_Wnd_Main, P_Wnd_Main.Box_Toolbar_Game);

      Gtk.Table.Gtk_New (P_Wnd_Main.Layout_Table, 15, 20, False);
      Gtk.Table.Set_Row_Spacings (P_Wnd_Main.Layout_Table, 0);
      Gtk.Table.Set_Col_Spacings (P_Wnd_Main.Layout_Table, 0);

      Gtk.Drawing_Area.Gtk_New (P_Wnd_Main.Area_Map);
      Gtk.Drawing_Area.Set_Size_Request (P_Wnd_Main.Area_Map, 700, 600);

      Gtk.Table.Attach
        (P_Wnd_Main.all.Layout_Table,
         P_Wnd_Main.all.Area_Map,
         Left_Attach   => 1,
         Right_Attach  => 12,
         Top_Attach    => 0,
         Bottom_Attach => 19,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Drawing_Area.Gtk_New (P_Wnd_Main.all.Area_Player_1_Timer);
      Gtk.Drawing_Area.Set_Size_Request (P_Wnd_Main.all.Area_Player_1_Timer, 70, 40);

      Gtk.Table.Attach
        (P_Wnd_Main.all.Layout_Table,
         P_Wnd_Main.all.Area_Player_1_Timer,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Label.Gtk_New (P_Wnd_Main.all.Lbl_Player_1_Name);
      Gtk.Table.Attach
        (P_Wnd_Main.all.Layout_Table,
         P_Wnd_Main.all.Lbl_Player_1_Name,
         Left_Attach   => 14,
         Right_Attach  => 15,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Drawing_Area.Gtk_New (P_Wnd_Main.all.Area_Player_2_Timer);
      Gtk.Drawing_Area.Set_Size_Request (P_Wnd_Main.all.Area_Player_2_Timer, 70, 40);

      Gtk.Table.Attach
        (P_Wnd_Main.all.Layout_Table,
         P_Wnd_Main.all.Area_Player_2_Timer,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 1,
         Bottom_Attach => 2,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Label.Gtk_New (P_Wnd_Main.all.Lbl_Player_2_Name);
      Gtk.Table.Attach
        (P_Wnd_Main.all.Layout_Table,
         P_Wnd_Main.all.Lbl_Player_2_Name,
         Left_Attach   => 14,
         Right_Attach  => 15,
         Top_Attach    => 1,
         Bottom_Attach => 2,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Drawing_Area.Gtk_New (P_Wnd_Main.all.Area_Player_3_Timer);
      Gtk.Drawing_Area.Set_Size_Request (P_Wnd_Main.all.Area_Player_3_Timer, 70, 40);

      Gtk.Table.Attach
        (P_Wnd_Main.all.Layout_Table,
         P_Wnd_Main.all.Area_Player_3_Timer,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 2,
         Bottom_Attach => 3,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Label.Gtk_New (P_Wnd_Main.all.Lbl_Player_3_Name);
      Gtk.Table.Attach
        (P_Wnd_Main.all.Layout_Table,
         P_Wnd_Main.all.Lbl_Player_3_Name,
         Left_Attach   => 14,
         Right_Attach  => 15,
         Top_Attach    => 2,
         Bottom_Attach => 3,
         Xpadding      => 0,
         Ypadding      => 0);

      --
      --
      --
      Gtk.Text_Buffer.Gtk_New (P_Wnd_Main.Buffer_Activity_Report);
      Gtk.Text_View.Gtk_New (P_Wnd_Main.View_Activity_Report, P_Wnd_Main.Buffer_Activity_Report);
      Gtk.Text_View.Set_Size_Request (P_Wnd_Main.View_Activity_Report, 350, 600);

      Gtk.Table.Attach
        (P_Wnd_Main.Layout_Table,
         P_Wnd_Main.View_Activity_Report,
         Left_Attach   => 12,
         Right_Attach  => 13,
         Top_Attach    => 0,
         Bottom_Attach => 19,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Text_Buffer.Gtk_New (P_Wnd_Main.Buffer_Hover_Info);
      Gtk.Text_View.Gtk_New (P_Wnd_Main.View_Hover_Info, P_Wnd_Main.Buffer_Hover_Info);

      Gtk.Table.Attach
        (P_Wnd_Main.Layout_Table,
         P_Wnd_Main.View_Hover_Info,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 3,
         Bottom_Attach => 4,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Text_Buffer.Gtk_New (P_Wnd_Main.Buffer_Resources_Info);
      Gtk.Text_View.Gtk_New (P_Wnd_Main.View_Resources_Info, P_Wnd_Main.Buffer_Resources_Info);

      Gtk.Table.Attach
        (P_Wnd_Main.Layout_Table,
         P_Wnd_Main.View_Resources_Info,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 4,
         Bottom_Attach => 5,
         Xpadding      => 0,
         Ypadding      => 0);

      --
      --  Buttons
      --
      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Sentry);
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Sentry,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_sentry.png"));
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Sentry, "Place Sentry");
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Sentry);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Ship);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Ship, "Place Ship");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Ship,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_ship.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Ship);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Knight);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Knight, "Place Knight");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Knight,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_knight.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Knight);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Bowman);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Bowman, "Place Bowman");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Bowman,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_bowman.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Bowman);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Carrier);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Carrier, "Place Carrier");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Carrier,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_carrier.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Carrier);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Tower);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Tower, "Place Tower");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Tower,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_tower.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Tower);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Farm);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Farm, "Place Farm");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Farm,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_farm.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Farm);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Lumberjack);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Lumberjack, "Place Lumberjack");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Lumberjack,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_lumberjack.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Lumberjack);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Place_Stonecutter);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Place_Stonecutter, "Place Stonecutter");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Place_Stonecutter,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_stonecutter.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Place_Stonecutter);

      Gtk.Tool_Button.Gtk_New (P_Wnd_Main.Btn_Game);
      Gtk.Tool_Button.Set_Label (P_Wnd_Main.Btn_Game, "Game");
      Gtk.Tool_Button.Set_Icon_Widget
        (P_Wnd_Main.Btn_Game,
         Gtk.Image.Gtk_Image_New_From_File ("resources\main_menu.png"));
      Gtk.Toolbar.Insert (P_Wnd_Main.Toolbar_Main, P_Wnd_Main.Btn_Game);

      Gtk.Box.Pack_Start (P_Wnd_Main.Box_Toolbar_Game, P_Wnd_Main.Toolbar_Main, True, True);
      Gtk.Box.Pack_End (P_Wnd_Main.Box_Toolbar_Game, P_Wnd_Main.Layout_Table, True, True);

      Gtk.Drawing_Area.Set_Events
        (P_Wnd_Main.Area_Map,
         Gdk.Event.Exposure_Mask or
         Gdk.Event.Leave_Notify_Mask or
         Gdk.Event.Button_Press_Mask or
         Gdk.Event.Pointer_Motion_Mask or
         Gdk.Event.Pointer_Motion_Hint_Mask);

      --  Connect signals

      Callbacks_Tubastga.Drawing_Area_Callback.Connect
        (P_Wnd_Main.Area_Map,
         Gtk.Widget.Signal_Show,
         Callbacks_Tubastga.Drawing_Area_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Show'Unrestricted_Access));
      Callbacks_Tubastga.Event_Cb.Connect
        (P_Wnd_Main.Area_Map,
         Gtk.Widget.Signal_Draw,
         Callbacks_Tubastga.Event_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Expose_Event'Unrestricted_Access));

      Callbacks_Tubastga.Drawing_Area_Callback.Connect
        (P_Wnd_Main.all.Area_Player_1_Timer,
         Gtk.Widget.Signal_Show,
         Callbacks_Tubastga.Drawing_Area_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_Timer_Area_Show'Unrestricted_Access));
      Callbacks_Tubastga.Event_Cb.Connect
        (P_Wnd_Main.Area_Player_1_Timer,
         Gtk.Widget.Signal_Draw,
         Callbacks_Tubastga.Event_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_1_Timer_Area_Expose_Event'Unrestricted_Access));

      Callbacks_Tubastga.Drawing_Area_Callback.Connect
        (P_Wnd_Main.Area_Player_2_Timer,
         Gtk.Widget.Signal_Show,
         Callbacks_Tubastga.Drawing_Area_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_Timer_Area_Show'Unrestricted_Access));
      Callbacks_Tubastga.Event_Cb.Connect
        (P_Wnd_Main.Area_Player_2_Timer,
         Gtk.Widget.Signal_Draw,
         Callbacks_Tubastga.Event_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_2_Timer_Area_Expose_Event'Unrestricted_Access));

      Callbacks_Tubastga.Drawing_Area_Callback.Connect
        (P_Wnd_Main.Area_Player_3_Timer,
         Gtk.Widget.Signal_Show,
         Callbacks_Tubastga.Drawing_Area_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_Timer_Area_Show'Unrestricted_Access));
      Callbacks_Tubastga.Event_Cb.Connect
        (P_Wnd_Main.Area_Player_3_Timer,
         Gtk.Widget.Signal_Draw,
         Callbacks_Tubastga.Event_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Player_3_Timer_Area_Expose_Event'Unrestricted_Access));

      Callbacks_Tubastga.Window_Cb.Connect
        (P_Wnd_Main,
         "destroy",
         Callbacks_Tubastga.Window_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.Exit_Main'Access));

      Gtkada.Handlers.Return_Callback.Connect
        (P_Wnd_Main.Area_Map,
         "button_press_event",
         Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Button_Press_Event'Access,
         False);

      Gtkada.Handlers.Return_Callback.Connect
        (P_Wnd_Main.Area_Map,
         "scroll_event",
         Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Scroll_Event'Access,
         False);

      Gtkada.Handlers.Return_Callback.Connect
        (P_Wnd_Main.Area_Map,
         "motion_notify_event",
         Tubastga_Window_Pkg.Callbacks.Main_Window.On_Map_Area_Motion_Notify_Event'Access,
         False);

      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Sentry,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Sentry'Access),
         False);
      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Bowman,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Bowman'Access),
         False);
      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Carrier,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Carrier'Access),
         False);

      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Ship,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Ship'Access),
         False);
      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Farm,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Farm'Access),
         False);
      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Tower,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Tower'Access),
         False);
      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Lumberjack,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Lumberjack'Access),
         False);
      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Stonecutter,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Stonecutter'Access),
         False);
      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Place_Knight,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Place_Knight'Access),
         False);

      Callbacks_Tubastga.Tool_Button_Callback.Connect
        (P_Wnd_Main.Btn_Game,
         "clicked",
         Callbacks_Tubastga.Tool_Button_Callback.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Main_Window.On_Button_Game'Access),
         False);

      Gtkada.Handlers.Return_Callback.Connect
        (P_Wnd_Main,
         "key_press_event",
         Tubastga_Window_Pkg.Callbacks.Main_Window.On_Keyboard_Key_Press'Access);

      Gtkada.Handlers.Return_Callback.Connect
        (P_Wnd_Main,
         "key_release_event",
         Tubastga_Window_Pkg.Callbacks.Main_Window.On_Keyboard_Key_Release'Access);

      Gtk.Drawing_Area.Add_Events
        (P_Wnd_Main.Area_Map,
         Button_Press_Mask or
         Button_Release_Mask or
         Button_Motion_Mask or
         Key_Press_Mask or
         Key_Release_Mask or
         Scroll_Mask);

      --
      Gdk.Pixbuf.Gdk_New_From_File (P_Wnd_Main.Map_Image.Image_Data, "resources\map.png", Error);

      P_Wnd_Main.Map_Image.Image_Width := Gdk.Pixbuf.Get_Width (P_Wnd_Main.Map_Image.Image_Data);

      P_Wnd_Main.Map_Image.Image_Height := Gdk.Pixbuf.Get_Height (P_Wnd_Main.Map_Image.Image_Data);

      Tubastga_UI_Resources.Initialize;

      Tubastga_Window_Pkg.Show_All (P_Wnd_Main);

      Tubastga_Window_Pkg.Gtk_New (P_Wnd_Main.all.Wnd_Performing_Patch);
      Tubastga_Window_Pkg.Show_All (P_Wnd_Main.all.Wnd_Performing_Patch);

      Tubastga_Window_Pkg.Gtk_New (P_Wnd_Main.all.Wnd_Target_Patch);
      Tubastga_Window_Pkg.Show_All (P_Wnd_Main.all.Wnd_Target_Patch);

      Gdk.Color.Set_Rgb (Blue_Color, 0, 0, 65535);
      Gdk.Color.Set_Rgb (Red_Color, 65535, 0, 0);
      Gdk.Color.Set_Rgb (Green_Color, 0, 65535, 0);
      Gdk.Color.Set_Rgb (Grey_Color, 65000, 65000, 65000);
      Gdk.Color.Set_Rgb (Orange_Color, 65535, 32000, 16000);
      Gdk.Color.Set_Rgb (Yellow_Color, 65535, 65535, 0);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Wnd_Main)- exit");
      end if;
   end Initialize;

   procedure Gtk_New (P_Wnd_Performing_Patch : out Type_Wnd_Performing_Patch_Access) is
   begin
      P_Wnd_Performing_Patch := new Type_Wnd_Performing_Patch_Record;
      Tubastga_Window_Pkg.Initialize (P_Wnd_Performing_Patch);
   end Gtk_New;

   procedure Initialize (P_Wnd_Performing_Patch : access Type_Wnd_Performing_Patch_Record'Class) is
      pragma Suppress (All_Checks);

      Piece_Info : Glib.GType_Array (1 .. 3) :=
        Glib.GType_Array'(1 => Glib.GType_Int, 2 => Gdk.Pixbuf.Get_Type, 3 => Glib.GType_String);
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Pixbuf_Renderer : Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;
      Col_Piece_Name  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Col_Piece_Image : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      C               : Glib.Gint;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Wnd_Performing_Patch) - enter");
      end if;

      Gtk.Window.Initialize (P_Wnd_Performing_Patch, Gtk.Enums.Window_Toplevel);
      Tubastga_Window_Pkg.Set_Title (P_Wnd_Performing_Patch, -"Tubast'ga - Performing Patch");
      Tubastga_Window_Pkg.Set_Position (P_Wnd_Performing_Patch, Gtk.Enums.Win_Pos_None);
      Tubastga_Window_Pkg.Set_Modal (P_Wnd_Performing_Patch, False);
      Tubastga_Window_Pkg.Set_Resizable (P_Wnd_Performing_Patch, False);

      P_Wnd_Performing_Patch.all.Scroll_VBox := Gtk.Box.Gtk_Vbox_New (True, Glib.Gint (2));

      Gtk.List_Store.Gtk_New (P_Wnd_Performing_Patch.all.Performing_Pieces_List_Store, Piece_Info);
      Gtk.Tree_View.Gtk_New
        (P_Wnd_Performing_Patch.all.Perform_Pieces_Tree_View,
         P_Wnd_Performing_Patch.all.Performing_Pieces_List_Store);

      Gtk.Tree_View_Column.Gtk_New (Col_Piece_Image);
      Gtk.Tree_View_Column.Set_Title (Col_Piece_Image, "View");
      C :=
        Gtk.Tree_View.Append_Column
          (P_Wnd_Performing_Patch.all.Perform_Pieces_Tree_View,
           Col_Piece_Image);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Pixbuf_Renderer);
      Gtk.Tree_View_Column.Pack_Start (Col_Piece_Image, Pixbuf_Renderer, True);
      Gtk.Tree_View_Column.Add_Attribute (Col_Piece_Image, Pixbuf_Renderer, "pixbuf", 1);

      Gtk.Tree_View_Column.Gtk_New (Col_Piece_Name);
      Gtk.Tree_View_Column.Set_Title (Col_Piece_Name, "Name");
      C :=
        Gtk.Tree_View.Append_Column
          (P_Wnd_Performing_Patch.all.Perform_Pieces_Tree_View,
           Col_Piece_Name);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Gtk.Tree_View_Column.Pack_Start (Col_Piece_Name, Text_Renderer, True);
      Gtk.Tree_View_Column.Add_Attribute (Col_Piece_Name, Text_Renderer, "text", 2);

      Gtk.Tree_View.Set_Activate_On_Single_Click
        (P_Wnd_Performing_Patch.all.Perform_Pieces_Tree_View,
         True);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Scroll_VBox,
         P_Wnd_Performing_Patch.all.Perform_Pieces_Tree_View);
      Gtk.Tree_View.Set_Size_Request
        (P_Wnd_Performing_Patch.all.Perform_Pieces_Tree_View,
         150,
         200);

      P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox :=
        Gtk.Box.Gtk_Vbox_New (True, Glib.Gint (2));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Move);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Move, "Move");

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Attack);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Attack, "Attack");

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Ranged_Attack);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Ranged_Attack, "Range Attack");

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Search);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Search, "Search");

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Promote);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Promote, "Promote");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.Btn_Promote,
         Gtk.Image.Gtk_Image_New_From_File ("resources\promote.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Demote);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Demote, "Demote");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.Btn_Demote,
         Gtk.Image.Gtk_Image_New_From_File ("resources\demote.png"));

--        --
      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Create_Path);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Create_Path, "Create Path");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Create_Path,
         Gtk.Image.Gtk_Image_New_From_File ("resources\demote.png"));

--      Tubastga_Window_Pkg.Add (P_Wnd_Performing_Patch, P_Wnd_Performing_Patch.all.Btn_Create_Path);

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Remove_Path);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Remove_Path, "Remove Path");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Create_Path,
         Gtk.Image.Gtk_Image_New_From_File ("resources\demote.png"));

--      Tubastga_Window_Pkg.Add (P_Wnd_Performing_Patch, P_Wnd_Performing_Patch.all.Btn_Remove_Path);


      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Move);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Attack);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Ranged_Attack);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Search);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Promote);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Demote);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Create_Path);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox,
         P_Wnd_Performing_Patch.all.Btn_Remove_Path);

      -- Place WallN
      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Place_Wall1);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Place_Wall1, "Place Wall 1");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall1,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall1.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Place_Wall2);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Place_Wall2, "Place Wall 2");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall2,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall2.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Place_Wall2);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Place_Wall2, "Place Wall 2");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall2,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall2.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Place_Wall3);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Place_Wall3, "Place Wall 3");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall3,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall3.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Place_Wall4);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Place_Wall4, "Place Wall 4");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall4,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall4.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Place_Wall5);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Place_Wall5, "Place Wall 5");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.Btn_Place_Wall5,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall5.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Place_Wall6);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Place_Wall6, "Place Wall 6");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall6,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall6.png"));

      ---
      -- Remove WallN
      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Remove_Wall1);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Remove_Wall1, "Remove Wall 1");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall1,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall1.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Remove_Wall2);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Remove_Wall2, "Remove Wall 2");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall2,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall2.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Remove_Wall2);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Remove_Wall2, "Remove Wall 2");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall2,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall2.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Remove_Wall3);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Remove_Wall3, "Remove Wall 3");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall3,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall3.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Remove_Wall4);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Remove_Wall4, "Remove Wall 4");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall4,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall4.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.Btn_Remove_Wall5);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.Btn_Remove_Wall5, "Remove Wall 5");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.Btn_Remove_Wall5,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall5.png"));

      Gtk.Button.Gtk_New (P_Wnd_Performing_Patch.all.Btn_Remove_Wall6);
      Gtk.Button.Set_Label (P_Wnd_Performing_Patch.all.Btn_Remove_Wall6, "Remove Wall 6");
      Gtk.Button.Set_Image
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall6,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall6.png"));

      P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox := Gtk.Box.Gtk_Vbox_New (True, Glib.Gint (2));

      -- Place_WallN
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox,
         P_Wnd_Performing_Patch.all.Btn_Place_Wall1);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox,
         P_Wnd_Performing_Patch.all.Btn_Place_Wall2);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox,
         P_Wnd_Performing_Patch.all.Btn_Place_Wall3);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox,
         P_Wnd_Performing_Patch.all.Btn_Place_Wall4);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox,
         P_Wnd_Performing_Patch.all.Btn_Place_Wall5);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox,
         P_Wnd_Performing_Patch.all.Btn_Place_Wall6);

      P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox := Gtk.Box.Gtk_Vbox_New (True, Glib.Gint (2));

      -- Remove_WallN
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox,
         P_Wnd_Performing_Patch.all.Btn_Remove_Wall1);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox,
         P_Wnd_Performing_Patch.all.Btn_Remove_Wall2);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox,
         P_Wnd_Performing_Patch.all.Btn_Remove_Wall3);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox,
         P_Wnd_Performing_Patch.all.Btn_Remove_Wall4);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox,
         P_Wnd_Performing_Patch.all.Btn_Remove_Wall5);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox,
         P_Wnd_Performing_Patch.all.Btn_Remove_Wall6);

      --

      P_Wnd_Performing_Patch.all.Content_HBox := Gtk.Box.Gtk_Hbox_New (True, Glib.Gint (2));

      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Content_HBox,
         P_Wnd_Performing_Patch.all.Scroll_VBox);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Content_HBox,
         P_Wnd_Performing_Patch.all.Fighting_Piece_Action_VBox);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Content_HBox,
         P_Wnd_Performing_Patch.all.House_Piece_Action1_VBox);
      Gtk.Box.Pack_Start
        (P_Wnd_Performing_Patch.all.Content_HBox,
         P_Wnd_Performing_Patch.all.House_Piece_Action2_VBox);


      Tubastga_Window_Pkg.Add (P_Wnd_Performing_Patch, P_Wnd_Performing_Patch.all.Content_HBox);


      Tubastga_Window_Pkg.Show_All (P_Wnd_Performing_Patch);


      -- Place WallN
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall1,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Place_Wall1'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall2,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Place_Wall2'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall3,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Place_Wall3'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall4,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Place_Wall4'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall5,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Place_Wall5'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Place_Wall6,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Place_Wall6'Access),
         False);

      -- Remove WallN
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall1,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Remove_Wall1'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall2,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Remove_Wall2'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall3,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Remove_Wall3'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall4,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Remove_Wall4'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall5,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Remove_Wall5'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Remove_Wall6,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Remove_Wall6'Access),
         False);

      --
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Move,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Move'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Attack,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Attack'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Ranged_Attack,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Ranged_Attack'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Search,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Search'Access),
         False);

      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Promote,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Promote'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.all.Btn_Demote,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Demote'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.Btn_Create_Path,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Create_Path'Access),
         False);
      Callbacks_Tubastga.Button_Cb.Connect
        (P_Wnd_Performing_Patch.Btn_Remove_Path,
         "clicked",
         Callbacks_Tubastga.Button_Cb.To_Marshaller (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Button_Remove_Path'Access),
         False);

      Callbacks_Tubastga.Tree_View_Cb.Connect
        (P_Wnd_Performing_Patch.all.Perform_Pieces_Tree_View,
         "row-activated",
         Callbacks_Tubastga.Tree_View_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Performing_Patch.On_Performing_Patch_Tree_View'Access),
         False);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Wnd_Performing_Patch) - exit");
      end if;
   end Initialize;

   procedure Gtk_New (P_Wnd_Target_Patch : out Type_Wnd_Target_Patch_Access) is
   begin
      P_Wnd_Target_Patch := new Type_Wnd_Target_Patch_Record;
      Tubastga_Window_Pkg.Initialize (P_Wnd_Target_Patch);
   end Gtk_New;

   procedure Initialize (P_Wnd_Target_Patch : access Type_Wnd_Target_Patch_Record'Class) is
      pragma Suppress (All_Checks);

      Piece_Info : Glib.GType_Array (1 .. 3) :=
        Glib.GType_Array'(1 => Glib.GType_Int, 2 => Gdk.Pixbuf.Get_Type, 3 => Glib.GType_String);
      Text_Renderer   : Gtk.Cell_Renderer_Text.Gtk_Cell_Renderer_Text;
      Pixbuf_Renderer : Gtk.Cell_Renderer_Pixbuf.Gtk_Cell_Renderer_Pixbuf;
      Col_Piece_Name  : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      Col_Piece_Image : Gtk.Tree_View_Column.Gtk_Tree_View_Column;
      C               : Glib.Gint;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Wnd_Target_Patch) - enter");
      end if;

      Gtk.Window.Initialize (P_Wnd_Target_Patch, Gtk.Enums.Window_Toplevel);
      Tubastga_Window_Pkg.Set_Title (P_Wnd_Target_Patch, -"Tubast'ga - Target Patch");
      Tubastga_Window_Pkg.Set_Position (P_Wnd_Target_Patch, Gtk.Enums.Win_Pos_None);
      Tubastga_Window_Pkg.Set_Modal (P_Wnd_Target_Patch, False);
      Tubastga_Window_Pkg.Set_Resizable (P_Wnd_Target_Patch, False);

      P_Wnd_Target_Patch.all.Scroll_VBox := Gtk.Box.Gtk_Vbox_New (True, Glib.Gint (2));

      Gtk.List_Store.Gtk_New (P_Wnd_Target_Patch.all.Target_Pieces_List_Store, Piece_Info);
      Gtk.Tree_View.Gtk_New
        (P_Wnd_Target_Patch.all.Target_Pieces_Tree_View,
         P_Wnd_Target_Patch.all.Target_Pieces_List_Store);

      Gtk.Tree_View_Column.Gtk_New (Col_Piece_Image);
      Gtk.Tree_View_Column.Set_Title (Col_Piece_Image, "View");
      C :=
        Gtk.Tree_View.Append_Column
          (P_Wnd_Target_Patch.all.Target_Pieces_Tree_View,
           Col_Piece_Image);
      Gtk.Cell_Renderer_Pixbuf.Gtk_New (Pixbuf_Renderer);
      Gtk.Tree_View_Column.Pack_Start (Col_Piece_Image, Pixbuf_Renderer, True);
      Gtk.Tree_View_Column.Add_Attribute (Col_Piece_Image, Pixbuf_Renderer, "pixbuf", 1);

      Gtk.Tree_View_Column.Gtk_New (Col_Piece_Name);
      Gtk.Tree_View_Column.Set_Title (Col_Piece_Name, "Name");
      C :=
        Gtk.Tree_View.Append_Column
          (P_Wnd_Target_Patch.all.Target_Pieces_Tree_View,
           Col_Piece_Name);
      Gtk.Cell_Renderer_Text.Gtk_New (Text_Renderer);
      Gtk.Tree_View_Column.Pack_Start (Col_Piece_Name, Text_Renderer, True);
      Gtk.Tree_View_Column.Add_Attribute (Col_Piece_Name, Text_Renderer, "text", 2);

      Gtk.Tree_View.Set_Activate_On_Single_Click
        (P_Wnd_Target_Patch.all.Target_Pieces_Tree_View,
         True);
      Gtk.Box.Pack_Start
        (P_Wnd_Target_Patch.all.Scroll_VBox,
         P_Wnd_Target_Patch.all.Target_Pieces_Tree_View);
      Gtk.Tree_View.Set_Size_Request (P_Wnd_Target_Patch.all.Target_Pieces_Tree_View, 450, 210);

      Tubastga_Window_Pkg.Add (P_Wnd_Target_Patch, P_Wnd_Target_Patch.all.Scroll_VBox);
      Tubastga_Window_Pkg.Show_All (P_Wnd_Target_Patch);

      Callbacks_Tubastga.Tree_View_Cb.Connect
        (P_Wnd_Target_Patch.all.Target_Pieces_Tree_View,
         "row-activated",
         Callbacks_Tubastga.Tree_View_Cb.To_Marshaller
           (Tubastga_Window_Pkg.Callbacks.Target_Patch.On_Target_Patch_Tree_View'Access),
         False);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize (P_Wnd_Target_Patch) - exit");
      end if;
   end Initialize;
end Tubastga_Window_Pkg;
