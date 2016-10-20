--
--
--      Tubastga Game - A turn based strategy game.
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
with Glib;                          use Glib;
with Gtk;                           use Gtk;
with Gtk.Widget;                    use Gtk.Widget;
with Gtk.Enums;                     use Gtk.Enums;
with Gtkada.Handlers;               use Gtkada.Handlers;
with Callbacks_tubastga;            use Callbacks_tubastga;
with Tubastga_Window_Intl;          use Tubastga_Window_Intl;
with Tubastga_Window_Pkg.Callbacks; use Tubastga_Window_Pkg.Callbacks;
with Gdk.Event;                     use Gdk.Event;
with Gdk.Color;
with Text_IO;

--
--
with Gdk.Event;  use Gdk.Event;
with Gdk.Pixbuf;
with Glib.Error; use Glib.Error;
with Gtk.Image;
with Gtk.Adjustment;
with Tubastga_UI_Aux;
with Utilities;

package body Tubastga_Window_Pkg is

   Verbose : constant Boolean := False;

   procedure Gtk_New (dlg_MainMenu : out Dlg_MainMenu_Access) is
   begin
      dlg_MainMenu := new Dlg_MainMenu_Record;
      Tubastga_Window_Pkg.Initialize (dlg_MainMenu);
   end Gtk_New;

   procedure Initialize (dlg_MainMenu : access Dlg_MainMenu_Record'Class) is
      TCPIPOctet1, TCPIPOctet2, TCPIPOctet3, TCPIPOctet4 : Natural;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize - dialog enter");
      end if;

      Gtk.Dialog.Initialize
        (Gtk.Dialog.Gtk_Dialog (dlg_MainMenu),
         "Main Menu",
         Flags => Gtk.Dialog.Destroy_With_Parent);
      Gtk.Table.Gtk_New (dlg_MainMenu.all.Layout_Table, 9, 2, False);

      Gtk.Box.Gtk_New (dlg_MainMenu.all.Box_Connect, Gtk.Enums.Orientation_Horizontal, Gint (0));
      Tubastga_UI_Aux.Get_TCPIP_Adress (TCPIPOctet1, TCPIPOctet2, TCPIPOctet3, TCPIPOctet4);
      Gtk.Label.Gtk_New (dlg_MainMenu.all.TCPIPLabel, "Server TCP/IP:");
      Gtk.Adjustment.Gtk_New
        (dlg_MainMenu.all.Adj_TCPIPOctet1,
         Gdouble (TCPIPOctet1),
         Gdouble (0),
         Gdouble (255),
         Gdouble (1),
         Gdouble (1),
         Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (dlg_MainMenu.all.Adj_TCPIPOctet2,
         Gdouble (TCPIPOctet2),
         Gdouble (0),
         Gdouble (255),
         Gdouble (1),
         Gdouble (1),
         Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (dlg_MainMenu.all.Adj_TCPIPOctet3,
         Gdouble (TCPIPOctet3),
         Gdouble (0),
         Gdouble (255),
         Gdouble (1),
         Gdouble (1),
         Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (dlg_MainMenu.all.Adj_TCPIPOctet4,
         Gdouble (TCPIPOctet4),
         Gdouble (0),
         Gdouble (255),
         Gdouble (1),
         Gdouble (1),
         Gdouble (1));
      Gtk.Adjustment.Gtk_New
        (dlg_MainMenu.all.Adj_TCPIPPort,
         Gdouble (0),
         Gdouble (1),
         Gdouble (65535),
         Gdouble (1),
         Gdouble (1),
         Gdouble (1));

      Gtk.Spin_Button.Gtk_New
        (dlg_MainMenu.all.Spn_TCPIPOctet1,
         dlg_MainMenu.all.Adj_TCPIPOctet1,
         Gdouble (1),
         0);
      Gtk.Spin_Button.Gtk_New
        (dlg_MainMenu.all.Spn_TCPIPOctet2,
         dlg_MainMenu.all.Adj_TCPIPOctet2,
         Gdouble (1),
         0);
      Gtk.Spin_Button.Gtk_New
        (dlg_MainMenu.all.Spn_TCPIPOctet3,
         dlg_MainMenu.all.Adj_TCPIPOctet3,
         Gdouble (1),
         0);
      Gtk.Spin_Button.Gtk_New
        (dlg_MainMenu.all.Spn_TCPIPOctet4,
         dlg_MainMenu.all.Adj_TCPIPOctet4,
         Gdouble (1),
         0);
      Gtk.Label.Gtk_New (dlg_MainMenu.all.TCPIPPortLabel, "Port:");

      Gtk.Spin_Button.Gtk_New
        (dlg_MainMenu.all.Spn_TCPIPPort,
         dlg_MainMenu.all.Adj_TCPIPPort,
         Gdouble (1),
         0);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Connect,
         dlg_MainMenu.all.TCPIPLabel,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Connect,
         dlg_MainMenu.all.Spn_TCPIPOctet1,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Connect,
         dlg_MainMenu.all.Spn_TCPIPOctet2,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Connect,
         dlg_MainMenu.all.Spn_TCPIPOctet3,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Connect,
         dlg_MainMenu.all.Spn_TCPIPOctet4,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Connect,
         dlg_MainMenu.all.TCPIPPortLabel,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Connect,
         dlg_MainMenu.all.Spn_TCPIPPort,
         False,
         False,
         2);

      Gtk.Spin_Button.Set_Value
        (Gtk.Spin_Button.Gtk_Spin_Button (dlg_MainMenu.all.Spn_TCPIPPort),
         Gdouble (Tubastga_UI_Aux.Get_Server_Port));

      --
      --      Player info
      Gtk.Box.Gtk_New
        (dlg_MainMenu.all.Box_Create_Game,
         Gtk.Enums.Orientation_Horizontal,
         Gint (0));
      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Create_Game_Player_Name_1, "Player 1 Name:");
      Gtk.GEntry.Gtk_New (dlg_MainMenu.all.En_Create_Game_Player_Name_1);
      Gtk.GEntry.Set_Max_Length (dlg_MainMenu.all.En_Create_Game_Player_Name_1, Gint (10));
      Gtk.GEntry.Set_Width_Chars (dlg_MainMenu.all.En_Create_Game_Player_Name_1, Gint (11));

      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Create_Game_Player_Name_2, "Player 2 Name:");
      Gtk.GEntry.Gtk_New (dlg_MainMenu.all.En_Create_Game_Player_Name_2);
      Gtk.GEntry.Set_Max_Length (dlg_MainMenu.all.En_Create_Game_Player_Name_2, Gint (10));
      Gtk.GEntry.Set_Width_Chars (dlg_MainMenu.all.En_Create_Game_Player_Name_2, Gint (11));

      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Create_Game_Player_Name_3, "Player 3 Name:");
      Gtk.GEntry.Gtk_New (dlg_MainMenu.all.En_Create_Game_Player_Name_3);
      Gtk.GEntry.Set_Max_Length (dlg_MainMenu.all.En_Create_Game_Player_Name_3, Gint (10));
      Gtk.GEntry.Set_Width_Chars (dlg_MainMenu.all.En_Create_Game_Player_Name_3, Gint (11));

      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Create_Game_Chose_Scenario, "Chose scenario:");
      Gtk.Combo_Box_Text.Gtk_New (dlg_MainMenu.all.Cmb_Create_Game_Chose_Scenario);
      Gtk.Combo_Box_Text.Append_Text
        (dlg_MainMenu.all.Cmb_Create_Game_Chose_Scenario,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (dlg_MainMenu.all.Cmb_Create_Game_Chose_Scenario, 0);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.Lbl_Create_Game_Player_Name_1,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.En_Create_Game_Player_Name_1,
         False,
         False,
         2);

      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.Lbl_Create_Game_Player_Name_2,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.En_Create_Game_Player_Name_2,
         False,
         False,
         2);

      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.Lbl_Create_Game_Player_Name_3,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.En_Create_Game_Player_Name_3,
         False,
         False,
         2);

      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.Lbl_Create_Game_Chose_Scenario,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Create_Game,
         dlg_MainMenu.all.Cmb_Create_Game_Chose_Scenario,
         False,
         False,
         2);
      --
      Gtk.Box.Show (dlg_MainMenu.all.Box_Create_Game);
      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Create_Game_Player_Name_1);
      Gtk.GEntry.Show (dlg_MainMenu.all.En_Create_Game_Player_Name_1);

      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Create_Game_Player_Name_2);
      Gtk.GEntry.Show (dlg_MainMenu.all.En_Create_Game_Player_Name_2);

      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Create_Game_Player_Name_3);
      Gtk.GEntry.Show (dlg_MainMenu.all.En_Create_Game_Player_Name_3);

      Gtk.Combo_Box_Text.Show (dlg_MainMenu.all.Cmb_Create_Game_Chose_Scenario);
      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Create_Game_Chose_Scenario);

      Gtk.GEntry.Set_Text
        (dlg_MainMenu.all.En_Create_Game_Player_Name_1,
         Utilities.RemoteString.To_String (Tubastga_UI_Aux.Get_My_Player_Name));

      --
      Gtk.Box.Gtk_New (dlg_MainMenu.all.Box_Load_Game, Gtk.Enums.Orientation_Horizontal, Gint (0));
      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Load_Game_Name, "Chose game to load:");
      Gtk.Combo_Box_Text.Gtk_New (dlg_MainMenu.all.Cmb_Load_Game_Name);
      Gtk.Combo_Box_Text.Append_Text
        (dlg_MainMenu.all.Cmb_Load_Game_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (dlg_MainMenu.all.Cmb_Load_Game_Name, 0);

      Gtk.Combo_Box_Text.Show (dlg_MainMenu.all.Cmb_Load_Game_Name);
      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Load_Game_Name);
      Gtk.Box.Show (dlg_MainMenu.all.Box_Load_Game);

      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Load_Game,
         dlg_MainMenu.all.Lbl_Load_Game_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Load_Game,
         dlg_MainMenu.all.Cmb_Load_Game_Name,
         False,
         False,
         2);
      --
      --
      Gtk.Box.Gtk_New (dlg_MainMenu.all.Box_Save_Game, Gtk.Enums.Orientation_Horizontal, Gint (0));
      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Save_Game_Name, "Save game as... :");
      Gtk.GEntry.Gtk_New (dlg_MainMenu.all.En_Save_Game_Name);
      Gtk.GEntry.Set_Max_Length (dlg_MainMenu.all.En_Save_Game_Name, 20);
      Gtk.GEntry.Set_Width_Chars (dlg_MainMenu.all.En_Save_Game_Name, 21);

      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Save_Game_Name);
      Gtk.GEntry.Show (dlg_MainMenu.all.En_Save_Game_Name);
      Gtk.Box.Show (dlg_MainMenu.all.Box_Save_Game);

      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Save_Game,
         dlg_MainMenu.all.Lbl_Save_Game_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Save_Game,
         dlg_MainMenu.all.En_Save_Game_Name,
         False,
         False,
         2);
      --
      Gtk.Box.Gtk_New (dlg_MainMenu.all.Box_Join_Game, Gtk.Enums.Orientation_Horizontal, Gint (0));
      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Join_Player_Name, "Player Name");
      Gtk.Combo_Box_Text.Gtk_New (dlg_MainMenu.all.Cmb_Join_Player_Name);
      Gtk.Combo_Box_Text.Append_Text
        (dlg_MainMenu.all.Cmb_Join_Player_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (dlg_MainMenu.all.Cmb_Join_Player_Name, 0);
      Gtk.Label.Gtk_New (dlg_MainMenu.all.Lbl_Join_Game, "description of connection");
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Join_Game,
         dlg_MainMenu.all.Lbl_Join_Player_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Join_Game,
         dlg_MainMenu.all.Cmb_Join_Player_Name,
         False,
         False,
         2);
      Gtk.Box.Pack_Start
        (dlg_MainMenu.all.Box_Join_Game,
         dlg_MainMenu.all.Lbl_Join_Game,
         False,
         False,
         2);
      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Join_Player_Name);
      Gtk.Combo_Box_Text.Show (dlg_MainMenu.all.Cmb_Join_Player_Name);
      Gtk.Label.Show (dlg_MainMenu.all.Lbl_Join_Game);
      Gtk.Box.Show (dlg_MainMenu.all.Box_Join_Game);
      --
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Box_Connect, 1, 2, 0, 1);
      Gtk.Table.Attach
        (dlg_MainMenu.all.Layout_Table,
         dlg_MainMenu.all.Box_Create_Game,
         1,
         2,
         2,
         3);
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Box_Load_Game, 1, 2, 3, 4);
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Box_Save_Game, 1, 2, 4, 5);
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Box_Join_Game, 1, 2, 5, 6);

      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Connect_Server, "Connect Server");
      Gtk.Table.Attach
        (dlg_MainMenu.all.Layout_Table,
         dlg_MainMenu.all.Btn_Connect_Server,
         0,
         1,
         0,
         1);

      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Refresh, "Refresh");
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Btn_Refresh, 0, 1, 1, 2);

      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Create_Game, "Create Game");
      Gtk.Table.Attach
        (dlg_MainMenu.all.Layout_Table,
         dlg_MainMenu.all.Btn_Create_Game,
         0,
         1,
         2,
         3);
      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Load_Game, "Load Game");
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Btn_Load_Game, 0, 1, 3, 4);
      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Save_Game, "Save Game");
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Btn_Save_Game, 0, 1, 4, 5);
      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Join_Game, "Join Game");
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Btn_Join_Game, 0, 1, 5, 6);
      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Leave_Game, "Leave Game");
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Btn_Leave_Game, 0, 1, 6, 7);
      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Disconnect_Server, "Disconnect Server");
      Gtk.Table.Attach
        (dlg_MainMenu.all.Layout_Table,
         dlg_MainMenu.all.Btn_Disconnect_Server,
         0,
         1,
         7,
         8);
      Gtk.Button.Gtk_New (dlg_MainMenu.all.Btn_Close, "Close");
      Gtk.Table.Attach (dlg_MainMenu.all.Layout_Table, dlg_MainMenu.all.Btn_Close, 0, 1, 8, 9);

      Gtk.Box.Pack_Start (Get_Action_Area (dlg_MainMenu), dlg_MainMenu.all.Layout_Table);

      Gtk.Table.Show (dlg_MainMenu.all.Layout_Table);
      Gtk.Box.Show (dlg_MainMenu.all.Box_Connect);
      Gtk.Label.Show (dlg_MainMenu.all.TCPIPLabel);
      Gtk.Spin_Button.Show (dlg_MainMenu.all.Spn_TCPIPOctet1);
      Gtk.Spin_Button.Show (dlg_MainMenu.all.Spn_TCPIPOctet2);
      Gtk.Spin_Button.Show (dlg_MainMenu.all.Spn_TCPIPOctet3);
      Gtk.Spin_Button.Show (dlg_MainMenu.all.Spn_TCPIPOctet4);
      Gtk.Spin_Button.Show (dlg_MainMenu.all.Spn_TCPIPPort);
      Gtk.Label.Show (dlg_MainMenu.all.TCPIPPortLabel);

      Gtk.Button.Show (dlg_MainMenu.all.Btn_Connect_Server);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Refresh);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Create_Game);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Load_Game);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Save_Game);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Join_Game);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Leave_Game);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Disconnect_Server);
      Gtk.Button.Show (dlg_MainMenu.all.Btn_Close);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Connect_Server,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Connect'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Refresh,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Refresh'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Create_Game,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Create_Game'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Join_Game,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Join_Game'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Leave_Game,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Leave_Game'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Load_Game,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Load_Game'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Save_Game,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Save_Game'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Disconnect_Server,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Disconnect'Access),
         False);

      Button_Cb.Connect
        (dlg_MainMenu.Btn_Close,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Close'Access),
         False);

      Spin_Button_Cb.Connect
        (dlg_MainMenu.Spn_TCPIPOctet1,
         "value-changed",
         Spin_Button_Cb.To_Marshaller (On_Spin_Button_Change'Access),
         False);
      Spin_Button_Cb.Connect
        (dlg_MainMenu.Spn_TCPIPOctet2,
         "value-changed",
         Spin_Button_Cb.To_Marshaller (On_Spin_Button_Change'Access),
         False);
      Spin_Button_Cb.Connect
        (dlg_MainMenu.Spn_TCPIPOctet3,
         "value-changed",
         Spin_Button_Cb.To_Marshaller (On_Spin_Button_Change'Access),
         False);
      Spin_Button_Cb.Connect
        (dlg_MainMenu.Spn_TCPIPOctet4,
         "value-changed",
         Spin_Button_Cb.To_Marshaller (On_Spin_Button_Change'Access),
         False);
      Spin_Button_Cb.Connect
        (dlg_MainMenu.Spn_TCPIPPort,
         "value-changed",
         Spin_Button_Cb.To_Marshaller (On_Spin_Button_Change'Access),
         False);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Initialize - dialog exit");
      end if;

   end Initialize;

   procedure Gtk_New (wndMain : out Wnd_Main_Access) is
   begin
      wndMain := new Wnd_Main_Record;
      Tubastga_Window_Pkg.Initialize (wndMain);
   end Gtk_New;

   procedure Initialize (wndMain : access Wnd_Main_Record'Class) is
      pragma Suppress (All_Checks);

      Error : Glib.Error.GError;

      Blue_Color,
      Red_Color,
      Grey_Color,
      Green_Color,
      Orange_Color,
      Yellow_Color : Gdk.Color.Gdk_Color;

      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Initialize - Wnd Main - enter");
      end if;

      Gtk.Window.Initialize (wndMain, Window_Toplevel);
      Set_Title (wndMain, -"Tubast'ga");
      Set_Position (wndMain, Win_Pos_None);
      Set_Modal (wndMain, False);
      Set_Resizable (wndMain, True);

      Gtk.Toolbar.Gtk_New (wndMain.Toolbar_Main);
      Gtk.Toolbar.Set_Orientation (wndMain.Toolbar_Main, Orientation_Horizontal);
      Gtk.Toolbar.Set_Style (wndMain.Toolbar_Main, Toolbar_Icons);

      Gtk.Box.Gtk_New (wndMain.Box_Toolbar_Game, Orientation_Vertical, Gint (2));
      Add (wndMain, wndMain.Box_Toolbar_Game);

      Gtk_New (wndMain.Layout_Table, 15, 20, False);
      Set_Row_Spacings (wndMain.Layout_Table, 0);
      Set_Col_Spacings (wndMain.Layout_Table, 0);

      Gtk_New (wndMain.Area_Map);
      Set_Size_Request (wndMain.Area_Map, 700, 600);

      Attach
        (wndMain.all.Layout_Table,
         wndMain.all.Area_Map,
         Left_Attach   => 1,
         Right_Attach  => 12,
         Top_Attach    => 0,
         Bottom_Attach => 19,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.all.Area_Player_1_Timer);
      Set_Size_Request (wndMain.all.Area_Player_1_Timer, 70, 40);

      Attach
        (wndMain.all.Layout_Table,
         wndMain.all.Area_Player_1_Timer,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.all.Lbl_Player_1_Name);
      Attach
        (wndMain.all.Layout_Table,
         wndMain.all.Lbl_Player_1_Name,
         Left_Attach   => 14,
         Right_Attach  => 15,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.all.Area_Player_2_Timer);
      Set_Size_Request (wndMain.all.Area_Player_2_Timer, 70, 40);

      Attach
        (wndMain.all.Layout_Table,
         wndMain.all.Area_Player_2_Timer,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 1,
         Bottom_Attach => 2,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.all.Lbl_Player_2_Name);
      Attach
        (wndMain.all.Layout_Table,
         wndMain.all.Lbl_Player_2_Name,
         Left_Attach   => 14,
         Right_Attach  => 15,
         Top_Attach    => 1,
         Bottom_Attach => 2,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.all.Area_Player_3_Timer);
      Set_Size_Request (wndMain.all.Area_Player_3_Timer, 70, 40);

      Attach
        (wndMain.all.Layout_Table,
         wndMain.all.Area_Player_3_Timer,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 2,
         Bottom_Attach => 3,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.all.Lbl_Player_3_Name);
      Attach
        (wndMain.all.Layout_Table,
         wndMain.all.Lbl_Player_3_Name,
         Left_Attach   => 14,
         Right_Attach  => 15,
         Top_Attach    => 2,
         Bottom_Attach => 3,
         Xpadding      => 0,
         Ypadding      => 0);

      --
      --
      --
      Gtk_New (wndMain.Buffer_Activity_Report);
      Gtk_New (wndMain.View_Activity_Report, wndMain.Buffer_Activity_Report);
      Set_Size_Request (wndMain.View_Activity_Report, 350, 600);

      Attach
        (wndMain.Layout_Table,
         wndMain.View_Activity_Report,
         Left_Attach   => 12,
         Right_Attach  => 13,
         Top_Attach    => 0,
         Bottom_Attach => 19,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.Buffer_Hover_Info);
      Gtk_New (wndMain.View_Hover_Info, wndMain.Buffer_Hover_Info);

      Attach
        (wndMain.Layout_Table,
         wndMain.View_Hover_Info,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 3,
         Bottom_Attach => 4,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk_New (wndMain.Buffer_Resources_Info);
      Gtk_New (wndMain.View_Resources_Info, wndMain.Buffer_Resources_Info);

      Attach
        (wndMain.Layout_Table,
         wndMain.View_Resources_Info,
         Left_Attach   => 13,
         Right_Attach  => 14,
         Top_Attach    => 4,
         Bottom_Attach => 5,
         Xpadding      => 0,
         Ypadding      => 0);

      --
      --  Buttons
      --
      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Sentry);
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Sentry,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_sentry.png"));
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Sentry, "Place Sentry");
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Sentry);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Ship);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Ship, "Place Ship");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Ship,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_ship.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Ship);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Knight);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Knight, "Place Knight");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Knight,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_knight.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Knight);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Bowman);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Bowman, "Place Bowman");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Bowman,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_bowman.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Bowman);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Carrier);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Carrier, "Place Carrier");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Carrier,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_carrier.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Carrier);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Tower);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Tower, "Place Tower");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Tower,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_tower.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Tower);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Farm);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Farm, "Place Farm");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Farm,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_farm.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Farm);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Lumberjack);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Lumberjack, "Place Lumberjack");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Lumberjack,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_lumberjack.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Lumberjack);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Place_Stonecutter);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Place_Stonecutter, "Place Stonecutter");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Place_Stonecutter,
         Gtk.Image.Gtk_Image_New_From_File ("resources\place_stonecutter.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Place_Stonecutter);

      Gtk.Tool_Button.Gtk_New (wndMain.Btn_Game);
      Gtk.Tool_Button.Set_Label (wndMain.Btn_Game, "Game");
      Gtk.Tool_Button.Set_Icon_Widget
        (wndMain.Btn_Game,
         Gtk.Image.Gtk_Image_New_From_File ("resources\main_menu.png"));
      Gtk.Toolbar.Insert (wndMain.Toolbar_Main, wndMain.Btn_Game);

      Gtk.Box.Pack_Start (wndMain.Box_Toolbar_Game, wndMain.Toolbar_Main, True, True);
      Gtk.Box.Pack_End (wndMain.Box_Toolbar_Game, wndMain.Layout_Table, True, True);

      Gtk.Drawing_Area.Set_Events
        (wndMain.Area_Map,
         Exposure_Mask or
         Leave_Notify_Mask or
         Button_Press_Mask or
         Pointer_Motion_Mask or
         Pointer_Motion_Hint_Mask);

      --  Connect signals

      Drawing_Area_Callback.Connect
        (wndMain.Area_Map,
         Signal_Show,
         Drawing_Area_Callback.To_Marshaller (On_Map_Area_Show'Unrestricted_Access));
      Event_Cb.Connect
        (wndMain.Area_Map,
         Signal_Draw,
         Event_Cb.To_Marshaller (On_Map_Area_Expose_Event'Unrestricted_Access));

      Drawing_Area_Callback.Connect
        (wndMain.all.Area_Player_1_Timer,
         Signal_Show,
         Drawing_Area_Callback.To_Marshaller (On_Player_Timer_Area_Show'Unrestricted_Access));
      Event_Cb.Connect
        (wndMain.Area_Player_1_Timer,
         Signal_Draw,
         Event_Cb.To_Marshaller (On_Player_1_Timer_Area_Expose_Event'Unrestricted_Access));

      Drawing_Area_Callback.Connect
        (wndMain.Area_Player_2_Timer,
         Signal_Show,
         Drawing_Area_Callback.To_Marshaller (On_Player_Timer_Area_Show'Unrestricted_Access));
      Event_Cb.Connect
        (wndMain.Area_Player_2_Timer,
         Signal_Draw,
         Event_Cb.To_Marshaller (On_Player_2_Timer_Area_Expose_Event'Unrestricted_Access));

      Drawing_Area_Callback.Connect
        (wndMain.Area_Player_3_Timer,
         Signal_Show,
         Drawing_Area_Callback.To_Marshaller (On_Player_Timer_Area_Show'Unrestricted_Access));
      Event_Cb.Connect
        (wndMain.Area_Player_3_Timer,
         Signal_Draw,
         Event_Cb.To_Marshaller (On_Player_3_Timer_Area_Expose_Event'Unrestricted_Access));

      Window_Cb.Connect (wndMain, "destroy", Window_Cb.To_Marshaller (Exit_Main'Access));

      Return_Callback.Connect
        (wndMain.Area_Map,
         "button_press_event",
         On_Map_Area_Button_Press_Event'Access,
         False);

      Return_Callback.Connect
        (wndMain.Area_Map,
         "scroll_event",
         On_Map_Area_Scroll_Event'Access,
         False);

      Return_Callback.Connect
        (wndMain.Area_Map,
         "motion_notify_event",
         On_Map_Area_Motion_Notify_Event'Access,
         False);

      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Sentry,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Sentry'Access),
         False);
      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Bowman,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Bowman'Access),
         False);
      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Carrier,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Carrier'Access),
         False);

      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Ship,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Ship'Access),
         False);
      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Farm,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Farm'Access),
         False);
      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Tower,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Tower'Access),
         False);
      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Lumberjack,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Lumberjack'Access),
         False);
      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Stonecutter,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Stonecutter'Access),
         False);
      Tool_Button_Callback.Connect
        (wndMain.Btn_Place_Knight,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Place_Knight'Access),
         False);

      Tool_Button_Callback.Connect
        (wndMain.Btn_Game,
         "clicked",
         Tool_Button_Callback.To_Marshaller (On_Button_Game'Access),
         False);

      Return_Callback.Connect (wndMain, "key_press_event", On_Keyboard_Key_Press'Access);

      Return_Callback.Connect (wndMain, "key_release_event", On_Keyboard_Key_Release'Access);

      Add_Events
        (wndMain.Area_Map,
         Button_Press_Mask or
         Button_Release_Mask or
         Button_Motion_Mask or
               Key_Press_Mask or
                 Key_Release_Mask or
         Scroll_Mask);

      --
      Gdk.Pixbuf.Gdk_New_From_File (wndMain.Map_Image.Image_Data, "resources\map.png", Error);

      wndMain.Map_Image.Image_Width := Gdk.Pixbuf.Get_Width (wndMain.Map_Image.Image_Data);

      wndMain.Map_Image.Image_Height := Gdk.Pixbuf.Get_Height (wndMain.Map_Image.Image_Data);

      wndMain.All_Images :=
        ((Ada.Strings.Unbounded.To_Unbounded_String
            ("resources\outside_view_invisible_minimap_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String
            ("resources\outside_view_visible_minimap_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String
            ("resources\inside_view_invisible_minimap_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String
            ("resources\inside_view_visible_minimap_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\invisible_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\reachable_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\attackable_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\selected_hexagon.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\selected_area_hexagon.png"),
          null,
          0,
          0),

         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\grass_hexagon.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\mountain_hexagon.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\water_hexagon.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\forest_hexagon.png"), null, 0, 0),

         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\empty_hexagon.png"), null, 0, 0),

         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\red.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\green.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\blue.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\selected.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\chest.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\chest.png"), null, 0, 0),--Wall
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\sentry.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\sentry.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\sentry.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\knight.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\knight.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\knight.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\bowman.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\bowman.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\bowman.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\ship.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\ship.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\ship.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\farm.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\farm.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\farm.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\tower.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\tower.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\tower.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\lumberjack.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\lumberjack.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\lumberjack.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\stonecutter.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\stonecutter.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\stonecutter.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\carrier.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\carrier.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\carrier.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall1.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall2.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall3.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall4.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall5.png"), null, 0, 0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\wall6.png"), null, 0, 0),

         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_grass_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_mountain_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_water_hexagon.png"),
          null,
          0,
          0),
         (Ada.Strings.Unbounded.To_Unbounded_String ("resources\minimap_forest_hexagon.png"),
          null,
          0,
          0));

      for Trav in wndMain.All_Images'First .. wndMain.All_Images'Last loop
         if Verbose then
            Text_IO.Put_Line
              ("Tubastga_Window_Pkg - searching for image: " &
               Ada.Strings.Unbounded.To_String (wndMain.All_Images (Trav).Filename));
         end if;

         Gdk.Pixbuf.Gdk_New_From_File
           (wndMain.All_Images (Trav).Image_Data,
            Ada.Strings.Unbounded.To_String (wndMain.All_Images (Trav).Filename),
            Error);

         if Error = null then
            wndMain.All_Images (Trav).Image_Width :=
              Gdk.Pixbuf.Get_Width (wndMain.All_Images (Trav).Image_Data);
            wndMain.All_Images (Trav).Image_Height :=
              Gdk.Pixbuf.Get_Height (wndMain.All_Images (Trav).Image_Data);
         else
            Text_IO.Put_Line ("Error: " & Glib.Error.Get_Message (Error));
            Glib.Error.Error_Free (Error);
            wndMain.All_Images (Trav).Image_Width  := 45;
            wndMain.All_Images (Trav).Image_Height := 60;

            wndMain.All_Images (Trav).Image_Data :=
              Gdk.Pixbuf.Gdk_New
                (Bits_Per_Sample => 24,
                 Width           => wndMain.All_Images (Trav).Image_Width,
                 Height          => wndMain.All_Images (Trav).Image_Height);

            Gdk.Pixbuf.Fill (wndMain.All_Images (Trav).Image_Data, 16#0000FF00#);
         end if;
      end loop;

      Show_All (wndMain);

      Tubastga_Window_Pkg.Gtk_New (wndMain.Wnd_Performing_Piece);
      Show_All (wndMain.Wnd_Performing_Piece);

      Tubastga_Window_Pkg.Gtk_New (wndMain.Wnd_Target);
      Show_All (wndMain.Wnd_Target);

      Gdk.Color.Set_Rgb (Blue_Color, 0, 0, 65535);
      Gdk.Color.Set_Rgb (Red_Color, 65535, 0, 0);
      Gdk.Color.Set_Rgb (Green_Color, 0, 65535, 0);
      Gdk.Color.Set_Rgb (Grey_Color, 65000, 65000, 65000);
      Gdk.Color.Set_Rgb (Orange_Color, 65535, 32000, 16000);
      Gdk.Color.Set_Rgb (Yellow_Color, 65535, 65535, 0);

      if Verbose then
         Text_IO.Put_Line ("Initialize - Wnd Main - enter");
      end if;

   end Initialize;

   procedure Gtk_New (Wnd_Performing_Piece : out Wnd_Piece_Access) is
   begin
      Wnd_Performing_Piece := new Wnd_Performing_Piece_Record;
      Tubastga_Window_Pkg.Initialize (Wnd_Performing_Piece);
   end Gtk_New;

   procedure Initialize (Wnd_Performing_Piece : access Wnd_Performing_Piece_Record'Class) is
      pragma Suppress (All_Checks);

   begin
      if Verbose then
         Text_IO.Put_Line ("Initialize - Wnd Piece - enter");
      end if;

      Gtk.Window.Initialize (Wnd_Performing_Piece, Window_Toplevel);
      Set_Title (Wnd_Performing_Piece, -"Tubast'ga - Performing Piece");
      Set_Position (Wnd_Performing_Piece, Win_Pos_None);
      Set_Modal (Wnd_Performing_Piece, False);
      Set_Resizable (Wnd_Performing_Piece, False);

      Gtk_New (Wnd_Performing_Piece.Layout_Table, 5, 2, False);
      Set_Row_Spacings (Wnd_Performing_Piece.Layout_Table, 0);
      Set_Col_Spacings (Wnd_Performing_Piece.Layout_Table, 0);
      Add (Wnd_Performing_Piece, Wnd_Performing_Piece.Layout_Table);

      Gtk.Label.Gtk_New (Wnd_Performing_Piece.all.Lbl_Piece_Name, "Piece name:");
      Gtk.GEntry.Gtk_New (Wnd_Performing_Piece.all.En_Piece_Name);
      Gtk.GEntry.Set_Editable (Wnd_Performing_Piece.all.En_Piece_Name, False);

      Gtk.Label.Gtk_New (Wnd_Performing_Piece.all.Lbl_Piece_Type, "Piece type");

      Wnd_Performing_Piece.Piece_Info_Box := Gtk.Box.Gtk_Hbox_New;

      Gtk.Box.Pack_Start
        (Wnd_Performing_Piece.Piece_Info_Box,
         Wnd_Performing_Piece.all.Lbl_Piece_Name);
      Gtk.Box.Pack_Start
        (Wnd_Performing_Piece.Piece_Info_Box,
         Wnd_Performing_Piece.all.En_Piece_Name);
      Gtk.Box.Pack_Start
        (Wnd_Performing_Piece.Piece_Info_Box,
         Wnd_Performing_Piece.all.Lbl_Piece_Type);

      Attach
        (Wnd_Performing_Piece.Layout_Table,
         Wnd_Performing_Piece.Piece_Info_Box,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Wall1);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Wall1, "Wall 1");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Wall1,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall1.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Wall2);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Wall2, "Wall 2");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Wall2,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall2.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Wall2);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Wall2, "Wall 2");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Wall2,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall2.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Wall3);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Wall3, "Wall 3");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Wall3,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall3.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Wall4);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Wall4, "Wall 4");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Wall4,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall4.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Wall5);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Wall5, "Wall 5");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Wall5,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall5.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Wall6);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Wall6, "Wall 6");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Wall6,
         Gtk.Image.Gtk_Image_New_From_File ("resources\button_wall6.png"));

      Wnd_Performing_Piece.Construction_Box := Gtk.Box.Gtk_Hbox_New;
      Attach
        (Wnd_Performing_Piece.Layout_Table,
         Wnd_Performing_Piece.Construction_Box,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 1,
         Bottom_Attach => 2,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Construction_Box, Wnd_Performing_Piece.Btn_Wall1);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Construction_Box, Wnd_Performing_Piece.Btn_Wall2);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Construction_Box, Wnd_Performing_Piece.Btn_Wall3);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Construction_Box, Wnd_Performing_Piece.Btn_Wall4);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Construction_Box, Wnd_Performing_Piece.Btn_Wall5);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Construction_Box, Wnd_Performing_Piece.Btn_Wall6);

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Move);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Move, "Move");

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Attack);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Attack, "Attack");

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Ranged_Attack);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Ranged_Attack, "Range Attack");

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Search);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Search, "Search");

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Promote);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Promote, "Promote");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Promote,
         Gtk.Image.Gtk_Image_New_From_File ("resources\promote.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Demote);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Demote, "Demote");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Demote,
         Gtk.Image.Gtk_Image_New_From_File ("resources\demote.png"));

      Wnd_Performing_Piece.Fighting_Piece_Box := Gtk.Box.Gtk_Hbox_New;
      Attach
        (Wnd_Performing_Piece.Layout_Table,
         Wnd_Performing_Piece.Fighting_Piece_Box,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 2,
         Bottom_Attach => 3,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Fighting_Piece_Box, Wnd_Performing_Piece.Btn_Move);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Fighting_Piece_Box, Wnd_Performing_Piece.Btn_Attack);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Fighting_Piece_Box, Wnd_Performing_Piece.Btn_Ranged_Attack);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Fighting_Piece_Box, Wnd_Performing_Piece.Btn_Search);
      Gtk.Box.Pack_Start
        (Wnd_Performing_Piece.Fighting_Piece_Box,
         Wnd_Performing_Piece.Btn_Promote);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Fighting_Piece_Box, Wnd_Performing_Piece.Btn_Demote);
      --
      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Create_Path);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Create_Path, "Create Path");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Create_Path,
         Gtk.Image.Gtk_Image_New_From_File ("resources\demote.png"));

      Gtk.Button.Gtk_New (Wnd_Performing_Piece.Btn_Remove_Path);
      Gtk.Button.Set_Label (Wnd_Performing_Piece.Btn_Remove_Path, "Remove Path");
      Gtk.Button.Set_Image
        (Wnd_Performing_Piece.Btn_Create_Path,
         Gtk.Image.Gtk_Image_New_From_File ("resources\demote.png"));

      Wnd_Performing_Piece.Path_Box := Gtk.Box.Gtk_Hbox_New;
      Attach
        (Wnd_Performing_Piece.Layout_Table,
         Wnd_Performing_Piece.Path_Box,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 3,
         Bottom_Attach => 4,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Path_Box, Wnd_Performing_Piece.Btn_Create_Path);
      Gtk.Box.Pack_Start (Wnd_Performing_Piece.Path_Box, Wnd_Performing_Piece.Btn_Remove_Path);

      --

      Gtk.Text_Buffer.Gtk_New (Wnd_Performing_Piece.all.Buffer_Performing_Piece_Patch_Info);
      Gtk.Text_View.Gtk_New
        (Wnd_Performing_Piece.all.View_Performing_Piece_Patch_Info,
         Wnd_Performing_Piece.all.Buffer_Performing_Piece_Patch_Info);
      Gtk.Text_View.Set_Editable (Wnd_Performing_Piece.all.View_Performing_Piece_Patch_Info, False);
      Gtk.Text_View.Set_Size_Request
        (Wnd_Performing_Piece.all.View_Performing_Piece_Patch_Info,
         450,
         200);
      Wnd_Performing_Piece.all.View_Performing_Piece_Patch_Box := Gtk.Box.Gtk_Hbox_New;
      Attach
        (Wnd_Performing_Piece.Layout_Table,
         Wnd_Performing_Piece.View_Performing_Piece_Patch_Box,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 4,
         Bottom_Attach => 5,
         Xpadding      => 0,
         Ypadding      => 0);
      Gtk.Box.Pack_Start
        (Wnd_Performing_Piece.all.View_Performing_Piece_Patch_Box,
         Wnd_Performing_Piece.all.View_Performing_Piece_Patch_Info);

      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Wall1,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Wall1'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Wall2,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Wall2'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Wall3,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Wall3'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Wall4,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Wall4'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Wall5,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Wall5'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Wall6,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Wall6'Access),
         False);

      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Move,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Move'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Attack,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Attack'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Ranged_Attack,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Ranged_Attack'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Search,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Search'Access),
         False);

      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Promote,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Promote'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Demote,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Demote'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Create_Path,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Create_Path'Access),
         False);
      Button_Cb.Connect
        (Wnd_Performing_Piece.Btn_Remove_Path,
         "clicked",
         Button_Cb.To_Marshaller (On_Button_Remove_Path'Access),
         False);

   end Initialize;

   procedure Gtk_New (Wnd_Target : out Wnd_Target_Access) is
   begin
      Wnd_Target := new Wnd_Target_Record;
      Tubastga_Window_Pkg.Initialize (Wnd_Target);
   end Gtk_New;

   procedure Initialize (Wnd_Target : access Wnd_Target_Record'Class) is
      pragma Suppress (All_Checks);

   begin
      if Verbose then
         Text_IO.Put_Line ("Initialize - Wnd Piece - enter");
      end if;

      Gtk.Window.Initialize (Wnd_Target, Window_Toplevel);
      Set_Title (Wnd_Target, -"Tubast'ga - Target");
      Set_Position (Wnd_Target, Win_Pos_None);
      Set_Modal (Wnd_Target, False);
      Set_Resizable (Wnd_Target, False);

      Gtk_New (Wnd_Target.Layout_Table, 5, 2, False);
      Set_Row_Spacings (Wnd_Target.Layout_Table, 0);
      Set_Col_Spacings (Wnd_Target.Layout_Table, 0);
      Add (Wnd_Target, Wnd_Target.Layout_Table);

      Gtk.Label.Gtk_New (Wnd_Target.all.Lbl_Piece_Name, "Piece name:");
      Gtk.GEntry.Gtk_New (Wnd_Target.all.En_Piece_Name);
      Gtk.GEntry.Set_Editable (Wnd_Target.all.En_Piece_Name, False);

      Gtk.Label.Gtk_New (Wnd_Target.all.Lbl_Piece_Type, "Piece type");

      Wnd_Target.Piece_Info_Box := Gtk.Box.Gtk_Hbox_New;

      Gtk.Box.Pack_Start (Wnd_Target.Piece_Info_Box, Wnd_Target.all.Lbl_Piece_Name);
      Gtk.Box.Pack_Start (Wnd_Target.Piece_Info_Box, Wnd_Target.all.En_Piece_Name);
      Gtk.Box.Pack_Start (Wnd_Target.Piece_Info_Box, Wnd_Target.all.Lbl_Piece_Type);

      Attach
        (Wnd_Target.Layout_Table,
         Wnd_Target.Piece_Info_Box,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 0,
         Bottom_Attach => 1,
         Xpadding      => 0,
         Ypadding      => 0);

      Gtk.Text_Buffer.Gtk_New (Wnd_Target.all.Buffer_Target_Piece_Patch_Info);
      Gtk.Text_View.Gtk_New
        (Wnd_Target.all.View_Target_Piece_Patch_Info,
         Wnd_Target.all.Buffer_Target_Piece_Patch_Info);
      Gtk.Text_View.Set_Editable (Wnd_Target.all.View_Target_Piece_Patch_Info, False);
      Gtk.Text_View.Set_Size_Request (Wnd_Target.all.View_Target_Piece_Patch_Info, 450, 200);
      Wnd_Target.all.View_Target_Piece_Patch_Box := Gtk.Box.Gtk_Hbox_New;
      Attach
        (Wnd_Target.Layout_Table,
         Wnd_Target.View_Target_Piece_Patch_Box,
         Left_Attach   => 0,
         Right_Attach  => 1,
         Top_Attach    => 1,
         Bottom_Attach => 2,
         Xpadding      => 0,
         Ypadding      => 0);
      Gtk.Box.Pack_Start
        (Wnd_Target.all.View_Target_Piece_Patch_Box,
         Wnd_Target.all.View_Target_Piece_Patch_Info);

   end Initialize;
end Tubastga_Window_Pkg;
