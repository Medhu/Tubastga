--
--
--      Tubastga Game - A turn based strategy game.
--      Copyright (C) 2015-2021  Frank J Jorgensen
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
with Status;
with Utilities;
with Client.Server_Adm;
with Ada.Strings.Fixed;
with Text_IO;
with Glib;
with Player;

package body Tubastga_Window_Pkg.Callbacks.Main_Menu is

   Verbose : constant Boolean := False;

   procedure DlgGame_Populate
     (P_Dlg_Main_Menu : in out Type_Dlg_Main_Menu_Access;
      P_Server_Info   : in out Utilities.RemoteString_List.Vector;
      P_Connect       :    out Boolean;
      P_Create        :    out Boolean;
      P_Save          :    out Boolean;
      P_Load          :    out Boolean;
      P_Join          :    out Boolean;
      P_Leave         :    out Boolean;
      P_Disconnect    :    out Boolean)
   is

      Trav       : Utilities.RemoteString_List.Cursor;
      An_Element : Utilities.RemoteString.Type_String;

      use Ada.Containers;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.DlgGame_Populate - enter");
      end if;

      P_Connect    := False;
      P_Create     := False;
      P_Save       := False;
      P_Load       := False;
      P_Join       := False;
      P_Leave      := False;
      P_Disconnect := False;

      Gtk.Combo_Box_Text.Remove_All (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario);
      Gtk.Combo_Box_Text.Remove_All (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name);
      Gtk.Combo_Box_Text.Remove_All (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name);

      Gtk.Combo_Box_Text.Append_Text
        (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario,
         "Chose scenario...");
      Gtk.Combo_Box_Text.Append_Text (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name, "Load game...");
      Gtk.Combo_Box_Text.Append_Text (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name, "Join as...");

      P_Join  := True;
      P_Leave := False;
      Trav    := Utilities.RemoteString_List.First (P_Server_Info);
      while Utilities.RemoteString_List.Has_Element (Trav) loop
         An_Element := Utilities.RemoteString_List.Element (Trav);

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 10) =
           "Player " & Utilities.Number_To_Fixed_String (Natural (Me_Player_Id), 2) & ":"
         then

            P_Join  := False;
            P_Leave := True;
         end if;

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 9) =
           "Connected"
         then
            P_Connect    := False;
            P_Disconnect := True;
         end if;

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 12) =
           "Disconnected"
         then
            P_Connect    := True;
            P_Disconnect := False;
         end if;

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 4) = "Run:" then
            if Ada.Strings.Fixed.Tail
                (Utilities.RemoteString.To_String (An_Element),
                 Utilities.RemoteString.To_String (An_Element)'Last - 4) =
              "STARTING"
            then
               P_Connect    := False;
               P_Create     := True;
               P_Save       := False;
               P_Load       := True;
               P_Disconnect := True;
            end if;

            if Ada.Strings.Fixed.Tail
                (Utilities.RemoteString.To_String (An_Element),
                 Utilities.RemoteString.To_String (An_Element)'Last - 4) =
              "ONGOING"
            then
               P_Connect    := False;
               P_Create     := False;
               P_Save       := True;
               P_Load       := False;
               P_Disconnect := True;
            end if;

         end if;

         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 12) =
           "Description:"
         then

            Gtk.Label.Set_Text
              (P_Dlg_Main_Menu.all.Lbl_Join_Game,
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 12));
         end if;
         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 4) = "Map:" then

            Gtk.Combo_Box_Text.Insert
              (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario,
               100,
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 4),
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 4));
         end if;
         if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 6) =
           "Saved:"
         then
            Gtk.Combo_Box_Text.Append_Text
              (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name,
               Ada.Strings.Fixed.Tail
                 (Utilities.RemoteString.To_String (An_Element),
                  Utilities.RemoteString.To_String (An_Element)'Last - 6));
         end if;

         for I in Player.Type_Player_Id'First .. Player.Type_Player_Id'Last loop
            if Ada.Strings.Fixed.Head (Utilities.RemoteString.To_String (An_Element), 20) =
              "Registered Player" & I'Img & ":"
            then
               Gtk.Combo_Box_Text.Append_Text
                 (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name,
                  Ada.Strings.Fixed.Tail
                    (Utilities.RemoteString.To_String (An_Element),
                     Utilities.RemoteString.To_String (An_Element)'Last - 20));
            end if;
         end loop;

         Trav := Utilities.RemoteString_List.Next (Trav);
      end loop;

      Utilities.RemoteString_List.Clear (P_Server_Info);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.DlgGame_Populate- exit");
      end if;

   end DlgGame_Populate;

   procedure Refresh_Server_Configuration (P_Dlg_Main_Menu : in out Type_Dlg_Main_Menu_Access) is
      Adm_Status                                           : Status.Type_Adm_Status;
      Server_Info                                          : Utilities.RemoteString_List.Vector;
      Connect, Create, Save, Load, Join, Leave, Disconnect : Boolean;

      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.Refresh_Server_Configuration - enter");
      end if;

      Utilities.RemoteString_List.Clear (Server_Info);
      Client.Server_Adm.Get_Server_Info (Server_Info, Adm_Status);
      if Adm_Status = Status.Adm_Ok then
         DlgGame_Populate
           (P_Dlg_Main_Menu,
            Server_Info,
            Connect,
            Create,
            Save,
            Load,
            Join,
            Leave,
            Disconnect);
         Gtk.Combo_Box_Text.Set_Active (P_Dlg_Main_Menu.all.Cmb_Create_Game_Chose_Scenario, 0);
         Gtk.Combo_Box_Text.Set_Active (P_Dlg_Main_Menu.all.Cmb_Load_Game_Name, 0);
         Gtk.Combo_Box_Text.Set_Active (P_Dlg_Main_Menu.all.Cmb_Join_Player_Name, 0);

         Gtk.Button.Set_Sensitive (P_Dlg_Main_Menu.all.Btn_Connect_Server, Connect);
         Gtk.Button.Set_Sensitive (P_Dlg_Main_Menu.all.Btn_Disconnect_Server, Disconnect);
         Gtk.Button.Set_Sensitive (P_Dlg_Main_Menu.all.Btn_Create_Game, Create);
         Gtk.Button.Set_Sensitive (P_Dlg_Main_Menu.all.Btn_Join_Game, Join);
         Gtk.Button.Set_Sensitive (P_Dlg_Main_Menu.all.Btn_Leave_Game, Leave);
         Gtk.Button.Set_Sensitive (P_Dlg_Main_Menu.all.Btn_Save_Game, Save);
         Gtk.Button.Set_Sensitive (P_Dlg_Main_Menu.all.Btn_Load_Game, Load);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.Refresh_Server_Configuration - exit");
      end if;

   end Refresh_Server_Configuration;

   procedure On_Spin_Button_Change (Object : access Gtk.Spin_Button.Gtk_Spin_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Spin_Button_Change - clicked");
      end if;
      Gtk.Combo_Box_Text.Remove_All (The_Window.all.Dlg_Main_Menu.Cmb_Create_Game_Chose_Scenario);
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.Dlg_Main_Menu.Cmb_Create_Game_Chose_Scenario,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active
        (The_Window.all.Dlg_Main_Menu.Cmb_Create_Game_Chose_Scenario,
         0);

      Gtk.Combo_Box_Text.Remove_All (The_Window.all.Dlg_Main_Menu.Cmb_Load_Game_Name);
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.Dlg_Main_Menu.Cmb_Load_Game_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (The_Window.all.Dlg_Main_Menu.Cmb_Load_Game_Name, 0);

      Gtk.Combo_Box_Text.Remove_All (The_Window.all.Dlg_Main_Menu.Cmb_Join_Player_Name);
      Gtk.Combo_Box_Text.Append_Text
        (The_Window.all.Dlg_Main_Menu.Cmb_Join_Player_Name,
         "Not connected to server yet");
      Gtk.Combo_Box_Text.Set_Active (The_Window.all.Dlg_Main_Menu.Cmb_Join_Player_Name, 0);

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);
   end On_Spin_Button_Change;

   procedure On_Button_Connect (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      TCPIP_Octet1, TCPIP_Octet2, TCPIP_Octet3, TCPIP_Octet4 : Natural;
      Server_Connection                                      : Utilities.RemoteString.Type_String;

   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Connect - clicked");
      end if;
      TCPIP_Octet1 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.Dlg_Main_Menu.all.Adj_TCPIPOctet1));
      TCPIP_Octet2 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.Dlg_Main_Menu.all.Adj_TCPIPOctet2));
      TCPIP_Octet3 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.Dlg_Main_Menu.all.Adj_TCPIPOctet3));
      TCPIP_Octet4 :=
        Natural (Gtk.Adjustment.Get_Value (The_Window.all.Dlg_Main_Menu.all.Adj_TCPIPOctet4));

      Server_Connection :=
        Utilities.RemoteString.To_Unbounded_String
          (Ada.Strings.Fixed.Trim (TCPIP_Octet1'Img, Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim (TCPIP_Octet2'Img, Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim (TCPIP_Octet3'Img, Ada.Strings.Left) &
           "." &
           Ada.Strings.Fixed.Trim (TCPIP_Octet4'Img, Ada.Strings.Left));

      Client.Server_Adm.Connect
        (Server_Connection,
         Natural (Gtk.Adjustment.Get_Value (The_Window.all.Dlg_Main_Menu.all.Adj_TCPIPPort)));

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);
   end On_Button_Connect;

   procedure On_Button_Disconnect (Object : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Disconnect - clicked");
      end if;

      Client.Server_Adm.Client_Stopped (Me_Player_Id);
      Client.Server_Adm.Disconnect;

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);
   end On_Button_Disconnect;

   procedure On_Button_Refresh (Object : access Gtk.Button.Gtk_Button_Record'Class) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Refresh - clicked");
      end if;

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);

   end On_Button_Refresh;

   procedure On_Button_Create_Game (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;

      Player_Name_List : Utilities.RemoteString_List.Vector;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Create_Game - clicked");
      end if;

      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.Dlg_Main_Menu.En_Create_Game_Player_Name_1)));
      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.Dlg_Main_Menu.En_Create_Game_Player_Name_2)));
      Utilities.RemoteString_List.Append
        (Player_Name_List,
         Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.Dlg_Main_Menu.En_Create_Game_Player_Name_3)));
      Client.Server_Adm.Create_Game
        (Utilities.RemoteString.To_Unbounded_String
           (Gtk.Combo_Box_Text.Get_Active_Text
              (The_Window.all.Dlg_Main_Menu.Cmb_Create_Game_Chose_Scenario)),
         Player_Name_List,
         Adm_Status);

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);
   end On_Button_Create_Game;

   procedure On_Button_Save_Game (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;

      use Utilities.RemoteString;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Save_Game - clicked");
      end if;

      Client.Server_Adm.Save_Game
        (Utilities.RemoteString.To_Unbounded_String
           (Gtk.GEntry.Get_Text (The_Window.all.Dlg_Main_Menu.En_Save_Game_Name)),
         Adm_Status);

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);
   end On_Button_Save_Game;

   procedure On_Button_Load_Game (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;

      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Load_Game - clicked");
      end if;

      Client.Server_Adm.Load_Game
        (Utilities.RemoteString.To_Unbounded_String
           (Gtk.Combo_Box_Text.Get_Active_Text (The_Window.all.Dlg_Main_Menu.Cmb_Load_Game_Name)),
         Adm_Status);

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);
   end On_Button_Load_Game;

   procedure On_Button_Join_Game (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;
      My_Name    : Utilities.RemoteString.Type_String;

      use Status;
      use Utilities.RemoteString;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Join_Game - clicked - enter");
      end if;

      My_Name :=
        Utilities.RemoteString.To_Unbounded_String
          (Gtk.Combo_Box_Text.Get_Active_Text (The_Window.all.Dlg_Main_Menu.Cmb_Join_Player_Name));

      Me_Player_Id := Client.Server_Adm.Join_Game (My_Name, Adm_Status);
      if Adm_Status = Status.Adm_Ok then
         Hexagon.Client_Map.Get_Map (Me_Player_Id, A_Client_Map);
      end if;
      if Me_Player_Id = Player.Type_Player_Id (1) then
         Gtk.Label.Set_Text
           (The_Window.all.Lbl_Player_1_Name,
            Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (2) then
         Gtk.Label.Set_Text
           (The_Window.all.Lbl_Player_2_Name,
            Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (3) then
         Gtk.Label.Set_Text
           (The_Window.all.Lbl_Player_3_Name,
            Utilities.RemoteString.To_String (My_Name));
      end if;

      Set_Title
        (The_Window,
         Glib.UTF8_String'("Tubast'ga - " & Utilities.RemoteString.To_String (My_Name)));
      Set_Title
        (The_Window.all.Wnd_Performing_Patch,
         Glib.UTF8_String'
           ("Tubast'ga - Performing Patch - " & Utilities.RemoteString.To_String (My_Name)));

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Join_Game - clicked - exit Me_Player_Id=" &
            Me_Player_Id'Img);
      end if;
   end On_Button_Join_Game;

   procedure On_Button_Leave_Game (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      Adm_Status : Status.Type_Adm_Status;
      My_Name    : Utilities.RemoteString.Type_String;

      use Status;
      use Utilities.RemoteString;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Leave_Game - clicked - enter");
      end if;

      My_Name :=
        Utilities.RemoteString.To_Unbounded_String
          (Utilities.RemoteString.To_String (Player_Name_List (Integer (Me_Player_Id))));

      Client.Server_Adm.Leave_Game (Me_Player_Id, My_Name, Adm_Status);
      if Adm_Status = Status.Adm_Ok then
         Hexagon.Client_Map.Get_Map (Me_Player_Id, A_Client_Map);
      end if;
      if Me_Player_Id = Player.Type_Player_Id (1) then
         Gtk.Label.Set_Text
           (The_Window.all.Lbl_Player_1_Name,
            Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (2) then
         Gtk.Label.Set_Text
           (The_Window.all.Lbl_Player_2_Name,
            Utilities.RemoteString.To_String (My_Name));
      elsif Me_Player_Id = Player.Type_Player_Id (3) then
         Gtk.Label.Set_Text
           (The_Window.all.Lbl_Player_3_Name,
            Utilities.RemoteString.To_String (My_Name));
      end if;

      Set_Title (The_Window, Glib.UTF8_String'("Tubast'ga"));
      Set_Title
        (The_Window.all.Wnd_Performing_Patch,
         Glib.UTF8_String'("Tubast'ga - Performing Patch"));

      Refresh_Server_Configuration (The_Window.all.Dlg_Main_Menu);

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Leave_Game - clicked - exit Me_Player_Id=" &
            Me_Player_Id'Img);
      end if;
   end On_Button_Leave_Game;

   procedure On_Button_Close (Object : access Gtk.Button.Gtk_Button_Record'Class) is
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Window_Pkg.Callbacks.Main_Menu.On_Button_Close - clicked");
      end if;
      Gtk.Dialog.Response
        (Gtk.Dialog.Gtk_Dialog (The_Window.all.Dlg_Main_Menu),
         Gtk.Dialog.Gtk_Response_OK);

   end On_Button_Close;

end Tubastga_Window_Pkg.Callbacks.Main_Menu;
