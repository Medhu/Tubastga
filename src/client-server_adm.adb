--
--
--      Sisyfos Client/Server logic. This logic is a part of both server and client of Sisyfos.
--      Copyright (C) 2015  Frank J Jorgensen
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

with Text_IO;
with Client.ClientRPC;
with Server;

package body Client.Server_Adm is

   procedure Init is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Init - enter");
      end if;

      Client.ClientRPC.Init;

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Init - exit");
      end if;
   end Init;

   procedure Connect (P_Server_Connection : in Utilities.RemoteString.Type_String;
                      P_Aux               : in Natural)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Connect - enter");
      end if;

      Client.ClientRPC.Connect(P_Server_Connection, P_Aux);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Connect - exit");
      end if;
   end Connect;

   procedure Disconnect
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Disconnect - enter");
      end if;

      Client.ClientRPC.Disconnect;

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Disconnect - exit");
      end if;
   end Disconnect;

   procedure Get_Server_Info (P_Server_Info : out Utilities.RemoteString_List.Vector;
                             P_Status : out Status.Type_Adm_Status) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Server_Info - enter");
      end if;

      Client.ClientRPC.Get_Server_Info(P_Server_Info, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Server_Info - exit");
      end if;

   end Get_Server_Info;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Map - enter");
      end if;

      Client.ClientRPC.Get_Map (P_Server_Map);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Map - exit");
      end if;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Client.Server_Adm.Get_Map - Server not available");
         raise;
   end Get_Map;

   procedure Create_Game (P_Create_File_Name : in Utilities.RemoteString.Type_String;
                          P_Player_Name_List   : in     Utilities.RemoteString_List.Vector;
                          P_Status        : out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Create_Game - enter");
      end if;

      Client.ClientRPC.Create_Game(P_Create_File_Name, P_Player_Name_List, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Create_Game - enter");
      end if;
   end Create_Game;

   procedure Save_Game (P_Save_File : in Utilities.RemoteString.Type_String;
                        P_Status        : out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Save_Game - enter");
      end if;

      Client.ClientRPC.Save_Game(P_Save_File,P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Save_Game - exit");
      end if;
   end Save_Game;

   procedure Load_Game (P_Load_File : in Utilities.RemoteString.Type_String;
                        P_Status        : out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Load_Game - enter");
      end if;

      Client.ClientRPC.Load_Game(P_Load_File, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Load_Game - exit");
      end if;
   end Load_Game;

   function Join_Game
     (P_Player_Name : in Utilities.RemoteString.Type_String;
      P_Status      : out Status.Type_Adm_Status)
   return Player.Type_Player_Id
   is
      Player_Id : Player.Type_Player_Id;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Join_Game - enter");
      end if;

      Player_Id := Client.ClientRPC.Join_Game (P_Player_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Join_Game - exit Player_Id=" & Player_Id'Img);
      end if;

      return Player_Id;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Client.Server_Adm.Join_Game - Server not available");
         raise; --return 0;
   end Join_Game;

   procedure Leave_Game
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Leave_Game - enter");
      end if;

      Client.ClientRPC.Leave_Game (P_Player_Id, P_Player_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Leave_Game - exit");
      end if;
   end Leave_Game;

   procedure Get_Pieces_Report
     (P_Player_Id         : in Player.Type_Player_Id;
      P_Visibility_Frames : out Observation.Frames.Piece_Visibility_Frames.Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Pieces_Report - enter");
      end if;

      Client.ClientRPC.Get_Pieces_Report (P_Player_Id, P_Visibility_Frames);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Pieces_Report - exit");
      end if;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Client.Server_Adm.Get_Pieces_Report - Server not available");
         raise;
   end Get_Pieces_Report;

   function Get_Player_Name
     (P_Player_Id : in Player.Type_Player_Id;
     P_Status : out Status.Type_Adm_Status)
      return        Utilities.RemoteString.Type_String
   is
      Player_Name : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Player_Name - enter");
      end if;

      Player_Name := Client.ClientRPC.Get_Player_Name (P_Player_Id, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Player_Name - exit");
      end if;

      return Player_Name;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Client.Server_Adm.Get_Player_Name - Server not available");
         raise;
   end Get_Player_Name;

   procedure Get_Updates_Summary
     (P_Player_Id         : in Player.Type_Player_Id;  -- The player of this
      --client
      P_Current_Player_Id : out Player.Type_Player_Id; -- The player that as
      --turn now
      P_Countdown         : out Positive;  -- The countdown of this turn
      P_Game_Status       : out Status.Type_Game_Status;
      P_System_Messages   : out Observation.Activity.Activity_Report.Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Updates_Summary - enter");
      end if;

      Client.ClientRPC.Get_Updates_Summary
        (P_Player_Id,
         P_Current_Player_Id,
         P_Countdown,
         P_Game_Status,
         P_System_Messages);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Get_Updates_Summary - exit");
      end if;

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Client.Server_Adm.Get_Updates_Summary - Server not available");
         raise;
   end Get_Updates_Summary;

   function End_Turn (P_Player_Id : in Player.Type_Player_Id) return Boolean is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.End_Turn - enter - exit");
      end if;

      return Client.ClientRPC.End_Turn (P_Player_Id);

   exception
      when Server.Game_Engine_Doesnt_Exists =>
         Text_IO.Put_Line ("Client.Server_Adm.End_Turn - Server not available");
         raise;
   end End_Turn;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Client_Stopped - enter");
      end if;

      Client.ClientRPC.Client_Stopped (P_Player_Id);

      if Verbose then
         Text_IO.Put_Line ("Client.Server_Adm.Client_Stopped - exit");
      end if;

   end Client_Stopped;

end Client.Server_Adm;
