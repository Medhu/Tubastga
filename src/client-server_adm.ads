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

with Status;
with Observation;
with Player;
with Landscape;
with Utilities;

package Client.Server_Adm is
   Verbose : Boolean := True;

   procedure Init;
   procedure Connect
     (P_Server_Connection : in Utilities.RemoteString.Type_String;
      P_Aux               : in Natural);
   procedure Disconnect;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector;
      P_Status      : out Status.Type_Adm_Status);
   procedure Create_Game
     (P_Create_File_Name : in     Utilities.RemoteString.Type_String;
      P_Player_Name_List : in     Utilities.RemoteString_List.Vector;
      P_Status           :    out Status.Type_Adm_Status);

   procedure Save_Game
     (P_Save_File : in     Utilities.RemoteString.Type_String;
      P_Status    :    out Status.Type_Adm_Status);

   procedure Load_Game
     (P_Load_File : in     Utilities.RemoteString.Type_String;
      P_Status    :    out Status.Type_Adm_Status);

   function Join_Game
     (P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status) return Player.Type_Player_Id;

   procedure Leave_Game
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status);

   function Get_Player_Name
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Adm_Status) return Utilities.RemoteString.Type_String;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map);

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames.Vector);

   procedure Get_Updates_Summary
     (P_Player_Id : in Player.Type_Player_Id;  -- The player of this
   --client
      P_Current_Player_Id : out Player.Type_Player_Id; -- The player that as
   --turn now
      P_Countdown       : out Positive;  -- The countdown of this turn
      P_Game_Status     : out Status.Type_Game_Status;
      P_System_Messages : out Observation.Activity.Activity_Report.Vector);

   function End_Turn (P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id);
end Client.Server_Adm;
