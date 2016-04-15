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

with Hexagon;
with Hexagon.Area;
with Piece;
with Player;
with Utilities;
with Landscape;
with Status;
with Observation;
with Effect;
with Construction;
with Action;

package Client.ClientRPC is
   
   procedure Init;
   procedure Connect
     (P_Server_Connection : in Utilities.RemoteString.Type_String;
      P_Aux               : in Natural);
   procedure Disconnect;

   procedure Start;
   procedure Stop;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector;
      P_Status      : out Status.Type_Adm_Status);

   procedure Create_Game
     (P_Create_File_Name : in     Utilities.RemoteString.Type_String;
      P_Player_Name_List : in     Utilities.RemoteString_List.Vector;
      P_Status           :    out Status.Type_Adm_Status);

   procedure Save_Game
     (P_Save_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status);

   procedure Load_Game
     (P_Load_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status);

   function Join_Game
     (P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      : out Status.Type_Adm_Status) return Player.Type_Player_Id;

   procedure Leave_Game
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      : out Status.Type_Adm_Status);

   function Get_Player_Name
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Adm_Status)
      return Utilities.RemoteString.Type_String;

   procedure Create_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in     Piece.Type_Piece;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Remove_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames
        .Vector);

   procedure Perform_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Player_Id                               : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status);

   procedure Perform_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Path                                    : in     Hexagon.Path.Vector;
      P_Player_Id                               : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status);

   procedure Perform_Ranged_Attack
     (P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id;
      P_Attacking_Pos, P_Attacked_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Player_Id                               : in     Player.Type_Player_Id;
      P_Winner                                  :    out Player.Type_Player_Id;
      P_Status                                  :    out Status.Type_Status);

   procedure Perform_Move
     (P_Action_Type        : in     Action.Type_Action_Type;
      P_Moving_Piece_Id    : in     Piece.Type_Piece_Id;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Player_Id          : in     Player.Type_Player_Id;
      P_Status             :    out Status.Type_Status);

   procedure Perform_Move
     (P_Action_Type     : in     Action.Type_Action_Type;
      P_Moving_Piece_Id : in     Piece.Type_Piece_Id;
      P_Path            : in     Hexagon.Path.Vector;
      P_Player_Id       : in     Player.Type_Player_Id;
      P_Status          :    out Status.Type_Status);

   procedure Perform_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Perform_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map);

   procedure Get_Updates_Summary
     (P_Player_Id : in Player.Type_Player_Id;  -- The player of this
   --client
      P_Current_Player_Id : out Player.Type_Player_Id; -- The player that as
   --turn now
      P_Countdown       : out Positive;  -- The countdown of this turn
      P_Game_Status     : out Status.Type_Game_Status;
      P_System_Messages : out Observation.Activity.Activity_Report.Vector);

   procedure Grant_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Revoke_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Grant_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Revoke_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status);

   procedure Perform_Construction
     (P_Action_Type           : in     Action.Type_Action_Type;
      P_Construction_Piece_Id : in     Piece.Type_Piece_Id;
      P_Piece_Pos             : in     Hexagon.Type_Hexagon_Position;
      P_Construction_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Construction          : in     Construction.Type_Construction;
      P_Player_Id             : in     Player.Type_Player_Id;
      P_Status                :    out Status.Type_Status);

   function End_Turn (P_Player_Id : in Player.Type_Player_Id) return Boolean;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id);

   function Observation_Area
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Type_Action_Capabilities;

   function Movement_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Type_Action_Capabilities;

   function Attack_Capability
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Hexagon.Area.Type_Action_Capabilities;

end Client.ClientRPC;
