--
--
--      Tubastga Game - A turn based strategy game.
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
with Hexagon;
with Piece;
with Text_IO;
with Utilities;
with Status;
with Tubastga_ServerRCI;

package body Client.ClientRPC is

   Verbose : constant Boolean := False;

   Server_Connection : Utilities.RemoteString.Type_String;

   procedure Init is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Init - enter");
      end if;

      Text_IO.Put_Line ("Client.ClientRPC - PolyORB version");

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Init - enter");
      end if;
   end Init;

   procedure Connect
     (P_Server_Connection : in Utilities.RemoteString.Type_String;
      P_Aux               : in Natural)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Connect - enter");
      end if;

      Server_Connection := P_Server_Connection;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Connect - exit");
      end if;
   end Connect;

   procedure Disconnect is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Disconnect - enter");
      end if;
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Disconnect - exit");
      end if;
   end Disconnect;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector;
      P_Status      : out Status.Type_Adm_Status)
   is
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Server_Info - enter ");
      end if;

      Tubastga_ServerRCI.Get_Server_Info (P_Server_Info, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Server_Info - exit ");
      end if;
   end Get_Server_Info;

   procedure Start is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Start - enter");
      end if;

      Tubastga_ServerRCI.Start;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Start - exit");
      end if;
   end Start;

   procedure Stop is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Stop - enter");
      end if;

      Tubastga_ServerRCI.Stop;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Stop - exit");
      end if;
   end Stop;

   procedure Create_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece       : in Piece.Type_Piece)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Create_Piece - enter");
      end if;

      Tubastga_ServerRCI.Create_Piece (P_Player_Id, P_Action_Type, P_Pos, P_Piece);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Create_Piece - exit");
      end if;

   end Create_Piece;

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Pos         : in Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Put_Piece - enter");
      end if;

      Tubastga_ServerRCI.Put_Piece (P_Player_Id, P_Action_Type, P_Pos, P_Piece_Id);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Put_Piece - exit ");
      end if;
   end Put_Piece;

   procedure Remove_Piece
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Remove_Piece - enter");
      end if;

      Tubastga_ServerRCI.Remove_Piece (P_Player_Id, P_Action_Type, P_Piece_Id);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Remove_Piece - exit");
      end if;

   end Remove_Piece;

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames.Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Pieces_Report - enter");
      end if;

      Tubastga_ServerRCI.Get_Pieces_Report (P_Player_Id, P_Visibility_Frames);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Pieces_Report - exit");
      end if;

   end Get_Pieces_Report;

   procedure Perform_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Attack - enter");
      end if;

      Tubastga_ServerRCI.Perform_Attack
        (P_Player_Id,
         P_Action_Type,
         P_Attacking_Piece_Id,
         P_Attacked_Piece_Id);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Attack - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack
     (P_Player_Id                               : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Ranged_Attack - enter");
      end if;

      Tubastga_ServerRCI.Perform_Ranged_Attack
        (P_Player_Id,
         P_Action_Type,
         P_Attacking_Piece_Id,
         P_Attacked_Piece_Id);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_To_Pos      : in Hexagon.Type_Hexagon_Position)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Client.ClientRPC.Perform_Move - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Tubastga_ServerRCI.Perform_Move (P_Player_Id, P_Action_Type, P_Piece_Id, P_To_Pos);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Move - exit");
      end if;

   end Perform_Move;

   procedure Perform_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Patch_Effect - enter");
      end if;

      Tubastga_ServerRCI.Perform_Patch_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect,
         P_Area);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Patch_Effect - exit");
      end if;

   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Piece_Effect - enter");
      end if;

      Tubastga_ServerRCI.Perform_Piece_Effect (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect);
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Piece_Effect - exit");
      end if;

   end Perform_Piece_Effect;

   procedure Grant_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Piece_Effect - enter");
      end if;

      Tubastga_ServerRCI.Grant_Piece_Effect (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Piece_Effect - exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Effect - enter");
      end if;

      Tubastga_ServerRCI.Revoke_Piece_Effect (P_Player_Id, P_Action_Type, P_Piece_Id, P_Effect);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Effect - exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Patch_Effect - enter");
      end if;

      Tubastga_ServerRCI.Grant_Patch_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect,
         P_Area);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Patch_Effect - exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type;
      P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect      : in Effect.Type_Effect;
      P_Area        : in Hexagon.Area.Type_Action_Capabilities_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Patch_Effect - enter");
      end if;

      Tubastga_ServerRCI.Revoke_Patch_Effect
        (P_Player_Id,
         P_Action_Type,
         P_Piece_Id,
         P_Effect,
         P_Area);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Patch_Effect - exit");
      end if;
   end Revoke_Patch_Effect;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Map - enter");
      end if;

      Tubastga_ServerRCI.Get_Map (P_Server_Map);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Map - exit");
      end if;
   end Get_Map;

   procedure Create_Game
     (P_Create_File_Name : in     Utilities.RemoteString.Type_String;
      P_Player_Name_List : in     Utilities.RemoteString_List.Vector;
      P_Status           :    out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Create_Game - enter");
      end if;

      Tubastga_ServerRCI.Create_Game (P_Create_File_Name, P_Player_Name_List, P_Status);
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Create_Game - exit");
      end if;
   end Create_Game;

   procedure Save_Game
     (P_Save_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Save_Game - enter");
      end if;

      Tubastga_ServerRCI.Save_Game (P_Save_File_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Save_Game - exit");
      end if;
   end Save_Game;

   procedure Load_Game
     (P_Load_File_Name : in     Utilities.RemoteString.Type_String;
      P_Status         :    out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Load_Game - enter");
      end if;

      Tubastga_ServerRCI.Load_Game (P_Load_File_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Load_Game - exit");
      end if;
   end Load_Game;

   function Join_Game
     (P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status) return Player.Type_Player_Id
   is
      Player_Id : Player.Type_Player_Id;

   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Join_Game - enter - exit");
      end if;

      Tubastga_ServerRCI.Join_Game (P_Player_Name, P_Status, Player_Id);

      return Player_Id;
   end Join_Game;

   procedure Leave_Game
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Leave_Game - enter");
      end if;

      Tubastga_ServerRCI.Leave_Game (P_Player_Id, P_Player_Name, P_Status);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Leave_Game - exit");
      end if;
   end Leave_Game;

   function Get_Player_Name
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Adm_Status) return Utilities.RemoteString.Type_String
   is
      Player_Name : Utilities.RemoteString.Type_String;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Player_Name - enter - exit");
      end if;

      Player_Name := Tubastga_ServerRCI.Get_Player_Name (P_Player_Id, P_Status);

      return Player_Name;
   end Get_Player_Name;

   procedure Get_Updates_Summary
     (P_Player_Id       : in     Player.Type_Player_Id;
      P_Countdown       :    out Positive;
      P_Game_Status     :    out Status.Type_Game_Status;
      P_System_Messages :    out Observation.Activity.Activity_Report.Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Client.ClientRPC.Get_Updates_Summary - enter P_Player_Id=" & P_Player_Id'Img);
      end if;

      Tubastga_ServerRCI.Get_Updates_Summary
        (P_Player_Id,
         P_Countdown,
         P_Game_Status,
         P_System_Messages);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Updates_Summary - exit");
      end if;
   end Get_Updates_Summary;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id) is

   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Client_Stopped - enter Player_Id=" & P_Player_Id'Img);
      end if;

      Tubastga_ServerRCI.Client_Stopped (P_Player_Id);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Client_Stopped - exit");
      end if;

   end Client_Stopped;

end Client.ClientRPC;
