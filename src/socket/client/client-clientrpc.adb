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

with Text_IO;
with Hexagon;
with Piece;
with Text_IO;
with Utilities;
with Status;
with GNAT.Sockets;
with Game_RPC;
with GNAT.Sockets;

-- socket version
package body Client.ClientRPC is

   Address : GNAT.Sockets.Sock_Addr_Type;
   Socket  : GNAT.Sockets.Socket_Type;
   Channel : GNAT.Sockets.Stream_Access := null;

   Verbose : constant Boolean := False;

   procedure Init is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Init - enter");
      end if;

      Text_IO.Put_Line ("Socket version");

      GNAT.Sockets.Initialize;   --  Initialize socket runtime

   end Init;

   procedure Connect
     (P_Server_Connection : in Utilities.RemoteString.Type_String;
      P_Aux               : in Natural)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Connect - enter");
      end if;

      Address.Addr := GNAT.Sockets.Inet_Addr (Utilities.RemoteString.To_String (P_Server_Connection));

      Text_IO.Put_Line ("Connecting Server IP:" & GNAT.Sockets.Image (Address.Addr));

      Address.Port := GNAT.Sockets.Port_Type (P_Aux);

      Text_IO.Put_Line ("Socket Port:" & Address.Port'Img);
      GNAT.Sockets.Create_Socket (Socket);
      GNAT.Sockets.Set_Socket_Option
        (Socket,
         GNAT.Sockets.IP_Protocol_For_TCP_Level,
         GNAT.Sockets.Option_Type'(GNAT.Sockets.No_Delay, True));

      GNAT.Sockets.Connect_Socket (Socket, Address);

      --  Connected, use a stream connected to the socket

      Channel := GNAT.Sockets.Stream (Socket);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Connect - exit");
      end if;
   end Connect;

   procedure Disconnect is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Disconnect - enter");
      end if;

      Text_IO.Put_Line ("Disconnecting Server IP:" & GNAT.Sockets.Image (Address.Addr));
      Text_IO.Put_Line ("Socket Port:" & Address.Port'Img);

      GNAT.Sockets.Close_Socket (Socket);
      Channel := null;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Disconnect - exit");
      end if;
   end Disconnect;

   procedure Start is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Start - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Start_Start);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Start_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Start - exit");
      end if;
   end Start;

   procedure Stop is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Stop - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Stop_Start);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Stop_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Stop - exit");
      end if;
   end Stop;

   procedure Create_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece       : in     Piece.Type_Piece)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Create_Piece - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Create_Piece_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Hexagon.Type_Hexagon_Position'Output (Channel, P_Pos);
      Piece.Type_Piece'Output (Channel, P_Piece);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Create_Piece_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Create_Piece - exit" );
      end if;

   end Create_Piece;

   -- Public procedures offered by Server
   procedure Put_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Pos         : in     Hexagon.Type_Hexagon_Position;
      P_Piece_Id    : in     Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Put_Piece - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Put_Piece_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Hexagon.Type_Hexagon_Position'Output (Channel, P_Pos);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Put_Piece_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Put_Piece - exit");
      end if;
   end Put_Piece;

   procedure Remove_Piece
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Remove_Piece - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Remove_Piece_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Remove_Piece_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Remove_Piece - exit");
      end if;

   end Remove_Piece;

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames.Vector)
   is
      use GNAT.Sockets;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Pieces_Report - enter");
      end if;

      if Channel /= null then
         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Pieces_Report_Start);

         Player.Type_Player_Id'Output (Channel, P_Player_Id);

         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Pieces_Report_End);

         P_Visibility_Frames := Observation.Frames.Piece_Visibility_Frames.Vector'Input (Channel);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Pieces_Report - exit");
      end if;

   end Get_Pieces_Report;

   procedure Perform_Attack
     (P_Player_Id                               : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Attack - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Attack_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Attacking_Piece_Id);
      Piece.Type_Piece_Id'Output (Channel, P_Attacked_Piece_Id);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Attack_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Attack - exit");
      end if;
   end Perform_Attack;

   procedure Perform_Ranged_Attack
     (P_Player_Id                               : in     Player.Type_Player_Id;
      P_Action_Type                             : in     Action.Type_Action_Type;
      P_Attacking_Piece_Id, P_Attacked_Piece_Id : in     Piece.Type_Piece_Id)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Ranged_Attack - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Ranged_Attack_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Attacking_Piece_Id);
      Piece.Type_Piece_Id'Output (Channel, P_Attacked_Piece_Id);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Ranged_Attack_End);

      if Verbose then
         Text_IO.Put_Line
           ("Client.ClientRPC.Perform_Ranged_Attack - exit");
      end if;
   end Perform_Ranged_Attack;

   procedure Perform_Move
     (P_Player_Id          : in     Player.Type_Player_Id;
      P_Action_Type        : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_To_Pos : in     Hexagon.Type_Hexagon_Position)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Client.ClientRPC.Perform_Move - enter piece.id=" &
            P_Piece_Id'Img &
            " player_id=" &
            P_Player_Id'Img);
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Move_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Hexagon.Type_Hexagon_Position'Output (Channel, P_To_Pos);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Move_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Move - exit");
      end if;

   end Perform_Move;

   procedure Perform_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Client.ClientRPC.Perform_Patch_Effect- enter piece.id=" &
            P_Piece_Id'Img &
            " P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Patch_Effect_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Effect.Type_Effect'Output (Channel, P_Effect);
      Hexagon.Area.Type_Action_Capabilities_A'Output (Channel, P_Area);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Patch_Effect_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Patch_Effect - exit");
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Client.ClientRPC.Perform_Piece_Effect- enter piece.id=" &
            P_Piece_Id'Img &
            " P_Player_Id=" &
            P_Player_Id'Img);
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Piece_Effect_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Effect.Type_Effect'Output (Channel, P_Effect);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Piece_Effect_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Piece_Effect - exit");
      end if;
   end Perform_Piece_Effect;

   procedure Perform_Construction
     (P_Player_Id             : in     Player.Type_Player_Id;
      P_Action_Type           : in     Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id;
      P_Construction_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Construction          : in     Construction.Type_Construction)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Construction - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Construction_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Hexagon.Type_Hexagon_Position'Output (Channel, P_Construction_Pos);
      Construction.Type_Construction'Output (Channel, P_Construction);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Construction_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Construction - exit");
      end if;
   end Perform_Construction;

   procedure Perform_Demolition
     (P_Player_Id             : in     Player.Type_Player_Id;
      P_Action_Type           : in     Action.Type_Action_Type;
      P_Piece_Id : in     Piece.Type_Piece_Id;
      P_Demolition_Pos      : in     Hexagon.Type_Hexagon_Position;
      P_Construction          : in     Construction.Type_Construction)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Demolition - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Demolition_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Hexagon.Type_Hexagon_Position'Output (Channel, P_Demolition_Pos);
      Construction.Type_Construction'Output (Channel, P_Construction);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Perform_Demolition_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Perform_Demolition - exit");
      end if;
   end Perform_Demolition;

   procedure Grant_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Piece_Effect - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Grant_Piece_Effect_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Effect.Type_Effect'Output (Channel, P_Effect);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Grant_Piece_Effect_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Piece_Effect - exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Piece_Effect - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Revoke_Piece_Effect_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Effect.Type_Effect'Output (Channel, P_Effect);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Revoke_Piece_Effect_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Piece_Effect - exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Patch_Effect - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Grant_Patch_Effect_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Effect.Type_Effect'Output (Channel, P_Effect);
      Hexagon.Area.Type_Action_Capabilities_A'Output (Channel, P_Area);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Grant_Patch_Effect_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Grant_Patch_Effect - exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type;
      P_Piece_Id    : in     Piece.Type_Piece_Id;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Patch_Effect - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Revoke_Patch_Effect_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Action.Type_Action_Type'Output (Channel, P_Action_Type);
      Piece.Type_Piece_Id'Output (Channel, P_Piece_Id);
      Effect.Type_Effect'Output (Channel, P_Effect);
      Hexagon.Area.Type_Action_Capabilities_A'Output (Channel, P_Area);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Revoke_Patch_Effect_End);

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Revoke_Patch_Effect - exit");
      end if;
   end Revoke_Patch_Effect;

   procedure Get_Map (P_Server_Map : out Landscape.Type_Map) is
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Map - enter");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Map_Start);
      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Map_End);

      Landscape.Type_Map'Read (Channel, P_Server_Map);

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

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Create_Game_Start);

      Utilities.RemoteString.Type_String'Write (Channel, P_Create_File_Name);
      Utilities.RemoteString_List.Vector'Write (Channel, P_Player_Name_List);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Create_Game_End);

      P_Status := Status.Type_Adm_Status'Input (Channel);

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

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Save_Game_Start);

      Utilities.RemoteString.Type_String'Write (Channel, P_Save_File_Name);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Save_Game_End);

      P_Status := Status.Type_Adm_Status'Input (Channel);

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

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Load_Game_Start);

      Utilities.RemoteString.Type_String'Write (Channel, P_Load_File_Name);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Load_Game_End);

      P_Status := Status.Type_Adm_Status'Input (Channel);

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

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Join_Game_Start);

      Utilities.RemoteString.Type_String'Write (Channel, P_Player_Name);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Join_Game_End);

      Player_Id := Player.Type_Player_Id'Input (Channel);
      P_Status  := Status.Type_Adm_Status'Input (Channel);

      return Player_Id;
   end Join_Game;

   procedure Leave_Game
     (P_Player_Id   : in     Player.Type_Player_Id;
      P_Player_Name : in     Utilities.RemoteString.Type_String;
      P_Status      :    out Status.Type_Adm_Status)
   is

   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Leave_Game - enter - exit");
      end if;

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Leave_Game_Start);

      Player.Type_Player_Id'Output (Channel, P_Player_Id);
      Utilities.RemoteString.Type_String'Write (Channel, P_Player_Name);

      Game_RPC.Type_RPC'Output (Channel, Game_RPC.Leave_Game_End);

      P_Status := Status.Type_Adm_Status'Input (Channel);

   end Leave_Game;

   function Get_Player_Name
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Status    :    out Status.Type_Adm_Status) return Utilities.RemoteString.Type_String
   is
      Player_Name : Utilities.RemoteString.Type_String;

      use GNAT.Sockets;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Player_Name - enter - exit");
      end if;

      if Channel /= null then
         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Player_Name_Start);

         Player.Type_Player_Id'Output (Channel, P_Player_Id);

         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Player_Name_End);

         Utilities.RemoteString.Type_String'Read (Channel, Player_Name);
         P_Status := Status.Type_Adm_Status'Input (Channel);
      else
         Player_Name := Utilities.RemoteString.To_Unbounded_String ("");
      end if;

      return Player_Name;
   end Get_Player_Name;

   procedure Get_Updates_Summary
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Countdown         :    out Positive;
      P_Game_Status       :    out Status.Type_Game_Status;
      P_System_Messages   :    out Observation.Activity.Activity_Report.Vector)
   is
      use GNAT.Sockets;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Client.ClientRPC.Get_Updates_Summary - enter P_Player_Id=" & P_Player_Id'Img);
      end if;

      if Channel /= null then
         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Updates_Summary_Start);
         Player.Type_Player_Id'Output (Channel, P_Player_Id);
         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Updates_Summary_End);

         P_Countdown := Positive'Input (Channel);

         P_Game_Status := Status.Type_Game_Status'Input (Channel);

         Observation.Activity.Activity_Report.Clear (P_System_Messages);
         P_System_Messages := Observation.Activity.Activity_Report.Vector'Input (Channel);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Updates_Summary - exit");
      end if;
   end Get_Updates_Summary;

   procedure Get_Server_Info
     (P_Server_Info : out Utilities.RemoteString_List.Vector;
      P_Status      : out Status.Type_Adm_Status)
   is

      use GNAT.Sockets;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Server_Info - enter ");
      end if;

      if Channel /= null then
         Utilities.RemoteString_List.Append
           (P_Server_Info,
            Utilities.RemoteString.To_Unbounded_String ("Connected"));
         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Server_Info_Start);

         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Get_Server_Info_End);

         P_Server_Info := Utilities.RemoteString_List.Vector'Input (Channel);
         P_Status      := Status.Type_Adm_Status'Input (Channel);
      else
         Utilities.RemoteString_List.Append
           (P_Server_Info,
            Utilities.RemoteString.To_Unbounded_String ("Disconnected"));
      end if;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Get_Server_Info - exit ");
      end if;
   end Get_Server_Info;

   procedure Client_Stopped (P_Player_Id : in Player.Type_Player_Id) is

      use GNAT.Sockets;
   begin
      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Client_Stopped - enter Player_Id=" & P_Player_Id'Img);
      end if;

      if Channel /= null then
         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Client_Stopped_Start);

         Player.Type_Player_Id'Output (Channel, P_Player_Id);

         Game_RPC.Type_RPC'Output (Channel, Game_RPC.Client_Stopped_End);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Client.ClientRPC.Client_Stopped - exit");
      end if;

   end Client_Stopped;

end Client.ClientRPC;
