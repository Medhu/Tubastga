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

package body Piece.Client_Piece is

   Verbose : constant Boolean := False;

   Piece_Class : Piece.Client_Piece.Type_Client_Piece_Class_Access := null;

   procedure Init
     (P_Piece_Class : in Piece.Client_Piece.Type_Client_Piece'Class)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Init - enter");
      end if;

      Piece_Class :=
        new Piece.Client_Piece.Type_Client_Piece'Class'(P_Piece_Class);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Init - exit");
      end if;
   end Init;

   procedure Create_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Piece_Type  : in     Type_Piece_Type;
      P_Category    : in     Type_Category;
      P_Patch       : in out Landscape.Type_Patch;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is

   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Client_Piece.Create_Piece - enter Create the " &
            P_Player_Id'Img &
            " piece_type " &
            P_Piece_Type'Img);
      end if;

      P_Piece.Type_Of_Piece := P_Piece_Type;
      P_Piece.Player_Id     := P_Player_Id;
      P_Piece.Id            := Undefined_Piece_Id;
      P_Piece.Category      := P_Category;

      Client.ClientRPC.Create_Piece
        (P_Action_Type,
         P_Patch.Pos,
         P_Piece,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Create_Piece - exit");
      end if;

   end Create_Piece;

   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Piece.Client_Piece.Pieces_Client_List.Cursor
   is
      Trav : Piece.Client_Piece.Pieces_Client_List.Cursor;

      Found : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Piece.Client_Piece.Find_Piece_In_List - enter - (returning Cursor)");
      end if;
      Found := False;

      Trav :=
        Piece.Client_Piece.Pieces_Client_List.First
          (Piece.Client_Piece.Client_Pieces_In_Game);
      while Piece.Client_Piece.Pieces_Client_List.Has_Element (Trav) and
        not Found
      loop
         if Piece.Client_Piece.Pieces_Client_List.Element (Trav).all.Id =
           P_Piece_Id
         then
            Found := True;
         else
            Trav := Piece.Client_Piece.Pieces_Client_List.Next (Trav);
         end if;
      end loop;

      if Verbose then
         Text_IO.Put_Line
           ("Piece.Client_Piece.Find_Piece_In_List - exit - (returning Cursor)");
      end if;
      return Trav;
   end Find_Piece_In_List;

   function Find_Piece_In_List
     (P_Piece_Id : in Piece.Type_Piece_Id)
      return Type_Client_Piece_Class_Access
   is
      Trav : Piece.Client_Piece.Pieces_Client_List.Cursor;
      Ret  : Type_Client_Piece_Class_Access;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Find_Piece_In_List - enter");
      end if;

      Trav := Find_Piece_In_List (P_Piece_Id);

      if Piece.Client_Piece.Pieces_Client_List.Has_Element (Trav) then
         Ret := Piece.Client_Piece.Pieces_Client_List.Element (Trav);
      else
         Ret := null;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Find_Piece_In_List - exit");
      end if;

      return Ret;
   end Find_Piece_In_List;

   procedure Put (P_Piece : in Type_Piece) is
   begin
      Text_IO.Put
        (" Id=" &
         P_Piece.Id'Img &
         " Type_Of_Piece=" &
         P_Piece.Type_Of_Piece'Img &
         " Player_Id=" &
         P_Piece.Player_Id'Img);
   end Put;

   function Validate_Executing_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := False;

      use Piece;
      use Player;
   begin

      -- Player can only move his own piece
      if Ret and P_Piece.Player_Id = P_Player_Id then
         Ret := True;
      end if;

      return Ret;
   end Validate_Executing_Piece;

   function Validate_Target_Piece
     (P_Piece     : in Type_Piece;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := False;

      use Piece;
      use Player;
   begin

      -- Player can only attack another players piece
      if Ret and P_Piece.Player_Id /= P_Player_Id then
         Ret := True;
      end if;

      return Ret;
   end Validate_Target_Piece;

   function Movement_Capability
     (P_Piece : in Type_Piece)
      return Hexagon.Area.Client_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Client_Area.Type_Action_Capabilities_Access;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Movement_Capability - enter");
      end if;

      Ret :=
        new Hexagon.Area.Type_Action_Capabilities'
          (Client.ClientRPC.Movement_Capability (P_Piece.Id));

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Movement_Capability - exit");
      end if;

      return Ret;

   end Movement_Capability;

   function Attack_Capability
     (P_Piece : in Type_Piece)
      return Hexagon.Area.Client_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Client_Area.Type_Action_Capabilities_Access;

      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Attack_Capability - enter");
      end if;

      Ret :=
        new Hexagon.Area.Type_Action_Capabilities'
          (Client.ClientRPC.Attack_Capability (P_Piece.Id));

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Attack_Capability - exit");
      end if;

      return Ret;

   end Attack_Capability;

   function Observation_Area
     (P_Piece : in Type_Piece)
      return Hexagon.Area.Client_Area.Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Client_Area.Type_Action_Capabilities_Access;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Observation_Area - enter");
      end if;

      Ret :=
        new Hexagon.Area.Type_Action_Capabilities'
          (Client.ClientRPC.Observation_Area (P_Piece.Id));

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Observation_Area - exit");
      end if;

      return Ret;

   end Observation_Area;

   procedure Put_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Patch       : in out Landscape.Type_Patch;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Put_Piece - enter");
      end if;

      Client.ClientRPC.Put_Piece
        (P_Action_Type,
         P_Patch.Pos,
         P_Piece.Id,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Put_Piece - exit");
      end if;

   end Put_Piece;

   procedure Remove_Piece
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in out Type_Piece;
      P_Patch       : in out Landscape.Type_Patch;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Remove_Piece - enter");
      end if;

      Client.ClientRPC.Remove_Piece
        (P_Action_Type,
         P_Patch.Pos,
         P_Piece.Id,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Remove_Piece - exit");
      end if;

   end Remove_Piece;

   procedure Perform_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_Attacking_Patch, P_Attacked_Patch : in out Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            :    out Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Attack - enter");
      end if;

      Client.ClientRPC.Perform_Attack
        (P_Action_Type,
         P_Attacking_Piece.Id,
         P_Attacked_Piece.Id,
         Hexagon.Type_Hexagon_Position'
           (True, P_Attacking_Patch.Pos.A, P_Attacking_Patch.Pos.B),
         Hexagon.Type_Hexagon_Position'
           (True, P_Attacked_Patch.Pos.A, P_Attacked_Patch.Pos.B),
         P_Player_Id,
         P_Winner,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Attack - exit");
      end if;

   end Perform_Attack;

   procedure Perform_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_Path                              : in     Hexagon.Path.Vector;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            :    out Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Attack - enter");
      end if;

      Client.ClientRPC.Perform_Attack
        (P_Action_Type,
         P_Attacking_Piece.Id,
         P_Attacked_Piece.Id,
         P_Path,
         P_Player_Id,
         P_Winner,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Attack - exit");
      end if;

   end Perform_Attack;

   procedure Perform_Ranged_Attack
     (P_Action_Type                       : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in out Type_Piece;
      P_Attacking_Patch, P_Attacked_Patch : in out Landscape.Type_Patch;
      P_Player_Id                         : in     Player.Type_Player_Id;
      P_Winner                            :    out Player.Type_Player_Id;
      P_Status                            :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Ranged_Attack - enter");
      end if;

      Client.ClientRPC.Perform_Ranged_Attack
        (P_Action_Type,
         P_Attacking_Piece.Id,
         P_Attacked_Piece.Id,
         Hexagon.Type_Hexagon_Position'
           (True, P_Attacking_Patch.Pos.A, P_Attacking_Patch.Pos.B),
         Hexagon.Type_Hexagon_Position'
           (True, P_Attacked_Patch.Pos.A, P_Attacked_Patch.Pos.B),
         P_Player_Id,
         P_Winner,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Ranged_Attack - exit");
      end if;

   end Perform_Ranged_Attack;

   procedure Perform_Move
     (P_Action_Type            : in     Action.Type_Action_Type;
      P_Moving_Piece           : in out Type_Piece;
      P_From_Patch, P_To_Patch : in out Landscape.Type_Patch;
      P_Player_Id              : in     Player.Type_Player_Id;
      P_Status                 :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Move - enter");
      end if;

      Client.ClientRPC.Perform_Move
        (P_Action_Type,
         P_Moving_Piece.Id,
         Hexagon.Type_Hexagon_Position'
           (True, P_From_Patch.Pos.A, P_From_Patch.Pos.B),
         Hexagon.Type_Hexagon_Position'
           (True, P_To_Patch.Pos.A, P_To_Patch.Pos.B),
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Move - exit");
      end if;

   end Perform_Move;

   procedure Perform_Move
     (P_Action_Type  : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Type_Piece;
      P_Path         : in     Hexagon.Path.Vector;
      P_Player_Id    : in     Player.Type_Player_Id;
      P_Status       :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Move (Path) - enter");
      end if;

      Client.ClientRPC.Perform_Move
        (P_Action_Type,
         P_Moving_Piece.Id,
         P_Path,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Move (Path) - exit");
      end if;
   end Perform_Move;

   procedure Perform_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Patch       : in     Landscape.Type_Patch;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Patch_Effect - enter");
      end if;

      Client.ClientRPC.Perform_Patch_Effect
        (P_Action_Type,
         P_Piece.Id,
         Hexagon.Type_Hexagon_Position'(True, P_Patch.Pos.A, P_Patch.Pos.B),
         P_Effect,
         P_Area,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Patch_Effect - exit");
      end if;

   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Type_Piece;
      P_Patch       : in     Landscape.Type_Patch;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Piece_Effect - enter");
      end if;

      Client.ClientRPC.Perform_Piece_Effect
        (P_Action_Type,
         P_Piece.Id,
         Hexagon.Type_Hexagon_Position'(True, P_Patch.Pos.A, P_Patch.Pos.B),
         P_Effect,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Piece_Effect - exit");
      end if;

   end Perform_Piece_Effect;

   procedure Get_Pieces_Report
     (P_Player_Id         : in     Player.Type_Player_Id;
      P_Visibility_Frames :    out Observation.Frames.Piece_Visibility_Frames
        .Vector)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Get_Pieces_Report - enter");
      end if;

      Client.ClientRPC.Get_Pieces_Report (P_Player_Id, P_Visibility_Frames);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Get_Pieces_Report - exit");
      end if;

   end Get_Pieces_Report;

   procedure Grant_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Grant_Effect - enter");
      end if;

      Client.ClientRPC.Grant_Piece_Effect
        (P_Action_Type,
         P_Piece.Id,
         P_Effect,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Grant_Effect - exit");
      end if;
   end Grant_Piece_Effect;

   procedure Revoke_Piece_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Effect      : in     Effect.Type_Effect;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Revoke_Piece_Effect - enter");
      end if;

      Client.ClientRPC.Revoke_Piece_Effect
        (P_Action_Type,
         P_Piece.Id,
         P_Effect,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Revoke_Piece_Effect - exit");
      end if;
   end Revoke_Piece_Effect;

   procedure Grant_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Patch       : in     Landscape.Type_Patch;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Grant_Patch_Effect - enter");
      end if;

      Client.ClientRPC.Grant_Patch_Effect
        (P_Action_Type,
         P_Piece.Id,
         Hexagon.Type_Hexagon_Position'
           (P_Valid => True, A => P_Patch.Pos.A, B => P_Patch.Pos.B),
         P_Effect,
         P_Area,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Grant_Patch_Effect - exit");
      end if;
   end Grant_Patch_Effect;

   procedure Revoke_Patch_Effect
     (P_Action_Type : in     Action.Type_Action_Type;
      P_Piece       : in     Piece.Type_Piece;
      P_Patch       : in     Landscape.Type_Patch;
      P_Effect      : in     Effect.Type_Effect;
      P_Area        : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Player_Id   : in     Player.Type_Player_Id;
      P_Status      :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Revoke_Patch_Effect - enter");
      end if;

      Client.ClientRPC.Revoke_Patch_Effect
        (P_Action_Type,
         P_Piece.Id,
         Hexagon.Type_Hexagon_Position'
           (P_Valid => True, A => P_Patch.Pos.A, B => P_Patch.Pos.B),
         P_Effect,
         P_Area,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Revoke_Patch_Effect - exit");
      end if;
   end Revoke_Patch_Effect;

   procedure Perform_Construction
     (P_Action_Type        : in     Action.Type_Action_Type;
      P_Construction_Piece : in     Type_Piece;
      P_Piece_Pos          : in     Landscape.Type_Patch;
      P_Construction_Patch : in     Landscape.Type_Patch;
      P_Construction       : in     Construction.Type_Construction;
      P_Player_Id          : in     Player.Type_Player_Id;
      P_Status             :    out Status.Type_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Construction - enter");
      end if;

      Client.ClientRPC.Perform_Construction
        (P_Action_Type,
         P_Construction_Piece.Id,
         Hexagon.Type_Hexagon_Position'
           (P_Valid => True, A => P_Piece_Pos.Pos.A, B => P_Piece_Pos.Pos.B),
         Hexagon.Type_Hexagon_Position'
           (P_Valid => True,
            A       => P_Construction_Patch.Pos.A,
            B       => P_Construction_Patch.Pos.B),
         P_Construction,
         P_Player_Id,
         P_Status);

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Perform_Construction - exit");
      end if;
   end Perform_Construction;

   function Find_Effect
     (P_Piece_Id    : in Piece.Type_Piece_Id;
      P_Effect_Name :    Effect.Type_Effect_Name) return Natural
   is
      Trav_Piece   : Piece.Client_Piece.Pieces_Client_List.Cursor;
      Trav_Effects : Effect.Effect_List.Cursor;
      An_Effect    : Effect.Type_Effect;

      A_Piece : Piece.Client_Piece.Type_Client_Piece_Class_Access;

      Found : Boolean := False;
      Ret   : Natural := 0;

      use Piece;
      use Effect;
   begin

      -- Find the piece
      A_Piece := Piece.Client_Piece.Find_Piece_In_List (P_Piece_Id);

      Trav_Effects := Effect.Effect_List.First (A_Piece.Effects_On_Piece);
      while Effect.Effect_List.Has_Element (Trav_Effects) and not Found loop
         An_Effect := Effect.Effect_List.Element (Trav_Effects);

         if An_Effect.Effect_Name = P_Effect_Name then
            Ret   := An_Effect.Aux;
            Found := True;
         end if;
         Trav_Effects := Effect.Effect_List.Next (Trav_Effects);
      end loop;

      return Ret;
   end Find_Effect;

   procedure Set_Effects_On_Piece
     (P_Piece   : in out Piece.Client_Piece.Type_Client_Piece;
      P_Effects : in     Observation.Observation_Of_Pieces_Effects
        .Changes_To_Pieces_Effects
        .Vector)
   is
      Trav : Observation.Observation_Of_Pieces_Effects
        .Changes_To_Pieces_Effects
        .Cursor;
      An_Effect : Effect.Type_Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Set_Effects_On_Piece - enter");
      end if;

      -- First remove all that needs to be removed
      Trav :=
        Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
          .First
          (P_Effects);
      while Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
          .Has_Element
          (Trav)
      loop
         if P_Piece.Id =
           Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
             .Element
             (Trav)
             .Piece_Id
         then
            An_Effect :=
              Observation.Observation_Of_Pieces_Effects
                .Changes_To_Pieces_Effects
                .Element
                (Trav)
                .Effect_Info;

            if not Observation.Observation_Of_Pieces_Effects
                .Changes_To_Pieces_Effects
                .Element
                (Trav)
                .Valid
            then
               Effect.Effect_List.Exclude
                 (P_Piece.Effects_On_Piece,
                  An_Effect.Effect_Name);
            end if;
         end if;

         Trav :=
           Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
             .Next
             (Trav);
      end loop;

      -- Then add all that needs to be added.
      Trav :=
        Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
          .First
          (P_Effects);
      while Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
          .Has_Element
          (Trav)
      loop
         if P_Piece.Id =
           Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
             .Element
             (Trav)
             .Piece_Id
         then
            An_Effect :=
              Observation.Observation_Of_Pieces_Effects
                .Changes_To_Pieces_Effects
                .Element
                (Trav)
                .Effect_Info;

            if Observation.Observation_Of_Pieces_Effects
                .Changes_To_Pieces_Effects
                .Element
                (Trav)
                .Valid
            then
               Effect.Effect_List.Include
                 (P_Piece.Effects_On_Piece,
                  An_Effect.Effect_Name,
                  An_Effect);
            end if;

         end if;
         Trav :=
           Observation.Observation_Of_Pieces_Effects.Changes_To_Pieces_Effects
             .Next
             (Trav);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Set_Effects_On_Piece - exit");
      end if;

   end Set_Effects_On_Piece;

   procedure Set_Reports_On_Pieces
     (P_Observed_Piece_Info : in Observation.Observation_Of_Pieces_Info
        .Changes_To_Pieces_Info
        .Vector;
      P_Effects : in Observation.Observation_Of_Pieces_Effects
        .Changes_To_Pieces_Effects
        .Vector)
   is
      Trav : Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
        .Cursor;

      Trav_Pieces_Change_Effects : Piece.Client_Piece.Pieces_Client_List
        .Cursor;

      Existing : Pieces_Client_List.Cursor;
      A_Piece  : Piece.Client_Piece.Type_Client_Piece_Class_Access;
   begin
      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Set_Reports_On_Pieces - enter");
      end if;

      Trav :=
        Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.First
          (P_Observed_Piece_Info);
      while Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
          .Has_Element
          (Trav)
      loop

         if Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
             .Element
             (Trav)
             .Valid
         then
            A_Piece :=
              new Piece.Client_Piece.Type_Client_Piece'Class'(Piece_Class.all);

            A_Piece.all.Id :=
              Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
                .Element
                (Trav)
                .Piece_Here
                .Id;
            A_Piece.all.Type_Of_Piece :=
              Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
                .Element
                (Trav)
                .Piece_Here
                .Type_Of_Piece;
            A_Piece.all.Category :=
              Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
                .Element
                (Trav)
                .Piece_Here
                .Category;
            A_Piece.all.Name :=
              Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
                .Element
                (Trav)
                .Piece_Here
                .Name;
            A_Piece.all.Player_Id :=
              Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
                .Element
                (Trav)
                .Piece_Here
                .Player_Id;
            Effect.Effect_List.Clear (A_Piece.all.Effects_On_Piece);

            Pieces_Client_List.Append
              (Piece.Client_Piece.Client_Pieces_In_Game,
               A_Piece);

         else
            Existing :=
              Piece.Client_Piece.Find_Piece_In_List
                (Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info
                   .Element
                   (Trav)
                   .Piece_Here
                   .Id);

            if Pieces_Client_List.Has_Element (Existing) then
               Pieces_Client_List.Delete
                 (Piece.Client_Piece.Client_Pieces_In_Game,
                  Existing);
            end if;

         end if;

         Trav :=
           Observation.Observation_Of_Pieces_Info.Changes_To_Pieces_Info.Next
             (Trav);
      end loop;

      Trav_Pieces_Change_Effects :=
        Piece.Client_Piece.Pieces_Client_List.First (Client_Pieces_In_Game);
      while Piece.Client_Piece.Pieces_Client_List.Has_Element
          (Trav_Pieces_Change_Effects)
      loop

         A_Piece :=
           Piece.Client_Piece.Pieces_Client_List.Element
             (Trav_Pieces_Change_Effects);

         Piece.Client_Piece.Set_Effects_On_Piece
           (Piece.Client_Piece.Type_Client_Piece (A_Piece.all),
            P_Effects);

         Trav_Pieces_Change_Effects :=
           Piece.Client_Piece.Pieces_Client_List.Next
             (Trav_Pieces_Change_Effects);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Piece.Client_Piece.Set_Reports_On_Pieces - exit");
      end if;
   end Set_Reports_On_Pieces;

   function Get_Pieces_Players
     (P_Patch : in Hexagon.Client_Map.Type_Client_Patch)
      return Player.Type_Player_Id
   is
      Piece_Cursor : Piece.Client_Piece.Pieces_Client_List.Cursor;
      Trav         : Landscape.Pieces_Here_List.Cursor;
      Ret          : Player.Type_Player_Id;

      use Piece;
   begin

   ---only one player can be on any patch, so it is enough to test one of them
      Trav         := Landscape.Pieces_Here_List.First (P_Patch.Pieces_Here);
      Piece_Cursor :=
        Piece.Client_Piece.Find_Piece_In_List
          (Landscape.Pieces_Here_List.Element (Trav));
      Ret :=
        Piece.Client_Piece.Pieces_Client_List.Element (Piece_Cursor).Player_Id;

      return Ret;

   end Get_Pieces_Players;

   function Is_Patch_Empty
     (P_Patch : in Hexagon.Client_Map.Type_Client_Patch) return Boolean
   is
      use Ada.Containers;
   begin

      return Landscape.Pieces_Here_List.Length (P_Patch.Pieces_Here) = 0;
   end Is_Patch_Empty;

   function Patch_Belongs_To_Player
     (P_Patch     : in Hexagon.Client_Map.Type_Client_Patch;
      P_Player_Id : in Player.Type_Player_Id) return Boolean
   is
      Ret : Boolean := True;

      use Player;
   begin
      if Is_Patch_Empty (P_Patch) then
         Ret := True; -- Neutral patch belongs to noone and everyone
      else
         Ret := Get_Pieces_Players (P_Patch) = P_Player_Id;
      end if;

      return Ret;
   end Patch_Belongs_To_Player;

end Piece.Client_Piece;
