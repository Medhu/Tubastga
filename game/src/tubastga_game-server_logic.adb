--
--
--      Tubastga Game
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

with Piece;
with Piece.Server;
with Piece.Server.House_Piece;
with Piece.Server.Fighting_Piece;
with Text_IO;
with Hexagon;
with Hexagon.Area;
with Hexagon.Area.Server_Area;
with Hexagon.Server_Navigation.Modify;
with Ada.Numerics.Discrete_Random;
with Status;
with Effect.Server;
with Tubastga_Game.Carrier;
with Tubastga_Game.Server_Logic.Carrier;
with Goods;
with Observation;
with Server.ServerAPI;
with Ada.Streams.Stream_IO;
with Tubastga_Game.Server_Logic.House_Piece;
with Ada.Real_Time;
with Tubastga_Game.Server_Logic.Move_Logic;
with Ada.Strings.Unbounded;

package body Tubastga_Game.Server_Logic is
   package Random is new Ada.Numerics.Discrete_Random (Positive);
   RandomGen : Random.Generator;

   Verbose : constant Boolean := True;

   Current_Scenario : Utilities.RemoteString.Type_String;

   use Hexagon.Area;
   Win_Pattern : Hexagon.Area
     .Server_Area.Type_Action_Capabilities_Access :=
     new Hexagon.Area
       .Type_Action_Capabilities'
       (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1), --13
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0), --14
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),--15
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),--16
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),--17
        Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1)--18
        );

   Energy_Update : Ada.Real_Time.Time;

   Navigation_Original : Hexagon.Server_Navigation.Type_Navigation;

   procedure Init_Piece (P_Piece_Class : in out Type_My_Tubastga_Piece) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Init_Piece (Piece) - enter");
      end if;

      if P_Piece_Class.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
         P_Piece_Class.Storage := new Goods.Type_Storage (1);
         Effect.Effect_List.Include
           (P_Piece_Class.Effects_On_Piece, Tubastga_Game.Effect_Stops,
            Effect.Type_Effect'
              (Tubastga_Game.Effect_Stops,
               Tubastga_Game.Carrier.Get_Tower_Code
                 (Piece.Undefined_Piece_Id, Piece.Undefined_Piece_Id, Piece.Undefined_Piece_Id)));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Init_Piece (Piece) - exit");
      end if;
   end Init_Piece;

   procedure Init_Piece (P_Piece_Class : in out Type_My_Tubastga_House) is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Init_Piece (House) - enter"); --&
      end if;

      if P_Piece_Class.Type_Of_Piece = Tubastga_Game.Tower_House then
         P_Piece_Class.Storage := new Goods.Type_Storage (3);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Init_Piece (House) - exit");
      end if;
   end Init_Piece;

   function Create_Piece_Name
     (P_Piece : in Type_My_Tubastga_Piece) return Utilities.RemoteString.Type_String
   is
      Name : Utilities.RemoteString.Type_String;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Create_Piece_Name - enter");
      end if;

      if P_Piece.Id = 1 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Mojevar");
      elsif P_Piece.Id = 2 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Ablis");
      elsif P_Piece.Id = 3 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Mosdjer");
      elsif P_Piece.Id = 4 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Votraver");
      elsif P_Piece.Id = 5 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Amtilga");
      elsif P_Piece.Id = 6 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Omojevar");
      elsif P_Piece.Id = 7 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Arbnia");
      elsif P_Piece.Id = 8 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Akanioo");
      elsif P_Piece.Id = 9 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Erinuo");
      elsif P_Piece.Id = 10 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Arrini");
      elsif P_Piece.Id = 11 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Uyt");
      elsif P_Piece.Id = 12 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Ahs");

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Create_Piece_Name - exit");
      end if;

      return Name;
   end Create_Piece_Name;

   function Create_Piece_Name
     (P_Piece : in Type_My_Tubastga_House) return Utilities.RemoteString.Type_String
   is
      Name : Utilities.RemoteString.Type_String;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Create_Piece_Name - enter");
      end if;

      if P_Piece.Id = 1 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Vatki Oratan");
      elsif P_Piece.Id = 2 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Tuelli Arsin");
      elsif P_Piece.Id = 3 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Vatki Abtan");
      elsif P_Piece.Id = 4 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Inatan Ayo");
      elsif P_Piece.Id = 5 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Vatki Omeni");
      elsif P_Piece.Id = 6 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Haau Jilta");
      elsif P_Piece.Id = 7 then
         Name := Utilities.RemoteString.To_Unbounded_String ("Saviu Adji");

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Create_Piece_Name - exit");
      end if;

      return Name;
   end Create_Piece_Name;

   function Observation_Area
     (P_Piece : in Type_My_Tubastga_Piece) return Hexagon.Area.Server_Area
     .Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Observation_Area - enter " & P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
         Ret :=
           new Hexagon.Area
             .Type_Action_Capabilities'
         -- group I
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1), --13
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0), --14
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),--15
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),--16
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),--17
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),--18
         -- group II
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),--1
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),--2
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),--3
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -1),--4
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),--5
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -2),--6
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),--7
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),--8
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),--9
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 1),--10
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),--11
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2)--12
              );

      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
         Ret :=
           new Hexagon.Area
             .Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
         -- group I
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 4),--1
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 3),--3
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 1),--6
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 4, -1),--8
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 4, -3),--11
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -4),--13
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -4),--16
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -3),--18
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, -1),--21
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -4, 1),--23
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -4, 3),--26
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 4),--28
         --group II
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 3),--2
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 0),--7
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -3),--12
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -3),--17
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 0),--22
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 3),--27
         --group III
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),--4
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),--31
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),--9
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -1),--32
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),--14
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -2),--33
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),--19
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),--34
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),--24
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 1),--35
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),--29
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2),--36
         --group IV
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),--5
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),--10
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),--15
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),--20
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),--25
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1)--30
              );

      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Ship_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
         -- group I
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Observation_Area - exit");
      end if;

      return Ret;
   end Observation_Area;

   --
   -- Create Piece
   --Piece
   function Validate_Create_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece) return Boolean
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Create_Piece (Piece)- enter - exit " &
            P_Piece.Type_Of_Piece'Img & " player_id=" & P_Player_Id'Img);
      end if;

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      if not Pieces_Type_Info_List (P_Piece.Type_Of_Piece).Move_Landscape
          (A_Patch.all.Landscape_Here) then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String ("You can't create that piece there"));

         return False;
      else

         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("You entered a Create Piece command (Piece)"));

         return True;
      end if;

   end Validate_Create_Piece;

   procedure Before_Create_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Result                                :    out Status.Type_Result_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Create_Piece (Piece)- enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Create_Piece (Piece)- exit ");
      end if;

      P_Result := Status.Proceed;
   end Before_Create_Piece;

   procedure End_Create_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Utilities.RemoteString;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.End_Create_Piece (Piece)- enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      Init_Piece (P_Piece);

      Piece.Set_Name (Piece.Type_Piece (P_Piece), Create_Piece_Name (P_Piece));

      Server.ServerAPI.Player_Activity_Report_Append
        (Observation.Activity.Internal_Details, P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String
           ("You placed a " &
            Utilities.RemoteString.To_String
              (Piece.Server.Get_Type_Of_Piece_Name (Piece.Type_Piece (P_Piece))) &
            " at " & P_Pos.A'Img & ", " & P_Pos.B'Img & " called " &
            Utilities.RemoteString.To_String (Piece.Get_Name (Piece.Type_Piece (P_Piece)))));

      Server.ServerAPI.Player_Activity_Report_Append
        (6, P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String ("Narrative of Create Piece (Piece)"));

      if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
         P_Piece.Health := 300;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
         P_Piece.Health := 260;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
         P_Piece.Health := 180;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
         P_Piece.Health := 40;
      end if;

      if Current_Scenario = "demo_1" then
         Server.ServerAPI.Player_Activity_Report_Append
           (6, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Narrative of Create Piece (Piece) in Demo_1 scenario"));
      end if;

      P_Attempt_Info := Attempt.Attempt_Done;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Create_Piece (Piece)- exit");
      end if;
   end End_Create_Piece;

   --
   -- Create Piece
   -- House
   function Validate_Create_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House) return Boolean
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Create_Piece (House)- enter - exit " &
            P_Piece.Type_Of_Piece'Img & " player_id=" & P_Player_Id'Img);
      end if;

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Pos.A, P_Pos.B);

      if not Houses_Type_Info_List (P_Piece.Type_Of_Piece).Construct_Landscape
          (A_Patch.all.Landscape_Here) then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String ("You can't create that house there"));

         return False;
      else

         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("You entered a Create Piece command (House)"));

         return True;
      end if;

   end Validate_Create_Piece;

   procedure Before_Create_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Result                                :    out Status.Type_Result_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Create_Piece (House)- enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      Server.ServerAPI.Player_Activity_Report_Append
        (1, P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String ("Before create piece (House)"));

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Create_Piece (House)- exit ");
      end if;

      P_Result := Status.Proceed;
   end Before_Create_Piece;

   procedure End_Create_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.End_Create_Piece (House)- enter " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      Init_Piece (P_Piece);

      Piece.Set_Name (Piece.Type_Piece (P_Piece), Create_Piece_Name (P_Piece));

      Server.ServerAPI.Player_Activity_Report_Append
        (6, P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String ("Narrative of Create Piece (House)"));

      P_Attempt_Info := Attempt.Attempt_Done;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Create_Piece (House)- exit");
      end if;
   end End_Create_Piece;

   --
   -- Put Piece
   -- Piece
   function Validate_Put_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Put_Piece - enter - exit " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      return True;
   end Validate_Put_Piece;

   procedure Before_Put_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Result                             :    out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Put_Piece - enter - exit " &
            P_Piece.Type_Of_Piece'Img);
      end if;

      P_Result := Status.Proceed;
   end Before_Put_Piece;

   procedure End_Put_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece                           : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.End_Put_Piece - enter - exit " & P_Piece.Type_Of_Piece'Img);
      end if;

   end End_Put_Piece;

   --
   -- Put Piece
   -- House
   function Validate_Put_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type : in Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Put_Piece - enter - exit");
      end if;

      return True;
   end Validate_Put_Piece;

   procedure Before_Put_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Result                             :    out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Put_Piece - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Put_Piece;

   procedure End_Put_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Pos : in Hexagon.Type_Hexagon_Position;
      P_Piece                           : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Put_Piece - enter - exit");
      end if;

   end End_Put_Piece;

   --
   -- Remove Piece
   -- Piece
   function Validate_Remove_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                            : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Remove_Piece - enter - exit");
      end if;

      return True;
   end Validate_Remove_Piece;

   procedure Before_Remove_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                           : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Result                                :    out Status.Type_Result_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Remove_Piece (Piece)- enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Remove_Piece (Piece)- exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Remove_Piece;

   procedure End_Remove_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Patch : in out Landscape.Type_Patch;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Remove_Piece (Piece)- enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Remove_Piece (Piece)- exit");
      end if;
   end End_Remove_Piece;

   --
   -- Remove Piece
   -- House
   function Validate_Remove_Piece (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                            : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Remove_Piece - enter - exit");
      end if;

      return True;
   end Validate_Remove_Piece;

   procedure Before_Remove_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                           : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Result                                :    out Status.Type_Result_Status)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Remove_Piece (House) - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Remove_Piece (House) - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Remove_Piece;

   procedure End_Remove_Piece (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type : in     Action.Type_Action_Type; P_Patch : in out Landscape.Type_Patch;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Remove_Piece (House) - enter");
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Remove_Piece (House) - exit");
      end if;
   end End_Remove_Piece;

   --
   -- Perform Attack
   --
   function Validate_Perform_Attack (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                              : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Perform_Attack - enter - exit");
      end if;

      return True;
   end Validate_Perform_Attack;

   procedure Before_Perform_Attack (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                             : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Result             :    out Status.Type_Result_Status)
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Perform_Attack - enter - exit");
      end if;

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_To_Pos.A, P_To_Pos.B);

      if not Pieces_Type_Info_List (P_Attacking_Piece.Type_Of_Piece).Attack_Landscape
          (A_Patch.all.Landscape_Here) then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("This piece piece can't enter that position (Piece)"));
      else
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("You entered an Attack Piece command (Piece)"));
      end if;

      -- Attack FSM
      --
      declare
         use Effect.Effect_List;
         Ret : Status.Type_Status;
      begin
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Attack_State:" & P_Attacking_Piece.Attack_State'Img));

         if P_Attacking_Piece.Attack_State = Idle then
            if Server.ServerAPI.Is_Effect_On_Piece (P_Player_id, P_Attacking_Piece.Id, Effect_Attack_Start) then
               P_Attacking_Piece.Attack_State := Tubastga_Game.Server_Logic.Start_Attacking;
               Server.ServerAPI.Grant_Piece_Effect(P_Player_Id, P_Action_Type,
                                            P_Attacking_Piece.Id,
                                            Effect.Type_Effect'(Tubastga_Game.Effect_Attack_Start, 0),
                                            Ret);

               P_Attacking_Piece.Attack_State := Tubastga_Game.Server_Logic.Start_Attacking;
            end if;
            P_Result := Status.Fail;

         elsif P_Attacking_Piece.Attack_State = Start_Attacking then
            if Effect.Effect_List.Find (P_Attacking_Piece.Effects_On_Piece, Effect_Defence_Done) /=
              Effect.Effect_List.No_Element then
               P_Attacking_Piece.Attack_State := Tubastga_Game.Server_Logic.Defence_Done;
            end if;

            P_Result := Status.Fail;
         elsif P_Attacking_Piece.Attack_State = Defence_Done then
            null;
            -- Defending done - attack can be calculate them
            P_Attacking_Piece.Attack_State := Tubastga_Game.Server_Logic.Idle;
            P_Result                       := Status.Proceed;
         end if;
      end;

      --
      --
      if Hexagon.Server_Navigation.Hexagon_Distance (P_From_Pos, P_To_Pos) /= 1 then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String ("Attacker is too far away from target"));
         P_Result := Status.Fail;
         Server.ServerAPI.Observe_Game (1);
      else
         P_Result := Status.Proceed;
      end if;
   end Before_Perform_Attack;

   procedure Calculate_Attack_Result (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                               : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece         : in out Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in Hexagon.Type_Hexagon_Position; P_Winner : out Player.Type_Player_Id)
   is

      Attack_Defence : Tubastga_Game.Server_Logic.Type_Attack_Defence_Record;

      Ret : Type_Attack_Defence_Types;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Calculate_Attack_Result - enter");

      end if;

      Attack_Defence :=
        Tubastga_Game.Server_Logic.Attack_Defence_Table
          (P_Attacking_Piece.Type_Of_Piece, P_Attacked_Piece.Type_Of_Piece);

      Ret := Tubastga_Game.Server_Logic.Roll (Attack_Defence);

      if Ret = Attacking_Wins then
         P_Attacked_Piece.Health := P_Attacked_Piece.Health - Attack_Defence.Attack;
      else
         P_Attacking_Piece.Health := P_Attacking_Piece.Health - Attack_Defence.Defence;
      end if;

      if P_Attacked_Piece.Health <= 0 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Health <= 0 then
         P_Winner := P_Attacked_Piece.Player_Id;
      else
         P_Winner := Player.Undefined_Player_Id;
      end if;

      P_Winner := P_Attacked_Piece.Player_Id;
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Calculate_Attack_Result - exit Winner=" & P_Winner'Img);
      end if;
   end Calculate_Attack_Result;

   procedure End_Perform_Attack (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                          : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in Hexagon.Type_Hexagon_Position; P_Winner : in Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      Attacker_Pos, Attacked_Pos : Hexagon.Type_Hexagon_Position;

      use Status;
      use Player;
      use Attempt;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Perform_Attack - enter");
      end if;

      if P_End_Status = Status.Ok then
         P_Attempt_Info := Attempt.Attempt_Done;

         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Attacked_Piece.Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              (Utilities.RemoteString.To_String (P_Attacking_Piece.Name) & " has " &
               P_Attacking_Piece.Health'Img & " " &
               Utilities.RemoteString.To_String (P_Attacked_Piece.Name) & " has " &
               P_Attacked_Piece.Health'Img));

         if P_Attacking_Piece.Player_Id = P_Winner then

            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacking_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacking_Piece.Name) &
                  " won the fight over " &
                  Utilities.RemoteString.To_String (P_Attacked_Piece.Name)));
            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacked_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacked_Piece.Name) &
                  " lost the fight against " &
                  Utilities.RemoteString.To_String (P_Attacking_Piece.Name)));

         elsif P_Attacked_Piece.Player_Id = P_Winner then

            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacked_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacked_Piece.Name) &
                  " won the fight over " &
                  Utilities.RemoteString.To_String (P_Attacking_Piece.Name)));
            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacking_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacking_Piece.Name) &
                  " lost the fight against " &
                  Utilities.RemoteString.To_String (P_Attacked_Piece.Name)));
         else
            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacked_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String ("The fight was not conclusive"));
         end if;

      else
         Attacker_Pos := Piece.Server.Find_Piece_In_List (P_Attacking_Piece.Id).Actual_Pos;
         Attacked_Pos := Piece.Server.Find_Piece_In_List (P_Attacked_Piece.Id).Actual_Pos;

         if Hexagon.Server_Navigation.Hexagon_Distance (Attacker_Pos, Attacked_Pos) /= 1 then
            P_Attempt_Info := Attempt.Attempt_Done;
         else
            Attempt.Set_Attempt(P_Attempt_Info, Attempt.Get_Attempt (P_Attempt_Info) - 1 );
         end if;

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Perform_Attack - exit");
      end if;
   end End_Perform_Attack;

   --
   -- Perform Ranged Attack
   --
   function Validate_Perform_Ranged_Attack (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                     : in Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece)
      return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Perform_Ranged_Attack - enter - exit");
      end if;

      return True;
   end Validate_Perform_Ranged_Attack;

   procedure Before_Perform_Ranged_Attack (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                    : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in     Hexagon.Type_Hexagon_Position;
      P_Result             :    out Status.Type_Result_Status)
   is
      Attacker_Pos, Attacked_Pos : Hexagon.Type_Hexagon_Position;

      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Perform_Ranged_Attack - enter - exit");
      end if;

      Attacker_Pos := Piece.Server.Find_Piece_In_List (P_Attacking_Piece.Id).Actual_Pos;
      Attacked_Pos := Piece.Server.Find_Piece_In_List (P_Attacked_Piece.Id).Actual_Pos;

      if Hexagon.Server_Navigation.Hexagon_Distance (Attacker_Pos, Attacked_Pos) > 2 then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String ("Attacker is too far away from target"));
         P_Result := Status.Fail;
         Server.ServerAPI.Observe_Game (1);
      else
         P_Result := Status.Proceed;
      end if;

   end Before_Perform_Ranged_Attack;

   procedure Calculate_Ranged_Attack_Result (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                      : in     Action.Type_Action_Type;
      P_Attacking_Piece, P_Attacked_Piece                : in out Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in Hexagon.Type_Hexagon_Position; P_Winner : out Player.Type_Player_Id)
   is
      Attack_Defence : Tubastga_Game.Server_Logic.Type_Attack_Defence_Record;

      Ret : Type_Attack_Defence_Types;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Calculate_Ranged_Attack_Result - enter");
      end if;

      Attack_Defence :=
        Tubastga_Game.Server_Logic.Attack_Defence_Table
          (P_Attacking_Piece.Type_Of_Piece, P_Attacked_Piece.Type_Of_Piece);

      Ret := Tubastga_Game.Server_Logic.Roll (Attack_Defence);

      if Ret = Attacking_Wins then
         P_Attacked_Piece.Health := P_Attacked_Piece.Health - Attack_Defence.Attack;
      else
         P_Attacking_Piece.Health := P_Attacking_Piece.Health - Attack_Defence.Defence;
      end if;

      if P_Attacked_Piece.Health <= 0 then
         P_Winner := P_Attacking_Piece.Player_Id;
      elsif P_Attacking_Piece.Health <= 0 then
         P_Winner := P_Attacked_Piece.Player_Id;
      else
         P_Winner := Player.Undefined_Player_Id;
      end if;

      P_Winner := P_Attacking_Piece.Player_Id;
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Calculate_Ranged_Attack_Result - exit Winner=" &
            P_Winner'Img);
      end if;
   end Calculate_Ranged_Attack_Result;

   procedure End_Perform_Ranged_Attack (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                 : in Action.Type_Action_Type;
      P_Attacking_Piece,
      P_Attacked_Piece     : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos : in Hexagon.Type_Hexagon_Position; P_Winner : in Player.Type_Player_Id;
      P_End_Status         : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      Attacker_Pos, Attacked_Pos : Hexagon.Type_Hexagon_Position;

      use Attempt;
      use Status;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Perform_Ranged_Attack - enter");
      end if;

      if P_End_Status = Status.Ok then
         P_Attempt_Info := Attempt.Attempt_Done;

         if P_Attacking_Piece.Player_Id = P_Winner then
            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacking_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacking_Piece.Name) &
                  " won the fight over " &
                  Utilities.RemoteString.To_String (P_Attacked_Piece.Name)));
            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacked_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacked_Piece.Name) &
                  " lost the fight against " &
                  Utilities.RemoteString.To_String (P_Attacking_Piece.Name)));
         else
            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacked_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacking_Piece.Name) & " missed "));
            Server.ServerAPI.Player_Activity_Report_Append
              (1, P_Attacking_Piece.Player_Id,
               Utilities.RemoteString.To_Unbounded_String
                 (Utilities.RemoteString.To_String (P_Attacking_Piece.Name) & " missed "));
         end if;

      else
         Attacker_Pos := Piece.Server.Find_Piece_In_List (P_Attacking_Piece.Id).Actual_Pos;
         Attacked_Pos := Piece.Server.Find_Piece_In_List (P_Attacked_Piece.Id).Actual_Pos;

         if Hexagon.Server_Navigation.Hexagon_Distance (Attacker_Pos, Attacked_Pos) > 2 then
            P_Attempt_Info := Attempt.Attempt_Done;
         else
            Attempt.Set_Attempt(P_Attempt_Info, Attempt.Get_Attempt (P_Attempt_Info) - 1 );
         end if;

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Perform_Ranged_Attack - exit");
      end if;
   end End_Perform_Ranged_Attack;
   --
   -- Perform Move
   --
   function Validate_Perform_Move (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                            : in Action.Type_Action_Type;
      P_Moving_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_To_Pos                                 : in Hexagon.Type_Hexagon_Position) return Boolean
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Ret : Boolean;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Perform_Move - enter - exit");
      end if;

      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_To_Pos.A, P_To_Pos.B);

      if not Pieces_Type_Info_List (P_Moving_Piece.Type_Of_Piece).Move_Landscape
          (A_Patch.all.Landscape_Here) then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("This piece piece can't enter that position (Piece) Pos=(" & P_To_Pos.A'Img & ", " &
               P_To_Pos.B'Img & ")"));

         Ret := False;
      else
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("You entered a Move Piece command (Piece)"));

         Ret := True;
      end if;

      return Ret;
   end Validate_Perform_Move;

   procedure Before_Perform_Move (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                           : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos                              : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos : in out Hexagon.Type_Hexagon_Position; P_End_Pos : in Hexagon.Type_Hexagon_Position;
      P_Result                                :    out Status.Type_Result_Status)
   is
      use Ada.Real_Time;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Perform_Move - enter");
      end if;

      if Ada.Real_Time.Clock > P_Moving_Piece.Next_Move_Attempt  then
         P_Moving_Piece.Next_Move_Attempt := Ada.Real_Time.Clock + Ada.Real_Time.Seconds(5);
         Text_IO.Put_Line("Halloiluken Tidspunkt til sjekk");

         Server.ServerAPI.Player_Activity_Report_Append
                 (1, P_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    ("Beregn path" ) );


         P_Result := Status.Proceed;
      else

         P_Result := Status.Fail;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Perform_Move - exit P_Status=" & P_Result'Img);
      end if;
   end Before_Perform_Move;

   procedure Before_Perform_Move_Step (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                           : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos                              : in     Hexagon.Type_Hexagon_Position;
      P_To_Pos : in out Hexagon.Type_Hexagon_Position; P_End_Pos : in Hexagon.Type_Hexagon_Position;
      P_Result                                :    out Status.Type_Result_Status)
   is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Move_Status : Boolean;

      use Landscape;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Perform_Move_Step - enter");
      end if;
      A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_To_Pos.A, P_To_Pos.B);

      Tubastga_Game.Server_Logic.Move_Logic.Move_Energy
        (P_Moving_Piece, Landscape.Type_Patch (A_Patch.all), Move_Status);
      if Move_Status then
         P_Result := Status.Proceed;
      else
         P_Result := Status.Fail;
      end if;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Perform_Move_Step - exit P_Status=" & P_Result'Img);
      end if;
   end Before_Perform_Move_Step;

   procedure End_Perform_Move (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                        : in     Action.Type_Action_Type;
      P_Moving_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Pos, P_To_Pos                 : in     Hexagon.Type_Hexagon_Position;
      P_End_Pos : in     Hexagon.Type_Hexagon_Position; P_End_Status : in Status.Type_Status;
      P_Attempt_Info                       : in out Attempt.Type_Attempt_Info)
   is
      n : Natural;

      use Attempt;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Perform_Move - enter P_End_Status:"
                           & P_End_Status'Img
                          & " P_Attempt_Info:" & Attempt.To_String (P_Attempt_Info)  );
      end if;


      if P_End_Status = Status.Completed_Ok then
         Attempt.Set_Done_Attempt (P_Attempt_Info);
            Server.ServerAPI.Player_Activity_Report_Append(1, P_Player_Id, Utilities.RemoteString.To_Unbounded_String("Movement succeeded") );
      else
         n := Attempt.Get_Attempt (P_Attempt_Info);
         Text_IO.Put_Line("");

         Attempt.Set_Attempt(P_Attempt_Info, n + 1 );
         --
         -- hva var resultatet fra forriige kjøring?
         -- ta vare på resultatet fra denne kjøringen:
         if n > 1000  then
            Server.ServerAPI.Player_Activity_Report_Append(1, P_Player_Id, Utilities.RemoteString.To_Unbounded_String("P_End_Pos:" & P_End_Status'Img) );
         end if;


      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Perform_Move - exit");
      end if;
   end End_Perform_Move;

   procedure Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                            : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
                                   P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
                                   P_Effect_Name : in Effect.Type_Effect_Name)
   is
      Effect_Area  : Hexagon.Area.Type_Action_Capabilities_A (1 .. 1);
      An_Effect    : Effect.Type_Effect;
      Ret_Status   : Status.Type_Status;
      Lua_Status   : Lua.Lua_Return_Code;
      Effect_Found : Boolean;

      use Lua;
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Perform_Patch_Effect - enter P_Piece.id=" & P_Piece.Id'Img &
            " P_Effect_Name=" & P_Effect_Name'Img );
      end if;

      Text_IO.Put_Line("Perform_Patch_Effect- A");
      Effect_Found := False;
      for T in P_Area'First .. P_Area'Last loop
         declare
            A_Patch       : Hexagon.Server_Map.Type_Server_Patch_Adress;
            Cursor_Effect : Effect.Effect_List.Cursor;
         begin
            A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (P_Area (T).A, P_Area (T).B);
            Cursor_Effect := Effect.Effect_List.First (A_Patch.all.Effects_Here);

            if Effect.Effect_List.Has_Element(Cursor_Effect) then
               An_Effect     := Effect.Effect_List.Element (Cursor_Effect);
               Server.ServerAPI.Player_Activity_Report_Append
                 (6, P_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    ("Effect=" & An_Effect.Aux'Img & " on " & P_Area (T).A'Img & " " &
                       P_Area (T).B'Img));

               Effect_Found := True;
            end if;
         end;

      end loop;

      Text_IO.Put_Line("Perform_Patch_Effect- B");

      if Effect_Found then
         Lua.Get_Global (Tubastga_Game.Server_Logic.Lua_State, "Tubastga");
         Lua.Get_Field (Tubastga_Game.Server_Logic.Lua_State, -1, "foundTreasure");
         Lua.Push (Tubastga_Game.Server_Logic.Lua_State, Lua.Lua_Integer (P_Player_Id));
         Lua.Push (Tubastga_Game.Server_Logic.Lua_State, Lua.Lua_Integer (An_Effect.Aux));
         Lua_Status := Lua.PCall (Tubastga_Game.Server_Logic.Lua_State, 2, 0, 0);
         if Lua_Status /= Lua.LUA_OK then
            --  An error occurs during the execution
            Text_IO.Put_Line (Lua_Status'Img);
            Text_IO.Put_Line (To_Ada (Lua_State, -1));
         end if;

         Server.ServerAPI.Player_Activity_Report_Append
           (6, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String ("We are searching the patch...."));


         -- for the moment only piece with id=8 will find something:)
         if P_Piece.Id = 8 then
            Server.ServerAPI.Player_Activity_Report_Append
              (6, P_Player_Id, Utilities.RemoteString.To_Unbounded_String ("We found something!"));

            Server.ServerAPI.Revoke_Patch_Effect
              (P_Player_Id, P_Action_Type, P_Piece.Id, P_Area, An_Effect.Effect_Name, Ret_Status);

         else
            Server.ServerAPI.Player_Activity_Report_Append
              (6, P_Player_Id, Utilities.RemoteString.To_Unbounded_String ("We found nothing!"));
         end if;

      else
         Server.ServerAPI.Player_Activity_Report_Append
           (6, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String ("There is nowhere particular to search here"));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Perform_Patch_Effect - exit");
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                            : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A; P_Effect_Name : in Effect.Type_Effect_Name)
   is

      type Type_Neighbours is array (1 .. 6) of Hexagon.Area.Type_Hexagon_Delta_Position;

      Neighbours : Type_Neighbours :=
        ((True, +1, 0), (True, +1, -1), (True, 0, -1), (True, -1, 0), (True, -1, +1),
         (True, 0, +1));

--
--      Wall_In_Navigation_Node : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
--      Other_Navigation_Node   : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
--      Other_Pos : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 1, 1);
--      Direction, Opposite     : Integer;

--      Trav : Hexagon.Server_Navigation.Navigation_List_Pkg.Cursor;
--      Trav_Neighbour : Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Cursor;
--      Original_Node : Hexagon.Server_Navigation.Type_Navigation_Node_Access;

      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Perform_Patch_Effect (House)- enter P_Piece.id=" &
            P_Piece.Id'Img);
      end if;

      -- 1) Copy Navigation information from the original Navigation map.
      -- Make sure that this node and nodes that are neighbours to this one
      -- has their neighbours set to original state.
      --
--      Trav := Hexagon.Server_Navigation.Navigation_List_Pkg.First(
--                                                                  Navigation_Original.Navigation_List);
--      while Hexagon.Server_Navigation.Navigation_List_Pkg.Has_Element(Trav) loop

--      nytt forsøk:
--      vi får inn Effect_Build_Wall_N så bygger vi vegg
--      vi får inn Effect_Demolish_Wall_N så resetter vi path mellom de to nodene.
--      når veggen er bygd så fjernes Effect_Build_N og Effect_Wall_N settes
--      når veggen er revet så gjernes Effect_Demolish_N og Effect_Wall_N

--      Trav_Neighbour := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.First
--        (Original_Node.all.Neighbours);

--      while Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element(Trav_Neighbour) loop

--         Trav_Neighbour :=
--           Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Next(Trav_Neighbour);
--      end loop;

      --
      --
--      Original_Node :=
--        Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
--          (Navigation_Original, P_Area (1));

--      Trav_Neighbour := Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.First
--        (Original_Node.all.Neighbours);

--      while Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Has_Element(Trav_Neighbour) loop

--         Trav_Neighbour :=
--           Hexagon.Server_Navigation.Navigation_Neighbours_List_Pkg.Next(Trav_Neighbour);
--      end loop;

--         Directions (P_Effect.Effect_Name, Direction, Opposite);

--

--        Other_Pos.A :=
--          Hexagon.Type_Hexagon_Numbers
--            (Integer (Wall_In_Navigation_Node.all.Pos.A) + Integer (Neighbours (Direction).A));
--        Other_Pos.B :=
--          Hexagon.Type_Hexagon_Numbers
--            (Integer (Wall_In_Navigation_Node.all.Pos.B) + Integer (Neighbours (Direction).B));
--
--        Other_Navigation_Node :=
--          Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
--            (Hexagon.Server_Navigation.A_Land_Navigation, Other_Pos);
--
--        Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
--          (Wall_In_Navigation_Node.all, Other_Navigation_Node.all.Id);
--
--        Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
--          (Other_Navigation_Node.all, Wall_In_Navigation_Node.all.Id);
--
--        Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
--          (Wall_In_Navigation_Node.all, Other_Navigation_Node.all.Id);
--
--        Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
--          (Other_Navigation_Node.all, Wall_In_Navigation_Node.all.Id);
--
--        -- 2) Go through all the effects on this paths, and
--        -- put in place the navigation information that we wish to achieve.
--        --
--
--        Directions (P_Effect.Effect_Name, Direction, Opposite);
--
--        Wall_In_Navigation_Node :=
--          Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
--            (Hexagon.Server_Navigation.A_Land_Navigation, P_Area (1));
--
--        Other_Pos.A :=
--          Hexagon.Type_Hexagon_Numbers
--            (Integer (Wall_In_Navigation_Node.all.Pos.A) + Integer (Neighbours (Direction).A));
--        Other_Pos.B :=
--          Hexagon.Type_Hexagon_Numbers
--            (Integer (Wall_In_Navigation_Node.all.Pos.B) + Integer (Neighbours (Direction).B));
--
--        Other_Navigation_Node :=
--          Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
--            (Hexagon.Server_Navigation.A_Land_Navigation, Other_Pos);
--
--        Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
--          (Wall_In_Navigation_Node.all, Other_Navigation_Node.all.Id);
--
--        Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
--          (Other_Navigation_Node.all, Wall_In_Navigation_Node.all.Id);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Perform_Patch_Effect (House) - exit");
      end if;
   end Perform_Patch_Effect;

   procedure Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                            : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name                                 : in     Effect.Type_Effect_Name)
   is
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Perform_Piece_Effect - enter P_Piece.id=" & P_Piece.Id'Img &
            " P_Effect.Effect_Name=" & P_Effect_Name'Img);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Perform_Piece_Effect - exit");
      end if;
   end Perform_Piece_Effect;

   procedure Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                            : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name                                 : in     Effect.Type_Effect_Name)
   is
      use Piece;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Perform_Piece_Effect (House)- enter P_Piece.id=" &
            P_Piece.Id'Img);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Perform_Piece_Effect (House) - exit");
      end if;
   end Perform_Piece_Effect;

   function Validate_Perform_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                    : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name                                         : in Effect.Type_Effect_Name;
      P_Area : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Perform_Patch_Effect(Piece) - enter - exit");
      end if;

      Text_IO.Put_Line("Validate_Perform_Patch_Effect");

      return True;
   end Validate_Perform_Patch_Effect;

   procedure Before_Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                   : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Result                                        :    out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Perform_Patch_Effect(Piece) - enter - exit");
      end if;

      Text_IO.Put_Line("Before_Perform_Patch_Effect");

      P_Result := Status.Proceed;
   end Before_Perform_Patch_Effect;

   procedure End_Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.End_Perform_Patch_Effect(Piece) - enter - exit");
      end if;

      Text_IO.Put_Line("End_Perform_Patch_Effect");
      if P_End_Status = Status.Patch_Effect_Not_Here then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String ("There was nothing to search for here"));
      end if;

      P_Attempt_Info := Attempt.Attempt_Done;
   end End_Perform_Patch_Effect;

   function Validate_Perform_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                    : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name                                         : in Effect.Type_Effect_Name;
      P_Area : in Hexagon.Area.Type_Action_Capabilities_A) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Perform_Patch_Effect (House) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Patch_Effect;

   procedure Before_Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                   : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Result                                        :    out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Perform_Patch_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Patch_Effect;

   procedure End_Perform_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Area : in Hexagon.Area.Type_Action_Capabilities_A;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.End_Perform_Patch_Effect (House) - enter - exit");
      end if;

      P_Attempt_Info := Attempt.Attempt_Done;
   end End_Perform_Patch_Effect;

   function Validate_Perform_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                    : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name                                         : in Effect.Type_Effect_Name) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Perform_Piece_Effect(Piece) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Piece_Effect;

   procedure Before_Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                   : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Result : out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Perform_Piece_Effect(Piece) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Piece_Effect;

   procedure End_Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_End_Status : in Status.Type_Status;
      P_Attempt_Info                               : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.End_Perform_Piece_Effect(Piece) - enter - exit");
      end if;

      P_Attempt_Info := Attempt.Attempt_Done;
   end End_Perform_Piece_Effect;

   function Validate_Perform_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                    : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name                                         : in Effect.Type_Effect_Name) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Perform_Piece_Effect (House) - enter - exit");
      end if;

      return True;
   end Validate_Perform_Piece_Effect;

   procedure Before_Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                   : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_Result : out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Perform_Piece_Effect (House) - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Perform_Piece_Effect;

   procedure End_Perform_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in     Effect.Type_Effect_Name; P_End_Status : in Status.Type_Status;
      P_Attempt_Info                               : in out Attempt.Type_Attempt_Info)
   is
      use Player;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.End_Perform_Piece_Effect (House) - enter - exit");
      end if;

      if P_End_Status = Status.Piece_Effect_Not_Here then
         Server.ServerAPI.Player_Activity_Report_Append
           (1, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("There were no effect " & P_Effect_Name'Img));
      end if;

   end End_Perform_Piece_Effect;

   function Validate_Grant_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                  : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect                                       : in Effect.Type_Effect) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Grant_Piece_Effect - enter - exit");
      end if;

      return True;
   end Validate_Grant_Piece_Effect;

   procedure Before_Grant_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                 : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect : in     Effect.Type_Effect; P_Result : out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Grant_Piece_Effect - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Piece_Effect;

   procedure End_Grant_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                              : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect : in     Effect.Type_Effect; P_End_Status : in Status.Type_Status;
      P_Attempt_Info                             : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Grant_Piece_Effect - enter - exit");
      end if;

      Server.ServerAPI.Player_Activity_Report_Append
        (1, P_Player_Id, Utilities.RemoteString.To_Unbounded_String ("Promoted"));

      P_Attempt_Info := Attempt.Attempt_Done;
   end End_Grant_Piece_Effect;

   function Validate_Grant_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                  : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect                                       : in Effect.Type_Effect) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Grant_Piece_Effect - enter - exit");
      end if;

      return True;
   end Validate_Grant_Piece_Effect;

   procedure Before_Grant_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                 : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect : in     Effect.Type_Effect; P_Result : out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Grant_Piece_Effect - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Piece_Effect;

   procedure End_Grant_Piece_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                              : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect : in     Effect.Type_Effect; P_End_Status : in Status.Type_Status;
      P_Attempt_Info                             : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Grant_Piece_Effect - enter - exit");
      end if;

      P_Attempt_Info := Attempt.Attempt_Done;
   end End_Grant_Piece_Effect;

   function Validate_Revoke_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                   : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name                                   : in Effect.Type_Effect_Name) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Revoke_Piece_Effect - enter - exit");
      end if;

      return True;
   end Validate_Revoke_Piece_Effect;

   procedure Before_Revoke_Piece_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                  : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result : out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Revoke_Piece_Effect - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Piece_Effect;

   procedure End_Revoke_Piece_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                               : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result : in Status.Type_Status;
      P_Attempt_Info                        : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Revoke_Piece_Effect - enter - exit");
      end if;

      Server.ServerAPI.Player_Activity_Report_Append
        (1, P_Player_Id, Utilities.RemoteString.To_Unbounded_String ("Demoted"));

      P_Attempt_Info := Attempt.Attempt_Done;
   end End_Revoke_Piece_Effect;

   function Validate_Revoke_Piece_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                   : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name                                   : in Effect.Type_Effect_Name) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Revoke_Piece_Effect - enter - exit");
      end if;

      Server.ServerAPI.Player_Activity_Report_Append
        (6, P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String ("Narrative of Revoke Piece Effect (House)"));

      Server.ServerAPI.Player_Activity_Report_Append
        (Observation.Activity.Internal_Details, P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String
           ("Your " & P_Piece.Type_Of_Piece'Img & " were revoked the effect " &
            P_Effect_Name'Img));

      return True;
   end Validate_Revoke_Piece_Effect;

   procedure Before_Revoke_Piece_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                  : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result : out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Revoke_Piece_Effect - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Piece_Effect;

   procedure End_Revoke_Piece_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                               : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Effect_Name : in     Effect.Type_Effect_Name;
      P_Result : in Status.Type_Status;
      P_Attempt_Info                        : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Revoke_Piece_Effect - enter - exit");
      end if;

      P_Attempt_Info := Attempt.Attempt_Done;
   end End_Revoke_Piece_Effect;

   function Validate_Grant_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                  : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area                                         : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect                                       : in Effect.Type_Effect) return Boolean
   is
      Ret : Boolean;

      use Player;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Grant_Patch_Effect - enter");
      end if;

      Ret := True;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Grant_Patch_Effect  - exit Ret=" & Ret'Img);
      end if;
      return Ret;
   end Validate_Grant_Patch_Effect;

   procedure Before_Grant_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                 : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A; P_Effect : in Effect.Type_Effect;
      P_Result                                      :    out Status.Type_Result_Status)
   is
      Ret : Boolean;

      use Player;
      use Piece;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Grant_Patch_Effect - enter");
      end if;

      Ret := True;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Before_Grant_Patch_Effect  - exit Ret=" & Ret'Img);
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Patch_Effect;

   procedure End_Grant_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                              : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A; P_Effect : in Effect.Type_Effect;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Attempt;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Grant_Patch_Effect - enter");
      end if;

      if P_End_Status = Status.Ok then
         P_Attempt_Info := Attempt.Attempt_Done;
      else
         Attempt.Set_Attempt(P_Attempt_Info, Attempt.Get_Attempt (P_Attempt_Info) - 1 );
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Grant_Patch_Effect  - exit");
      end if;

   end End_Grant_Patch_Effect;

   function Validate_Grant_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                  : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area                                         : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect                                       : in Effect.Type_Effect) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Validate_Grant_Patch_Effect - enter - exit");
      end if;

      return True;
   end Validate_Grant_Patch_Effect;

   procedure Before_Grant_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                 : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A; P_Effect : in Effect.Type_Effect;
      P_Result                                      :    out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Grant_Patch_Effect - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Grant_Patch_Effect;

   procedure Directions (P_Effect_Name : in     Effect.Type_Effect_Name; P_Direction : out Integer;
      P_Opposite                       :    out Integer)
   is

      use Effect;
   begin
      if P_Effect_Name = Tubastga_Game.Effect_Wall1 then
         P_Direction := 1;
         P_Opposite  := 4;

      elsif P_Effect_Name = Tubastga_Game.Effect_Wall2 then
         P_Direction := 2;
         P_Opposite  := 5;

      elsif P_Effect_Name = Tubastga_Game.Effect_Wall3 then
         P_Direction := 3;
         P_Opposite  := 6;

      elsif P_Effect_Name = Tubastga_Game.Effect_Wall4 then
         P_Direction := 4;
         P_Opposite  := 1;

      elsif P_Effect_Name = Tubastga_Game.Effect_Wall5 then
         P_Direction := 5;
         P_Opposite  := 2;

      elsif P_Effect_Name = Tubastga_Game.Effect_Wall6 then
         P_Direction := 6;
         P_Opposite  := 3;
      end if;
   end Directions;

   procedure End_Grant_Patch_Effect (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                              : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A; P_Effect : in Effect.Type_Effect;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      type Type_Neighbours is array (1 .. 6) of Hexagon.Area.Type_Hexagon_Delta_Position;

      Neighbours : Type_Neighbours :=
        ((True, +1, 0), (True, +1, -1), (True, 0, -1), (True, -1, 0), (True, -1, +1),
         (True, 0, +1));

      Wall_In_Navigation_Node : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Other_Navigation_Node   : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Other_Pos : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Direction, Opposite     : Integer;

      use Attempt;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Grant_Patch_Effect - enter - exit");
      end if;

      Server.ServerAPI.Player_Activity_Report_Append
        (6, P_Player_Id,
         Utilities.RemoteString.To_Unbounded_String
           ("Place House ===>" & P_Effect.Effect_Name'Img));

      if P_End_Status = Status.Ok then
         Directions (P_Effect.Effect_Name, Direction, Opposite);
         Wall_In_Navigation_Node :=
           Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
             (Hexagon.Server_Navigation.Get_Navigation
                (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
              P_Area (1));

         Other_Pos.A :=
           Hexagon.Type_Hexagon_Numbers
             (Integer (Wall_In_Navigation_Node.all.Pos.A) + Integer (Neighbours (Direction).A));
         Other_Pos.B :=
           Hexagon.Type_Hexagon_Numbers
             (Integer (Wall_In_Navigation_Node.all.Pos.B) + Integer (Neighbours (Direction).B));

         Other_Navigation_Node :=
           Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
             (Hexagon.Server_Navigation.Get_Navigation
                (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
              Other_Pos);

         Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
           (Wall_In_Navigation_Node.all, Other_Navigation_Node.all.Id);

         Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
           (Other_Navigation_Node.all, Wall_In_Navigation_Node.all.Id);

         P_Attempt_Info := Attempt.Attempt_Done;
      else
         Attempt.Set_Attempt(P_Attempt_Info, Attempt.Get_Attempt (P_Attempt_Info) - 1 );
      end if;
   end End_Grant_Patch_Effect;

   function Validate_Revoke_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                   : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area                                          : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name                                   : in Effect.Type_Effect_Name) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Revoke_Patch_Effect - enter - exit");
      end if;

      return True;
   end Validate_Revoke_Patch_Effect;

   procedure Before_Revoke_Patch_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                  : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_Result                                       :    out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Revoke_Patch_Effect - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Patch_Effect;

   procedure End_Revoke_Patch_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                               : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_End_Status : in     Status.Type_Status;
      P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Revoke_Patch_Effect - enter - exit");
      end if;

   end End_Revoke_Patch_Effect;

   function Validate_Revoke_Patch_Effect (P_Player_Id : in Player.Type_Player_Id;
      P_Action_Type                                   : in Action.Type_Action_Type;
      P_Piece : in Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area                                          : in Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name                                   : in Effect.Type_Effect_Name) return Boolean
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Validate_Revoke_Patch_Effect - enter - exit");
      end if;

      return True;
   end Validate_Revoke_Patch_Effect;

   procedure Before_Revoke_Patch_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                                  : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_Result                                       :    out Status.Type_Result_Status)
   is
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Before_Revoke_Patch_Effect - enter - exit");
      end if;

      P_Result := Status.Proceed;
   end Before_Revoke_Patch_Effect;

   procedure End_Revoke_Patch_Effect
     (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                               : in     Action.Type_Action_Type;
      P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_House;
      P_Area : in     Hexagon.Area.Type_Action_Capabilities_A;
      P_Effect_Name : in Effect.Type_Effect_Name;
      P_End_Status : in     Status.Type_Status; P_Attempt_Info : in out Attempt.Type_Attempt_Info)
   is
      type Type_Neighbours is array (1 .. 6) of Hexagon.Area.Type_Hexagon_Delta_Position;

      Neighbours : Type_Neighbours :=
        ((True, +1, 0), (True, +1, -1), (True, 0, -1), (True, -1, 0), (True, -1, +1),
         (True, 0, +1));

      Original_Wall_In_Navigation_Node : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Active_Wall_In_Navigation_Node   : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Original_Other_Navigation_Node   : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Active_Other_Navigation_Node     : Hexagon.Server_Navigation.Type_Navigation_Node_Access;
      Other_Pos : Hexagon.Type_Hexagon_Position := Hexagon.Type_Hexagon_Position'(True, 1, 1);
      Direction, Opposite              : Integer;

      use Attempt;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.End_Revoke_Patch_Effect - enter - exit");
      end if;

      if P_End_Status = Status.Ok then
         Server.ServerAPI.Player_Activity_Report_Append
           (6, P_Player_Id,
            Utilities.RemoteString.To_Unbounded_String
              ("Remove House ===>" & P_Effect_Name'Img));

         Directions (P_Effect_Name, Direction, Opposite);

         Original_Wall_In_Navigation_Node :=
           Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
             (Navigation_Original, P_Area (1));
         Active_Wall_In_Navigation_Node :=
           Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
             (Hexagon.Server_Navigation.Get_Navigation
                (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
              P_Area (1));

         Other_Pos.A :=
           Hexagon.Type_Hexagon_Numbers
             (Integer (Original_Wall_In_Navigation_Node.all.Pos.A) +
              Integer (Neighbours (Direction).A));
         Other_Pos.B :=
           Hexagon.Type_Hexagon_Numbers
             (Integer (Original_Wall_In_Navigation_Node.all.Pos.B) +
              Integer (Neighbours (Direction).B));

         Original_Other_Navigation_Node :=
           Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
             (Navigation_Original, Other_Pos);
         Active_Other_Navigation_Node :=
           Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
             (Hexagon.Server_Navigation.Get_Navigation
                (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
              Other_Pos);

         if Hexagon.Server_Navigation.Has_Neighbour
             (Original_Wall_In_Navigation_Node.all, Original_Other_Navigation_Node.all.Id) then

            Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
              (Active_Wall_In_Navigation_Node.all, Active_Other_Navigation_Node.all.Id);
         else
            Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
              (Active_Wall_In_Navigation_Node.all, Active_Other_Navigation_Node.all.Id);
         end if;

         if Hexagon.Server_Navigation.Has_Neighbour
             (Original_Other_Navigation_Node.all, Original_Wall_In_Navigation_Node.all.Id) then

            Hexagon.Server_Navigation.Modify.Add_Path_To_Neighbour
              (Active_Other_Navigation_Node.all, Active_Wall_In_Navigation_Node.all.Id);
         else
            Hexagon.Server_Navigation.Modify.Remove_Path_To_Neighbour
              (Active_Other_Navigation_Node.all, Active_Wall_In_Navigation_Node.all.Id);
         end if;

         P_Attempt_Info := Attempt.Attempt_Done;
      else
         Attempt.Set_Attempt(P_Attempt_Info, Attempt.Get_Attempt (P_Attempt_Info) - 1 );
      end if;
   end End_Revoke_Patch_Effect;

   procedure Upkeep (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_Piece                : in out Type_My_Tubastga_Piece)
   is
      Tower_Id    : Integer;
      Tower       : Piece.Server.Type_Piece_Access_Class;
      Tower_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      Tower_Pos   : Hexagon.Type_Hexagon_Position;
--
      Load_Goods, Unload_Goods : Goods.Type_Goods;

      Ret_Status : Status.Type_Status;

      Carrying_Goods : Integer;

      use Piece;
      use Goods;
      use Player;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Upkeep - Piece - enter P_Piece.Type_Of_Piece=" &
            P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then

         for Tower_Number_Trav in 1 .. 3 loop
            Tower_Id :=
              Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Stops (P_Piece, Tower_Number_Trav);
            Load_Goods :=
              Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Load (P_Piece, Tower_Number_Trav);
            Unload_Goods :=
              Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Unload (P_Piece, Tower_Number_Trav);

            if Tower_Id /= 99 then
               Tower_Pos :=
                 Piece.Server.Find_Piece_In_List (Piece.Type_Piece_Id (Tower_Id)).Actual_Pos;
               Tower :=
                 Piece.Server.Find_Piece_In_List (Piece.Type_Piece_Id (Tower_Id)).Actual_Piece;
               Tower_Patch :=
                 Hexagon.Server_Map.Get_Patch_Adress_From_AB (Tower_Pos.A, Tower_Pos.B);

               if Hexagon.Server_Map.Are_Neighbours (P_Patch, Tower_Patch.all) then
                  -- We are next to the from tower
                  Tubastga_Game.Server_Logic.Carrier.Carrier_Tower_Transaction
                    (Tubastga_Game.Server_Logic.Type_My_Tubastga_House (Tower.all),
                     Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece (P_Piece), Load_Goods,
                     Unload_Goods);
               end if;
            end if;
         end loop;

         Server.ServerAPI.Observe_Game_Minimum_Details (5);

         for Trav_Opponents in 1 .. 10 loop
            if Server.ServerAPI.Is_Player_In_Scenario (Player.Type_Player_Id (Trav_Opponents)) then

               Tubastga_Game.Server_Logic.Carrier.Carrier_Move
                 (Player.Type_Player_Id (Trav_Opponents), P_Patch, P_Piece);

               Server.ServerAPI.Observe_Game_Minimum_Details (1);

               -- Show what the carrier is carrying
               Carrying_Goods := Goods.Goods_Info_To_Aux (P_Piece.Storage.Slots (1));

               Piece.Server.Grant_Piece_Effect
                 (Player.Type_Player_Id (Trav_Opponents), Action.Type_Action_Type (1),
                  Piece.Server.Type_Piece (P_Piece),
                  Effect.Type_Effect'(Tubastga_Game.Effect_Slot_1, Carrying_Goods), Ret_Status);

            end if;
         end loop;

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Upkeep - Piece - exit");
      end if;
   end Upkeep;

   function Observation_Area
     (P_Piece : in Type_My_Tubastga_House) return Hexagon.Area.Server_Area
     .Type_Action_Capabilities_Access
   is
      Ret : Hexagon.Area.Server_Area.Type_Action_Capabilities_Access;

      use Piece;
      use Hexagon.Area;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Observation_Area - enter " & P_Piece.Type_Of_Piece'Img);
      end if;

      if P_Piece.Type_Of_Piece = Tubastga_Game.Farm_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Lumberjack_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Stonecutter_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1));

      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Tower_House then
         Ret :=
           new Hexagon.Area.Type_Action_Capabilities'
             (Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 1),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, 3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 0, -3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 0),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 3),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, 2),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, 1),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 3, -2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 1, -3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, 2, -3),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, -1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, -2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 1),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -3, 2),

              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -1, 3),
              Hexagon.Area.Type_Hexagon_Delta_Position'(True, -2, 3));

      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Observation_Area - exit");
      end if;

      return Ret;
   end Observation_Area;

   procedure Upkeep (P_Patch : in out Hexagon.Server_Map.Type_Server_Patch;
      P_House                : in out Type_My_Tubastga_House)
   is
      Ret_Status     : Status.Type_Status;
      Carrying_Goods : Integer;
      An_Effect_Name : Effect.Type_Effect_Name;

      use Piece;
      use Goods;
      use Landscape;
      use Effect;
   begin
      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Upkeep - House - enter P_House.Type_Of_Piece=" &
            P_House.Type_Of_Piece'Img);
      end if;

      if P_House.Type_Of_Piece = Tubastga_Game.Tower_House then

         for Trav_Slots in P_House.Storage.Slots'First .. P_House.Storage.Slots'Last loop
            -- Show what the carrier is carrying
            Carrying_Goods := Goods.Goods_Info_To_Aux (P_House.Storage.Slots (Trav_Slots));

            if Trav_Slots = 1 then
               An_Effect_Name := Tubastga_Game.Effect_Slot_1;
            elsif Trav_Slots = 2 then
               An_Effect_Name := Tubastga_Game.Effect_Slot_2;
            elsif Trav_Slots = 3 then
               An_Effect_Name := Tubastga_Game.Effect_Slot_3;
            end if;

            Piece.Server.Grant_Piece_Effect
              (P_House.Player_Id, Action.Type_Action_Type (1), Piece.Server.Type_Piece (P_House),
               Effect.Type_Effect'(An_Effect_Name, Carrying_Goods), Ret_Status);

         end loop;

      end if;

      if P_House.Type_Of_Piece = Tubastga_Game.Farm_House then
         Tubastga_Game.Server_Logic.House_Piece.Farm_House_Production (P_Patch, P_House);
      end if;

      if P_House.Type_Of_Piece = Tubastga_Game.Lumberjack_House then
         Tubastga_Game.Server_Logic.House_Piece.Lumberjack_House_Production (P_Patch, P_House);
      end if;

      if P_House.Type_Of_Piece = Tubastga_Game.Stonecutter_House then
         Tubastga_Game.Server_Logic.House_Piece.Stonecutter_House_Production (P_Patch, P_House);
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Upkeep - House - exit");
      end if;
   end Upkeep;

   function Can_Load (P_Piece : in Type_My_Tubastga_Piece;
      P_Goods                 : in Goods.Type_Goods_Info) return Boolean
   is
   begin
      return True;
   end Can_Load;

   procedure Tubastga_Creating_Game (P_Map_Name : in Utilities.RemoteString.Type_String;
      P_Scenario_Name                           : in Utilities.RemoteString.Type_String)
   is
      use Lua;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Creating_Game - enter");
      end if;

      Current_Scenario := P_Scenario_Name;

      if Verbose then
         Text_IO.Put_Line
           ("Tubastga_Game.Server_Logic.Tubastga_Creating_Game - exit Current_Scenario=" &
            Utilities.RemoteString.To_String (Current_Scenario));
      end if;
   end Tubastga_Creating_Game;

   procedure Tubastga_Saving_Game (P_Map_Name : in Utilities.RemoteString.Type_String;
      P_Scenario_Name                         : in Utilities.RemoteString.Type_String)
   is
      Trav_Piece : Piece.Server.Pieces_Server_List.Cursor;

      A_Piece   : Piece.Server.Type_Piece_Access_Class;
      A_Storage : Goods.Type_Storage_Access;

      Save_File   : Ada.Streams.Stream_IO.File_Type;
      Save_Stream : Ada.Streams.Stream_IO.Stream_Access;

      use Piece;
      use Goods;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Saving_Game - enter");
      end if;

      Ada.Streams.Stream_IO.Create
        (Save_File, Ada.Streams.Stream_IO.Out_File,
         "saved\tubastga\" & Utilities.RemoteString.To_String (P_Map_Name));
      Save_Stream := Ada.Streams.Stream_IO.Stream (Save_File);

      Utilities.RemoteString.Type_String'Write (Save_Stream, Current_Scenario);

      Trav_Piece := Piece.Server.Pieces_Server_List.First (Piece.Server.All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav_Piece) loop
         A_Piece := Piece.Server.Pieces_Server_List.Element (Trav_Piece).Actual_Piece;

         if A_Piece.all.Category = Piece.Fighting_Piece then

            if Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece (A_Piece.all).Storage /= null then
               A_Storage := Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece (A_Piece.all).Storage;
               Piece.Type_Piece_Id'Write (Save_Stream, A_Piece.all.Id);
               Goods.Type_Storage'Write (Save_Stream, A_Storage.all);
            end if;
         elsif A_Piece.all.Category = Piece.House_Piece then
            if Tubastga_Game.Server_Logic.Type_My_Tubastga_House (A_Piece.all).Storage /= null then
               A_Storage := Tubastga_Game.Server_Logic.Type_My_Tubastga_House (A_Piece.all).Storage;
               Piece.Type_Piece_Id'Write (Save_Stream, A_Piece.all.Id);
               Goods.Type_Storage'Write (Save_Stream, A_Storage.all);
            end if;
         end if;

         Trav_Piece := Piece.Server.Pieces_Server_List.Next (Trav_Piece);
      end loop;

      Piece.Type_Piece_Id'Write (Save_Stream, Piece.Undefined_Piece_Id);

      Tubastga_Game.Server_Logic.Carrier.Save_Workers_Path (Save_Stream);

      Ada.Streams.Stream_IO.Close (Save_File);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Saving_Game - exit");
      end if;
   end Tubastga_Saving_Game;

   procedure Tubastga_Loading_Game (P_Map_Name : in Utilities.RemoteString.Type_String;
      P_Scenario_Name                          : in Utilities.RemoteString.Type_String)
   is

      A_Piece    : Piece.Server.Type_Piece_Access_Class;
      A_Piece_Id : Piece.Type_Piece_Id;
      A_Storage  : Goods.Type_Storage_Access;

      Load_File   : Ada.Streams.Stream_IO.File_Type;
      Load_Stream : Ada.Streams.Stream_IO.Stream_Access;

      use Piece;
      use Goods;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Loading_Game - enter");
      end if;

      Ada.Streams.Stream_IO.Open
        (Load_File, Ada.Streams.Stream_IO.In_File,
         "saved\tubastga\" & Utilities.RemoteString.To_String (P_Map_Name));
      Load_Stream := Ada.Streams.Stream_IO.Stream (Load_File);

      Utilities.RemoteString.Type_String'Read (Load_Stream, Current_Scenario);

      Piece.Type_Piece_Id'Read (Load_Stream, A_Piece_Id);
      while A_Piece_Id /= Piece.Undefined_Piece_Id loop

         A_Piece := Piece.Server.Find_Piece_In_List (A_Piece_Id).Actual_Piece;
         if A_Piece.all.Category = Piece.Fighting_Piece then
            A_Storage := new Goods.Type_Storage (1);
            Goods.Type_Storage'Read (Load_Stream, A_Storage.all);
            Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece (A_Piece.all).Storage := A_Storage;

         elsif A_Piece.all.Category = Piece.House_Piece then
            A_Storage := new Goods.Type_Storage (3);
            Goods.Type_Storage'Read (Load_Stream, A_Storage.all);
            Tubastga_Game.Server_Logic.Type_My_Tubastga_House (A_Piece.all).Storage := A_Storage;
         end if;
         Piece.Type_Piece_Id'Read (Load_Stream, A_Piece_Id);
      end loop;

      Tubastga_Game.Server_Logic.Carrier.Load_Workers_Path (Load_Stream);

      Ada.Streams.Stream_IO.Close (Load_File);

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Loading_Game - exit");
      end if;
   end Tubastga_Loading_Game;

   procedure Print_Stack (P_Lua_State : in Lua.Lua_State) is
      Max_Index, N : Lua.Lua_Index;
   begin
      Text_IO.Put_Line ("Stack:");
      Max_Index := Lua.Get_Top (P_Lua_State);
      N         := 1;
      while N <= Max_Index loop

         Text_IO.Put_Line ("Index " & N'Img & " " & Lua.Get_Type (P_Lua_State, N)'Img);
         N := N + 1;
      end loop;
   end Print_Stack;

   procedure Tubastga_Joining_Game is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Joining_Game -enter - exit");
      end if;

   end Tubastga_Joining_Game;

   procedure Tubastga_Leaving_Game is
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Leaving_Game -enter - exit");
      end if;

   end Tubastga_Leaving_Game;

   procedure Tubastga_Start_Game is
      A_Piece    : Piece.Type_Piece;
      Ret_Status : Status.Type_Status;

      A_Pos_Blue1 : Hexagon.Type_Hexagon_Position;
      A_Pos_Blue2 : Hexagon.Type_Hexagon_Position;
      A_Pos_Blue3 : Hexagon.Type_Hexagon_Position;

      A_Pos_Red2   : Hexagon.Type_Hexagon_Position;
      A_Pos_Red1   : Hexagon.Type_Hexagon_Position;
      A_Pos_Green1 : Hexagon.Type_Hexagon_Position;
      A_Pos_Worker : Hexagon.Type_Hexagon_Position;

      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;

      Worker_Path : Hexagon.Server_Navigation.Type_Path;

      Lua_Status : Lua.Lua_Return_Code;

      use Lua;
      use Status;
      use Utilities.RemoteString;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Start_Game - enter ");
      end if;

      Lua.Load_File (Tubastga_Game.Server_Logic.Lua_State, "lua\tubastga.lua");
      Lua_Status := Lua.PCall (Tubastga_Game.Server_Logic.Lua_State, 0, 0, 0);
      if Lua_Status /= Lua.LUA_OK then
         --  An error occurs during the execution
         Text_IO.Put_Line (Lua_Status'Img);
         Text_IO.Put_Line (To_Ada (Lua_State, -1));
      end if;

      if Current_Scenario = "demo_1" then

         Energy_Update := Ada.Real_Time.Clock;

         Lua.Get_Global (Tubastga_Game.Server_Logic.Lua_State, "Tubastga");
         Lua.Get_Field (Tubastga_Game.Server_Logic.Lua_State, -1, "foundTreasure");
         Lua.Push (Tubastga_Game.Server_Logic.Lua_State, Lua.Lua_Integer (3));
         Lua.Push (Tubastga_Game.Server_Logic.Lua_State, Lua.Lua_Integer (4));
         Lua_Status := Lua.PCall (Tubastga_Game.Server_Logic.Lua_State, 2, 0, 0);
         if Lua_Status /= Lua.LUA_OK then
            --  An error occurs during the execution
            Text_IO.Put_Line (Lua_Status'Img);
            Text_IO.Put_Line (To_Ada (Lua_State, -1));
         end if;

         --

         declare
            Server_Info : Utilities.RemoteString_List.Vector;
         begin
            Server.ServerAPI.Get_Server_Info (Server_Info);

            Utilities.RemoteString_List.Append
              (Server_Info, Utilities.RemoteString.To_Unbounded_String ("Change Server: 01"));

            Server.ServerAPI.Set_Server_Info (Server_Info);

         end;

         Text_IO.Put_Line ("Server is running the scenario : demo_1.dat");

         A_Pos_Blue1 := Hexagon.Type_Hexagon_Position'(True, 16, 20);
         A_Pos_Blue2 := Hexagon.Type_Hexagon_Position'(True, 17, 20);

         A_Pos_Blue3 := Hexagon.Type_Hexagon_Position'(True, 13, 17);

         A_Pos_Red1 := Hexagon.Type_Hexagon_Position'(True, 19, 20);
         A_Pos_Red2 := Hexagon.Type_Hexagon_Position'(True, 20, 20);

         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (18, 21);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 1821));
         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (19, 22);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 1922));

         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (19, 23);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 0));

         -- Place one tower for both players
         -- Give each player stone, wood and money

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue1, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue3, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue2, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red1, A_Piece, A_Piece.Id,
            Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red2, A_Piece, A_Piece.Id,
            Ret_Status, True);

         Server.ServerAPI.Observe_Game (5);

         A_Piece.Type_Of_Piece := Tubastga_Game.Carrier_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 1;

         -- Path / Worker
         A_Pos_Worker := Hexagon.Type_Hexagon_Position'(True, 15, 19);
         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Worker, A_Piece,
            A_Piece.Id, Ret_Status, True);

         Hexagon.Server_Navigation.Path_Pkg.Append
           (Worker_Path.This_Path,
            Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
              (Hexagon.Server_Navigation.Get_Navigation
                 (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
               Hexagon.Type_Hexagon_Position'(True, 16, 19)));

         Hexagon.Server_Navigation.Path_Pkg.Append
           (Worker_Path.This_Path,
            Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
              (Hexagon.Server_Navigation.Get_Navigation
                 (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
               Hexagon.Type_Hexagon_Position'(True, 15, 19)));
         Hexagon.Server_Navigation.Path_Pkg.Append
           (Worker_Path.This_Path,
            Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
              (Hexagon.Server_Navigation.Get_Navigation
                 (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
               Hexagon.Type_Hexagon_Position'(True, 15, 18)));
         Hexagon.Server_Navigation.Path_Pkg.Append
           (Worker_Path.This_Path,
            Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
              (Hexagon.Server_Navigation.Get_Navigation
                 (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
               Hexagon.Type_Hexagon_Position'(True, 14, 18)));
         Hexagon.Server_Navigation.Path_Pkg.Append
           (Worker_Path.This_Path,
            Hexagon.Server_Navigation.Get_Navigation_Node_By_Position
              (Hexagon.Server_Navigation.Get_Navigation
                 (Hexagon.Server_Navigation.A_Navigation_List, 1).all,
               Hexagon.Type_Hexagon_Position'(True, 13, 18)));

         Carrier_Paths_List.Insert (All_Paths, A_Piece.Id, Worker_Path);

         -- Init storage
         declare
            Tower_1, Tower_2 : Piece.Server.Type_Piece_Access_Class;
            Carrier          : Piece.Server.Type_Piece_Access_Class;
            Storage_Ret      : Boolean;
            The_Goods        : Goods.Type_Goods_Info;
         begin
            Tower_1 := Piece.Server.Find_Piece_In_List (1).Actual_Piece;
            Tower_2 := Piece.Server.Find_Piece_In_List (2).Actual_Piece;
            Carrier := Piece.Server.Find_Piece_In_List (6).Actual_Piece;

            Effect.Effect_List.Include
              (Carrier.all.Effects_On_Piece, Tubastga_Game.Effect_Stops,
               Effect.Type_Effect'
                 (Tubastga_Game.Effect_Stops,
                  Tubastga_Game.Carrier.Get_Tower_Code
                    (Tower_1.all.Id, Tower_2.all.Id, Piece.Undefined_Piece_Id)));

            Effect.Effect_List.Include
              (Carrier.all.Effects_On_Piece, Tubastga_Game.Effect_Load,
               Effect.Type_Effect'
                 (Tubastga_Game.Effect_Load,
                  Tubastga_Game.Carrier.Get_Tower_Goods_Code
                    (Goods.Stone, Goods.Wood, Goods.None)));

            Effect.Effect_List.Include
              (Carrier.all.Effects_On_Piece, Tubastga_Game.Effect_Unload,
               Effect.Type_Effect'
                 (Tubastga_Game.Effect_Unload,
                  Tubastga_Game.Carrier.Get_Tower_Goods_Code
                    (Goods.Wood, Goods.Stone, Goods.None)));

            The_Goods := Goods.Type_Goods_Info'(Goods.Stone, 10);
            Goods.Into_Storage
              (Tubastga_Game.Server_Logic.Type_My_Tubastga_House (Tower_1.all).Storage.all,
               The_Goods, Storage_Ret);
            The_Goods := Goods.Type_Goods_Info'(Goods.Wood, 20);
            Goods.Into_Storage
              (Tubastga_Game.Server_Logic.Type_My_Tubastga_House (Tower_1.all).Storage.all,
               The_Goods, Storage_Ret);

            The_Goods := Goods.Type_Goods_Info'(Goods.Stone, 30);
            Goods.Into_Storage
              (Tubastga_Game.Server_Logic.Type_My_Tubastga_House (Tower_2.all).Storage.all,
               The_Goods, Storage_Ret);
            The_Goods := Goods.Type_Goods_Info'(Goods.Wood, 40);
            Goods.Into_Storage
              (Tubastga_Game.Server_Logic.Type_My_Tubastga_House (Tower_2.all).Storage.all,
               The_Goods, Storage_Ret);

         end;

      elsif Current_Scenario = "scenario_1" then
         Text_IO.Put_Line ("Server is running the scenario : scenario_1.dat");

         Server.ServerAPI.Player_Activity_Report_Append
           (6, 1, Utilities.RemoteString.To_Unbounded_String ("Welcome to Tubast'ga"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 1,
            Utilities.RemoteString.To_Unbounded_String
              ("You have landed on the South part of the world fraction Arka"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 1,
            Utilities.RemoteString.To_Unbounded_String
              ("The scouts have mapped Arka - and found an enemy in the North part."));
         --
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 2, Utilities.RemoteString.To_Unbounded_String ("Welcome to Tubast'ga"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 2,
            Utilities.RemoteString.To_Unbounded_String
              ("You have landed on the North part of the world fraction Arka"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 2,
            Utilities.RemoteString.To_Unbounded_String
              ("The scouts have mapped Arka - and found an enemy in the South part."));

         A_Pos_Blue1 := Hexagon.Type_Hexagon_Position'(True, 16, 20);
         A_Pos_Blue2 := Hexagon.Type_Hexagon_Position'(True, 17, 20);

         A_Pos_Red1 := Hexagon.Type_Hexagon_Position'(True, 73, 87);
         A_Pos_Red2 := Hexagon.Type_Hexagon_Position'(True, 74, 87);

         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (18, 21);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 1821));
         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (19, 22);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 0));

         -- Construction
         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (19, 23);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 0));

         -- Place one tower for both players
         -- Give each player stone, wood and money

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue1, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue2, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red1, A_Piece, A_Piece.Id,
            Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red2, A_Piece, A_Piece.Id,
            Ret_Status, True);

         Server.ServerAPI.Observe_Game (5);

      elsif Current_Scenario = "scenario_3player" then
         Text_IO.Put_Line ("Server is running the scenario : scenario_3player.dat");

         A_Pos_Green1 := Hexagon.Type_Hexagon_Position'(True, 30, 30);

         Server.ServerAPI.Player_Activity_Report_Append
           (6, 1, Utilities.RemoteString.To_Unbounded_String ("Welcome to Tubast'ga (3 players)"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 1,
            Utilities.RemoteString.To_Unbounded_String
              ("You have landed on the South part of the world fraction Arka"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 1,
            Utilities.RemoteString.To_Unbounded_String
              ("The scouts have mapped Arka - and found an enemy in the North part."));
         --
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 2, Utilities.RemoteString.To_Unbounded_String ("Welcome to Tubast'ga"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 2,
            Utilities.RemoteString.To_Unbounded_String
              ("You have landed on the North part of the world fraction Arka"));
         Server.ServerAPI.Player_Activity_Report_Append
           (6, 2,
            Utilities.RemoteString.To_Unbounded_String
              ("The scouts have mapped Arka - and found an enemy in the South part."));

         A_Pos_Blue1 := Hexagon.Type_Hexagon_Position'(True, 16, 20);
         A_Pos_Blue2 := Hexagon.Type_Hexagon_Position'(True, 17, 20);

         A_Pos_Red1 := Hexagon.Type_Hexagon_Position'(True, 73, 87);
         A_Pos_Red2 := Hexagon.Type_Hexagon_Position'(True, 74, 87);

         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (18, 21);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 1821));
         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (19, 22);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 0));

         -- Construction
         A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (19, 23);
         Effect.Effect_List.Include
           (A_Patch.all.Effects_Here, Tubastga_Game.Effect_Treasure,
            Effect.Type_Effect'(Tubastga_Game.Effect_Treasure, 0));

         -- Place one tower for both players
         -- Give each player stone, wood and money

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue1, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue2, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red1, A_Piece, A_Piece.Id,
            Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red2, A_Piece, A_Piece.Id,
            Ret_Status, True);

         --
         Text_IO.Put_Line ("3dje spiller start");
         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 3;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (3), Action.Type_Action_Type (1), A_Pos_Green1, A_Piece,
            A_Piece.Id, Ret_Status, True);
         Text_IO.Put_Line ("3dje spiller slutt");

         Server.ServerAPI.Observe_Game (5);

         if Ret_Status = Status.Ok then
            A_Piece.Id := 2;
            Server.ServerAPI.Perform_Move
              (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Piece.Id, A_Pos_Blue1,
               Ret_Status);
         end if;

         Server.ServerAPI.Observe_Game (5);

      elsif Current_Scenario = "scenario_battle" then
         Text_IO.Put_Line ("Server is running the scenario : scenario_battle.dat");

         A_Pos_Blue1 := Hexagon.Type_Hexagon_Position'(True, 16, 20);
         A_Pos_Blue2 := Hexagon.Type_Hexagon_Position'(True, 17, 20);
         A_Pos_Blue3 := Hexagon.Type_Hexagon_Position'(True, 17, 19);


         A_Pos_Red1 := Hexagon.Type_Hexagon_Position'(True, 73, 87);
         A_Pos_Red2 := Hexagon.Type_Hexagon_Position'(True, 18, 21);


         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 1;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue1, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 1;

        Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue2, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Bowman_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 1;

        Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (1), Action.Type_Action_Type (1), A_Pos_Blue3, A_Piece,
            A_Piece.Id, Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Tower_House;
         A_Piece.Category      := Piece.House_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red1, A_Piece, A_Piece.Id,
            Ret_Status, True);

         A_Piece.Type_Of_Piece := Tubastga_Game.Sentry_Piece;
         A_Piece.Category      := Piece.Fighting_Piece;
         A_Piece.Player_Id     := 2;

         Server.ServerAPI.Create_Piece
           (Player.Type_Player_Id (2), Action.Type_Action_Type (1), A_Pos_Red2, A_Piece, A_Piece.Id,
            Ret_Status, True);

         --

         Server.ServerAPI.Observe_Game (5);
      end if; --  Scenario dependent logic

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Start_Game - exit");
      end if;
   end Tubastga_Start_Game;

   procedure Tubastga_Upkeep_Game is
      Trav_All_Pieces  : Piece.Server.Pieces_Server_List.Cursor;
      A_Fighting_Piece : Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece_Access_Class := null;
      A_Piece_Position : Piece.Server.Type_Piece_Position;

      use Piece;
      use Hexagon.Server_Map;
      use Utilities.RemoteString;
      use Ada.Real_Time;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Upkeep_Game - enter");
      end if;

      for Player_Id in 1 .. 2 loop
         Tubastga_Game.Server_Logic.Carrier.Remove_Workers_Path (Player.Type_Player_Id (Player_Id));

         Tubastga_Game.Server_Logic.Carrier.Create_Workers_Path (Player.Type_Player_Id (Player_Id));
      end loop;

      if Energy_Update < Ada.Real_Time.Clock then
         Energy_Update := Ada.Real_Time.Clock + Ada.Real_Time.To_Time_Span (5.0);

         Trav_All_Pieces := Piece.Server.Pieces_Server_List.First (Piece.Server.All_Pieces_In_Game);
         while Piece.Server.Pieces_Server_List.Has_Element (Trav_All_Pieces) loop
            A_Piece_Position := Piece.Server.Pieces_Server_List.Element (Trav_All_Pieces);

            if A_Piece_Position.Actual_Piece.all.Category = Piece.Fighting_Piece then
               A_Fighting_Piece :=
                 Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece_Access_Class
                   (A_Piece_Position.Actual_Piece);

               if A_Fighting_Piece.all.Category = Piece.Fighting_Piece then
                  Tubastga_Game.Server_Logic.Move_Logic.Rest_Energy
                    (Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece (A_Fighting_Piece.all));
               end if;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1, A_Piece_Position.Actual_Piece.all.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (A_Fighting_Piece.all.Name) & " has ") &
                  A_Fighting_Piece.all.Energy'Img);
            elsif A_Piece_Position.Actual_Piece.all.Category = Piece.House_Piece then
               --            Tubastga_Game.Server_Logic.Type_My_Tubastga_House(A_Piece_Position.Actual_Piece.all).Energy := 101;
               null;
            end if;

            Trav_All_Pieces := Piece.Server.Pieces_Server_List.Next (Trav_All_Pieces);
         end loop;
      end if;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_Upkeep_Game - exit");
      end if;

   end Tubastga_Upkeep_Game;

   procedure Tubastga_End_Game (P_Game_Status : out Status.Type_Game_Status) is
      A_Patch : Hexagon.Server_Map.Type_Server_Patch_Adress;
      A_Pos   : Hexagon.Type_Hexagon_Position;

      Trav_All_Pieces                   : Piece.Server.Pieces_Server_List.Cursor;
      A_Piece                           : Piece.Server.Type_Piece_Access_Class := null;
      Enemy_Neighbours, Neighbour_Tiles : Integer;

      Winning_Player_Id, Loosing_Player_Id : Player.Type_Player_Id;
      use Piece;
      use Status;
   begin
      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_End_Game - enter");
      end if;

      P_Game_Status   := Status.Playing;
      Trav_All_Pieces := Piece.Server.Pieces_Server_List.First (Piece.Server.All_Pieces_In_Game);
      while Piece.Server.Pieces_Server_List.Has_Element (Trav_All_Pieces) and
        P_Game_Status = Status.Playing
      loop

         A_Piece := Piece.Server.Pieces_Server_List.Element (Trav_All_Pieces).Actual_Piece;
         A_Pos   := Piece.Server.Pieces_Server_List.Element (Trav_All_Pieces).Actual_Pos;

         if A_Piece.all.Type_Of_Piece = Tower_House then
            -- check tiles nearby. if they are occupied by enemy the player owning the tower has
            --lost.

            Neighbour_Tiles  := 0;
            Enemy_Neighbours := 0;

            for Trav in Win_Pattern'First .. Win_Pattern'Last loop
               declare
                  A, B : Hexagon.Type_Hexagon_Numbers;
               begin
                  A :=
                    Hexagon.Type_Hexagon_Numbers
                      (Integer (A_Pos.A) + Integer (Win_Pattern.all (Trav).A));
                  B :=
                    Hexagon.Type_Hexagon_Numbers
                      (Integer (A_Pos.B) + Integer (Win_Pattern.all (Trav).B));
                  A_Patch := Hexagon.Server_Map.Get_Patch_Adress_From_AB (A, B);

                  Neighbour_Tiles := Neighbour_Tiles + 1;

                  if not Piece.Server.Patch_Belongs_To_Player
                      (Landscape.Type_Patch (A_Patch.all), A_Piece.Player_Id) then
                     Enemy_Neighbours  := Enemy_Neighbours + 1;
                     Winning_Player_Id :=
                       Piece.Server.Get_Pieces_Players (Landscape.Type_Patch (A_Patch.all));
                     Loosing_Player_Id := A_Piece.Player_Id;
                  end if;

               exception
                  when others =>
                     null;
               end;
            end loop;

            if Enemy_Neighbours = Neighbour_Tiles and Neighbour_Tiles /= 0 then
               P_Game_Status := Status.End_Of_Game;

               Server.ServerAPI.Player_System_Report_Append
                 (Observation.Activity.Internal_Details, Winning_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String
                       (Server.ServerAPI.Get_Player_Name (Winning_Player_Id)) &
                     " won!"));
               Server.ServerAPI.Player_System_Report_Append
                 (Observation.Activity.Internal_Details, Winning_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String
                       (Server.ServerAPI.Get_Player_Name (Loosing_Player_Id)) &
                     " lost!"));

               Server.ServerAPI.Player_System_Report_Append
                 (Observation.Activity.Internal_Details, Loosing_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String
                       (Server.ServerAPI.Get_Player_Name (Winning_Player_Id)) &
                     " won!"));
               Server.ServerAPI.Player_System_Report_Append
                 (Observation.Activity.Internal_Details, Loosing_Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String
                       (Server.ServerAPI.Get_Player_Name (Loosing_Player_Id)) &
                     " lost!"));
            end if;

         end if;

         Trav_All_Pieces := Piece.Server.Pieces_Server_List.Next (Trav_All_Pieces);
      end loop;

      if Verbose then
         Text_IO.Put_Line ("Tubastga_Game.Server_Logic.Tubastga_End_Game - exit");
      end if;
   end Tubastga_End_Game;

   function Roll
     (P_Attack_Defence : in Tubastga_Game.Server_Logic.Type_Attack_Defence_Record)
      return Type_Attack_Defence_Types
   is
      Random_Number : Positive;
      Ret           : Type_Attack_Defence_Types;
   begin
      -- 1 to P_Attack_Defence.Attack gives attacker the win
      -- (P_Attack_Defence.Attack + 1) to (P_Attack_Defence.Attack + P_Attack_Defence.Defence)

      Random_Number := Random.Random (RandomGen);

      if Random_Number in 1 .. (P_Attack_Defence.Attack) then
         Ret := Attacking_Wins;
      elsif Random_Number in
          (P_Attack_Defence.Attack + 1) .. (P_Attack_Defence.Attack + P_Attack_Defence.Defence) then
         Ret := Attacked_Wins;
      end if;

      Ret := Attacked_Wins;

      return Ret;
   end Roll;

   function Movement_Cost (P_Player_Id : in     Player.Type_Player_Id;
      P_Action_Type                    : in     Action.Type_Action_Type;
      P_Piece                          : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_From_Patch                     : in out Landscape.Type_Patch;
      P_To_Patch                       : in out Landscape.Type_Patch) return Integer
   is
   begin
      return 1;
   end Movement_Cost;

begin
   Random.Reset (RandomGen, 1);
end Tubastga_Game.Server_Logic;
