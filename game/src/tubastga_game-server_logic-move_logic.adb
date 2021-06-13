--
--
--      Tubastga Game
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

with Server.ServerAPI;
with Text_IO;

package body Tubastga_Game.Server_Logic.Move_Logic is

   procedure Move_Energy
     (P_Piece       : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece;
      P_To_Patch    : in     Landscape.Type_Patch;
      P_Move_Status :    out Boolean)
   is
      use Landscape;
      use Piece;
   begin
      if P_To_Patch.Landscape_Here = Tubastga_Game.Landscape_Grass then
         if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
            if P_Piece.Energy >= 12 then
               P_Piece.Energy := P_Piece.Energy - 4;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
            if P_Piece.Energy >= 22 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
            if P_Piece.Energy >= 12 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
            if P_Piece.Energy >= 7 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;
         end if;

      elsif P_To_Patch.Landscape_Here = Tubastga_Game.Landscape_Forest then
         if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
            if P_Piece.Energy >= 12 then
               P_Piece.Energy := P_Piece.Energy - 4;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
            if P_Piece.Energy >= 22 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
            if P_Piece.Energy >= 12 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
            if P_Piece.Energy >= 7 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;
         end if;
      elsif P_To_Patch.Landscape_Here = Tubastga_Game.Landscape_Mountain then
         if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
            if P_Piece.Energy >= 12 then
               P_Piece.Energy := P_Piece.Energy - 4;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
            if P_Piece.Energy >= 22 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
            if P_Piece.Energy >= 12 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;

         elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
            if P_Piece.Energy >= 7 then
               P_Piece.Energy := P_Piece.Energy - 2;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;
         end if;
      elsif P_To_Patch.Landscape_Here = Tubastga_Game.Landscape_Water then
         if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
            if P_Piece.Energy >= 12 then
               P_Piece.Energy := P_Piece.Energy - 4;
               Server.ServerAPI.Player_Activity_Report_Append
                 (1,
                  P_Piece.Player_Id,
                  Utilities.RemoteString.To_Unbounded_String
                    (Utilities.RemoteString.To_String (P_Piece.Name) &
                     " walks from " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     " to " &
                     P_To_Patch.Pos.A'Img &
                     ", " &
                     P_To_Patch.Pos.B'Img &
                     ". He has " &
                     P_Piece.Energy'Img &
                     " energy left"));
               P_Move_Status := True;
            else
               P_Move_Status := False;
            end if;
         end if;

      else
         null;
         Text_IO.Put_Line
           ("P_To_Patch A=" &
            P_To_Patch.Pos.A'Img &
            " B=" &
            P_To_Patch.Pos.B'Img &
            " Move energy P_To_Patch.Landscape_Here=" &
            P_To_Patch.Landscape_Here'Img);
      end if;

   end Move_Energy;

   procedure Rest_Energy (P_Piece : in out Tubastga_Game.Server_Logic.Type_My_Tubastga_Piece) is
      use Piece;
   begin
      if P_Piece.Type_Of_Piece = Tubastga_Game.Sentry_Piece then
         P_Piece.Energy := P_Piece.Energy + 6;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Knight_Piece then
         P_Piece.Energy := P_Piece.Energy + 9;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Bowman_Piece then
         P_Piece.Energy := P_Piece.Energy + 11;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Ship_Piece then
         P_Piece.Energy := P_Piece.Energy + 5;
      elsif P_Piece.Type_Of_Piece = Tubastga_Game.Carrier_Piece then
         P_Piece.Energy := P_Piece.Energy + 5;
      end if;

   end Rest_Energy;
end Tubastga_Game.Server_Logic.Move_Logic;
