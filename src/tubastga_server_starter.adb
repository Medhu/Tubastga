--
--
--      Tubastga Game - A Turn based strategy game. This is the main starter for server
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

with Ada.Command_Line;
with Ada.Strings.Fixed;
with Ada.Strings.Maps.Constants;
with Utilities;
with Text_IO;
with Tubastga_ServerRCI;
with Tubastga_Piece.Server_Logic;
with Piece;
with Server.ServerAPI;
with Server.Lua_Interface;
with Lua;
with Player;

procedure Tubastga_Server_Starter is

   Command_Line  : Utilities.RemoteString.Type_Command_Parameters;

   Tubastga_Class1 : Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece_Access_Class;
   Tubastga_Class2 : Tubastga_Piece.Server_Logic.Type_My_Tubastga_House_Access_Class;

begin
   Text_IO.Put_Line ("Tubast'ga - v0.3. Copyright (C) 2015-2016  Frank J Jorgensen");
   Text_IO.Put_Line
     ("This program comes with ABSOLUTELY NO WARRANTY; for details see attached gpl.txt");
   Text_IO.Put_Line ("or <http://www.gnu.org/licenses/>");
   Text_IO.Put_Line ("This is free software, and you are welcome to redistribute it");
   Text_IO.Put_Line ("under certain conditions; see attached file gpl.txt");
   Text_IO.Put_Line ("or <http://www.gnu.org/licenses/>");
   Text_IO.New_Line;
   Text_IO.New_Line;

   for Arguments_Index in 1 .. Ada.Command_Line.Argument_Count loop

      if Ada.Command_Line.Argument (Arguments_Index)'Length > 3
        and then Ada.Strings.Fixed.Translate
                    (Ada.Command_Line.Argument (Arguments_Index) (1 .. 3),
                     Ada.Strings.Maps.Constants.Lower_Case_Map) =
                 "ip:"
      then
         Command_Line (1) :=
            Utilities.RemoteString.To_Unbounded_String
              (Ada.Command_Line.Argument (Arguments_Index) (
           4 .. Ada.Command_Line.Argument (Arguments_Index)'Length));
      end if;

      if Ada.Command_Line.Argument (Arguments_Index)'Length > 5
        and then Ada.Strings.Fixed.Translate
                    (Ada.Command_Line.Argument (Arguments_Index) (1 .. 5),
                     Ada.Strings.Maps.Constants.Lower_Case_Map) =
                 "port:"
      then
         Command_Line (2) :=
            Utilities.RemoteString.To_Unbounded_String
              (Ada.Command_Line.Argument (Arguments_Index) (
           6 .. Ada.Command_Line.Argument (Arguments_Index)'Length));

      end if;

   end loop;

   Text_IO.New_Line;
   Text_IO.New_Line;

   delay 0.0;

   Tubastga_ServerRCI.Init (Command_Line);

   Tubastga_Class1               := new Tubastga_Piece.Server_Logic.Type_My_Tubastga_Piece;
   Tubastga_Class1.Id            := Piece.Undefined_Piece_Id;
   Tubastga_Class1.Type_Of_Piece := Piece.Undefined_Piece_Type;
   Tubastga_Class1.Player_Id     := Player.Undefined_Player_Id;

   Tubastga_Class2               := new Tubastga_Piece.Server_Logic.Type_My_Tubastga_House;
   Tubastga_Class2.Id            := Piece.Undefined_Piece_Id;
   Tubastga_Class2.Type_Of_Piece := Piece.Undefined_Piece_Type;
   Tubastga_Class2.Player_Id     := Player.Undefined_Player_Id;

   Server.ServerAPI.Init
     (Tubastga_Class1.all,
      Tubastga_Class2.all,
      --
      Tubastga_Piece.Landscapes_Type_Info_List,
      Tubastga_Piece.Pieces_Type_Info_List,
      Tubastga_Piece.Houses_Type_Info_List,
      Tubastga_Piece.Construction_Type_Info_List,
      Tubastga_Piece.Effect_Type_Info_List,
      --
      Tubastga_Piece.Server_Logic.Tubastga_Creating_Game'Access,
      Tubastga_Piece.Server_Logic.Tubastga_Saving_Game'Access,
      Tubastga_Piece.Server_Logic.Tubastga_Loading_Game'Access,
      --
      Tubastga_Piece.Server_Logic.Tubastga_Joining_Game'Access,
      Tubastga_Piece.Server_Logic.Tubastga_Leaving_Game'Access,
      --
      Tubastga_Piece.Server_Logic.Tubastga_Start_Game'Access,
      Tubastga_Piece.Server_Logic.Tubastga_Upkeep_Game'Access,
      Tubastga_Piece.Server_Logic.Tubastga_End_Game'Access);

   Tubastga_Piece.Server_Logic.Lua_State := Lua.New_State;
   Lua.Open_Libs (Tubastga_Piece.Server_Logic.Lua_State);
   Server.Lua_Interface.Init(Tubastga_Piece.Server_Logic.Lua_State);

   Server.ServerAPI.Start;

   Server.ServerAPI.Run;

   Server.ServerAPI.Stop;

end Tubastga_Server_Starter;
