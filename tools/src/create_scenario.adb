
with Utilities;
with Server.Server.Archive;
with Hexagon.Server_Map;

procedure create_scenario is
   A_Directory     : Utilities.RemoteString.Type_String;
   A_File_Name     : Utilities.RemoteString.Type_String;
   A_Scenario_Name : Utilities.RemoteString.Type_String;
   A_Player_List   : Server.Server.Type_Player_List;
   A_Countdown     : Positive;
begin 
   A_Directory := Utilities.RemoteString.To_Unbounded_String("scenarios\");
   A_File_Name := Utilities.RemoteString.To_Unbounded_String("scenario_battle.dat");
   Hexagon.Server_Map.Init(Hexagon.Server_Map.A_Map);
   
   Server.Server.Archive.Loading_Game
     (A_Directory,
      A_File_Name,
      A_Scenario_Name,
      A_Player_List,
      A_Countdown);
   
   A_Scenario_Name := Utilities.RemoteString.To_Unbounded_String("scenario_battle");

   Server.Server.Archive.Saving_Game
     (A_Directory,
      A_File_Name,
      A_Scenario_Name,
      A_Player_List,
      A_Countdown);
   
end create_scenario;




