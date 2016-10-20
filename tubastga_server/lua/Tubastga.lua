
Tubastga = {}

function Tubastga.startGame (p_player_id)
    Sisyfos.Player_Activity_Report_Append(1,1,"Welcome to Tubast'ga")
    Sisyfos.Player_Activity_Report_Append(1,1,"You are on the continent of Arbou.")
    Sisyfos.Player_Activity_Report_Append(1,1,"A small group of settlers have joined you. They look forward")
    Sisyfos.Player_Activity_Report_Append(1,1,"to build and live on this new continent.")
    Sisyfos.Player_Activity_Report_Append(1,1,"")
    Sisyfos.Player_Activity_Report_Append(1,1,"Your builders have already built two simple towers in wood.")
    Sisyfos.Player_Activity_Report_Append(1,1,"")
    Sisyfos.Player_Activity_Report_Append(1,1,"The wood and stone from the ship has been transported the towers.")
    Sisyfos.Player_Activity_Report_Append(1,1,"")
    
    Tubastga.q1_buildWoodcutterSawmill(p_player_id)

end

function Tubastga.q1_buildWoodcutterSawmill (p_player_id)
    Sisyfos.Player_Activity_Report_Append(1,1,"-- Quest: Build a Woodcutter and Sawmill")
    Sisyfos.Player_Activity_Report_Append(1,1,"Your first task, is to build a woodcutter and a sawmill.")
    Sisyfos.Player_Activity_Report_Append(1,1,"As soon as you have built these, they will start to produce wood")
    Sisyfos.Player_Activity_Report_Append(1,1,"that can be used to build more buildings later.")
    Sisyfos.Player_Activity_Report_Append(1,1,"")
end

function Tubastga.foundTreasure (p_player_id, p_treasure_no) 
    Sisyfos.Player_Activity_Report_Append(1,1,"Greetings from Lua")
end

