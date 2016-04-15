

local player_1_name = Sisyfos.Get_Player_Name(1);
local player_2_name = Sisyfos.Get_Player_Name(2);

print("player_1_name=" .. player_1_name)
print("player_2_name=" .. player_2_name)


if player_1_name == "Yulia" then
  yulia_id = 1
end

if player_1_name == "Frank" then
  frank_id = 1
end

if player_2_name == "Yulia" then
  yulia_id = 2
end

if player_2_name == "Frank" then
  frank_id = 2
end


local Story = {
{toWrite = function () print("Introduction") return true end,
text = 
"\n\n==            TUBASTGA           ==\n\n"
},
{toWrite = function () print("Chapter 1") return true end,
text = 
"Your small group has arrived to the new land.\n" ..
"You are all filled with the hope, that this \n" .. 
"land is peaceful.\n" ..
"The lumberjacks are looking for wood, to create\n" ..
"the first fireplace. Everyone are eager to get \n" .. 
"out of the ship - so you should start to look\n" ..
"for some place to start to build some houses.\n"
},
{toWrite = function () print("Chapter 2") return false end,
text = "You have succeded in in building your first lumberjack"}
}

ordered_keys= {}
for n in pairs(Story) do table.insert(ordered_keys, n) end

table.sort(ordered_keys)
for i = 1, #ordered_keys do
    local k, v = ordered_keys[i], Story[ ordered_keys[i] ]
    
    if Story[k].toWrite() then
      Sisyfos.Player_Activity_Report_Append(1, yulia_id, Story[k].text)
    end
end



