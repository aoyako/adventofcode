local input_path = "input.txt"

local res = 0

local x = 1
local y = 1
local points = { { x, y } }

for line in io.lines(input_path) do
    for dir, steps, str in line:gmatch("(%S)%s(%d+)%s%(#(%S+)%)") do
        dir = str:sub(#str, #str)
        steps = tonumber(str:sub(1, #str - 1), 16)

        if dir == "0" then
            y = y + steps
        elseif dir == "1" then
            x = x + steps
        elseif dir == "2" then
            y = y - steps
        elseif dir == "3" then
            x = x - steps
        end
        table.insert(points, { x, y })
        res = res + steps
    end
end

local inside = 0
for i = 1, #points - 1 do
    inside = inside + ((points[i][2] + points[i + 1][2]) * (points[i][1] - points[i + 1][1]))
end

print((math.abs(inside) + res) // 2 + 1)
