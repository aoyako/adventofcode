local input_path = "input.txt"

local times = ""
local dists = ""

for line in io.lines(input_path) do
    local inserted = ""
    for number in line:gmatch("(%d+)") do
        inserted = inserted .. number
    end

    dists = inserted
    if times == "" then
        times = inserted
    end
end

local time = tonumber(times)
local dist = tonumber(dists)

local left = (time - math.sqrt(time^2-4*dist)) // 2
local right = (time + math.sqrt(time^2-4*dist)) // 2 

print(math.ceil(right-left))

