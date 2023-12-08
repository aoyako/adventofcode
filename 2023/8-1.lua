local input_path = "input.txt"

local res = 0

local command = {}
local nodes = {}
for line in io.lines(input_path) do
    if not next(command) then
        for i = 1, #line do
            table.insert(command, line:sub(i, i))
        end
        goto continue
    end

    for x, y, z in line:gmatch("(%w+) = %((%w+), (%w+)%)") do
        nodes[x] = { ["L"] = y, ["R"] = z }
    end

    ::continue::
end

local pos = "AAA"
local instruction = 1

while pos ~= "ZZZ" do
    pos = nodes[pos][command[instruction]]
    instruction = instruction + 1
    if instruction > #command then
        instruction = 1
    end

    res = res + 1
end

print(res)
