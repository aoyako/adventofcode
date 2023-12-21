local input_path = "input.txt"

local map = {}

local x = 1
local y = 1
local start = {}

for line in io.lines(input_path) do
    local row = {}
    y = 1
    for e in line:gmatch("(%S)") do
        if e == "S" then
            e = "."
            start = { x, y }
        end
        table.insert(row, e)
        y = y + 1
    end

    x = x + 1
    table.insert(map, row)
end

local max_steps = 6
local res = 0

local visited = {}
local function key(x)
    return tostring(x[1]) .. "|" .. tostring(x[2])
end

local queue = { { start, 0 } }

while next(queue) do
    local item = table.remove(queue, 1)
    local pos, steps = table.unpack(item)

    if (max_steps - steps) % 2 == 0 then
        if visited[key(pos)] then
            goto continue
        end
        visited[key(pos)] = true

        res = res + 1
    end
    if steps == max_steps then
        goto continue
    end


    local x, y = table.unpack(pos)
    for _, d in ipairs({ -1, 1 }) do
        if map[x + d] and map[x + d][y] == "." then
            table.insert(queue, { { x + d, y }, steps + 1 })
        end
        if map[x] and map[x][y + d] == "." then
            table.insert(queue, { { x, y + d }, steps + 1 })
        end
    end
    ::continue::
end

print(res)
