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

local max_steps = 26501365

local function fun(pos, total_steps)
    local res = 0
    local visited = {}
    local function key(x)
        return tostring(x[1]) .. "|" .. tostring(x[2])
    end
    local queue = { { pos, 0 } }


    while next(queue) do
        local item = table.remove(queue, 1)
        local pos, steps = table.unpack(item)

        if (total_steps - steps) % 2 == 0 then
            if visited[key(pos)] then
                goto continue
            end
            visited[key(pos)] = true

            res = res + 1
        end
        if steps == total_steps then
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

    return res
end

local map_size = max_steps // #map - 1

local odd = (map_size // 2 * 2 + 1) ^ 2
local even = ((map_size + 1) // 2 * 2) ^ 2

local full_odd_points = fun(start, #map * 2 + 1)
local full_even_points = fun(start, #map * 2)

local from_sides = fun({ #map, start[2] }, #map - 1) + fun({ start[1], 1 }, #map - 1) + fun({ 1, start[2] }, #map - 1) +
    fun({ start[1], #map }, #map - 1)

local ssector = fun({ #map, 1 }, #map // 2 - 1) + fun({ #map, #map }, #map // 2 - 1) + fun({ 1, 1 }, #map // 2 - 1) +
    fun({ 1, #map }, #map // 2 - 1)

local lsectors = fun({ #map, 1 }, #map * 3 // 2 - 1) + fun({ #map, #map }, #map * 3 // 2 - 1) +
    fun({ 1, 1 }, #map * 3 // 2 - 1) + fun({ 1, #map }, #map * 3 // 2 - 1)

local res = odd * full_odd_points + even * full_even_points + from_sides +
    (map_size + 1) * ssector +
    map_size * lsectors

print(math.ceil(res))
