local input_path = "input.txt"

local map = {}
local res = 0

for line in io.lines(input_path) do
    local row = {}
    for c in line:gmatch("(%S)") do
        table.insert(row, c)
    end
    table.insert(map, row)
end

local queue = { { 1, 2, {}, 0 } }

local function key(x, y)
    return tostring(x) .. "," .. tostring(y)
end

-- from lua wiki
local function deepcopy(orig, copies)
    copies = copies or {}
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        if copies[orig] then
            copy = copies[orig]
        else
            copy = {}
            copies[orig] = copy
            for orig_key, orig_value in next, orig, nil do
                copy[deepcopy(orig_key, copies)] = deepcopy(orig_value, copies)
            end
            setmetatable(copy, deepcopy(getmetatable(orig), copies))
        end
    else
        copy = orig
    end
    return copy
end

while next(queue) do
    local item = table.remove(queue, 1)
    local x, y, path, c = table.unpack(item)

    if x == #map and y == #map[1] - 1 then
        res = math.max(res, c)
        goto continue
    end

    path = deepcopy(path)
    if not path[key(x, y)] then
        path[key(x, y)] = true
        if map[x] and map[x][y + 1] and map[x][y + 1] ~= "#" and (map[x][y] == ">" or map[x][y] == ".") then
            table.insert(queue, { x, y + 1, path, c + 1 })
        end
        if map[x] and map[x][y - 1] and map[x][y - 1] ~= "#" and (map[x][y] == "<" or map[x][y] == ".") then
            table.insert(queue, { x, y - 1, path, c + 1 })
        end
        if map[x - 1] and map[x - 1][y] and map[x - 1][y] ~= "#" and (map[x][y] == "^" or map[x][y] == ".") then
            table.insert(queue, { x - 1, y, path, c + 1 })
        end
        if map[x + 1] and map[x + 1][y] and map[x + 1][y] ~= "#" and (map[x][y] == "v" or map[x][y] == ".") then
            table.insert(queue, { x + 1, y, path, c + 1 })
        end
    end

    ::continue::
end

print(res)
