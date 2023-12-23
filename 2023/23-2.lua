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

local queue = { { 1, 2, "", 0, 0, 0 } }

local key_cache = {}
local function key(x, y)
    if key_cache[x] and key_cache[x][y] then
        return key_cache[x][y]
    end

    key_cache[x] = key_cache[x] or {}
    key_cache[x][y] = "|" .. tostring(x) .. "," .. tostring(y) .. "|"
    return key_cache[x][y]
end

while next(queue) do
    local item = table.remove(queue, 1)
    local x, y, path, c, fx, fy = table.unpack(item)

    if x == #map and y == #map[1] - 1 then
        res = math.max(res, c)
        goto continue
    end

    local xykey = key(x, y)
    local fxkey = key(fx, fy)
    if not string.find(path, xykey) then
        path = xykey .. path
        if map[x] and map[x][y + 1] and map[x][y + 1] ~= "#" and key(x, y + 1) ~= fxkey then
            table.insert(queue, { x, y + 1, path, c + 1, x, y })
        end
        if map[x] and map[x][y - 1] and map[x][y - 1] ~= "#" and key(x, y - 1) ~= fxkey then
            table.insert(queue, { x, y - 1, path, c + 1, x, y })
        end
        if map[x - 1] and map[x - 1][y] and map[x - 1][y] ~= "#" and key(x - 1, y) ~= fxkey then
            table.insert(queue, { x - 1, y, path, c + 1, x, y })
        end
        if map[x + 1] and map[x + 1][y] and map[x + 1][y] ~= "#" and key(x + 1, y) ~= fxkey then
            table.insert(queue, { x + 1, y, path, c + 1, x, y })
        end
    end

    ::continue::
end

print(res)
