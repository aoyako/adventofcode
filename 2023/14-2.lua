local input_path = "input.txt"

local res = 0

local map = {}
for line in io.lines(input_path) do
    local row = {}
    for x in line:gmatch("(%S)") do
        table.insert(row, x)
    end
    table.insert(map, row)
end

local function north()
    for y = 1, #map[1] do
        local insert_pos = 1
        for x = 1, #map do
            if map[x][y] == "O" then
                while insert_pos <= #map and map[insert_pos][y] == "#" do
                    insert_pos = insert_pos + 1
                end
                map[x][y] = "."
                map[insert_pos][y] = "O"
                insert_pos = insert_pos + 1
            elseif map[x][y] == "#" then
                insert_pos = x
            end
        end
    end
end

local function south()
    for y = 1, #map[1] do
        local insert_pos = #map
        for x = #map, 1, -1 do
            if map[x][y] == "O" then
                while insert_pos >= 1 and map[insert_pos][y] == "#" do
                    insert_pos = insert_pos - 1
                end
                map[x][y] = "."
                map[insert_pos][y] = "O"
                insert_pos = insert_pos - 1
            elseif map[x][y] == "#" then
                insert_pos = x
            end
        end
    end
end

local function east()
    for x = 1, #map do
        local insert_pos = #map[1]
        for y = #map[1], 1, -1 do
            if map[x][y] == "O" then
                while insert_pos >= 1 and map[x][insert_pos] == "#" do
                    insert_pos = insert_pos - 1
                end
                map[x][y] = "."
                map[x][insert_pos] = "O"
                insert_pos = insert_pos - 1
            elseif map[x][y] == "#" then
                insert_pos = y
            end
        end
    end
end

local function west()
    for x = 1, #map do
        local insert_pos = 1
        for y = 1, #map[1] do
            if map[x][y] == "O" then
                while insert_pos <= #map[1] and map[x][insert_pos] == "#" do
                    insert_pos = insert_pos + 1
                end
                map[x][y] = "."
                map[x][insert_pos] = "O"
                insert_pos = insert_pos + 1
            elseif map[x][y] == "#" then
                insert_pos = y
            end
        end
    end
end

local function load()
    for y = 1, #map[1] do
        for x = 1, #map do
            if map[x][y] == "O" then
                res = res + #map - x + 1
            end
        end
    end
end

local cache = {}
local function cached()
    for i, c in ipairs(cache) do
        local fail = false
        for x = 1, #map do
            for y = 1, #map[1] do
                if map[x][y] ~= c[x][y] then
                    fail = true
                    goto continue
                end
            end
        end

        if not fail then
            return true, i
        end

        ::continue::
    end

    local newcache = {}
    for x = 1, #map do
        table.insert(newcache, {})
        for y = 1, #map[1] do
            table.insert(newcache[x], map[x][y])
        end
    end
    table.insert(cache, newcache)

    return false, 0
end

local i = 1
while i <= 1000000000 do
    north()
    west()
    south()
    east()

    local cc, it = cached()
    if cc then
        local size = i - it
        i = i + (((1000000000 - i) // (size))) * size
    end

    i = i + 1
end

load()

print(res)
