local input_path = "input.txt"

local map = {}

local stars = {}
local x = 1
local y = 1

for line in io.lines(input_path) do
    local seq = {}
    y = 1
    for v in line:gmatch("(%S)") do
        table.insert(seq, v)

        if v == "#" then
            stars[table.concat({ x, y }, ",")] = { x, y }
        end
        y = y + 1
    end
    x = x + 1
    table.insert(map, seq)
end

local empty_rows = {}
local empty_cols = {}

for x = 1, #map do
    local has_star = false
    for y = 1, #map[x] do
        if map[x][y] == "#" then
            has_star = true
            break
        end
    end
    if not has_star then
        empty_rows[x] = 1
    end
end
for y = 1, #map[1] do
    local has_star = false
    for x = 1, #map do
        if map[x][y] == "#" then
            has_star = true
            break
        end
    end
    if not has_star then
        empty_cols[y] = 1
    end
end

local seen = {}
local res = 0
for starA, posA in pairs(stars) do
    for starB, posB in pairs(stars) do
        if starA == starB then
            goto continue
        end
        local comb1 = table.concat(posA, ",") .. ";" .. table.concat(posB, ",")
        local comb2 = table.concat(posB, ",") .. ";" .. table.concat(posA, ",")
        if seen[comb1] or seen[comb2] then
            goto continue
        end
        seen[comb1] = 1
        seen[comb2] = 1

        local xmin = math.min(posA[1], posB[1])
        local xmax = math.max(posA[1], posB[1])
        local ymin = math.min(posA[2], posB[2])
        local ymax = math.max(posA[2], posB[2])

        local d = -2

        for i = xmin, xmax do
            if empty_rows[i] then
                d = d + 1
            end
            d = d + 1
        end

        for i = ymin, ymax do
            if empty_cols[i] then
                d = d + 1
            end
            d = d + 1
        end

        res = res + d

        ::continue::
    end
end

print(res)
