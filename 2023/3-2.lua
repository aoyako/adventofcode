local input_path = "input.txt"

local res = 0
local map = {}

for line in io.lines(input_path) do
    table.insert(map, {})
    line:gsub(".", function(c) table.insert(map[#map], c) end)
end

local line = string.rep('.', #map[#map])
table.insert(map, {})
_ = line:gsub(".", function(c) table.insert(map[#map], c) end)

local function key(x)
    return tostring(x[1]) .. "," .. tostring(x[2])
end

local counter = {}
local mults = {}
local curr_num = 0
local connected = false
local gears = {}

for x = 1, #map do
    for y = 1, #map[x] do
        if string.match(map[x][y], "%d") ~= nil then
            curr_num = 10 * curr_num + tonumber(map[x][y])

            local neighbours = {}
            for dx = -1, 1 do
                for dy = -1, 1 do
                    table.insert(neighbours, {
                        math.min(#map, math.max(1, x + dx)),
                        math.min(#map[x], math.max(1, y + dy)),
                    })
                end
            end

            for _, nb in ipairs(neighbours) do
                if map[nb[1]][nb[2]] == '*' then
                    gears[key(nb)] = true
                    connected = true
                end
            end
        else
            if connected then
                for gear, _ in pairs(gears) do
                    counter[gear] = (tonumber(counter[gear]) or 0) + 1
                    mults[gear] = (tonumber(mults[gear]) or 1) * curr_num
                end
            end
            gears = {}
            curr_num = 0
            connected = false
        end
    end
end

for loc, c in pairs(counter) do
    if c == 2 then
        res = res + mults[loc]
    end
end

print(res)
