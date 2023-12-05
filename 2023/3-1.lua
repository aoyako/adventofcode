local input_path = "input.txt"

local res = 0

local map = {}

for line in io.lines(input_path) do
    table.insert(map, {})
    line:gsub(".", function(c) table.insert(map[#map], c) end)
end

local line = string.rep('.', #map[#map])
table.insert(map, {})
_= line:gsub(".", function(c) table.insert(map[#map], c) end)

local curr_num = 0
local connected = false

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
                if map[nb[1]][nb[2]] ~= '.' and string.match(map[nb[1]][nb[2]], "%d") == nil then
                    connected = true
                end
            end
        else
            if connected then
                res = res + curr_num
            end
            curr_num = 0
            connected = false
        end
    end
end

print(res)
