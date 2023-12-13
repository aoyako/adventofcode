local input_path = "input.txt"

local res = 0

local function count_reflrection(map)
    -- vertical
    for pos = 2, #map[1] do
        local fail = false
        for row = 1, #map do
            for check = pos, math.min((pos - 1) * 2, #map[1]) do
                if map[row][pos - (check - pos) - 1] ~= map[row][check] then
                    fail = true
                    break
                end
            end
            if fail then
                break
            end
        end
        if not fail then
            return pos - 1
        end
    end

    for pos = 2, #map do
        local fail = false
        for col = 1, #map[1] do
            for check = pos, math.min((pos - 1) * 2, #map) do
                if map[pos - (check - pos) - 1][col] ~= map[check][col] then
                    fail = true
                    break
                end
            end
            if fail then
                break
            end
        end
        if not fail then
            return (pos - 1) * 100
        end
    end

    return 0
end

local file = io.open(input_path, "a")
file:write("\n\n")
file:close()

local map = {}
for line in io.lines(input_path) do
    if line == "" then
        if not next(map) then
            goto continue
        end
        res = res + count_reflrection(map)
        map = {}
        goto continue
    end

    local row = {}
    for x in line:gmatch("(%S)") do
        table.insert(row, x)
    end
    table.insert(map, row)

    ::continue::
end

print(res)
