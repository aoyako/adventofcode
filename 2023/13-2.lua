local input_path = "input.txt"

local res = 0

local function count_reflrection(map, old_val)
    old_val = old_val or 0
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
            if pos - 1 == old_val then
                goto continue
            end
            return pos - 1
        end
        ::continue::
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
            if old_val == (pos - 1) * 100 then
                goto continue
            end
            return (pos - 1) * 100
        end
        ::continue::
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

        local old_val = count_reflrection(map)

        for x = 1, #map do
            for y = 1, #map[1] do
                local last = map[x][y]
                if last == "#" then
                    map[x][y] = "."
                else
                    map[x][y] = "#"
                end

                local new_mirror = count_reflrection(map, old_val)
                if new_mirror ~= 0 then
                    res = res + new_mirror
                    map = {}
                    goto continue
                end

                map[x][y] = last
            end
        end

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
