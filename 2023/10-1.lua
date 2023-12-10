local input_path = "input.txt"

local map = {}

local start = {}
local x = 1
local y = 1

for line in io.lines(input_path) do
    local seq = {}
    y = 1
    for v in line:gmatch("(%S)") do
        table.insert(seq, v)

        if v == "S" then
            start = { x, y }
        end
        y = y + 1
    end
    x = x + 1
    table.insert(map, seq)
end

local next_pos = start
local prev_pos = start
local size = 0
local pipe = ""
while table.concat(next_pos) ~= table.concat(start) or size == 0 do
    local x, y = next_pos[1], next_pos[2]

    if pipe == "" then
        if ({ ["|"] = 1, ["J"] = 1, ["L"] = 1 })[map[x + 1][y]] then
            pipe = map[x + 1][y]
            next_pos = { x + 1, y }
        elseif ({ ["|"] = 1, ["7"] = 1, ["F"] = 1 })[map[x - 1][y]] then
            pipe = map[x - 1][y]
            next_pos = { x - 1, y }
        elseif ({ ["-"] = 1, ["L"] = 1, ["F"] = 1 })[map[x][y - 1]] then
            next_pos = { x, y - 1 }
            pipe = map[x][y - 1]
        elseif ({ ["-"] = 1, ["7"] = 1, ["J"] = 1 })[map[x][y + 1]] then
            next_pos = { x, y + 1 }
            pipe = map[x][y + 1]
        end
    else
        local tmp = { next_pos[1], next_pos[2] }
        if pipe == "|" then
            tmp = { next_pos[1] + 1, next_pos[2] }
            if table.concat(tmp) == table.concat(prev_pos) then
                tmp = { next_pos[1] - 1, next_pos[2] }
            end
        end
        if pipe == "-" then
            tmp = { next_pos[1], next_pos[2] - 1 }
            if table.concat(tmp) == table.concat(prev_pos) then
                tmp = { next_pos[1], next_pos[2] + 1 }
            end
        end
        if pipe == "J" then
            tmp = { next_pos[1] - 1, next_pos[2] }
            if table.concat(tmp) == table.concat(prev_pos) then
                tmp = { next_pos[1], next_pos[2] - 1 }
            end
        end
        if pipe == "L" then
            tmp = { next_pos[1] - 1, next_pos[2] }
            if table.concat(tmp) == table.concat(prev_pos) then
                tmp = { next_pos[1], next_pos[2] + 1 }
            end
        end
        if pipe == "7" then
            tmp = { next_pos[1] + 1, next_pos[2] }
            if table.concat(tmp) == table.concat(prev_pos) then
                tmp = { next_pos[1], next_pos[2] - 1 }
            end
        end
        if pipe == "F" then
            tmp = { next_pos[1] + 1, next_pos[2] }
            if table.concat(tmp) == table.concat(prev_pos) then
                tmp = { next_pos[1], next_pos[2] + 1 }
            end
        end

        pipe = map[tmp[1]][tmp[2]]
        prev_pos = next_pos
        next_pos = tmp
    end

    size = size + 1
end

print(size // 2)
