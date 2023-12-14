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

for y = 1, #map[1] do
    local insert_pos = 1
    for x = 1, #map do
        if map[x][y] == "O" then
            while insert_pos <= #map and map[insert_pos][y] == "#" do
                insert_pos = insert_pos + 1
            end
            res = res + #map - insert_pos + 1
            insert_pos = insert_pos + 1
        elseif map[x][y] == "#" then
            insert_pos = x
        end
    end
end

print(res)
