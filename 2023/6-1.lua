local input_path = "input.txt"

local res = 1

local time = {}
local dist = {}

for line in io.lines(input_path) do
    local inserted = {}
    for number in line:gmatch("(%d+)") do
        table.insert(inserted, tonumber(number))
    end

    dist = inserted
    if not next(time) then
        time = inserted
    end
end

for i, _ in ipairs(time) do
    local t = time[i]
    local d = dist[i]
    local record = 0

    for hold=0,t do
        if (t-hold)*hold > d then
            record = record + 1
        end
    end

    res = res * record
end

print(res)
