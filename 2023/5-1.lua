local input_path = "input.txt"

local res = 10e10

local trans = {}
local inters = {}

local seeds = {}

local active_map = {}
for line in io.lines(input_path) do
    if line:sub(1, #"seeds:") == "seeds:" then
        for number in line:gmatch("(%d+)") do
            table.insert(seeds, tonumber(number))
        end
    elseif line:sub(- #"map:") == "map:" then
        if next(active_map) == nil then
            goto continue
        end
        table.insert(trans, active_map["map"])
        table.sort(active_map["interval"], function(a, b) return (a[1] == b[1] and a[2] < b[2]) or (a[1] < b[1]) end)
        table.insert(inters, active_map["interval"])
        active_map = {}
    elseif line ~= "" then
        local tmp = {}
        for v in line:gmatch("(%d+)") do
            table.insert(tmp, tonumber(v))
        end

        local y, x, rec = table.unpack(tmp)
        active_map["interval"] = active_map["interval"] or {}
        active_map["map"] = active_map["map"] or {}

        table.insert(active_map["interval"], {
            tonumber(x), tonumber(x + rec)
        })
        active_map["map"][tonumber(x)] = tonumber(y)
    end
    ::continue::
end

table.insert(trans, active_map["map"])
table.sort(active_map["interval"], function(a, b) return (a[1] == b[1] and a[2] < b[2]) or (a[1] < b[1]) end)
table.insert(inters, active_map["interval"])

local function apply(x, intervals, map)
    for _, interval in ipairs(intervals) do
        local b, e = table.unpack(interval)
        if x >= b and x < e then
            return map[b] + (x - b)
        end
    end
    return x
end

for _, seed in ipairs(seeds) do
    local num = seed

    for stage = 1, #trans do
        num = apply(num, inters[stage], trans[stage])
    end

    res = math.min(res, num)
end

print(res)
