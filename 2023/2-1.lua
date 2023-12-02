local input_path = "input.txt"

local res = 0

local colors = {}
local have = {
    ["red"] = 12,
    ["green"] = 13,
    ["blue"] = 14
}

local id = 1
for line in io.lines(input_path) do
    line = line .. ";"
    local pattern = "(%d+) ([^,;]+)[,;]"
    colors = {}

    for n, c in line:gmatch(pattern) do
        colors[c] = math.max(tonumber(colors[c]) or 0, tonumber(n))
    end

    local fail = false
    for k, v in pairs(colors) do
        if v > have[k] then
            fail = true
            break
        end
    end

    if not fail then
        res = res + id
    end

    id = id + 1
end

print(res)
