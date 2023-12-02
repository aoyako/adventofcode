local input_path = "input.txt"

local res = 0

local colors = {}

for line in io.lines(input_path) do
    line = line .. ";"
    local pattern = "(%d+) ([^,;]+)[,;]"
    colors = {}

    for n, c in line:gmatch(pattern) do
        colors[c] = math.max(tonumber(colors[c]) or 0, tonumber(n))
    end

    local power = 1
    for k, v in pairs(colors) do
        power = power * v
    end

    res = res + power
end

print(res)
