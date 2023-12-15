local input_path = "input.txt"

local res = 0

local function hash(str)
    local x = 0
    for i = 1, #str do
        x = ((x + string.byte(str:sub(i, i))) * 17) % 256
    end

    return x
end

for line in io.lines(input_path) do
    line = line .. ","
    for pattern in line:gmatch("([^,]+),") do
        res = res + hash(pattern)
    end
end

print(res)
