local input_path = "input.txt"

local res = 0

local function parse(s)
    local numbers = {}
    for number in s:gmatch("(%d+%s+)") do
        numbers[tonumber(number)] = true
    end
    return numbers
end

for line in io.lines(input_path) do
    local parts = {}
    line = line .. " "

    for part in string.gmatch(line, "([^|]+)") do
        table.insert(parts, part)
    end

    local need = parse(parts[1])
    local have = parse(parts[2])

    local matched = 0

    for k, _ in pairs(have) do
        if need[k] then
            matched = matched + 1
        end
    end

    matched = (matched == 0) and 0 or 2 ^ (matched - 1)

    res = res + matched
end

print(math.ceil(res))
