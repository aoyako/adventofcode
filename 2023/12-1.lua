local input_path = "input.txt"

local res = 0

local function key(val)
    return table.concat(val, " ")
end

local function unkey(key)
    local tokens = {}
    for x in key:gmatch("(%d+)") do
        table.insert(tokens, tonumber(x))
    end

    return tokens
end

local function rec(line, cc)
    local res = 0
    local cc = unkey(cc)
    if #line == 0 and next(cc) == nil then
        return 1
    end
    if next(cc) == nil then
        for i = 1, #line do
            if line:sub(i, i) == "#" then
                return 0
            end
        end
        return 1
    end
    if #line == 0 then
        return 0
    end

    if line:sub(1, 1) == "?" then
        res = res + rec(line:sub(2), key(cc))
    end
    if line:sub(1, 1) ~= "." then
        local fail = false
        for i = 1, cc[1] do
            if i > #line then
                fail = true
                break
            end
            if line:sub(i, i) == "." then
                fail = true
                break
            end
        end

        if not fail then
            if cc[1] + 1 > #line or (cc[1] + 1 <= #line and line:sub(cc[1] + 1, cc[1] + 1) ~= "#") then
                local dist = table.remove(cc, 1) + 1
                res = res + rec(line:sub(dist + 1), key(cc))
            end
        end
    else
        local start = 1
        while start <= #line and line:sub(start, start) == "." do
            start = start + 1
        end
        res = res + rec(line:sub(start), key(cc))
    end

    return res
end

for line in io.lines(input_path) do
    local tokens = {}
    for x in line:gmatch("(%S+)") do
        table.insert(tokens, x)
    end

    res = res + rec(tokens[1], tokens[2])
end

print(res)
