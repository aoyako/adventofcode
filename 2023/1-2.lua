local input_path = "input.txt"

local res = 0

local digits = {
    ["one"] = 1,
    ["two"] = 2,
    ["three"] = 3,
    ["four"] = 4,
    ["five"] = 5,
    ["six"] = 6,
    ["seven"] = 7,
    ["eight"] = 8,
    ["nine"] = 9
}

for line in io.lines(input_path) do
    local tmp = ""

    for i = 1, #line do
        local c = line:sub(i, i)

        if string.match(c, "%d") ~= nil then
            tmp = tmp .. c
        else
            for k, v in pairs(digits) do
                if string.match(line:sub(i), "^" .. k .. ".*$") ~= nil then
                    tmp = tmp .. tostring(v)
                end
            end
        end
    end

    tmp = tmp:sub(1, 1) .. tmp:sub(-1)
    local num = tonumber(tmp)
    res = res + num
end

print(res)
