local input_path = "input.txt"

local res = 0

for line in io.lines(input_path) do
    local tmp = ""

    for i = 1, #line do
        local c = line:sub(i, i)

        if string.match(c, "%d") ~= nil then
            tmp = tmp .. c
        end
    end

    tmp = tmp:sub(1, 1) .. tmp:sub(-1)
    local num = tonumber(tmp)
    res = res + num
end

print(res)
