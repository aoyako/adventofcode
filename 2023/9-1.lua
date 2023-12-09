local input_path = "input.txt"

local res = 0

local function interpolate(x)
    local fail = false
    local diffs = {}
    local last = nil
    for _, v in ipairs(x) do
        if v ~= 0 then
            fail = true
        end
        if last then
            table.insert(diffs, v - last)
        end
        last = v
    end

    if not fail then
        return 0
    end

    local res = interpolate(diffs)
    if #diffs == 0 then
        return res
    end
    return diffs[#diffs] + res
end

for line in io.lines(input_path) do
    local seq = {}
    for x in line:gmatch("(%S+)") do
        table.insert(seq, tonumber(x))
    end

    local tmp = interpolate(seq) + seq[#seq]
    res = res + tmp
end

print(res)
