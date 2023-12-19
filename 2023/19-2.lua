local input_path = "input.txt"

local res = 0

-- from lua wiki
local function deepcopy(orig, copies)
    copies = copies or {}
    local orig_type = type(orig)
    local copy
    if orig_type == 'table' then
        if copies[orig] then
            copy = copies[orig]
        else
            copy = {}
            copies[orig] = copy
            for orig_key, orig_value in next, orig, nil do
                copy[deepcopy(orig_key, copies)] = deepcopy(orig_value, copies)
            end
            setmetatable(copy, deepcopy(getmetatable(orig), copies))
        end
    else
        copy = orig
    end
    return copy
end

local read_rule = 1
local pipelines = {}
for line in io.lines(input_path) do
    if line == "" then
        read_rule = read_rule + 1
        goto continue
    end

    if read_rule == 1 then
        local name = ""
        for p in line:gmatch("([A-Za-z]+){") do
            name = p
        end

        pipelines[name] = {}
        for prop, c, v, trans in line:gmatch("([A-Za-z]+)([<>])(%d+):([A-Za-z]+),") do
            table.insert(pipelines[name], { prop, c, trans, tonumber(v) })
        end

        for trans in line:gmatch(",([A-Za-z]+)}") do
            table.insert(pipelines[name], { "x", "=", trans })
        end
    end
    ::continue::
end

local queue = { { { ["x"] = { { 1, 4000 } }, ["m"] = { { 1, 4000 } }, ["a"] = { { 1, 4000 } }, ["s"] = { { 1, 4000 } } }, "in", 1 } }

while next(queue) do
    local el = table.remove(queue, 1)
    local var, pipeline, step = table.unpack(el)

    if pipeline == "A" then
        local s = 1
        for _, v in pairs(var) do
            local range = 0
            for _, inter in ipairs(v) do
                range = range + inter[2] - inter[1] + 1
            end
            s = s * range
        end
        res = res + s
        goto continue
    end
    if pipeline == "R" then
        goto continue
    end

    if #pipelines[pipeline] == step then
        table.insert(queue, { var, pipelines[pipeline][#pipelines[pipeline]][3], 1 })
        goto continue
    end

    local to_accept = deepcopy(var)
    local to_reject = deepcopy(var)

    local rule = pipelines[pipeline][step]
    local feature = rule[1]
    local c = rule[2]
    local trans = rule[3]
    local v = rule[4]
    to_accept[feature] = {}
    to_reject[feature] = {}
    if c == "<" then
        for _, interval in ipairs(var[feature]) do
            if interval[2] < v then
                table.insert(to_accept[feature], interval)
            elseif interval[1] >= v then
                table.insert(to_reject[feature], interval)
            else
                table.insert(to_accept[feature], { math.min(v - 1, interval[1]), math.min(v - 1, interval[2]) })
                table.insert(to_reject[feature], { math.max(v, interval[1]), math.max(v, interval[2]) })
            end
        end
    else
        for _, interval in ipairs(var[feature]) do
            if interval[1] > v then
                table.insert(to_accept[feature], interval)
            elseif interval[2] <= v then
                table.insert(to_reject[feature], interval)
            else
                table.insert(to_accept[feature], { math.max(v + 1, interval[1]), math.max(v + 1, interval[2]) })
                table.insert(to_reject[feature], { math.min(v, interval[1]), math.min(v, interval[2]) })
            end
        end
    end
    table.sort(to_accept[feature], function(a, b) return (a[1] == b[1] and a[2] < b[2]) or (a[1] < b[1]) end)
    table.sort(to_reject[feature], function(a, b) return (a[1] == b[1] and a[2] < b[2]) or (a[1] < b[1]) end)

    table.insert(queue, { to_accept, trans, 1 })
    table.insert(queue, { to_reject, pipeline, step + 1 })

    ::continue::
end

print(res)
