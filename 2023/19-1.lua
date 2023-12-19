local input_path = "input.txt"

local res = 0

local function cmp(x, c)
    if c == "<" then
        return function(v)
            return v < x
        end
    else
        return function(v)
            return v > x
        end
    end
end

local function accept(x)
    return true
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
            table.insert(pipelines[name], { prop, cmp(tonumber(v), c), trans })
            -- print(trans)
        end

        for trans in line:gmatch(",([A-Za-z]+)}") do
            table.insert(pipelines[name], { "x", accept, trans })
        end
    else
        local var = {}
        for feature, value in line:gmatch("([xmas]+)=(%d+)") do
            var[feature] = tonumber(value)
        end

        local pipeline = "in"
        while true do
            local pipe = pipelines[pipeline]
            for _, rule in ipairs(pipe) do
                local feature, c, trans = table.unpack(rule)
                if c(var[feature]) then
                    pipeline = trans
                    break
                end
            end

            if pipeline == "A" then
                local s = 0
                for _, v in pairs(var) do
                    s = s + v
                end
                res = res + s
                break
            elseif pipeline == "R" then
                break
            end
        end
    end
    ::continue::
end

print(res)
