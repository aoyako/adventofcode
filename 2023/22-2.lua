local input_path = "input.txt"

local blocks = {}
local fdeps = {}
local bdeps = {}
local map = {}

local maxx = 0
local maxy = 0

for line in io.lines(input_path) do
    for x1, y1, z1, x2, y2, z2 in line:gmatch("(%d+),(%d+),(%d+)~(%d+),(%d+),(%d+)") do
        x1 = tonumber(x1)
        x2 = tonumber(x2)
        y1 = tonumber(y1)
        y2 = tonumber(y2)
        z1 = tonumber(z1)
        z2 = tonumber(z2)
        table.insert(blocks, { { x1, y1, z1 }, { x2, y2, z2 } })
        maxx = math.max(maxx, x1, x2)
        maxy = math.max(maxy, y1, y2)
    end
end

local function key(x)
    return tostring(x[1]) .. "|" .. tostring(x[2])
end

local function map_insert(x, y, z, name)
    if map[key({ x, y })] and map[key({ x, y })][2] ~= name then
        local prev = map[key({ x, y })]
        if prev[1] + 1 == z then
            fdeps[prev[2]] = fdeps[prev[2]] or {}
            fdeps[prev[2]][name] = true
            bdeps[name] = bdeps[name] or {}
            bdeps[name][prev[2]] = true
        end
    end
    map[key({ x, y })] = { z, name }
end

local function apply(block, name)
    local pos1, pos2 = table.unpack(block)
    local x1, y1, z1 = table.unpack(pos1)
    local x2, y2, z2 = table.unpack(pos2)

    -- vertical bar
    if x1 == x2 and y1 == y2 then
        local top = (map[key({ x1, y1 })] or { 0, "" })[1]
        for i = z1, z2 do
            map_insert(x1, y1, top + 1 + (i - z1), name)
        end
        -- horizontal bar
    elseif x1 == x2 then
        local top = 0
        for i = y1, y2 do
            top = math.max(top, (map[key({ x1, i })] or { 0, "" })[1])
        end
        for i = y1, y2 do
            map_insert(x1, i, top + 1, name)
        end
    elseif y1 == y2 then
        local top = 0
        for i = x1, x2 do
            top = math.max(top, (map[key({ i, y1 })] or { 0, "" })[1])
        end
        for i = x1, x2 do
            map_insert(i, y1, top + 1, name)
        end
    end
end

table.sort(blocks, function(a, b)
    return a[1][3] < b[1][3]
end)

for i, block in ipairs(blocks) do
    apply(block, i)
end

local function len(x)
    local c = 0
    for n in pairs(x) do
        c = c + 1
    end
    return c
end

local function dissintegrate(block)
    local fall = 0
    local queue = {}
    local falled = { [block] = true }

    if fdeps[block] then
        for dep, _ in pairs(fdeps[block]) do
            table.insert(queue, dep)
        end
    end

    while next(queue) do
        local shaking = table.remove(queue, 1)
        local c = 0
        for supp, _ in pairs(bdeps[shaking]) do
            if not falled[supp] then
                c = c + 1
            end
        end

        if c <= 0 then
            if not falled[shaking] then
                fall = fall + 1
                falled[shaking] = true

                if fdeps[shaking] then
                    for dep, _ in pairs(fdeps[shaking]) do
                        table.insert(queue, dep)
                    end
                end
            end
        end
    end

    return fall
end

local res = 0
for block, _ in ipairs(blocks) do
    res = res + dissintegrate(block)
end

print(res)
