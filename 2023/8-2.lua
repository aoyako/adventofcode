local input_path = "input.txt"

local res = 0

local command = {}
local nodes = {}
for line in io.lines(input_path) do
    if not next(command) then
        for i = 1, #line do
            table.insert(command, line:sub(i, i))
        end
        goto continue
    end

    for x, y, z in line:gmatch("(%w+) = %((%w+), (%w+)%)") do
        nodes[x] = { ["L"] = y, ["R"] = z }
    end

    ::continue::
end

local pos = {}
for k, _ in pairs(nodes) do
    if k:sub(-1) == "A" then
        table.insert(pos, k)
    end
end

local rings = {}
for _, v in ipairs(pos) do
    local instruction = 1
    local pos = nodes[v][command[instruction]]
    instruction = 2
    local meets = {}
    local it = 2

    while #meets ~= 2 do
        pos = nodes[pos][command[instruction]]
        if pos:sub(-1) == "Z" then
            table.insert(meets, it)
        end

        instruction = instruction + 1
        if instruction > #command then
            instruction = 1
        end

        it = it + 1
    end

    table.insert(rings, { meets[1], meets[#meets] - meets[#meets - 1] })
end

local function gcd(m, n)
    while n ~= 0 do
        local q = m
        m = n
        n = q % n
    end
    return m
end

local function lcm(m, n)
    return (m ~= 0 and n ~= 0) and m * n / gcd(m, n) or 0
end

table.sort(rings, function(a, b)
    return a[1] < b[1]
end)


local a = rings[1][2]
for b = 2, #rings do
    a = lcm(a, rings[b][2])
end

res = (a / rings[1][2] - 1)

print(math.ceil(rings[1][1] + res * rings[1][2]))
