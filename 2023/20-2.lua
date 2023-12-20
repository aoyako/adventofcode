local input_path = "input.txt"

local nodes = {}
local nodes_status = {}
local nodes_in = {}

for line in io.lines(input_path) do
    local name = ""
    for t, n in line:gmatch("(%S)(%S+)%s%->") do
        name = n
        nodes_status[name] = { ["type"] = t, ["status"] = false }
    end

    line = line .. ","
    for c in line:gmatch("(%S+),") do
        nodes[name] = nodes[name] or {}
        table.insert(nodes[name], c)
        nodes_in[c] = nodes_in[c] or {}
        nodes_in[c][name] = false
    end
end

local low = 1000
local high = 0

local function push(queue, from, signal)
    if signal then
        high = high + #nodes[from]
    else
        low = low + #nodes[from]
    end
    for _, receivers in ipairs(nodes[from]) do
        table.insert(queue, { receivers, signal, from })
    end
end

local target_node = ""
for from, pipeline in pairs(nodes) do
    for _, node in ipairs(pipeline) do
        if node == "rx" then
            target_node = from
        end
    end
end
local ins = 0
for _, pipeline in pairs(nodes) do
    for _, node in ipairs(pipeline) do
        if node == target_node then
            ins = ins + 1
        end
    end
end

local function len(x)
    local c = 0
    for n in pairs(x) do
        c = c + 1
    end
    return c
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

local cycle = {}

for times = 1, 1000000000 do
    local queue = { { "roadcaster", false, "button" } }

    while next(queue) do
        local item = table.remove(queue, 1)
        local node, signal, from = table.unpack(item)

        if node == target_node and signal then
            if cycle[from] then
                cycle[from] = times - cycle[from]
            else
                cycle[from] = times
            end
        end
        if len(cycle) == ins then
            local res = 1
            for x, v in pairs(cycle) do
                res = lcm(res, v)
            end

            print(math.ceil(res))
            os.exit(0)
        end

        if not nodes_status[node] then
            goto continue
        end
        if nodes_status[node]["type"] == "%" then
            if not signal then
                nodes_status[node]["status"] = not nodes_status[node]["status"]
                if nodes_status[node]["status"] then
                    -- return high
                    push(queue, node, true)
                    goto continue
                else
                    -- return low
                    push(queue, node, false)
                    goto continue
                end
            end
        elseif nodes_status[node]["type"] == "&" then
            nodes_in[node][from] = signal
            for _, v in pairs(nodes_in[node]) do
                if not v then
                    -- return high
                    push(queue, node, true)
                    goto continue
                end
            end
            -- return low
            push(queue, node, false)
            goto continue
        else
            push(queue, node, false)
            goto continue
        end
        ::continue::
    end
end
