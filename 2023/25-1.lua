local input_path = "input.txt"

local connections = {}
local nodes = {}
local edges = {}

local function len(x)
    local c = 0
    for n in pairs(x) do
        c = c + 1
    end
    return c
end

for line in io.lines(input_path) do
    local main = ""
    line = line .. " "
    for a in line:gmatch("(%w+):%s") do
        main = a
    end
    nodes[main] = true
    for b in line:gmatch("(%w+)%s") do
        nodes[b] = true
        connections[main] = connections[main] or {}
        connections[b] = connections[b] or {}
        connections[main][b] = true
        connections[b][main] = true
        table.insert(edges, { main, b })
    end
end

local function get_groups()
    local visited = {}
    local group_sizes = { 0 }
    local visited_nodes = 0
    local queue = {}
    while visited_nodes ~= len(nodes) do
        for node, _ in pairs(nodes) do
            if not visited[node] then
                queue = { node }
                break
            end
        end

        while next(queue) do
            local cnode = table.remove(queue)
            if not visited[cnode] then
                visited[cnode] = true
                visited_nodes = visited_nodes + 1
            else
                goto continue
            end
            if connections[cnode] then
                for c, _ in pairs(connections[cnode]) do
                    if visited[c] then
                        goto next_node
                    end
                    table.insert(queue, c)

                    ::next_node::
                end
            end
            ::continue::
        end

        table.insert(group_sizes, visited_nodes - group_sizes[#group_sizes])
    end

    return group_sizes
end

local function remove_node(i)
    connections[edges[i][1]][edges[i][2]] = nil
    connections[edges[i][2]][edges[i][1]] = nil
end

local function add_node(i)
    connections[edges[i][1]][edges[i][2]] = true
    connections[edges[i][2]][edges[i][1]] = true
end

for a = 1, #edges - 2 do
    remove_node(a)
    for b = a + 1, #edges - 1 do
        remove_node(b)
        for c = b + 1, #edges do
            remove_node(c)

            local groups = get_groups()
            if #groups == 3 then
                print(groups[2] * groups[3])
                os.exit(0)
            end
            add_node(c)
        end
        add_node(b)
    end
    add_node(a)
end
