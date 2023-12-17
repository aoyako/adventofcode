local input_path = "input.txt"

-- heap implementation
-- it works too long without heap, sadly
local function less(l, r)
    for i = 1, #l do
        if l[i] < r[i] then
            return true
        elseif l[i] > r[i] then
            return false
        end
    end
    return false
end

local function heapify(heap, i)
    local left = 2 * i
    local right = 2 * i + 1
    local smallest = i

    if left <= #heap and less(heap[left], heap[smallest]) then
        smallest = left
    end

    if right <= #heap and less(heap[right], heap[smallest]) then
        smallest = right
    end

    if smallest ~= i then
        heap[i], heap[smallest] = heap[smallest], heap[i]
        heapify(heap, smallest)
    end
end

local function build_heap(array)
    local heap_size = #array
    for i = math.floor(heap_size / 2), 1, -1 do
        heapify(array, i)
    end

    return array
end

local function heap_push(heap, value)
    table.insert(heap, value)
    local i = #heap
    while i > 1 and less(heap[i], heap[i // 2]) do
        heap[i], heap[i // 2] = heap[i // 2], heap[i]
        i = i // 2
    end
end

local function heap_pop(heap)
    if #heap == 0 then
        return nil
    end

    local min_value = heap[1]
    heap[1] = heap[#heap]
    table.remove(heap)
    heapify(heap, 1)

    return min_value
end

local map = {}
local res = 100000000000

for line in io.lines(input_path) do
    local row = {}
    for x in line:gmatch("(%S)") do
        table.insert(row, tonumber(x))
    end
    table.insert(map, row)
end

local function key(x)
    local res = ""
    for k, v in pairs(x) do
        res = res .. tostring(v) .. "|"
    end

    return res
end

-- x, y, direction, distance, cost
local positions = build_heap({
    { 1, 2, 1, 1, map[1][2] },
    { 2, 1, 2, 1, map[2][1] },
})

local cost = {}

local function boundary_check(x, y, dir, d, c, new_dir)
    if dir ~= new_dir and d < 4 then
        return
    end
    if dir == -new_dir then
        return
    end
    if new_dir == dir then
        d = d + 1
    else
        d = 1
    end
    if x >= 1 and x <= #map and y >= 1 and y <= #map[1] then
        heap_push(positions, { x, y, new_dir, d, c + map[x][y] })
    end
end

local function move(x, y, dir, d, c)
    for _, new_dir in ipairs({ -1, -2, 1, 2 }) do
        if new_dir == dir and d >= 10 then
            goto continue
        end
        if new_dir == 1 then
            boundary_check(x, y + 1, dir, d, c, new_dir)
        elseif new_dir == 2 then
            boundary_check(x + 1, y, dir, d, c, new_dir)
        elseif new_dir == -1 then
            boundary_check(x, y - 1, dir, d, c, new_dir)
        elseif new_dir == -2 then
            boundary_check(x - 1, y, dir, d, c, new_dir)
        end
        ::continue::
    end
end

while next(positions) do
    local point = heap_pop(positions)
    local x, y, dir, d, c = table.unpack(point)
    if cost[key({ x, y, dir, d })] and cost[key({ x, y, dir, d })] <= c then
        goto continue
    end

    cost[key({ x, y, dir, d })] = c
    move(x, y, dir, d, c)

    if x == #map and y == #map[1] and d >= 4 then
        res = math.min(res, c)
    end

    ::continue::
end

print(res)
