local input_path = "input.txt"

local points = {}
local directions = {}

local lower = 200000000000000
local upper = 400000000000000
local res = 0

for line in io.lines(input_path) do
    for x, y, z, vx, vy, vz in line:gmatch("([%s-%d]+), ([%s-%d]+), ([%s-%d]+) @ ([%s-%d]+), ([%s-%d]+), ([%s-%d]+)") do
        table.insert(points, { x, y, z })
        table.insert(directions, { vx, vy, vz })
    end
end

local function insersect(A, B, VA, VB)
    local Ax, Ay = table.unpack(A)
    local Bx, By = table.unpack(B)
    local VAx, VAy = table.unpack(VA)
    local VBx, VBy = table.unpack(VB)

    local ct2 = VBy * VAx - VBx * VAy

    if ct2 == 0 then
        return
    end

    local cf = VAx * (Ay - By + (VAy / VAx) * (Bx - Ax))

    local t2 = cf / ct2
    if t2 < 0 then
        return
    end
    local t1 = (Bx - Ax + VBx * t2) / VAx
    if t1 < 0 then
        return
    end

    local Px = Bx + VBx * t2
    local Py = By + VBy * t2
    if lower <= Px and Px <= upper and lower <= Py and Py <= upper then
        res = res + 1
    end
end

for i = 1, #points - 1 do
    for j = i + 1, #points do
        local old = res
        insersect(points[i], points[j], directions[i], directions[j])
    end
end

print(res)
