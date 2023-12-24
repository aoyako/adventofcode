local input_path = "input.txt"

local points = {}
local directions = {}

local res = 0

for line in io.lines(input_path) do
    for x, y, z, vx, vy, vz in line:gmatch("([%s-%d]+), ([%s-%d]+), ([%s-%d]+) @ ([%s-%d]+), ([%s-%d]+), ([%s-%d]+)") do
        table.insert(points, { x, y, z })
        table.insert(directions, { vx, vy, vz })
    end
end

local function insersect(A1, B1, C1, A2, B2, C2)
    local c = 1000000
    local cx = c * (B1 / c * A2 - B2 / c * A1)
    if cx == 0 then
        return nil, nil
    end

    local x = (B1 / cx * C2 - B2 / cx * C1)
    local y = (C1 / B1 - A1 * x / B1)
    return x, y
end

local function getABC_xy(pos, vx0, vy0, vz0)
    local vx, vy, _ = table.unpack(directions[pos])
    local x, y, _ = table.unpack(points[pos])
    local cx = (vx - vx0)
    local cy = (vy - vy0)

    local A = cy
    local B = -cx
    local C = x * cy - y * cx

    return A, B, C
end

local function getABC_xz(pos, vx0, vy0, vz0)
    local vx, _, vz = table.unpack(directions[pos])
    local x, _, z = table.unpack(points[pos])
    local cx = (vx - vx0)
    local cz = (vz - vz0)

    local A = cz
    local B = -cx
    local C = x * cz - z * cx

    return A, B, C
end

local res_vx0 = 0
local res_vy0 = 0
local res_vz0 = 0

for vx0 = -600, 600 do
    for vy0 = -600, 600 do
        local vz0 = 0
        local lastx = nil
        local lasty = nil
        for p1 = 1, #points - 1 do
            for p2 = p1 + 1, #points do
                local A1, B1, C1 = getABC_xy(p1, vx0, vy0, vz0)
                local A2, B2, C2 = getABC_xy(p2, vx0, vy0, vz0)

                local x, y = insersect(A1, B1, C1, A2, B2, C2)
                if lastx == nil and x ~= nil then
                    lastx = x
                    lasty = y
                else
                    if x ~= nil and y ~= nil and (math.abs(lastx - x) >= 10000 or math.abs(lasty - y) >= 10000) then
                        goto continue
                    end
                end
            end
        end

        res_vx0 = vx0
        res_vy0 = vy0
        goto find_z

        ::continue::
    end
end


::find_z::
for vz0 = -600, 600 do
    local lastx = nil
    local lastz = nil
    for p1 = 1, #points - 1 do
        for p2 = p1 + 1, #points do
            local A1, B1, C1 = getABC_xz(p1, res_vx0, res_vy0, vz0)
            local A2, B2, C2 = getABC_xz(p2, res_vx0, res_vy0, vz0)

            local x, z = insersect(A1, B1, C1, A2, B2, C2)
            if lastx == nil and x ~= nil then
                lastx = x
                lastz = z
            else
                if x ~= nil and z ~= nil and (math.abs(lastx - x) >= 10000 or math.abs(lastz - z) >= 10000) then
                    goto continue
                end
            end
        end
    end

    res_vz0 = vz0
    goto find_coords

    ::continue::
end

::find_coords::
local cx1 = directions[1][1] - res_vx0
local cy1 = directions[1][2] - res_vy0
local cz1 = directions[1][3] - res_vz0
local cx2 = directions[2][1] - res_vx0
local cy2 = directions[2][2] - res_vy0
local x1 = points[1][1]
local y1 = points[1][2]
local z1 = points[1][3]
local x2 = points[2][1]
local y2 = points[2][2]

local Cy0 = cy2 / cy1 * cx1 - cx2
local y = (cy2 * x2 - cx2 * y2 + (cy2 / cy1) * (cx1 * y1 - cy1 * x1)) / Cy0
y = math.floor(y + 0.5)

local x = (-cx1 * y1 + cx1 * y + cy1 * x1) / cy1
x = math.floor(x + 0.5)

local z = (-cz1 * x1 + cz1 * x + cx1 * z1) / cx1
z = math.floor(z + 0.5)

print(x + y + z)
