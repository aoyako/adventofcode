local input_path = "input.txt"

local res = 10e10

local trans = {}
local inters = {}

local seeds = {}

local active_map = {}
for line in io.lines(input_path) do
    if line:sub(1, #"seeds:") == "seeds:" then
        for number in line:gmatch("(%d+)") do
            table.insert(seeds, tonumber(number))
        end
    elseif line:sub(- #"map:") == "map:" then
        if next(active_map) == nil then
            goto continue
        end
        table.insert(trans, active_map["map"])
        table.sort(active_map["interval"], function(a, b) return (a[1] == b[1] and a[2] < b[2]) or (a[1] < b[1]) end)
        table.insert(inters, active_map["interval"])
        active_map = {}
    elseif line ~= "" then
        local tmp = {}
        for v in line:gmatch("(%d+)") do
            table.insert(tmp, tonumber(v))
        end

        local y, x, rec = table.unpack(tmp)
        active_map["interval"] = active_map["interval"] or {}
        active_map["map"] = active_map["map"] or {}

        table.insert(active_map["interval"], {
            tonumber(x), tonumber(x + rec)
        })
        active_map["map"][tonumber(x)] = tonumber(y)
    end
    ::continue::
end

table.insert(trans, active_map["map"])
table.sort(active_map["interval"], function(a, b) return (a[1] == b[1] and a[2] < b[2]) or (a[1] < b[1]) end)
table.insert(inters, active_map["interval"])

local function apply(ints, intervals, map)
    local res = {}
    while #ints > 0 do
        local b, e = table.unpack(table.remove(ints, 1))

        for _, interval in ipairs(intervals) do
            local ib, ie = table.unpack(interval)

            -- ====
            --      ====
            if b < ib and e <= ib then
                table.insert(res, { b, e })
                b = -1
                break

                --      ====
                -- ====
            elseif b >= ie then
                goto continue
            else
                if b < ib then
                    table.insert(res, { b, ib })
                    b = ib
                end
                local converted_interval = { b, math.min(e, ie) }
                if converted_interval[1] ~= converted_interval[2] then
                    local new_interval = { map[ib] + (converted_interval[1] - ib), map[ib] + (converted_interval[2] - ib) }
                    table.insert(res, new_interval)
                end

                if e < ie then
                    b = -1
                    break
                end
                b = ie
            end
            ::continue::
        end

        if b ~= -1 and b ~= e then
            table.insert(res, {b, e})
        end
    end

    table.sort(res, function(a, b) return (a[1] == b[1] and a[2] < b[2]) or (a[1] < b[1]) end)
    return res
end

local seed_pairs = {}
for i, seed in ipairs(seeds) do
    if i % 2 == 1 then
        table.insert(seed_pairs, { [1] = seed })
    end
end
for i, seed in ipairs(seeds) do
    if i % 2 == 0 then
        table.insert(seed_pairs[i // 2], seed_pairs[i // 2][1] + seed)
    end
end

for _, seed in ipairs(seed_pairs) do
    local ints = { seed }

    for stage = 1, #trans do
        ints = apply(ints, inters[stage], trans[stage])
    end

    res = math.min(res, ints[1][1])
end

print(res)
