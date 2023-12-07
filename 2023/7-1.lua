local input_path = "input.txt"

local res = 0

local function fit(x)
    local counter = {}
    for i = 1, #x do
        local c = x:sub(i,i)
        counter[c] = (counter[c] or 0) + 1
    end

    local counter_red = {}
    for _, v in pairs(counter) do
        table.insert(counter_red, v)
    end

    table.sort(counter_red, function(a, b) return a > b end)

    if counter_red[1] == 5 then
        return 600
    elseif counter_red[1] == 4 then
        return 500
    elseif counter_red[1] == 3 and counter_red[2] == 2 then
        return 400
    elseif counter_red[1] == 3 then
        return 300
    elseif counter_red[1] == 2 and counter_red[2] == 2 then
        return 200
    elseif counter_red[1] == 2 then
        return 100
    else
        return 50
    end
end

local cards = {}
for line in io.lines(input_path) do
    local data = {}
    for x in line:gmatch("%w+") do
        table.insert(data, x)
    end

    data[1] =  data[1]:gsub("T", string.char(string.byte("9")+1))
    data[1] =  data[1]:gsub("J", string.char(string.byte("9")+2))
    data[1] =  data[1]:gsub("Q", string.char(string.byte("9")+3))
    data[1] =  data[1]:gsub("K", string.char(string.byte("9")+4))
    data[1] =  data[1]:gsub("A", string.char(string.byte("9")+5))

    table.insert(cards, {fit(data[1]), data[1], tonumber(data[2])})
end

table.sort(cards, function(a, b)
    if a[1] == b[1] then
        return a[2] < b[2]
    end
    return a[1] < b[1]
end)

for i, v in ipairs(cards) do
    res = res + i*v[3]
end

print(res)
