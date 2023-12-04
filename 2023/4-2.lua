local input_path = "input.txt"

local res = 0

local function parse(s)
    local numbers = {}
    for number in s:gmatch("(%d+%s+)") do
        numbers[tonumber(number)] = true
    end
    return numbers
end

local wins = {}
local cards = {}

for line in io.lines(input_path) do
    local parts = {}
    table.insert(cards, 1)
    line = line .. " "

    for part in string.gmatch(line, "([^|]+)") do
        table.insert(parts, part)
    end

    local need = parse(parts[1])
    local have = parse(parts[2])

    local matched = 0

    for k, _ in pairs(have) do
        if need[k] then
            matched = matched + 1
        end
    end

    table.insert(wins, matched)
end


for card, won in ipairs(wins) do
    for append_card = card + 1, math.min(#cards, card + won) do
        cards[append_card] = cards[append_card] + cards[card]
    end
end

for _, v in ipairs(cards) do
    res = res + v
end

print(math.ceil(res))
