local function starts(String, Start)
  return string.sub(String, 1, string.len(Start)) == Start
end

-- https://stackoverflow.com/questions/9168058/how-to-dump-a-table-to-console
local function dump(o)
  if type(o) == 'table' then
    local s = '{ '
    for k, v in pairs(o) do
      if type(k) ~= 'number' then k = '"' .. k .. '"' end
      s = s .. '[' .. k .. '] = ' .. dump(v) .. ','
    end
    return s .. '} '
  else
    return tostring(o)
  end
end

local function len(t)
  local sum = 0
  for _, _ in pairs(t) do
    sum = sum + 1
  end
  return sum
end

local function read_file(path)
  local file
  file = io.open(path, "r")
  if file == nil then
    return nil
  end
  local tmp = {}
  for data in file:lines() do
    table.insert(tmp, data)
  end
  io.close(file)
  return tmp
end

local foo = read_file("../input/day19.txt")
-- local foo = read_file("./test.text")

if foo == nil then
  os.exit(1)
end

local function parse_head(data)
  local hd = data[1]
  local ret = {}
  for word in string.gmatch(hd, "[a-z]+") do
    table.insert(ret, word)
  end
  return ret
end

local parsed = parse_head(foo)

local sum = 0
for i, line in pairs(foo) do
  if i == 1 then
    goto continue
  end

  -- I need backtracking ...

  ::rep::
  local matched = false
  for _, prefix in pairs(parsed) do
    while starts(line, prefix) do
      line = string.sub(line, string.len(prefix) + 1, string.len(line))
      matched = true
      print("line with " .. prefix)
      if line ~= "" then
        print(line)
      end
    end

    if matched then
      goto rep
    end
  end

  if line == "" then
    sum = sum + 1
    print("adding to sum\n")
  end

  ::continue::
end

print(sum)
