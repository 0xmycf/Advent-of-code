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

-- lets write a helper file for proper regex

local pyfile = io.open("helper.py", "w+")

local content = "import re, sys\n\ns = sys.argv[1]\npattern = \"("
for i, value in pairs(parsed) do
  if i == 1 then
    content = content .. value
  else
    content = content .. "|" .. value
  end
end

content = content .. ")*$\"\n\nif re.match(pattern, s):\n\tsys.exit(0)\nelse:\n\tsys.exit(1)\n"

pyfile:write(content)
pyfile:flush()
pyfile:close()

for i, line in pairs(foo) do
  if i == 1 or line == "" then
    goto continue
  end

  -- print("running line: " .. line)
  local status = os.execute("python helper.py " .. line)
  -- print("status is: " .. status)
  if status == 0 then
    sum = sum +1
  end

  ::continue::
end

print(sum)
