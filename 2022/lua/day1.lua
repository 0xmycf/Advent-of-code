#! /usr/bin/env lua

--[[
Probably not how you should do it, but this is the first time I
did lua (ourside of basic vim config stuff) so ig its fine?
--]]

print "Solving day 1 of aoc 2022"
print "https://adventofcode.com/2022/day/1"

local function solve(path)
  local ans = { [0] = 0 }
  local idx = 0
  for line in io.lines(path) do
    if line == "" then
      idx = idx + 1
      ans[idx] = 0
    else
      ans[idx] = ans[idx] + tonumber(line)
    end
  end
  return ans
end

local res = solve("./input")

local max = 0
for _, sum in pairs(res) do
  if sum > max then
    max = sum
  end
end

print "part 1:"
print(max)
print ""

local max1 = 0
local max2 = 0
local max3 = 0

for _, sum in pairs(res) do
  if sum >= max1 then
    max3 = max2
    max2 = max1
    max1 = sum
  elseif sum >= max2 then
    max3 = max2
    max2 = sum
  elseif sum >= max3 then
    max3 = sum
  end
end

print "part 2"
print(max1 + max2 + max3)
