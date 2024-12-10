#! /usr/bin/env ruby
# frozen_string_literal: true

require 'set'

# @return [Array<String>]
def input
  # @type [File]
  file = File.new("../input/day10.txt")
  # file = File.new('test.txt')
  file.readlines.map(&:strip) # remove whitespace at the end
end

# Point is a 2d coord
# Point  {{{
class Point
  attr_accessor :x, :y

  def initialize(x, y)
    @x = x
    @y = y
  end

  # Retruns the direct 90° neighbours
  # @returns [Array<Point>]
  def nbs
    [Point.new(@x - 1, @y),
     Point.new(@x + 1, @y),
     Point.new(@x, @y - 1),
     Point.new(@x, @y + 1)]
  end

  def hash
    [x, y].hash
  end

  def equal?(other)
    @x == other.x && @y == other.y
  end

  def eql?(other)
    @x == other.x && @y == other.y
  end

  def to_s
    "P(#{@x}, #{@y})"
  end
end
# end }}}

# @param data [Array<String>]
# @return [Hash<Point, Integer>]
def parse(data)
  graph = {}
  zeroes = []

  data.each_index do |y|
    x = 0
    data[y].each_char do |c|
      pnt = Point.new(x, y)
      graph[pnt] = Integer(c)
      x += 1
      zeroes.append(pnt) if c == '0'
    end
  end

  [graph, zeroes]
end

# Finds all paths from initial to any pos with top height 9
class Pather
  def initialize(graph, initial)
    # @type [Hash<String, Integer>]
    @graph = graph
    # @type [Point]
    @initial = initial
    # @type [Set]
    @visited = Set.new
  end

  def paths_amount
    _paths_amount(@graph, @initial)
  end

  private

  def _paths_amount(graph, initial)
    # for some reason he returns / goes to duplicates,..
    return 1 if graph[initial] == 9

    nbs = higher_nbs(graph, initial)
    sum = 0
    nbs.each do |nb|
      @visited.add(nb)
      sum += _paths_amount(graph, nb)
    end
    sum
  end

  def higher_nbs(graph, pnt)
    nbs = pnt.nbs
    ret = []
    nbs.each do |nb|
      ret.append(nb) if !graph[nb].nil? && graph[nb] == (graph[pnt] + 1) && !@visited.include?(nb)
    end
    ret
  end
end

def solve_a(graph, zeroes)
  sum = 0
  zeroes.each { |z| sum += Pather.new(graph, z).paths_amount }
  sum
end

def main
  graph, zs = parse(input)
  puts solve_a(graph, zs)
end

main
