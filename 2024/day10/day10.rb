#! /usr/bin/env ruby
# frozen_string_literal: true

# @return [Array<String>]
def input
  # @type [File]
  file = File.new('../input/day10.txt')
  # file = File.new('test.txt')
  file.readlines.map(&:strip) # remove whitespace at the end
end

# 🐒
class Complex
  def nbs
    [
      0 + 1i,
      0 - 1i,
      1 + 0i,
      -1 + 0i
    ].map { |x| self + x }
  end
end

# @param data [Array<String>]
# @return [Hash<Complex, Integer>]
def parse(data)
  graph = {}
  zeroes = []

  data.each_index do |y_coord|
    x_coord = 0
    data[y_coord].each_char do |c|
      pnt = Complex(x_coord, y_coord)
      graph[pnt] = Integer(c)
      x_coord += 1
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
    # @type [Complex]
    @initial = initial
    # @type [Set]
    @visited = Set.new
  end

  def paths_amount
    _paths_amount(@graph, @initial)
  end

  private

  def add_visited(pnt)
    @visited.add pnt
  end

  def _paths_amount(graph, initial)
    # for some reason he returns / goes to duplicates,..
    return 1 if graph[initial] == 9

    nbs = higher_nbs(graph, initial)
    sum = 0
    nbs.each do |nb|
      add_visited(nb)
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

# Pather but ignores the visitied points
# this was my inital Solution for part 1
class PatherB < Pather
  def add_visited(pnt); end
end

def solve(mkpather, graph, zeroes)
  sum = 0
  zeroes.each { |z| sum += mkpather.call(graph, z).paths_amount }
  sum
end

def main
  graph, zs = parse(input)
  puts solve(->(g, z) { Pather.new(g, z) }, graph, zs)
  puts solve(->(g, z) { PatherB.new(g, z) }, graph, zs)
end

main
