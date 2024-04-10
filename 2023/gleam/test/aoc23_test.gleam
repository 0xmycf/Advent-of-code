import gleeunit
import gleeunit/should
import aoc23
import gleam/dict
import gleam/result
import gleam/option.{None, Some}
import gleam/list
import lib/range

pub fn main() {
  gleeunit.main()
}

// gleeunit test functions end in `_test`
pub fn parse_map_test() {
  let str = "fertilizer-to-water map:"
  let expected = Some(#("fertilizer", "water"))
  str
  |> aoc23.parse_map_name
  |> should.equal(expected)
}

pub fn parse_mapper_test() {
  let str = "0 15 37"
  let expected = Some(aoc23.Map(0, 15, 37))
  str
  |> aoc23.parse_mapper
  |> should.equal(expected)
}

pub fn parse_seeds_test() {
  let str = "seeds: 79 14 55 13"
  let expected = Some([79, 14, 55, 13])
  str
  |> aoc23.parse_seed
  |> should.equal(expected)
}

const test_data = "seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"

pub fn parse_input_len_test() {
  let #(_, aoc23.Input(map)) = aoc23.parse(test_data)
  map
  |> dict.size
  |> should.equal(7)
}

pub fn parse_input_first_map_len_test() {
  let #(_, aoc23.Input(map)) = aoc23.parse(test_data)
  let key = #("seed", "soil")
  map
  |> dict.get(key)
  |> result.unwrap([])
  |> list.length
  |> should.equal(2)
}

pub fn parse_input_first_map_values_test() {
  let #(_, aoc23.Input(map)) = aoc23.parse(test_data)
  let key = #("seed", "soil")
  let values = [aoc23.Map(50, 98, 2), aoc23.Map(52, 50, 48)]
  map
  |> dict.get(key)
  |> result.unwrap([])
  |> should.equal(values)
}

pub fn parse_input_seeds_1_test() {
  let #(seeds, _) = aoc23.parse(test_data)
  seeds
  |> list.length
  |> should.equal(4)
}

pub fn parse_input_seeds_2_test() {
  let #(seeds, _) = aoc23.parse(test_data)
  seeds
  |> should.equal([79, 14, 55, 13])
}

pub fn mapped_value_not_inside_test() {
  let mapper = aoc23.Map(50, 98, 2)
  let value = 55
  aoc23.mapped_value(mapper, value)
  |> should.equal(None)
}

pub fn mapped_value_2_inside_test() {
  let mapper = aoc23.Map(52, 50, 48)
  //           2 `to` 50
  // 55 - 50 = 5
  //
  // 52 + (5 - 2) = 55 
  let value = 55
  aoc23.mapped_value(mapper, value)
  |> should.equal(Some(57))
}

pub fn mapped_range_same_no_inside_test() {
  let mapper = aoc23.Map(50, 98, 2)
  let value = range.singleton(55)
  aoc23.mapped_range(mapper, value)
  |> should.equal(range.singleton(55))
}

pub fn mapped_range_different_no_inside_test() {
  let mapper = aoc23.Map(50, 98, 2)
  let value = range.new_left(55, 57)
  aoc23.mapped_range(mapper, value)
  |> should.equal(range.new_left(55, 57))
}

pub fn mapped_range_2_same_inside_test() {
  let mapper = aoc23.Map(52, 50, 48)
  //           2 `to` 50
  // 55 - 50 = 5
  //
  // 52 + (5 - 2) = 55 
  let value = range.singleton(55)
  aoc23.mapped_range(mapper, value)
  |> should.equal(range.singleton(57))
}

pub fn mapped_range_2_different_inside_test() {
  let mapper = aoc23.Map(52, 50, 48)
  //           2 `to` 50
  // 55 - 50 = 5
  //
  // 52 + (5 - 2) = 55 
  let value = range.new_left(55, 58)
  aoc23.mapped_range(mapper, value)
  |> should.equal(range.new_left(57, 60))
}


pub fn range_test() {
  let foo = [0, 1, 2, 3, 4, 5]
  list.range(0, 5)
  |> should.equal(foo)
}

pub fn parts_local_test() {
  let one = range.new_both(79, 92)
  let two = aoc23.Map(52, 50, 48)
  let ps = range.parts(one, range.new_left(50, 50 + 48))
  aoc23.parts(one, two)
  |> should.be_ok
  |> should.equal(ps |> should.be_ok)
}
