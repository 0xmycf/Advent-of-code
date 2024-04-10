//// this has been fun but doesnt work right now

import gleam/io
import gleam/string
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{try}
import gleam/dict.{type Dict}
import lib/range.{type Range}
import argv

pub type FileError {
  Enoent
}

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(String, FileError)

pub type Part {
  A
  B
}

pub fn main() {
  let args = argv.load().arguments
  let part = case args {
    [part] -> {
      case part {
        "1" | "a" | "A" -> A
        "2" | "b" | "B" -> B
        _ -> panic as "Invalid part"
      }
    }
    _ -> panic as "No part provided"
  }
  let either_content = read_file("./input/test/day5.txt")
  case either_content, part {
    Ok(content), part -> {
      let #(seeds, Input(map_to_list)) = parse(content)
      let lowest_num = resolve(part, seeds, map_to_list)
      io.println("Lowest number is: " <> min_to_string(lowest_num))
    }
    Error(_), _ -> io.println("Error reading file")
  }
}

const oh_no = [
  #("seed", "soil"),
  #("soil", "fertilizer"),
  #("fertilizer", "water"),
  #("water", "light"),
  #("light", "temperature"),
  #("temperature", "humidity"),
  #("humidity", "location"),
]

type Min {
  Inf
  Num(Int)
}

fn min(m: Min, b: Int) {
  case m {
    Inf -> Num(b)
    Num(a) -> Num(int.min(a, b))
  }
}

fn min_to_string(min: Min) -> String {
  case min {
    Inf -> "Inf"
    Num(int) -> int.to_string(int)
  }
}

fn resolve(part, seeds, map: Dict(#(String, String), List(Mapper))) -> Min {
  case part {
    A -> resolve_a(seeds, map)
    B -> resolve_b(seeds, map)
  }
}

fn resolve_a(seeds, map) -> Min {
  use total, seed <- list.fold(seeds, Inf)
  let new = {
    use last_seed, key <- list.fold(oh_no, seed)
    // advent of code guarantees that the key is in the map
    let assert Ok(maps) = dict.get(map, key)
    use last, map <- list.fold(maps, last_seed)
    // either gets a new value or returns the same value unchanged
    mapped_value(map, last_seed)
    |> option.unwrap(last)
  }
  case total {
    Inf -> new
    Num(int) -> int.min(int, new)
  }
  |> Num
}

fn resolve_b(seeds, map) {
  inner_b(seeds, map)
  |> min_rs
}

fn min_rs(ranges: List(Range)) -> Min {
  use acc, value <- list.fold(ranges, Inf)
  let #(from, to) = range.bounds(value)
  min(acc, from)
  |> min(to)
}

fn inner_b(seeds, map) -> List(Range) {
  // if its not none there is something wrong 
  let assert #(b_seeds, None) =
    list.fold(seeds, #([], None), fn(acc, seed) {
      case acc {
        #(xs, None) -> #(xs, Some(seed))
        #(xs, Some(value)) -> #(
          [range.new_left(value, seed + value), ..xs],
          None,
        )
      }
    })
  let input =
    [
      list.at(
        b_seeds
          |> list.reverse,
        0,
      )
      |> result.unwrap(range.singleton(-1_202_303_404)),
    ]
    |> io.debug
  use seed_acc, seed_value <- list.fold(input, [])
  let ranges = map_seed(seed_value, map)
  list.append(ranges, seed_acc)
}

/// returns the new ranges after they've been mapped 
fn map_seed(
  seed: Range,
  map: Dict(#(String, String), List(Mapper)),
) -> List(Range) {
  use acc, key <- list.fold(oh_no, [seed])
  let assert Ok(maps) = dict.get(map, key)
  io.debug(#(key, maps, acc))
  let foo = {
    use value <- list.flat_map(acc)
    io.debug(#("calling resolve_maps with", value))
    resolve_maps(maps, value)
  }
  io.debug(#("foo:", foo))
  io.debug("---")
  foo
}

fn resolve_maps(maps: List(Mapper), seed: Range) -> List(Range) {
  use acc, map <- list.fold(maps, [seed])
  io.debug(#("iniside resolve_maps", "acc", acc, "map", map))
  use value <- list.flat_map(acc)
  io.debug(#("using value", value))
  let ps =
    parts(seed, map)
    |> result.unwrap([])
    |> io.debug
    |> list.map(mapped_range(map, _))
  case ps == [] {
    True -> [value]
    False -> ps
  }
}

pub fn parts(range: Range, map: Mapper) -> range.PartsResult {
  let map_range = range.new_left(map.src, map.src + map.range)
  case range.overlap(range, map_range) {
    range.BothOverlap -> {
      range.parts(
        range,
        map_range
          |> range.map_to(fn(x) { x + 1 }),
      )
    }
    _ -> range.parts(range, map_range)
  }
}

pub type Mapper {
  Map(dst: Int, src: Int, range: Int)
}

pub fn mapped_range(mapper: Mapper, r: Range) -> Range {
  let #(from, to) = range.bounds(r)
  let from1 =
    mapped_value(mapper, from)
    |> option.unwrap(from)
  let to1 =
    mapped_value(mapper, to)
    |> option.unwrap(to)
  range.new_both(from1, to1)
}

pub fn mapped_value(mapper: Mapper, value: Int) -> Option(Int) {
  let Map(dst, src, range) = mapper
  case value >= src && value <= src + range - 1 {
    True -> {
      let diff =
        src - value
        |> int.absolute_value
      dst + diff
      |> Some
    }
    False -> None
  }
}

pub type Input {
  Input(maps: Dict(#(String, String), List(Mapper)))
}

type Seed =
  Int

pub fn parse(content: String) -> #(List(Seed), Input) {
  let lines = string.split(content, "\n")
  let assert Some(seeds) =
    list.at(lines, 0)
    |> result.unwrap("")
    |> parse_seed

  let maps =
    list.drop(lines, 1)
    |> parse_maps_and_values

  #(seeds, maps)
}

type Go {
  Go(key: Option(#(From, To)), lst: Input)
}

fn parse_maps_and_values(lines) -> Input {
  let Go(_, in) = helper_parse_maps_and_values(lines)
  in
}

// how to NOT do it
fn helper_parse_maps_and_values(lines) -> Go {
  use acc, line <- list.fold(lines, Go(None, Input(maps: dict.new())))
  let maybe_map_name = parse_map_name(line)
  let maybe_mapper = parse_mapper(line)
  case maybe_map_name, maybe_mapper {
    Some(_) as some, None -> {
      let Go(_, input) = acc
      Go(some, input)
    }
    None, Some(mapper) -> {
      let Go(maybe_key, input) = acc
      case maybe_key {
        None -> acc
        Some(key) -> {
          let new_input = {
            use maybe_value <- dict.update(input.maps, key)
            case maybe_value {
              None -> [mapper]
              Some(value) -> list.append(value, [mapper])
            }
          }
          Go(maybe_key, Input(maps: new_input))
        }
      }
    }
    _, _ -> acc
  }
}

pub fn parse_seed(content: String) -> Option(List(Seed)) {
  case string.contains(content, "seeds: ") {
    False -> None
    True -> {
      let seed =
        string.split(content, "seeds: ")
        |> list.drop(1)
        |> list.first

      let ret = {
        use str <- result.map(seed)
        let seeds = string.split(str, " ")
        use seed <- list.filter_map(seeds)
        int.parse(seed)
      }
      option.from_result(ret)
    }
  }
}

type From =
  String

type To =
  String

pub fn parse_map_name(content: String) -> Option(#(From, To)) {
  let parts = string.split(content, "-")
  case parts {
    [from, "to", rest] -> {
      let to = string.drop_right(rest, string.length(" map:"))
      Some(#(from, to))
    }
    _ -> None
  }
}

pub fn parse_mapper(content: String) -> Option(Mapper) {
  let parts = string.split(content, " ")
  case parts {
    [dst, src, by] -> {
      let ret = {
        use destination <- try(int.parse(dst))
        use souce <- try(int.parse(src))
        use range <- result.map(int.parse(by))
        Map(destination, souce, range)
      }
      option.from_result(ret)
    }
    _ -> None
  }
}
