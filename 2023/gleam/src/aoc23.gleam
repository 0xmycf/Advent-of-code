//// this has been fun but doesnt work right now

import gleam/io
import gleam/string
import gleam/int
import gleam/list
import gleam/option.{type Option, None, Some}
import gleam/result.{try}
import gleam/dict.{type Dict}

pub type FileError {
  Enoent
}

@external(erlang, "file", "read_file")
fn read_file(path: String) -> Result(String, FileError)

pub fn main() {
  let either_content = read_file("./input/day5.txt")
  case either_content {
    Ok(content) -> {
      let #(seeds, Input(map_to_list)) = parse(content)
      let lowest_num = resolve(seeds, map_to_list)
      io.println("Lowest number is: " <> from_max(lowest_num))
    }
    Error(_) -> io.println("Error reading file")
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

type Max {
  Inf
  Num(Int)
}

fn from_max(max: Max) -> String {
  case max {
    Inf -> "Inf"
    Num(int) -> int.to_string(int)
  }
}

fn resolve(seeds, map) -> Max {
  use total, seed <- list.fold(seeds, Inf)
  let new = {
    use last_seed, key <- list.fold(oh_no, seed)
    // advent of code guarantees that the key is in the map
    let assert Ok(maps) = dict.get(map, key)
    use last, map <- list.fold(maps, last_seed)
    mapped_value(map, last_seed)
    |> option.unwrap(last)
  }
  case total {
    Inf -> new
    Num(int) -> int.min(int, new)
  }
  |> Num
}

pub type Mapper {
  Map(dst: Int, src: Int, range: Int)
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
    // |> Some
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
