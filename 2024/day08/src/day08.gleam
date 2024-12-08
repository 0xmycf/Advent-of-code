import gleam/dict.{type Dict}
import gleam/erlang/process
import gleam/int
import gleam/io
import gleam/list
import gleam/option.{None, Some}
import gleam/set.{type Set}
import gleam/string
import simplifile as fs

pub fn main() {
  // let #(file, height, width) = case fs.read("test.txt") {
  let #(file, height, width) = case fs.read("../input/day08.txt") {
    Error(err) -> panic as fs.describe_error(err)
    Ok(foo) -> {
      let split =
        string.split(foo, on: "\n")
        |> list.filter(fn(x) { !string.is_empty(x) })
      let assert Ok(head) = list.first(split)
      #(
        split
          |> parse(1),
        // |> dict.delete("."),
        list.length(split),
        string.length(head),
      )
    }
  }

  let assert Ok(dots) = dict.get(file, ".")
  let res =
    solve(
      file
        |> dict.delete("."),
      height,
      width,
      set.to_list(dots),
    )

  io.debug(res)
}

fn render_pos(pos, char) {
  let Pos(a, b) = pos
  io.print("\u{001b}[" <> int.to_string(b) <> ";" <> int.to_string(a) <> "H")
  io.print(char)
}

fn render(xs: List(Pos)) -> List(Pos) {
  use Pos(a, b) <- list.map(xs)
  io.print("\u{001b}[" <> int.to_string(b) <> ";" <> int.to_string(a) <> "H")
  io.print("\u{001b}[31mX\u{001b}[0m")
  Pos(a, b)
}

fn iter_till(with fun: fn(Int) -> b, from start: Int, till till: Int) -> List(b) {
  let r = fun(start)
  case start == till {
    True -> []
    False -> [r, ..{ iter_till(fun, start + 1, till) }]
  }
}

fn solve(file, height, width, dots) {
  io.print("\u{001b}[?1049h")

  list.map(dots, fn(x) { render_pos(x, ".") })

  // value : Set(Pos)
  // key: String (Char) (=> String.length == 1)
  let values = {
    use acc, #(key, value) <- list.fold(over: dict.to_list(file), from: [])
    list.map(set.to_list(value), fn(x) { render_pos(x, key) })
    let cross = cross_prod(set.to_list(value))
    let v =
      list.flat_map(cross, fn(x1) {
        let #(a, b) = x1
        case a == b {
          True -> []
          False -> {
            // let #(a, b) = new_points_b(a, b)
            // [a, b]
            new_points_b(a, b)
          }
        }
      })
    [v, ..acc]
  }

  let #(_, amnt) =
    values
    |> list.flatten()
    |> list.filter(fn(x) { in_bounds(x: x, height: height, width: width) })
    |> render()
    |> fn(x) { #(x, set.size(set.from_list(x))) }

  process.sleep(100 * 30)
  // halve a minute

  io.print("\u{001b}[?1049l")
  amnt
}

const coef: Int = 2

fn new_points_b(a: Pos, b: Pos) -> List(Pos) {
  let fst_diff = diff(a, b)
  // b --> a
  let snd_diff = diff(b, a)
  // a --> b
  // b + λ * a where λ = 2
  list.flatten([
    iter_till(fn(x) { do_thing(b, fst_diff, x) }, 0, 500),
    iter_till(fn(x) { do_thing(a, snd_diff, x) }, 0, 500),
  ])
}

// naming things is hard
// b + λ * a where λ = 2
fn do_thing(a, delta, lambda) {
  add(a, mult(delta, lambda))
}

// use for part a
pub fn new_points(a: Pos, b: Pos) -> #(Pos, Pos) {
  let fst_diff = diff(a, b)
  // b --> a
  let snd_diff = diff(b, a)
  // a --> b
  // b + λ * a where λ = 2
  let fst = add(b, mult(fst_diff, coef))
  let snd = add(a, mult(snd_diff, coef))
  #(fst, snd)
}

fn add(a: Pos, b: Pos) -> Pos {
  let Pos(a1, a2) = a
  let Pos(b1, b2) = b
  Pos(a1 + b1, a2 + b2)
}

fn mult(a: Pos, lambda: Int) -> Pos {
  let Pos(a1, a2) = a
  Pos(a1 * lambda, a2 * lambda)
}

fn diff(a: Pos, b: Pos) -> Pos {
  let Pos(a1, a2) = a
  let Pos(b1, b2) = b
  Pos(a1 - b1, a2 - b2)
}

///
/// same as xs >>= \x -> map (\y -> (x,y)) xs
fn cross_prod(xs: List(a)) -> List(#(a, a)) {
  use v <- list.flat_map(xs)
  use v2 <- list.map(xs)
  #(v, v2)
}

fn in_bounds(x pos: Pos, height height: Int, width width: Int) -> Bool {
  let Pos(col, row) = pos
  row >= 1 && row <= height && col >= 1 && col <= width
}

pub type Pos {
  /// x,y col, row, 1 indexed
  Pos(Int, Int)
}

fn parse(foo: List(String), height) -> Dict(String, Set(Pos)) {
  case foo {
    [] -> dict.new()
    [line, ..xs] -> {
      let foo = {
        let graphemes = string.to_graphemes(line)
        use acc, #(id, c) <- list.fold(
          over: list.zip(list.range(1, list.length(graphemes)), graphemes),
          from: dict.new(),
        )
        dict.upsert(in: acc, update: c, with: fn(x) {
          case x {
            None ->
              set.new()
              |> set.insert(Pos(id, height))
            Some(x) -> set.insert(x, Pos(id, height))
          }
        })
      }

      dict.combine(foo, parse(xs, height + 1), fn(a, b) { set.union(a, b) })
    }
  }
}
