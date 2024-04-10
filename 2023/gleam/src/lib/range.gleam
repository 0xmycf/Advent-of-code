//// Describes integer ranges of values, such as [120, 123] or (-420, 2939591]
//// Differente functions are exposed to create ranges

import gleam/list
import gleam/int

/// The basic range type, you can construct one
/// using one of the other smart constructors
///
/// This is always inclusive on both sides
pub opaque type Range {
  Range(from: Int, to: Int)
}

/// it doesnt matter if from is greater than to, the range will be created correctly,
/// i.e. Range(from, to, _) from will always be from <= to.
fn new(from from: Int, to to: Int) -> Range {
  case from <= to {
    True -> Range(from, to)
    False -> Range(to, from)
  }
}

/// left inclusive, right exclusive
pub fn new_left(from from: Int, to to: Int) -> Range {
  new(from, to)
  |> map_to(fn(x) { x - 1 })
}

// right inclusive, left exclusive
pub fn new_right(from from: Int, to to: Int) -> Range {
  new(from, to)
  |> map_from(fn(x) { x + 1 })
}

/// both inclusive
pub fn new_both(from from: Int, to to: Int) -> Range {
  new(from, to)
}

/// none inclusive
pub fn new_none(from from: Int, to to: Int) -> Range {
  new(from, to)
  |> map_from(fn(x) { x + 1 })
  |> map_to(fn(x) { x - 1 })
}

pub fn singleton(value: Int) -> Range {
  new(value, value)
}

pub fn bounds(r: Range) -> #(Int, Int) {
  #(r.from, r.to)
}

/// Left  [from, to)
///
/// Right (from, to]
///
/// Both  [from ,to]
///
/// None  (from, to)
pub type Inclusive {
  Left
  Right
  Both
  None
}

// creats a list of integers from the given range
pub fn to_list(r: Range) -> List(Int) {
  list.range(r.from, r.to)
}

pub fn contains(r: Range, value: Int) -> Bool {
  r.from <= value && value <= r.to
}

/// checks if the first range is a subset of the second
pub fn subset(r1: Range, subset_of r2: Range) -> Bool {
  contains(r2, r1.from) && contains(r2, r1.to)
}

pub type Overlap {
  NoOverlap
  RightOverlap
  LeftOverlap
  BothOverlap
  LeftAdjacent
  RightAdjacent
}

/// checks if the two ranges overlap, the result can be interpreted as r2 'overlap_value' r1
///
/// if there is no overlap, it returns `NoOverlap`
///
/// if r2 is completely contained in r1, it returns `BothOverlap`
///
/// if r2 is partially contained, but has a higher bound than r1, it returns `RightOverlap`
///
/// if r2 is partially contained, but has a lower bound than r1, it returns `LeftOverlap`
///
/// if r2 starts where r1 ends it returns `LeftAdjacent`
///
/// if r2 ends where r1 starts it returns `RightAdjacent`
pub fn overlap(r1: Range, r2: Range) -> Overlap {
  case r1.from == r2.to, r1.to == r2.from {
    True, _ -> LeftAdjacent
    _, True -> RightAdjacent
    _, _ ->
      case contains(r1, r2.from), contains(r1, r2.to) {
        True, True -> BothOverlap
        True, False -> RightOverlap
        False, True -> LeftOverlap
        False, False -> {
          case contains(r2, r1.from), contains(r2, r1.to) {
            True, True -> BothOverlap
            True, False -> RightOverlap
            False, True -> LeftOverlap
            False, False -> NoOverlap
          }
        }
      }
  }
}

pub type RangeError {
  NotInBounds(Int)
  RangesDontOverlap
}

/// if include is false, this function will return new ranges that are inclusive of up to (at - 1)
/// instead of Left/Right/None with at as the bound
pub fn split(
  r1,
  at at: Int,
  should_include inclusive: Bool,
) -> Result(#(Range, Range), RangeError) {
  let offset = case inclusive {
    True -> 0
    False -> 1
  }
  case contains(r1, at) {
    True -> Ok(#(new(r1.from, at - offset), new(at + offset, r1.to)))
    False -> Error(NotInBounds(at))
  }
}

/// will merge the tow ranges, even if there is no overlap
pub fn merge(r1: Range, r2: Range) -> Range {
  case r1.from <= r2.from, r1.to >= r2.to {
    True, True -> r1
    False, False -> r2
    True, False -> new(r1.from, r2.to)
    False, True -> new(r2.from, r1.to)
  }
}

fn filter_same_bounds(r: Range) -> Bool {
  r.from != r.to
}

pub type PartsResult =
  Result(List(Range), RangeError)

// returns a list of subranges defined by the bounds of the two given ranges
// each subrange is inclusive on the left and exclusive on the right,
// except the last list, which is inclusive on both side
pub fn parts(r1: Range, r2: Range) -> Result(List(Range), RangeError) {
  case overlap(r1, r2) {
    NoOverlap -> Error(RangesDontOverlap)
    LeftAdjacent | LeftOverlap -> parts(r2, r1)
    RightAdjacent -> Ok([new_left(r1.from, r1.to), r2])
    BothOverlap | RightOverlap -> {
      let #(from1, to1) = bounds(r1)
      let #(from2, to2) = bounds(r2)
      let offset = case to1 == to2 {
        True -> 0
        False -> 1
      }
      [
        from_to_range(from1, from2),
        new(int.min(to1, to2), int.max(from1, from2))
          |> map_to(fn(x) { x - offset }),
        new(to2, to1),
      ]
      |> list.filter(filter_same_bounds)
      |> Ok
    }
    // RightOverlap -> {
    //   let #(from1, to1) = bounds(r1)
    //   let #(from2, to2) = bounds(r2)
    //   [
    //     new(from1, from2)
    //       |> map_to(fn(x) { x - 1 }),
    //     new(from2, to1)
    //       |> map_to(fn(x) { x - 1 }),
    //     new(to1, to2),
    //   ]
    //   |> list.filter(filter_same_bounds)
    //   |> Ok
    // }
  }
}

pub fn map_from(r: Range, fun) -> Range {
  new(fun(r.from), r.to)
}

pub fn map_to(r: Range, fun) -> Range {
  new(r.from, fun(r.to))
}

fn from_to_range(from1, from2) -> Range {
  case from1 == from2 {
    True -> new_both(from1, from2)
    False -> new_left(from1, from2)
  }
}
