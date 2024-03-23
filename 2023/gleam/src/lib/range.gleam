//// Describes integer ranges of values, such as [120, 123] or (-420, 2939591]
//// Differente functions are exposed to create ranges

import gleam/list

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
  new(from, to - 1)
}

// right inclusive, left exclusive
pub fn new_right(from from: Int, to to: Int) -> Range {
  new(from + 1, to)
}

/// both inclusive
pub fn new_both(from from: Int, to to: Int) -> Range {
  new(from, to)
}

/// none inclusive
pub fn new_none(from from: Int, to to: Int) -> Range {
  new(from + 1, to - 1)
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
  Overlap(Inclusive)
}

/// checks if the two ranges overlap
///
/// if there is no overlap, it returns `NoOverlap`
/// if r2 is completely contained in r1, it returns `Overlap(Both)`
/// if r2 is partially contained, but has a higher bound than r1, it returns `Overlap(Right)`
/// if r2 is partially contained, but has a lower bound than r1, it returns `Overlap(Left)`
pub fn overlap(r1: Range, r2: Range) -> Overlap {
  case contains(r1, r2.from), contains(r1, r2.to) {
    True, True -> Overlap(Both)
    True, False -> Overlap(Right)
    False, True -> Overlap(Left)
    False, False -> NoOverlap
  }
}

pub type RangeError {
  NotInBounds(Int)
}

// this needs testing
/// if include is false, this function will return new ranges that are inclusive of up to (at - 1)
/// instead of Left/Right/None with at as the bound
pub fn split(
  r1,
  at at: Int,
  should_include inclusive: Bool,
) -> Result(#(Range, Range), RangeError) {
  let bound = case inclusive {
    True -> at
    False -> at - 1
  }
  case contains(r1, at) {
    True -> Ok(#(new(r1.from, bound), new(bound, r1.to)))
    False -> Error(NotInBounds(at))
  }
}


// TODO needs testing
/// will merge the tow ranges, even if there is no overlap
pub fn merge(r1: Range, r2: Range) -> Range {
  case r1.from <= r2.from, r1.to >= r2.to {
    True, True -> r1
    False, False -> r2
    True, False -> new(r1.from, r2.to)
    False, True -> new(r2.from, r1.to)
  }
}
