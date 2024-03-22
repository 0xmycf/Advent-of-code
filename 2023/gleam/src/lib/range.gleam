//// Describes integer ranges of values, such as [120, 123] or (-420, 2939591]
//// Differente functions are exposed to create ranges

import gleam/list

/// The basic range type, you can create one using the constructor
/// or one of the other smart constructors
pub type Range {
  Range(from: Int, to: Int, inclusive: Inclusive)
}

/// it doesnt matter if from is greater than to, the range will be created correctly,
/// i.e. Range(from, to, _) from will always be from <= to.
pub fn new(from from: Int, to to: Int, inclusive inclusive: Inclusive) -> Range {
  case from <= to {
    True -> Range(from, to, inclusive)
    False -> Range(to, from, inclusive)
  }
}

pub fn new_left(from from: Int, to to: Int) -> Range {
  new(from, to, Left)
}

pub fn new_right(from from: Int, to to: Int) -> Range {
  new(from, to, Right)
}

pub fn new_both(from from: Int, to to: Int) -> Range {
  new(from, to, Both)
}

pub fn new_none(from from: Int, to to: Int) -> Range {
  new(from, to, None)
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
  case r.inclusive {
    Left -> list.range(r.from, r.to - 1)
    Right -> list.range(r.from - 1, r.to)
    Both -> list.range(r.from, r.to)
    None -> list.range(r.from - 1, r.to - 1)
  }
}

pub fn contains(r: Range, value: Int) -> Bool {
  case r.inclusive {
    Left -> r.from <= value && value < r.to
    Right -> r.from < value && value <= r.to
    Both -> r.from <= value && value <= r.to
    None -> r.from < value && value < r.to
  }
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
  let #(from, to) = case r2.inclusive {
    Left -> #(r2.from, r2.to - 1)
    Right -> #(r2.from + 1, r2.to)
    Both -> #(r2.from, r2.to)
    None -> #(r2.from + 1, r2.to - 1)
  }
  case contains(r1, from), contains(r1, to) {
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
    True -> {
      case r1.inclusive {
        Left -> #(new_both(r1.from, bound), new_left(bound, r1.to))
        Right -> #(new_right(r1.from, bound), new_both(bound, r1.to))
        Both -> #(new_both(r1.from, bound), new_both(bound, r1.to))
        None -> #(new_right(r1.from, bound), new_left(bound, r1.to))
      }
      |> Ok
    }
    False -> Error(NotInBounds(at))
  }
}
//

// /// given that the two ranges overlap, it will return the intersection
// /// TODO this does not work, as Overlap(Left) will return the wrong values
// fn new_inc(r1: Range, r2: Range) -> Inclusive {
//   case r1.inclusive, r2.inclusive {
//     Left, Left -> Left
//     Left, Right -> Both
//     Left, Both -> Both
//     Left, None -> Left
// //
//     Right, Left -> None
//     Right, Right -> Right
//     Right, Both -> Right
//     Right, None -> None
// //
//     Both, Left -> Left
//     Both, Right -> Both
//     Both, Both -> Both
//     Both, None -> Left
// //
//     None, Left -> None
//     None, Right -> Right
//     None, Both -> Right
//     None, None -> None
//   }
// }

// /// will merge the tow ranges, even if there is no overlap
// pub fn merge(r1: Range, r2: Range) -> Range {
//   case overlap(r1, r2) {
//     NoOverlap -> new(r1.from, r2.to, Both)
//     Overlap(Both) -> todo
//     Overlap(_) -> {
//       todo
//     }
//   }
//   // new(from, to, inclusive)
//   todo
// }
