import gleeunit/should
import lib/range
import gleam/list

//
// new_both tests
//

pub fn new_test() {
  let expected = list.range(0, 10)
  let r = range.new_both(0, 10)
  range.to_list(r)
  |> should.equal(expected)
}

pub fn new_flipped_test() {
  let expected = list.range(0, 10)
  let r = range.new_both(10, 0)
  range.to_list(r)
  |> should.equal(expected)
}

pub fn new_same_test() {
  let expected = list.range(0, 0)
  let r = range.new_both(0, 0)
  range.to_list(r)
  |> should.equal(expected)
}

//
// new_left tests
// new_right tests
// new_none tests
//

pub fn new_left_test() {
  let expected = list.range(0, 9)
  let r = range.new_left(0, 10)
  range.to_list(r)
  |> should.equal(expected)
}

pub fn new_right_test() {
  let expected = list.range(1, 10)
  let r = range.new_right(0, 10)
  range.to_list(r)
  |> should.equal(expected)
}

pub fn new_none_test() {
  let expected = list.range(1, 9)
  let r = range.new_none(0, 10)
  range.to_list(r)
  |> should.equal(expected)
}

//
// overlap tests
//

pub fn overlap_no_overlap_test() {
  let expected = range.NoOverlap
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(11, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_right_test() {
  let expected = range.Overlap(range.Right)
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(5, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_right_extreme_right_test() {
  let expected = range.Overlap(range.Right)
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(10, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// this should not overlap
pub fn overlap_right_extreme_right_false_test() {
  let expected = range.NoOverlap
  let r1 = range.new_both(0, 10)
  let r2 = range.new_right(10, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// these should not overlap
pub fn overlap_right_extreme_right_false2_test() {
  let expected = range.NoOverlap
  let r1 = range.new_left(0, 10)
  let r2 = range.new_both(10, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_left_test() {
  let expected = range.Overlap(range.Left)
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(-5, 5)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_left_extreme_left_test() {
  let expected = range.Overlap(range.Left)
  let r1 = range.new_both(20, 200)
  let r2 = range.new_both(10, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// this should not overlap
pub fn overlap_left_extreme_left_false_test() {
  let expected = range.NoOverlap
  let r1 = range.new_right(20, 200)
  let r2 = range.new_both(10, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// these should not overlap
pub fn overlap_left_extreme_left_false2_test() {
  let expected = range.NoOverlap
  let r1 = range.new_right(20, 200)
  let r2 = range.new_left(10, 20)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_both_test() {
  let expected = range.Overlap(range.Both)
  let r1 = range.new_both(-20, 20)
  let r2 = range.new_both(5, 15)
  range.overlap(r1, r2)
  |> should.equal(expected)
}
