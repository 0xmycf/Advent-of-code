import gleeunit/should
import lib/range

pub fn new_test() {
  let expected = range.Range(0, 10, range.Both)
  let r = range.new(0, 10, range.Both)
  should.equal(r, expected)
}

pub fn new_flipped_test() {
  let expected = range.Range(0, 10, range.Both)
  let r = range.new(10, 0, range.Both)
  should.equal(r, expected)
}


pub fn new_same_test() {
  let expected = range.Range(0, 0, range.Both)
  let r = range.new(0, 0, range.Both)
  should.equal(r, expected)
}

pub fn overlap_no_overlap_test() {
  let expected = range.NoOverlap
  let r1 = range.new(0, 10, range.Both)
  let r2 = range.new(11, 20, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_right_test() {
  let expected = range.Overlap(range.Right)
  let r1 = range.new(0, 10, range.Both)
  let r2 = range.new(5, 20, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_right_extreme_right_test() {
  let expected = range.Overlap(range.Right)
  let r1 = range.new(0, 10, range.Both)
  let r2 = range.new(10, 20, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// this should not overlap
pub fn overlap_right_extreme_right_false_test() {
  let expected = range.NoOverlap
  let r1 = range.new(0, 10, range.Both)
  let r2 = range.new(10, 20, range.Right)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// these should not overlap
pub fn overlap_right_extreme_right_false2_test() {
  let expected = range.NoOverlap
  let r1 = range.new(0, 10, range.Left)
  let r2 = range.new(10, 20, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_left_test() {
  let expected = range.Overlap(range.Left)
  let r1 = range.new(0, 10, range.Both)
  let r2 = range.new(-5, 5, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_left_extreme_left_test() {
  let expected = range.Overlap(range.Left)
  let r1 = range.new(20, 200, range.Both)
  let r2 = range.new(10, 20, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// this should not overlap
pub fn overlap_left_extreme_left_false_test() {
  let expected = range.NoOverlap
  let r1 = range.new(20, 200, range.Right)
  let r2 = range.new(10, 20, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

// these should not overlap
pub fn overlap_left_extreme_left_false2_test() {
  let expected = range.NoOverlap
  let r1 = range.new(20, 200, range.Right)
  let r2 = range.new(10, 20, range.Left)
  range.overlap(r1, r2)
  |> should.equal(expected)
}

pub fn overlap_both_test() {
  let expected = range.Overlap(range.Both)
  let r1 = range.new(-20, 20, range.Both)
  let r2 = range.new(5, 15, range.Both)
  range.overlap(r1, r2)
  |> should.equal(expected)
}
