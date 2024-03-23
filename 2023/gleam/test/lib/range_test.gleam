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

pub fn new_neg_test() {
  let expected = list.range(-120, -10)
  let r = range.new_both(-120, -10)
  range.to_list(r)
  |> should.equal(expected)
}

pub fn new_neg_flipped_test() {
  let expected = list.range(-120, -10)
  let r = range.new_both(-10, -120)
  range.to_list(r)
  |> should.equal(expected)
}

pub fn new_mixed_test() {
  let expected = list.range(-120, 500)
  let r = range.new_both(-120, 500)
  range.to_list(r)
  |> should.equal(expected)
}

pub fn new_mixed_flipped_test() {
  let expected = list.range(-120, 500)
  let r = range.new_both(500, -120)
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

//
// Test subset
//

pub fn subset_true_same_test() {
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(0, 10)
  range.subset(r1, subset_of: r2)
  |> should.be_true
}

pub fn subset_true_true_test() {
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(-100, 100)
  range.subset(r1, subset_of: r2)
  |> should.be_true
}

pub fn subset_false_true_but_arguments_are_flipped_test() {
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(-100, 100)
  range.subset(r2, subset_of: r1)
  |> should.be_false
}

pub fn subset_false_test() {
  let r1 = range.new_both(200, 10_000)
  let r2 = range.new_both(-123332131, -1)
  range.subset(r1, subset_of: r2)
  |> should.be_false
}

//
// Test split
//

pub fn split_at_inside_test() {
  let where = 10
  let expected =
    #(range.new_both(0, 10), range.new_both(10, 20))
  let r1 = range.new_both(0, 20)
  range.split(r1, at: where, should_include: True)
  |> should.be_ok
  |> should.equal(expected)
}

pub fn split_at_outside_test() {
  let lower = 0
  let where = lower - 10_000
  let expected_error = range.NotInBounds(where)
  let r1 = range.new_both(lower, 20)
  range.split(r1, at: where, should_include: True)
  |> should.be_error
  |> should.equal(expected_error)
}

pub fn split_at_inside_and_not_inclusive_test() {
  let where = 10
  let expected =
    #(range.new_both(0, 9), range.new_both(11, 20))
  let r1 = range.new_both(0, 20)
  range.split(r1, at: where, should_include: False)
  |> should.be_ok
  |> should.equal(expected)
}

//
// Test merge
//

pub fn merge_no_overlap_test() {
  let expected = range.new_both(0, 20)
  let r1 = range.new_both(0, 10)
  let r2 = range.new_both(11, 20)
  range.merge(r1, r2)
  |> should.equal(expected)
}

pub fn merge_overlap_test() {
  let expected = range.new_both(0, 20)
  let r1 = range.new_both(0, 15)
  let r2 = range.new_both(5, 20)
  range.merge(r1, r2)
  |> should.equal(expected)
}

pub fn merge_r1_is_true_subset_test() {
  let r1 = range.new_both(5, 10)
  let r2 = range.new_both(0, 20)
  range.merge(r1, r2)
  |> should.equal(r2)
}

pub fn merge_r2_is_true_subset_test() {
  let r1 = range.new_both(0, 20)
  let r2 = range.new_both(5, 10)
  range.merge(r1, r2)
  |> should.equal(r1)
}
