
import Std.Data.HashMap

namespace Day04

def test_input := 
"..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@ some
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@."

structure Point where
  x : Int
  y : Int
deriving Repr, Hashable, BEq

instance : ToString Point where
  toString p := s!"({p.x},{p.y})"

instance : Add Point where
  add a b := Point.mk (a.x + b.x) (a.y + b.y)

instance : Sub Point where
  sub a b := Point.mk (a.x - b.x) (a.y - b.y)

-- (x,y) -> occupied?
abbrev Grid := Std.HashMap Point Bool

def is_occupied (g : Grid) (p : Point) : Bool :=
  match g.get? p with
  | some b => b
  | none   => false

def is_all_neighbours (p : Point) (ls : List Point) := 
    ls.all fun np => 
      let dx := Int.sub np.x p.x 
      let dy := Int.sub np.y p.y 
      (dx ≥ -1) ∧ (dx ≤ 1) ∧ (dy ≥ -1) ∧ (dy ≤ 1) ∧ ¬(dx = 0 ∧ dy = 0)

abbrev Neighbours (p : Point) := {  ls : List Point // ls.length = 8 ∧ is_all_neighbours p ls }

@[simp]
theorem zero_point_eq (p : Point) : (Point.mk 0 0) + p = p := by 
  change (Point.mk (0 + p.x) (0 + p.y)) = p
  simp

@[simp]
theorem zero_point_eq_other (p : Point) : p + (Point.mk 0 0)  = p := by 
  change (Point.mk (p.x + 0) (p.y + 0)) = p
  simp

-- theorem other_point_neq (p: Point) (q : Point) (h : q ≠ Point.mk 0 0) : q + p ≠ p := by
--   intro a
--   have := zero_point_eq p
--   rw [← this] at a
--   have h₁ : q.x + p.x = 0 :=
--     sorry
--   sorry


def get_all_neighbours (p : Point) : List Point :=
  let ls := [-1,0,1].flatMap (fun i => [-1,0,1].map (fun j => (Point.mk i j) + p)) |>.filter (· != p)
  ls

-- def part_a_pred (g : Grid) (p : Point) (ls : Neighbours p) :=
def part_a_pred (g : Grid) /- (p : Point) -/ (ls : List Point) : Bool :=
  (ls.filter (fun np => is_occupied g np) |>.length) < 4

def get_input (opt_path : Option String) : IO String := do
  match opt_path with
  | some path => IO.FS.readFile path -- 
  | none      => pure test_input

def parse (input : String) : Grid × (Nat × Nat) :=
  let lines := input.splitOn "\n" |>.filter (· ≠ "")
  let the_list := List.flatten $ lines.mapIdx (fun i v => v.trim.toList.mapIdx (fun j c => (Point.mk i j, c == '@')))
  let y := lines.length
  let x := lines.head!.trim.length
  (Std.HashMap.ofList (the_list.filter (fun tpl => tpl.2)), (x,y))

def calc_paper_one_step (input : Grid × (Nat × Nat)) : (Nat × List Point) := 
  let (g, (maxx, maxy)) := input
  let filter := (List.range maxx).flatMap (fun x =>
    (List.range maxy).filterMap (fun y => do
      match g.get? (Point.mk (Int.ofNat x) (Int.ofNat y)) with
      | none => none
      | some is_occupied => 
        if ¬ is_occupied then none else
        let nbs := get_all_neighbours (Point.mk (Int.ofNat x) (Int.ofNat y)) |>.filter (g.get? · |>.isSome)
        match part_a_pred g nbs with
        | true => some (Point.mk (Int.ofNat x) (Int.ofNat y))
        | false => none
    )
  )
  (filter.length, filter)

def solve_a (input : Grid × Nat × Nat) : Nat := 
    calc_paper_one_step input |>.1

def solve_b (input : Grid × (Nat × Nat)) : Nat := 
  let rec aux (g: Grid) (max_steps : Nat) :=
    let (new, new_pts) := calc_paper_one_step (g, input.2)
    if new = 0 ∨ max_steps = 0 then 0 else
      let new_g := g.filterMap (fun key value => if new_pts.contains key then none else some value)
      (aux new_g (max_steps - 1)) + new 
    termination_by max_steps
  aux input.1 100_000

def main : IO Unit := do
  let data_path := "../input/day4.txt"
  let s ← get_input (some data_path)
  -- let s ← get_input none
  let input := parse s
  println! s!"Day 04 Part A: {solve_a input}"
  println! s!"Day 04 Part B: {solve_b input}"

end Day04

def main := Day04.main
