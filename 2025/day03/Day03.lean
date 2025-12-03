

namespace Day03

def test_input := 
"987654321111111
811111111111119
234234234234278
818181911112111".trim

def get_input ( path : Option String ) : IO (List String) :=
  List.filter (¬ ·.isEmpty) <$> match path with
  | some p => String.splitOn (sep := "\n") <$> IO.FS.readFile p
  | none   => pure $ test_input.splitOn "\n"

def parse (lines : List String) : List (List Nat) :=
  lines.map (fun line => line.toList.map (fun c => c.toNat - '0'.toNat))

abbrev Input := List (List Nat)

def solve_one_input (cs : List Nat) : Nat :=
  Id.run do
    let max₁ := cs.max?.get!
    let idx_max₁ := cs.findIdx (· = max₁)
    if idx_max₁ = cs.length - 1 then -- if its the last element
      let max₂ := cs.foldl (fun acc v => if v < max₁ ∧ v > acc then v else acc) 0
      max₂ * 10 + max₁
    else 
      let res := cs.drop (idx_max₁ + 1)
      let max₂ := res.max?.get!
      max₁ * 10 + max₂

def solve_part_a (input : Input) : Nat :=
  (input.map solve_one_input).sum

def main : IO Unit := do
  -- let input ← parse <$> get_input none
  let input ← parse <$> get_input (some "../input/day3.txt")
  IO.println s!"Part A: {solve_part_a input}"

end Day03

def main := Day03.main
