

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

def parse_line (line : String) :=
  line.toList.map (fun c => c.toNat - '0'.toNat)

def parse (lines : List String) : List (List Nat) :=
  lines.map parse_line

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

def list_to_nat (ls : List Nat) : Nat :=
  let norm := ls.flatMap (fun i => if i > 10 then parse_line s!"{i}" else [i])
  let len := norm.length
  let (v, _) := norm.foldl (fun (num, times) v => (num + v * times, times / 10)) (0, 10 ^ (len - 1))
  v

def aux_next_highest (cs : List Nat) (max : Nat) : Nat := 
    cs.foldl (fun acc v => if v < max ∧ v > acc then v else acc) 0

-- max stesp to easily show termination
def find_next_highest (cs : List Nat) (hi : Nat) (n : Nat) (max_steps : Nat) : Nat :=
  if max_steps = 0 then 0 else
  let max := aux_next_highest cs hi
  let idx := cs.findIdx (· = max)
  if idx > cs.length - n then
    find_next_highest cs max n (max_steps - 1)
  else 
    max

def solve_one_step (cs : List Nat) (n : Nat) : Option ((List Nat) × Nat) :=
  if cs.isEmpty then none else
  some ( Id.run do
    let max := cs.max?.get!
    let idx_max := cs.findIdx (· = max)
    if idx_max = cs.length - n then -- if its the last element
      let num_as_list := cs.drop (idx_max)
      ([], list_to_nat num_as_list) 
    else if idx_max > cs.length - n then
      let max₂ := find_next_highest cs max n 10_000
      let idx_max₂ := cs.findIdx (· = max₂)
      (cs.drop (idx_max₂ + 1), max₂)
    else
      (cs.drop (idx_max + 1), max)
  )

def solve_part_a (input : Input) : Nat :=
  (input.map solve_one_input).sum


def solve_part_b (input : Input) : Nat :=
    let rec aux (n:Nat) (accum : List Nat) (ls : List Nat) : List Nat := 
      if n = 0 ∨ ls.isEmpty then accum
      else 
      match solve_one_step ls n with
        | none => [] 
        | some (ls', max) =>
          let new_accum := accum ++ [max]
          aux (n - 1) new_accum  ls'
    Id.run do
      let mut sum := 0
      for ls in input do
        let the_value := list_to_nat (aux 12 [] ls)
        sum := sum + the_value
      sum

def main : IO Unit := do
  -- let input ← parse <$> get_input none
  let input ← parse <$> get_input (some "../input/day3.txt")
  IO.println s!"Part A: {solve_part_a input}"
  IO.println s!"Part A: {solve_part_b input}"

end Day03

def main := Day03.main
