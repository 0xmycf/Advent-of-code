

namespace Day02

def test_input := "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124"

structure Range where
  high : Nat
  low : Nat

instance : ToString Range where
  toString r := s!"{r.low}-{r.high}"

def parse (s : String) : List Range :=
  let ranges := s.splitOn ","
  let parsed := ranges.filterMap fun rStr =>
    let bounds := rStr.trim.splitOn "-"
    match bounds with
    | [lowStr, highStr] =>
      let low := lowStr.toNat!
      let high := highStr.toNat!
      .some $ Range.mk high low
    | _ => .none
  parsed

def get_input (path : Option String) : IO (List Range) := do
  let content <- match path with
  | .none => pure test_input
  | .some p => IO.FS.readFile p
  return (parse content) 

def both_halfs_same (n : Nat) : Bool :=
  let s := toString n
  let half := s.length/2
  let fh := s.take half
  let sh := s.drop half
  fh = sh

def InvalidIdx := { n : Nat // both_halfs_same n }

-- brute
def unfold_range (r : Range) : List Nat := 
  List.range (r.high - r.low + 1) |>.map (fun x => x + r.low) 

def filter_for_day2 (predicate : Nat → Bool) (ranges : List Range) : List { n : Nat // predicate n } :=
  let unfolded := ranges.flatMap unfold_range
  unfolded.filterMap fun r => 
    if h : predicate r then
      .some ⟨r, h⟩
    else
      .none

def filter_for_a (ranges : List Range) : List InvalidIdx :=
  filter_for_day2 both_halfs_same ranges

/-
   I could not get this to work with termination checking
   so instead I used this other brute force method, which does esentially the same thing
-/

-- def sliding_window  (size : { n : Nat // 0 < n}) (s : String) : List String :=
--   if s.length = 0 ∨ s.length < size then 
--     []
--   else if s.length = size then
--     [s]
--   else
--     let rest := s.drop size
--     (s.take size) :: sliding_window size rest
--   termination_by s.length

def any_same_parts (n : Nat) : Bool := 
  let s := toString n
  let half := s.length/2
  let rec check (k : Nat) : Bool :=
    if k = 0 then
      false
    else 
      let pattern := s.take k 
      let repeated := List.replicate (s.length / k) pattern |> String.join
      if repeated = s then
        true
      else
        check (k - 1)
  check half

def InvalidIdxForB := { n : Nat // any_same_parts n }

def filter_for_b (ranges : List Range) : List InvalidIdxForB :=
  filter_for_day2 any_same_parts ranges

def solve_a (ranges : List Range) : Nat :=
  let invalids := filter_for_a ranges
  invalids.foldr (fun idx acc => acc + idx.val) 0

def solve_b (ranges : List Range) : Nat :=
  let invalids := filter_for_b ranges
  invalids.foldr (fun idx acc => acc + idx.val) 0

def main : IO Unit := do
  let ranges <- get_input (.some "../input/day2.txt")
  -- let ranges <- get_input .none
  IO.println ("Part A: " ++ toString (solve_a ranges))
  IO.println ("Part B: " ++ toString (solve_b ranges))

end Day02

def main := Day02.main
