
namespace Day01

def test_data := 
  "L68
  L30
  R48
  L5
  R60
  L55
  L1
  L99
  R14
  L82"

-- #eval test_data.splitOn "\n" |> List.map String.trim

inductive Dir where
  | L
  | R

instance : ToString Dir where
  toString
    | Dir.L => "L"
    | Dir.R => "R"

def Lock := {n : Nat // 0 ≤ n ∧ n < 100} 

instance : ToString Lock where
  toString l := s!"Lock({l.val})"

def Lock.mk (n : Nat) : Lock :=
  let n₁  := n % 100
  ⟨n₁ , by
    constructor
    · apply Nat.zero_le
    · unfold n₁
      apply Nat.mod_lt
      trivial
  ⟩
    
def move_lock (l : Lock) (dir : Dir) (delta : Int) : Lock :=
  let n := Int.ofNat l.val
  let f := match dir with
    | Dir.L => Int.sub
    | Dir.R => Int.add
  let new_n := Int.toNat (f n delta % 100) % 100 -- this just makes the proof easier (TODO: how to remove?)
  ⟨new_n, by 
    constructor
    case left => apply Nat.zero_le
    case right => 
      unfold new_n 
      apply Nat.mod_lt
      trivial
  ⟩

abbrev TimesZero := Nat

abbrev State := Lock × TimesZero

def State.mk (n : Nat) : State := (Lock.mk n, 0)

def run : State → List (Dir × Nat) → State
  | s, [] => s
  | (l, zs), (dir, delta) :: xs =>
    let new := (move_lock l dir delta)
    run (new, if new.val == 0 then zs + 1 else zs) xs

def data_path := "./../input/day1.txt"

def input (path : Option String) : IO (List {s : String // 1 < s.length}) := do
    let dosep str := String.splitOn (sep := "\n") str
    let list_string ← match path with
    | some p => dosep <$> IO.FS.readFile p
    | none => pure $ dosep test_data
    let no_whitespace := list_string.map String.trim
    let ret := no_whitespace.filterMap (fun s => if h : 1 < s.length then some ⟨s, h⟩ else none)
    pure $ ret

def parse (line : {s: String // 1 < s.length}) := 
    let line_as_list := line.val.toList
    let dir := if line_as_list.get (⟨0, by 
      have listCongr : line_as_list.length = line.val.length := by rfl
      rw [listCongr] 
      apply Nat.lt_trans (n := 0) (m := 1) (k:= line.val.length) (by decide)
      exact line.property
      ⟩ : Fin line_as_list.length) == some 'L' then Dir.L else Dir.R
    let delta := line_as_list.tail.asString.toNat?.getD 0
    (dir, delta)

def main : IO Unit := do
  let content ← input (.some data_path)
  let parsed := content.map parse
  let s : State := State.mk 50
  IO.println $ run s parsed

end Day01

def main : IO Unit :=
  Day01.main
