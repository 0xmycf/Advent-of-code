
l ← •FLines "./input/day1.txt"

# Taken from the haskell discord
# 0=≠¨𝕩     -- for each element in x, take its length and check if the length is equal to 0
# (⊢-˜+`×¬) -- Train?: First: (+`×¬) x
#                         ==  (+` x)  ×    (¬ x)
#                       scanl (+) x   mul  fmap not x
#                     Second: (⊢-˜) x
#                         ==  x - (⊢ w) (where w = result of 0=≠¨)
# 𝕩⊔˜       -- flip ⊔, then: 𝕩 ⊔ l where 𝕩 = result of ((⊢-˜+`×¬)0=≠¨)
#           -- Group with same indices, and drop all -1 ones
Parse ← {𝕩⊔˜(⊢-˜+`×¬)0=≠¨𝕩}

ToInt ← 10⊸×⊸+˜´∘⌽-⟜'0' # https://mlochbaum.github.io/bqncrate/

# over each entry, sum (foldr1 (+)) 
Part1 ← {+´¨ 𝕩}

# Part1
# then reduce to the maximum foldr1 max
•Show (⌈´ ∘ Part1 ∘ (ToInt¨¨) ∘ Parse) l
# First do Part 1
# then sort decreasing
# then take 3
# then sum (foldr1 (+))
Part2 ← (+´3↑∨)Part1

•Show (Part2 ∘ (ToInt¨¨) ∘ Parse) l
