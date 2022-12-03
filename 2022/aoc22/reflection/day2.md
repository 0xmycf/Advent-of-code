# Day 2 -- 2022

**Spoiler warning**  

This puzzle is about playing "Rock, Paper, Scissors".
  You need to figure out some logic to (a) identify a winner for one round with two players
    and (b) a shape (Rock, Paper or Scissors) for a known outcome.

If you know nothing about group theory the following paragraph might not make
  total sense too you, but bear with me, its really not hard.
When you play the game,
    you get to choose one option out of three: 

1. Rock
2. Paper
3. Scissors

And each option acts uniquely with each other option (eg. Paper beats Rock, but looses to Scissors).
  This means we can make a table that determines each outcome:

```
    | R | P | S
  --------------
  R | D | L | W
  --------------
  P | W | D | L
  --------------  
  S | L | W | D

```

This table means: If Rock plays against Rock (case top left), its a draw (D) (case top mid), but a Rock against a Paper is a loss (L) 
    and against a Scissor is a win (W) (case top right).
  Now can we use math to represent this relation?
  As it turns out we can using group theory!

Rock, Paper, Scissors can be mapped to 0, 1, 2.
  The set `{0, 1, 2}` actually represents a group called `R3`.
  We can do Addition in this group if we use modulos arithmetic.

```
    | 0 | 1 | 2
  --------------
  0 | D | L | W
  --------------
  1 | W | D | L
  --------------  
  2 | L | W | D

```

With this updated version we just need to figure out what operation we apply on the 
    two inputs.
  Because this is a (Semi)group[^1] we need a function of type `R3 x R3 -> R3` 
    (meaning it takes two inputs from `R3 = {0,1,2}` and produces a `R3` value).

  A simple pick would be `(+)` with modulos 3:

```
  + | 0 | 1 | 2
  --------------
  0 | 0 | 1 | 2
  --------------
  1 | 1 | 2 | 0
  --------------  
  2 | 2 | 0 | 1

```

But if we now map these numbers to D, W, L from above we notice that it wont match up at all!
  Another really easy pick would be `(-)` modulus 3.


```
  - | 0 | 1 | 2
  --------------
  0 | 0 | 2 | 1
  --------------
  1 | 1 | 0 | 2
  --------------  
  2 | 2 | 1 | 0

```

Which lines up perfectly with the first table!  


## What does that mean for us?

This means that the function `(-): R3 x R3 -> R3` will determine who wins in Rock, Paper, Scissors!
  (a) is solved.

If we think about this in symbols (and not tables) we get an equation like this:

```
For a, b, o in R3, where a is my choice and b is their choice, o is the outcome:

  a - b = o
```

(b) asks for a choice if one input and the outcome is given.
  Using normal algebra we can reorder this equation to find a (my choice) instead of the output:

```
  a - b = o <=> - b - o = - a <=> b + o = a
```

Note that `(-)` and `(+)` are both with modulos 3!

This solves (b)!
  And we can be really sure if we look at the table using `(+)` again:

```
  + | 0 | 1 | 2     <- These are the desired outcome: 0 draw, 1 win, 2 loss
  --------------
  0 | 0 | 1 | 2
  --------------
  1 | 1 | 2 | 0
  --------------  
  2 | 2 | 0 | 1

  ^ their input: 0 Rock, 1 Paper, 2 Scissors

```

That means if they pick a Rock (0) and the desired outcome is a draw, we should pick a Rock (0) too.
  If they choose Rock and we need to Win, we should pick Paper and lastly
    if they pick Rock and we should Lose, we should pick a Scissor.
  You can go through the other cases to validate that this true for all combinations.
  
## Scoring

The last step is to figure out the scoring.

We can find a pretty straight forward pattern if we look at the `(-)` table again:

```
  - | 0 | 1 | 2
  --------------
  0 | 0 | 2 | 1
  --------------
  1 | 1 | 0 | 2
  --------------  
  2 | 2 | 1 | 0

```

A Draw results in 3 Points, a Win in 6 and a lose in 0.
  These three numbers all are multiples of three (`3*0=0,3*1=3,3*2=6`).
  Can we somehow convert the numbers in the tables representing the three outcomes to these values?  

Indeed we can, because they are all just one off (mod 3): 

```
  If o is the outcome (0 Draw, 1 Win, 2 Loss) then:
  Draw => 3      Draw + 1 mod 3 * 3 = 0 + 1 mod 3 * 3 = 1 * 3 = 3
  Win  => 6  ==> Win  + 1 mod 3 * 3 = 1 + 1 mod 3 * 3 = 2 * 3 = 6
  Loss => 0      Loss + 1 mod 3 * 3 = 2 + 1 mod 3 * 3 = 0 * 3 = 0
```

This means the final score for one match can be calculated like this:

```
  s is the score {0, 3, 6}
  (a - b + 1 mod 3) * 3 = s

  And if we want the full score (Shape + Outcome)

  (a - b + 1 mod 3) * 3 + (a + 1) = full_score
```

For part b

```
  (b + o mod 3) + 1 + (o + 1 mod 3) * 3 = full_score

  I : b + o will return the a in {0,1,2} (The shape score), which will be one off.
  I': b + o + 1 will return the shape score (1 for Rock, 3 for Scissor)

  II: (o + 1) * 3 will map to the score as seen above.

  I + II = full_score
```

[^1]: The function of our choosing must actually be associative too, but `(-)` isn't.
  That means `((-),{0,1,2})` is *not* a Semigroup.
  Another way to look at this problem is algebraically:  

  Lets name our function `(<>): R3 x R3 -> R3`

    `If Rock  <> Rock  = Draw then`
    `   Paper <> Paper = Draw and `
    `   Sciss <> Sciss = Draw     `

    `  ==> Rock <> Rock = Paper <> Paper ==> (<>) == (-) and Draw == 0`
