# Day06

First thing I learned is that `dart create` creates a ton of files I probably wont need. 

## Part 2 (Notes for later)

Part b is not that hard, we just need to find certain intersections:

```txt
....#.....
....+->-+#
....|...|.
..#.^...|.
....|..#v.
....|...|.
.#-X--<-+.
........#.
#.........
......#...
```

Here `X` is an intersection of a row that walks left "^-<-+" and a line that 
walks up. Left -> Up is fine, we can put a box there.

One just needs to be sure that the rest of the line is a closed circle and does not
go into the abyss.

```txt
....#.....
....+->-+#
....|...|.
..#.^...|.
..|-+>-#v. <-- here
..^.|.|.|.
.#---<+-+.
.^->--+-#.
#---<-vv..
......#|..
```

If we come from the left (walk towards right), then
we would end up going just straight down, which would not circle back.

If we come from down (walk towards up), then
we would shortcut the normal root.

---

We can translate this into a graph problem:

1. Instead of walking step by step, we walk segment by segment[^1] (these are our **Edges**)
   We should also label them with a unique label and follow some ordering 
   (e.g. if we walk up *then* right, *up* should be labeled $k$ and *right* should be
   labeled $m$, with $k < m$) 
2. The boxes are now the **Vertrices**

We now need find new Vertricies that yield from our Edge (with id $k$) into a 
    different Edge $m$, where $k < m$.
  If we have $k < m$ then we have a loop.

To find these new Edges we only need to check transitions that follow 
    the behaviour of the guard.
  If the current Edge (Segement) is oriented towards *going up*,
    then we must only look at Edges (Segments) wich are *going right*, 
    as we cannot turn left or 90° back.

We can narrow the possible Edges (Segments) by "scanning" for 
    obstacles along the way.
  If we have one Segement from $(4,2)$ to $(4,6)$ (*up*),
    then any *left*-facing Segment above $y$-level $6$ can be ignored.
  There is no way to reach them once we hit $y=6$,
    as we have to turn *right* (walk towards *left*) then.

---

[^1]: This should also speed up part 1
