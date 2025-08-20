app [main!] { pf: platform "https://github.com/roc-lang/basic-cli/releases/download/0.19.0/Hj-J_zxz7V9YurCSTFcFdu6cQJie4guzsPMUi5kBYUk.tar.br" }

import pf.StdOut
import pf.File

main! = |_args|
    in = input! {}
    sol_a = parse(in, List.keep_if)
    _ = StdOut.line!(Num.to_str(a(sol_a)))
    rest = parse(in, List.drop_if)
    StdOut.line!("Part B: ${Inspect.to_str b(rest)}")

a = |in_order_x|
        middles = List.map(in_order_x, get_middle)
        List.sum(middles)

b = |not_in_order| not_in_order # implement toposort here

# test! = |_|
#     when File.read_utf8!("./test.txt") is
#         Ok(content) -> content
#         Err(_) -> crash "Input file does not exist or is not readable."

input! = |_|
    when File.read_utf8!("../input/day05.txt") is
        Ok(content) -> content
        Err(_) -> crash "Input file does not exist or is not readable."

parse_dep = |xs|
    when xs is
        [before, after] -> { before, after }
        _ -> crash "Invalid dependency format: ${Inspect.to_str(xs)}"

# parse_i32_or_fail = |s|
#     when Str.to_i32(s) is
#         Ok(i) -> i
#         Err(_) -> crash "Failed to parse integer: ${s}"

search_for_first : List { before : Str, after : Str }, Str -> List { before : Str, after : Str }
search_for_first = |deps, search|
    List.keep_if(deps, |elems| elems.before == search)

in_order = |move, parse_deps|
    { ok } = List.walk(
        move,
        { seen: Set.empty {}, ok: Bool.true },
        |acc, elem|
            any_dep_in_seen =
                search_for_first(parse_deps, elem)
                |> List.keep_if(|x| Set.contains(acc.seen, x.after))
                |> List.is_empty
                |> Bool.not

            if any_dep_in_seen then
                { seen: Set.insert(acc.seen, elem), ok: Bool.false }
            else
                { acc & seen: Set.insert(acc.seen, elem) },
    )
    ok

get_middle = |move|
    len = List.len move
    when List.get(move, Num.div_trunc(len, 2)) is
        Ok(value) ->
            maybe = value |> Str.to_u32
            when maybe is
                Ok(ok) -> ok
                _ -> crash "Number could not be converted ${Inspect.to_str maybe}"

        err -> crash "get_middle should never go this path: ${Inspect.to_str err} for ${Inspect.to_str(move)} with ${Inspect.to_str (Num.div_ceil(len, 2))}"

parse = |input, hof|
    when Str.split_on(input, "\n\n") is
        [deps, moves] ->
            parsed_deps = Str.split_on(deps, "\n") |> List.map(|line| line |> Str.split_on("|") |> parse_dep)
            parsed_moves = Str.split_on(moves, "\n") |> List.map(|line| Str.split_on(line, ",") |> List.drop_if(|e| e == "")) |> List.drop_if(List.is_empty)

            hof(parsed_moves, |move| in_order(move, parsed_deps))

        err -> crash "Invalid input format. ${Inspect.to_str(err)}"
