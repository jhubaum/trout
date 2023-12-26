type location = {
    line : int;
    column : int;
}

let string_of_location {line; column} =
    Printf.sprintf "%d,%d" line column
