# prolog-to-minizinc
This compiler translates a subset of Prolog to MiniZinc. It's still a work-in-progress.

For example, this Prolog term can be translated to MiniZinc:

    (Z=[1.0,2.0],
    length(Z,2),
    length(Z,L),
    length(T,L),
    foreach(memberchk(A,Z),A=3.0))

This is the compiler's output:

    array[1..2] of var float:A0;
    array[1..2] of var bool:A1;
    var float:A2;
    constraint ((A0 == [1.0,2.0])/\((length(A0) == 2)/\((length(A0) == 2)/\((length(A1) == 2)/\forall((A2 in A0))((A2 == 3.0))))));
