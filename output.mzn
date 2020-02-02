array[1..2] of var int:A0;
array[1..5] of var int:A1;
array[1..3] of var int:A2;
constraint ((1 in A0)/\((length(A1) == 5)/\(alldifferent(A1)/\((length(A0) == 2)/\(A0++A2==A1)))));
