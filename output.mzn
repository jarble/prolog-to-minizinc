array[1..2] of var float:A0;
array[1..2] of var bool:A1;
var float:A2;
var float:A3;
constraint ((A0 == [1.0,2.0])/\((length(A0) == 2)/\((length(A0) == 2)/\((length(A1) == 2)/\forall((A2 in A0))(((1.0<=A3)/\(A3<=3.0)))))));
