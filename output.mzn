array[1..4] of var float:A0;
constraint ((length(A0) == 4)/\(forall((A1 in A0))((A1>3.0))/\forall((A2 in A0))((A2>3.0))));
