:- ensure_loaded(borders).
:- ensure_loaded(cities).
:- ensure_loaded(countries).
:- ensure_loaded(rivers).

country(C) :- country(C,_,_,_,_,_,_,_).

larger(C1,C2) :- country(C1,_,_,_,R1,_,_,_),country(C2,_,_,_,R2,_,_,_),R1 > R2.

river_country(River,Country):- river(River,ListCountry), member(Country,ListCountry),country(Country).



country_region(Country, Region) :-
	country(Country,Region,_,_,_,_,_,_).


listof(_, []).
listof(Elt, [Elt|List]) :-
	listof(Elt, List).

all_same(List) :-
	listof(_, List).

adjacent(E1, E2, List) :-
	append(_, [E1,E2|_], List).


%This one will not work
member4(Elt,[_|List]):- append(_,[Elt],List).
member1(Elt,List):- append(_,[Elt|_],List).
 
abs1(X,AX):- (integer(X) ->
                 (X > 0 -> 
                     AX is X
                    ; AX is -X )
                 ;plusminus(X,AX)).

plusminus(X,AX) :- X is AX, X>0.
plusminus(X,AX) :- X is -AX, X<0.

abs2(X,AX):- X is AX, X >0.
abs2(X,AX):- X is -AX, X <0.
abs2(X,AX):- AX is X, X >0.
abs2(X,AX):- AX is -X, X <0.

% Ws9 q4
containers(Steps) :-
    containers(3, 5, 0, 0, _, 4, [0-0], Steps).

containers(_, _, V1, V2, V1, V2, _, []).
containers(C1, C2, V1, V2, T1, T2, Hist, [Move|Steps]) :-
    move(C1, C2, V1, V2, N1, N2, Move),
    State = N1-N2,
    \+ member(State, Hist),
    containers(C1, C2, N1, N2, T1, T2, [State|Hist], Steps).

move(C1, _, _, V2, 0, V2, empty(C1)).
move(_, C2, V1, _, V1, 0, empty(C2)).
move(C1, _, _, V2, C1, V2, fill(C1)).
move(_, C2, V1, _, V1, C2, fill(C2)).
move(C1, C2, V1, V2, N1, N2, pour(C1,C2)) :-
    pour(C2, V1, V2, N1, N2).
move(C1, C2, V1, V2, N1, N2, pour(C2,C1)) :-
    pour(C1, V2, V1, N2, N1).

pour(C2, V1, V2, N1, N2) :-
    (   V1 + V2 > C2 ->
            N1 is V1 - (C2 - V2),
            N2 is C2
    ;   N1 = 0,
        N2 is V1 + V2
    ).