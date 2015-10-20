isLinkSatisfied(es, _, E1, S2, _) :- S2 #>= E1.
isLinkSatisfied(ee, _, E1, _, E2) :- E1 #>= E2.
isLinkSatisfied(se, S1, _, _, E2) :- S1 #>= E2.
isLinkSatisfied(ss, S1, _, S2, _) :- S2 #>= S1.

minDuration((_, S, E)) :- E #>= S.

linkAll(_, []).
linkAll(POs, [Link|OtherLinks]) :- link(POs, Link), linkAll(POs, OtherLinks).
link(POs, (Name1, Name2, Type)) :-
    member((Name1, S1, E1), POs),
    member((Name2, S2, E2), POs),
    isLinkSatisfied(Type, S1, E1, S2, E2)
    .

plan(POs, Links, Duration) :-
    maplist(minDuration, POs),
    linkAll(POs, Links),
    totalDuration(POs, Duration)
    .

exactDuration(POs, Name, Duration) :-
    member((Name, S, E), POs),
    Duration #=# E - S
    .

minimalDuration(POs, Name, Duration) :-
    member((Name, S, E), POs),
    Duration #=< E - S
    .

maximalDuration(POs, Name, Duration) :-
    member((Name, S, E), POs),
    Duration #>= E - S
    .

minStart([(_, S, _)], S).
minStart([(_, S1, _)|POs], MinStart) :-
    minStart(POs, S2),
    MinStart #=# min(S1, S2)
    .

maxEnd([(_, _, E)], E).
maxEnd([(_, _, E1)|POs], MaxEnd) :-
    maxEnd(POs, E2),
    MaxEnd #=# max(E1, E2)
    .
start((_, S, _), S).
end((_, _, E), E).

totalDuration(POs, Duration) :-
    minStart(POs, S),
    maxEnd(POs, E),
    Duration #=# E - S
    .

% Simple Example
example1(POs, Links) :-
    POs = [
        (a, 1, 3),
        (b, _, _),
        (c, 2, 5)
    ],
    Links = [
        (a, b, es),
        (c, b, es)
    ],
    exactDuration(POs, b, 5),

    fd_minimize(plan(POs, Links, Duration), Duration), !,
    printPOs(POs).

% Cycle Example
example2(POs, Links) :-
    POs = [
        (a, 1, 3),
        (b, _, _)
    ],
    Links = [
        (a, b, ee),
        (b, a, ss)
    ],

    fd_minimize(plan(POs, Links, Duration), Duration),
    printPOs(POs).

% Stretching Example
example3(POs, Links) :-
    POs = [
        (a, 1, 3),
        (b, _, _),
        (c, _, _)
    ],
    Links = [
        (a, b, es),
        (b, c, es)
    ],
    exactDuration(POs, c, 5),
    Duration #>= 20,

    fd_minimize(plan(POs, Links, Duration), Duration),
    printPOs(POs).

% No Solution Example
example4(POs, Links) :-
    POs = [
        (a, 1, 10),
        (b, _, 15)
    ],
    Links = [
        (a, b, es)
    ],
    minimalDuration(POs, b, 7),

    fd_minimize(plan(POs, Links, Duration), Duration),
    printPOs(POs).


solve(POs, Links, Duration) :- data(POs, Links), plan(POs, Links), totalDuration(POs, Duration).
solveMinDuration(POs, Links, Duration) :- fd_minimize(solve(POs, Links, Duration), Duration).

printPO((Name, S, E)) :- D #=# E - S, format("~a\t0\t2015-10-~k\t2015-10-~k\t~k\n", [Name, S, E, D]).
printPOs(POs) :- maplist(printPO, POs).

%main :- solveMinDuration(POs, _, _), maplist(printPO, POs).
%:- initialization(main).
