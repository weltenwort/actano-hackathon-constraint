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

plan(POs, Links) :-
    maplist(minDuration, POs),
    linkAll(POs, Links)
    .

exactDuration(POs, Name, Duration) :-
    member((Name, S, E), POs),
    Duration #=# E - S
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

data(POs, Links) :-
    POs = [
        (a, 1, 2),
        (b, _, _)
    ],
    Links = [
        (a, b, es)
    ],
    exactDuration(POs, b, 5)
    .

solve(POs, Links, Duration) :- data(POs, Links), plan(POs, Links), totalDuration(POs, Duration).
solveMinDuration(POs, Links, Duration) :- fd_minimize(solve(POs, Links, Duration), Duration).

main :- solve(POs, Links, Duration), print(Duration).
:- initialization(main).
