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

data(POs, Links) :-
    member((a, 1, 2), POs),
    member((b, _, 3), POs),
    member((a, b, es), Links)
    .

main(POs, Links) :- data(POs, Links), plan(POs, Links).
