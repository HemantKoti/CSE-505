% The 'reduce' predicate carries out one-step reduction

reduce(X,X,B) :-
	atom(X),
	member(X,B), !.
reduce(X,Y, _) :-
    atom(X),
    fun(X,Y).
reduce(X,X,_) :-
    atom(X).
reduce(X,Y, _) :-
    number(X),
    church(X,Y).
reduce(l(V,a(T,V)),T, _) :-
    atom(V),
    \+ occurs_free_in(V, T),
    !.
reduce(l(V,T),l(V,R), B) :-
    reduce(T,R,[V|B]).

reduce(a(l(V,T1),T2), A, B) :- !,    % check leftmost vs normal
    (reduce(beta)
       -> substitute(T1,V,T2,A)
       ;  reduce(T2, T3, B),
	  (T2 == T3                     % must do alpha(T, R) test
	      -> (reducible(T3)
		  ->  A = a(l(V,T1),T2)
		  ;   substitute(T1,V,T2,A))
	      ; A = a(l(V,T1),T3)
	  )
    ).

reduce(a(T1,T2), A, B) :-
    reduce(T1,R1,B),
    (T1 == R1                        % must do alpha(T, R) test
        -> reduce(T2,R2,B),
           A = a(T1,R2)
        ;  A = a(R1,T2)
    ).


% alpha equivalence

alpha(a(T1,T2), a(T3,T4), L) :-
	alpha(T1,T3,L),
	alpha(T2,T4,L).
alpha(l(V1,T1), l(V2,T2), L) :-
	alpha(T1,T2, [(V1,V2)|L]).
alpha(X,Y,L) :-
	atom(X),
	atom(Y),
	checkvar(X,Y,L).

checkvar(X,Y,[]) :-
	X == Y.
checkvar(X,Y,[(P,Q)|_]) :-
        X == P, !,
	Y == Q.
checkvar(X,Y,[(P,Q)|_]) :-
        Y == Q, !,
	X == P.
checkvar(X,Y,[_|T]) :-
	checkvar(X,Y,T).







% generate Church numeral

church(N, l(f,l(x,T))) :-
    gen(N,T).

gen(0, x).
gen(N, a(f,T)) :-
     N > 0,
     M is N-1,
     gen(M, T).


% occurs_free_in is needed in defining eta-reduction

occurs_free_in(X, X) :-
	atom(X).
occurs_free_in(X, l(Y,T)) :-
	X \== Y,
	occurs_free_in(X, T).
occurs_free_in(X, a(T1, T2)) :-
	occurs_free_in(X, T1) ;
        occurs_free_in(X, T2).


reducible(a(l(_,_), _)) :- !.
reducible(a(T1, T2))   :-
	reducible(T1)  ;
	reducible(T2).

reducible(l(X,a(T, v(X)))) :-
	\+ occurs_free_in(X, T), !.    % eta-redex
reducible(l(_,T)) :-
	reducible(T).


% =========================================================
%
% Substitute is needed in defining beta-reduction: T2 = T1[V <- T]

substitute(T1,V,T,T2) :-
	free(T, Free),
	subst(T1,V,T,T2,Free),
	!.

subst(V,V,T,T, _) :-
        atom(V).
subst(V1,V2,_,V1,_) :-
	atomic(V1),
	V1 \== V2.
subst(l(V,T1),V,_,l(V,T1),_).
subst(l(V1,T1),V,T,l(V2,T3),Free) :-
	V1 \== V,
	member(V1,Free),
	rename(V1,V2),
	subst(T1,V1,V2,T2,Free),
	subst(T2,V,T,T3,Free).
subst(l(V1,T1),V,T,l(V1,T2),Free) :-
	V1 \== V,
	\+ member(V1,Free),
	subst(T1,V,T,T2,Free).
subst(a(T1,T2),V,T,a(T3,T4),Free) :-
	subst(T1,V,T,T3,Free),
	subst(T2,V,T,T4,Free).


rename(V,R) :-
	name(V,L),
	append(L,[49],L2),
	name(R,L2).

free(V, L) :-
	atomic(V),
	(number(V)
	   -> L = []
	   ; L =  [V]).

free(l(V,T), L) :-
	free(T, L2),
	remove(L2,V,L).
free(a(T1,T2), L) :-
	free(T1,L1),
	free(T2,L2),
	union(L1,L2,L).

remove([],_,[]).
remove([H|T],H,T).
remove([H|T],V,[H|T2]) :-
	H \== V,
	remove(T,V,T2).

