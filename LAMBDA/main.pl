% Lambda Calculus Simulator
% author: Bharat Jayaraman, University at Buffalo
% date: March 2018
%
% Javascript User Interface
% author: M.K. Jinesh, Amrita University
% date: February 2021


:- dynamic fun/2.
:- dynamic reduce/1.

:- include(grammar).
:- include(lambda).
:- include(web).


%To support web interface
go3(Input) :- string_codes(Input,C),tokenize(C, Tokens),(go(ParseTree,Tokens,[])
		      ->  process(ParseTree)
		      ;  write('syntax error - please re-enter'), nl
		  ).

set_reduction(In) :-
	      retractall(reduce(_)),asserta(reduce(In)).

load_file_content(Data) :-
	retractall(fun(_,_)),
	open_string(Data,Stream),
	lex(Stream,Tokens),
	program(Tokens, []),
        !.


go :- write('Default strategy is to reduce the leftmost redex. Use \'reduce normal\''), nl,
      write('command to change to call-by-value reduction. Commands:'), nl,
      write('          norm Term'), nl,
      write('          derive Term'), nl,
      write('          step Term'), nl,
      write('          subst Term1 [Var <- Term2]'), nl,
      write('          alpha Term1 = Term2'), nl,
      write('          let Name = Term'), nl,
      write('          reduce beta'), nl,
      write('          reduce normal'), nl,
      write('          end'), nl,
      nl,
      asserta(reduce(beta)),
      go2.



go2 :-
	read_line_to_codes(user_input, C),
	(C = []
	   -> go2
	   ;  tokenize(C, Tokens),
	     (Tokens == [end]
	        -> nl
	        ; (go(ParseTree,Tokens,[])
		      ->  process(ParseTree)
		      ;  write('syntax error - please re-enter'), nl
		  ),
	          go2)
	),
	!.


go(let(N,T)) --> [let], [id(N)], ['='], term(T).

go(subst(T1,V,T2)) --> [subst], term(T1), ['['], var(V), ['<-'], term(T2), [']'].

go(derive(T)) --> [derive], term(T).

go(norm(T)) --> [norm], term(T).

go(step(T)) --> [step], term(T).

go(alpha(T1, T2)) --> [alpha], term(T1), ['='], term(T2).  % not done

go(reduce(beta)) --> [reduce, beta].

go(reduce(normal)) --> [reduce, normal].


% ===========================================================


process(norm(T)) :-
	reduce(T, R, []),
	(alpha(T ,R, [])
	      -> (reducible(T)
	             -> write('=> ... nontermination'), nl
	              ; write('=> '), pretty_print(T), nl
	         )
              ; process(norm(R))
        ).


process(derive(T)) :-
       reduce(T, R, []),
       (alpha(T, R, [])
          -> (reducible(T)
	         -> write('=> ... nontermination'), nl
	          ; nl
	     )
          ; write('=> '), pretty_print(R), nl,
	    process(derive(R))
       ).

process(step(T)) :-
       reduce(T, R, []),
       (alpha(T, R, [])
          -> (reducible(T)
	         -> write('=> ... nontermination'), nl
	          ; nl
	     )
          ; write('=> '), pretty_print(R),
	    read_line_to_codes(user_input, C),
	    (C = [] -> process(step(R))
	           ;  (C = [35] -> true
			     ; process(norm(R))
		      )
	    )
       ).

process(subst(T1,V,T)) :-
       substitute(T1,V,T,T2),
       pretty_print(T2),
       nl.

process(let(N, T)) :-
	asserta(fun(N,T)),
	write('Adding '), write(N), write(' to database.'), nl.


process(reduce(beta)) :-
	retractall(reduce(_)),
	asserta(reduce(beta)),
	write('Changing to beta reduction of leftmost redex.'), nl.

process(reduce(normal))   :-
	retractall(reduce(_)),
	asserta(reduce(normal)),
	write('Changing to normal reduction.'), nl.

process(alpha(T1,T2)) :-
	(alpha(T1, T2, [])
	  -> write('true'), nl
	  ;  write('false'), nl
	).




% ===============


pretty_print(X) :-
    atomic(X),
    write(X).
pretty_print(l(V,T)) :-
    write('L'),write(V),write('.'),
    pretty_print(T).
pretty_print(a(T1,T2)) :-
    write('('), pretty_print(T1),
    write('  '), pretty_print(T2),
    write(')').






