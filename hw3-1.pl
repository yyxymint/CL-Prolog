% semLexLambda.pl

semLex(prep,M):-
   M = [symbol:without,
        sem:lam(K,lam(P,lam(Y,and(not(app(K,lam(X,with(Y,X)))),app(P,Y)))))],!.
        
semLex(prep,M):-
   M = [symbol:without,
        sem:lam(K,lam(P,lam(Y,and(not(app(K,lam(X,F))),app(P,Y)))))],
   compose(F,with,[Y,X]),!.
   
   
   



% englishLexicon.pl

lexEntry(prep,[symbol:without,syntax:[without]]).
