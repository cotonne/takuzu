# takuzu
Rules of takuzu can be found here : https://en.wikipedia.org/wiki/Takuzu

You will find implementations of this game in differents languages with TDD approach.

## Prolog
To test this solution, you need swipl (http://www.swi-prolog.org/).


    $ swipl
    ?- consult("solver.pl").
    true.
    
    ?- testAll.
    [OK] Should take the first element
    [OK] Should take the first element
    [OK] Should take the first element
    [OK] Should replace the center with 0
    [OK] should find a simple anwser for takuzu
    [OK] should find a simple anwser for takuzu
    [OK] should get the good terms from matrix
    [OK] should count correclty element 1 in list
    [OK] should get the second column
    [OK] should get the transposed matrix
    true ;
    false.

    ?- 

An example of takuzu is in the file example.pl

