SimpleProlog
============

Simple Prolog Interpreter which is able to interpret programs of the following simplified form:

- Only '[A-Z]', ':', '-', '.', ',', '?' and '\r\n' are allowed.
- Fact clauses are given by '[A-Z].'.
- Procedure clauses are given by '[A-Z]:-[A-Z](,[A-Z])*'.
- Target clauses are given by '?-[A-Z](,[A-Z])*'.
- Every line is exactly one of the above clauses.
- The last line of a program is a target clause, every program has to have at least one target-clause.
- Only one target clause is allowed.

The interpreter answers with 'Yes' when a successful path was found, 
and with 'No' if it has completed work and no path was found.

Example Programs:

A.
?-A.

Or 

A:-B,C.
A:-D.
B.
B:-D.
C.
D:-E.
D.
?-A.