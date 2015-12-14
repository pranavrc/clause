# Clause

Clause is a [Resolution Refutation](https://en.wikipedia.org/wiki/Resolution_%28logic%29) framework written in Common Lisp.

### Example problem

```
 EE-Student(Charley) 
 Hrs-Taken(122,Charley) 
 {EE-Student(X)&Hrs-Taken(Y,X)&Greaterp(Y,120)}==>EE-Sr(X) 

Deduce that Charley is an EE Senior.
```

##### Facts

```
> (setq ee-student (deffact 'ee-student 'charley))
#<FACT :FUNC EE-STUDENT :PARAMS (CHARLEY)>
> (setq hrs-taken (deffact 'hrs-taken 122 'charley))
#<FACT :FUNC HRS-TAKEN :PARAMS (122 CHARLEY)>
> (setq greaterp (deffact 'greaterp 122 120))
#<FACT :FUNC GREATERP :PARAMS (122 120)>
```

##### Rules

```
> (setq facts (defwff 'and (list (deffact 'ee-student (var 'x))
(deffact 'hrs-taken (var 'y) (var 'x)) (deffact 'greaterp (var 'y) 120))))
#<WFF :OPERATOR AND
  :WFFS
  (#<FACT :FUNC EE-STUDENT :PARAMS (#<VAR :X X>)> #<FACT :FUNC HRS-TAKEN :PARAMS (#<VAR :X Y> #<VAR :X X>)>
   #<FACT :FUNC GREATERP :PARAMS (#<VAR :X Y> 120)>)>
> (setq implications (deffact 'ee-sr (var 'x)))
#<FACT :FUNC EE-SR :PARAMS (#<VAR :X X>)>
> (setq rule (defrule facts implications))
#<RULE
  :FACTS
  #<WFF :OPERATOR AND
    :WFFS
    (#<FACT :FUNC EE-STUDENT :PARAMS (#<VAR :X X>)> #<FACT :FUNC HRS-TAKEN :PARAMS (#<VAR :X Y> #<VAR :X X>)>
     #<FACT :FUNC GREATERP :PARAMS (#<VAR :X Y> 120)>)>
  :IMPLICATIONS #<FACT :FUNC EE-SR :PARAMS (#<VAR :X X>)>
  :CNF
  #<WFF :OPERATOR OR
    :WFFS
    (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC EE-STUDENT :PARAMS (#<VAR :X X>)>)>
     #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC HRS-TAKEN :PARAMS (#<VAR :X Y> #<VAR :X X>)>)>
     #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC GREATERP :PARAMS (#<VAR :X Y> 120)>)>
     #<FACT :FUNC EE-SR :PARAMS (#<VAR :X X>)>)>>
```

##### Goals

```
> (setq goal (deffact 'ee-sr 'charley))
#<FACT :FUNC EE-SR :PARAMS (CHARLEY)>
```

##### Resolution Refutation

```
> (resolve (list ee-student hrs-taken greaterp) (list rule) goal)
Resolvent: #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC EE-SR :PARAMS (CHARLEY)>)>
Substitution: NIL

Resolvent: 
#<WFF :OPERATOR OR
  :WFFS
  (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC EE-STUDENT :PARAMS (#<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC HRS-TAKEN :PARAMS (#<VAR :X Y> #<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC GREATERP :PARAMS (#<VAR :X Y> 120)>)>)>
Substitution: (CHARLEY)

Resolvent: 
#<WFF :OPERATOR OR
  :WFFS
  (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC HRS-TAKEN :PARAMS (#<VAR :X Y> #<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC GREATERP :PARAMS (#<VAR :X Y> 120)>)>)>
Substitution: (CHARLEY)

Resolvent: #<WFF :OPERATOR OR :WFFS (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC GREATERP :PARAMS (#<VAR :X Y> 120)>)>)>
Substitution: (CHARLEY 122)

Resolvent: #<WFF :OPERATOR OR :WFFS NIL>
Substitution: (122)

((CHARLEY) (CHARLEY) (CHARLEY 122) (122))
```
