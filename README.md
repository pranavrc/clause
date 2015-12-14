# Clause

Clause is a [Resolution Refutation](https://en.wikipedia.org/wiki/Resolution_%28logic%29) framework written in Common Lisp.

### Application

James Whitcomb Riley [once said](https://en.wikipedia.org/wiki/Duck_test): *When I see a bird that walks like a duck and swims like a duck and quacks like a duck, I call that bird a duck.*

A mallard is a bird walks like a duck, quacks like a duck and swims like a duck. Is it a duck?

```
bird(mallard)
walks-like-duck(mallard)
swims-like-duck(mallard)
quacks-like-duck(mallard)
(bird(X)&walks-like-duck(X)&swims-like-duck(X)&quacks-like-duck(X))==>is-duck(X)
```

To prove: `is-duck(mallard)`

```
> (load "clause.lisp")
> (setq bird (deffact 'bird 'mallard))
#<FACT :FUNC BIRD :PARAMS (MALLARD)>
> (setq walks-like-duck (deffact 'walks-like-duck 'mallard))#<FACT :FUNC WALKS-LIKE-DUCK :PARAMS (MALLARD)>
> (setq swims-like-duck (deffact 'swims-like-duck 'mallard))#<FACT :FUNC SWIMS-LIKE-DUCK :PARAMS (MALLARD)>
> (setq quacks-like-duck (deffact 'quacks-like-duck 'mallard))#<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (MALLARD)>
> (setq facts (defwff 'and (list (deffact 'bird (var 'x)) (deffact 'walks-like-duck (var 'x)) (deffact 'swims-like-duck (var 'x)) (deffact 'quacks-like-duck (var 'x)))))
#<WFF :OPERATOR AND
  :WFFS
  (#<FACT :FUNC BIRD :PARAMS (#<VAR :X X>)> #<FACT :FUNC WALKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>
   #<FACT :FUNC SWIMS-LIKE-DUCK :PARAMS (#<VAR :X X>)> #<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
> (setq implications (deffact 'is-duck (var 'x)))
#<FACT :FUNC IS-DUCK :PARAMS (#<VAR :X X>)>
[28]> (setq rule (defrule facts implications))
#<RULE
  :FACTS
  #<WFF :OPERATOR AND
    :WFFS
    (#<FACT :FUNC BIRD :PARAMS (#<VAR :X X>)> #<FACT :FUNC WALKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>
     #<FACT :FUNC SWIMS-LIKE-DUCK :PARAMS (#<VAR :X X>)> #<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
  :IMPLICATIONS #<FACT :FUNC IS-DUCK :PARAMS (#<VAR :X X>)>
  :CNF
  #<WFF :OPERATOR OR
    :WFFS
    (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC BIRD :PARAMS (#<VAR :X X>)>)>
     #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC WALKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
     #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC SWIMS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
     #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)> #<FACT :FUNC IS-DUCK :PARAMS (#<VAR :X X>)>)>>
> (setq goal (deffact 'is-duck 'mallard))
#<FACT :FUNC IS-DUCK :PARAMS (MALLARD)>
> (resolve (list bird walks-like-duck swims-like-duck quacks-like-duck) (list rule) goal)
Resolvent: #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC IS-DUCK :PARAMS (MALLARD)>)>
Substitution: NIL

Resolvent: 
#<WFF :OPERATOR OR
  :WFFS
  (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC BIRD :PARAMS (#<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC WALKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC SWIMS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>)>
Substitution: (MALLARD)

Resolvent: 
#<WFF :OPERATOR OR
  :WFFS
  (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC WALKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC SWIMS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>)>
Substitution: (MALLARD)

Resolvent: 
#<WFF :OPERATOR OR
  :WFFS
  (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC SWIMS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>
   #<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>)>
Substitution: (MALLARD)

Resolvent: #<WFF :OPERATOR OR :WFFS (#<WFF :OPERATOR NOT :WFFS (#<FACT :FUNC QUACKS-LIKE-DUCK :PARAMS (#<VAR :X X>)>)>)>
Substitution: (MALLARD)

Resolvent: #<WFF :OPERATOR OR :WFFS NIL>
Substitution: (MALLARD)

((MALLARD) (MALLARD) (MALLARD) (MALLARD) (MALLARD))
```

It looks like a mallard is, indeed, a duck!


### Sample problem breakdown

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
