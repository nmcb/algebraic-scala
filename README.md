# Algebraic
Scala port of example Haskell interpreter/compiler employing algebras.

Ported from [Arian van Putten's Blog](http://arianvp.me/writing-interpreters-and-compilers-an-introduction-to-folds-and-algebras)

Load and run the algebraic script from the scala interpreter.

```
$ scala
Welcome to Scala version 2.11.6 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_45).
Type in expressions to have them evaluated.
Type :help for more information.

scala> :load algebraic.scala
Loading algebraic.scala...
defined object algebraic

scala> algebraic.run()
Expressions:
exp1:Add(Val(3),Val(4))
exp2:Sub(Add(Val(3),Val(4)),Val(2))
exp3:Add(Val(3),Sub(Val(2),Val(4)))

Direct interpretation:
exp1 = 7
exp2 = 5
exp3 = 1

Direct compilation into instructions:
exp1 = List(PushInstr(3), PushInstr(4), AddInstr)
exp2 = List(PushInstr(3), PushInstr(4), AddInstr, PushInstr(2), SubInstr)
exp3 = List(PushInstr(3), PushInstr(2), PushInstr(4), SubInstr, AddInstr)

Algebraic interpretation:
exp1 = 7
exp2 = 5
exp3 = 1

Algebraic compilation into instructions:
exp1 = List(PushInstr(3), PushInstr(4), AddInstr)
exp2 = List(PushInstr(3), PushInstr(4), AddInstr, PushInstr(2), SubInstr)
exp3 = List(PushInstr(3), PushInstr(2), PushInstr(4), SubInstr, AddInstr)

