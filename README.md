Example code and [slides](/slides/) originating from a talk I gave at
[ScalaMUC](http://www.meetup.com/ScalaMuc/events/150358622/) on 2013-12-17.

The code has been split into two separate packages, `pure` and `wrapped`, which
mainly differ in the implementation of the `apply` and `run` methods of both
enumerators and iteratees. Worksheets with usage examples are contained in the
`worksheets` folder.

Getting a grip on the basics of iteratees is supposedly easier by first looking
at the "pure" implementation. Note that the "wrapped" implementation simply
wraps the computations into the "monad-of-the-day" (aside from the fact
that I've used `Try` for demonstration purposes which actually isn't a
monad). Other frameworks implement this in a similar manner.

So far, enumeratees have only been implemented in the `pure` package (as a
deliberate choice). Enumeratees effectively act as monad transformers where the
type of the monad is fixed to `Iteratee` (which is also why the respective
code is a little more advanced). Note that the corresponding methods may call
the step functions of the transformed iteratees which means that exceptions
cannot be avoided by simply wrapping enumeratees like it was done with the
iteratees in the `wrapped` package.

Functional programming enthusiasts might want to check out
[scalaz's](https://github.com/scalaz/scalaz) implementation. The latter is
based on monad transformers and thus way more general once it comes to `IO`,
`Future` and friends.
