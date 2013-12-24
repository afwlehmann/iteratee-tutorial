Example code and [slides](/slides/) originating from a talk I gave at
[ScalaMUC](http://www.meetup.com/ScalaMuc/events/150358622/) on 2013-12-17.

The code has been split into two separate packages, `pure` and `wrapped`, which
basically differ in the implementation of the `apply` and `run` methods of both
enumerators and iteratees. Worksheets with usage examples are contained in the
`worksheets` folder.

Getting a grip on the basics of iteratees is supposedly easier by first looking
at the "pure" implementation. Note, however, that the "wrapped" implementation
simply wraps the computations into the "monad-of-the-day" (aside from the fact
that I've used `Try` for demonstration purposes which actually isn't a
monad). Other frameworks implement this in a similar manner.

Functional programming enthusiasts might want to checkout
[scalaz's](https://github.com/scalaz/scalaz) implementation. The latter is
based on monad transformers and hence way more general once it comes to `IO`,
`Future` and friends.
