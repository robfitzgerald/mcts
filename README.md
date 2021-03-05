# MCTS

a Monte Carlo Tree Search implementation for combinatorial search, using a variation of the UCT cost function suitable for arbitrary cost functions.

UCT variant detailed in _J.P. Pedroso and R. Rei, "Tree Search and Simulation", Applied Simulation and Optimization, Springer International Publishing, pp 119-131, 2015._

two versions concurrently offered in the repo:
- cse.bdlab.fitzgero.mcts.MCTS2
  - most recently implemented
- cse.bdlab.fitzgero.mcts.MonteCarloTreeSearch
  - older, works, inefficient

goals of MCTS2:
- MCTS is an online anytime search and so for performance, it has been implemented using mutable semantics
- operations in MCTS2 have been implemented as extension methods
- state representation is a BitSet (see [bitboard](https://en.wikipedia.org/wiki/Bitboard))
- specialized class representation for Leaf + Branch1 nodes

License: MIT
