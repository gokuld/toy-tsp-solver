# toy-tsp-solver

This is one of my first 'hello world' Haskell projects, a toy solver for the Travelling Salesman Problem.

It uses simulated annealing for optimization and the only metric it optimizes on is Euclidean distance.

As of now there is only one _move_, a stochastic two-opt move.

Since I wrote this while still learning Haskell, please lower your expectations about code quality, though I don't think it should be too bad.

Algorithmically speaking this is not the best possible algorithm for solving the TSP and there is a lot that can be improved. This is merely a hello world problem for me to learn Haskell.

Credits: Some of the implementation ideas are inspired from this fun [tutorial](http://www.lisperati.com/haskell/) as well as this related [talk on Youtube](https://www.youtube.com/watch?v=GjyE3tKscSw).
