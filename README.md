# Okasaki's Functional Data Structures in Haskell

[![Build Status](https://travis-ci.org/tejasbubane/functional-data-structures.svg?branch=solutions)](https://travis-ci.org/tejasbubane/functional-data-structures)

Data structures from [Okasaki's book](https://www.goodreads.com/book/show/594288.Purely_Functional_Data_Structures) in Haskell along with tests - so that you can implement and check your solutions.

### List of data structures:

* [List](src/List.hs)
* [BinarySearchTree](src/BinarySearchTree.hs)
* [LeftistHeap](src/LeftistHeap.hs)
* [BinomialHeap](src/BinomialHeap.hs)
* [RedBlackTree](src/RedBlackTree.hs)
* [Queue](src/Queue.hs)
* [AmortizedQueue](src/AmortizedQueue.hs)
* [Deque](src/Deque.hs)
* [Trie](src/Trie.hs)


Implementations can be found in `solutions` branch.

**Note:** This repo does not contain all of the structures from the book. These are the one which I mostly understood in first reading of the book and which were discussed in my talk at [Bangalore functional programming meetup](https://www.meetup.com/Bangalore-Functional-Programmers-Meetup/events/257190891/).

### Tests:

Tests are in `spec` directory.

Run individual tests:

```hs
stack test --test-arguments "-m "List""
```

Run all tests:

```hs
stack test
```
