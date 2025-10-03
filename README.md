# CS316 “Functional Programming”

Welcome to the source code repository for the University of Strathclyde CS316 “Functional Programming” course.

This is a course designed to teach Haskell to undergraduate students. The written course materials are available from this repository. Video lectures and access to the Mattermost forum for this course are available to Strathclyde students via the course's [MyPlace page](https://classes.myplace.strath.ac.uk/course/view.php?id=15897).

## Getting Started

The code in this repository is structured as a [Cabal](https://www.haskell.org/cabal/) project. You will need to install GHC (the Haskell compiler) and Cabal to get started. It is also advisable to install HLS (the Haskell Language Server) and an LSP-capable editor (e.g. Emacs or VSCode) to read and edit the code.

To load the code into `ghci` for interactive exploration, you can used

```
$ cabal repl
```

which will load all the lecture notes into the interactive `ghci` repl. Use `import WeekXX` to open a particular module for experimentation. Using the command `:reload` to reload after any changes are made.

## Syllabus and Lecture Notes

The lecture notes for this course are intended to accompany the video lectures (only available to Strathclyde students for now), and provide mostly the same information in a searchable, accessible and less bandwidth hungry format.

The notes are Haskell files with interleaved code and commentary. You are encouraged to experiment by loading these files into `ghci` (using `cabal repl`) and editing them. Each week also has a set of tutorial questions with solutions that you should have a go at to test your knowledge.

### Week 1

- [Lecture notes](lecture-notes/Week01.hs) : Data and Functions
- [Tutorial Problems](lecture-notes/Week01Problems.hs)
- [Tutorial Solutions](lecture-notes/Week01Solutions.hs)
- [Live Lecture code (Tuesday)](lecture-notes/Week01Live.hs)
- [Live Lecture code (Friday)](lecture-notes/Week01Live2.hs)
- [Week 2](lecture-notes/Week02.hs) : Solving Problems by Recursion
  - [Tutorial Problems](lecture-notes/Week02Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week02Solutions.hs)
<!--  - [Live Lecture code (Friday)](lecture-notes/Week02Live.hs)
!-->
- [Week 3](lecture-notes/Week03.hs) : Higher Order Functions
  - [Tutorial Problems](lecture-notes/Week03Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week03Solutions.hs)
<!--
  - [Live Lecture code (Tuesday)](lecture-notes/Week03Live.hs)
- [Week 4](lecture-notes/Week04.hs) : Patterns of Recursion
  - [Tutorial Problems](lecture-notes/Week04Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week04Solutions.hs)
  - [Live Lecture code (Tuesday and Friday)](lecture-notes/Week04Live.hs)
- [Week 5](lecture-notes/Week05.hs) : Classes of Types
  - [Tutorial Problems](lecture-notes/Week05Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week05Solutions.hs)
  - [Live Lecture Notes (Tuesday and Friday)](lecture-notes/Week05Live.hs)
- [Week 6](lecture-notes/Week06.hs) : Simulating side-effects: Exceptions, State, and Printing
  - [Tutorial Problems](lecture-notes/Week06Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week06Solutions.hs)
  - [Live Lecture code (Tuesday and Friday)](lecture-notes/Week06Live.hs)
- [Week 7](lecture-notes/Week07.hs) : Monads
  - [Tutorial Problems](lecture-notes/Week07Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week07Solutions.hs)
  - [Live Lecture Notes (Tuesday)](lecture-notes/Week07Live.hs)
- [Week 8](lecture-notes/Week08.hs) : Real I/O and Parser Combinators
  - [Tutorial Problems](lecture-notes/Week08Problems.hs)
  - [Tutorial Solutions](lecture-notes/Week08Solutions.hs)
  - [Live Lecture Notes (2023)](lecture-notes/Week08Live2023.hs)
  - [Live Lecture Notes (2024)](lecture-notes/Week08Live.hs)
- [Week 9](lecture-notes/Week09.hs) : Data Dependencies and Applicative Functors
  - [Live Lecture Code (2023)](lecture-notes/Week09Live.hs)
  - [Live Lecture Code (2024)](lecture-notes/Week09Lecture.hs)
- [Week 10](lecture-notes/Week10.hs) : Lazy Evaluation and Infinite Data
  - [Live Lecture Code (Tuesday)](lecture-notes/Week10Live.hs) on deriving `Functor` by type-level programming.
  - [Live Lecture Code (Friday)](lecture-notes/Week10Live2.hs) on testing with QuickCheck.
!-->

You can take a look at [last year's notes](archives/) for similar notes and some different exercises.
