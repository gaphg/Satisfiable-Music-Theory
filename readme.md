# Overview
Satie is a constraint-based music verification and synthesis tool.

# Building and Running
### Install and Build
Make sure OCaml (> 5.1.1 definitely works) and `Z3` are installed. 

The `Z3` executable should be included in your `$PATH`. (Note that this is NOT the OCaml `z3` package). This can be done, for example, by addding `export PATH=$PATH:/home/username/.local/share/racket/8.18/pkgs/rosette/bin` to your `~/.bashrc`, using your file path.

You may also need to install the following packages with `opam` (`opam install <packagename>`):
- `dune`
- `llama_midi`
- `re`

To execute the program, run `dune exec -- satie`. (You can also build and clean with `dune build` and `dune clean`, respectively). These commands should be run from within the `src` directory.

## End-to-end Example
Again, make sure to run these from your `src` file.

#### Synthesis
`dune exec -- satie ../example_rules/bach4part.rules -synth bach.mid`

This synthesizes a 4-part chorale in the style of Bach and outputs it to a file called `bach.mid`. You can open the MIDI file in a music-playing application or a score-reading application like MuseScore. Other example rules are included in the `example_rules` directory.

#### Verification
The file `example_midi/ParallelOctaves.mid` contains a simple musical example that features a parallel octave in the last two measures (a PDF of the score is in `example_midi/ParallelOctaves.pdf`). The `example_rules/no_parallel_octaves.rules` file contains a simple statement disallowing parallel octaves and fifths. To verify that it catches the error, run:

`dune exec -- satie ../example_rules/no_parallel_octaves.rules -midi ../example_midi/ParallelOctaves.mid `

which should output

`music in ../example_midi/ParallelOctaves.mid does not satisfy specification in ../example_rules/no_parallel_octaves.rules`.

Meanwhile, a fixed version updating the octaves is in `ParallelOctavesFixed.mid` (and the corresponding `pdf`) file, and running

`dune exec -- satie ../example_rules/no_parallel_octaves.rules -midi ../example_midi/ParallelOctavesFixed.mid `

should output

`music in ../example_midi/ParallelOctavesFixed.mid satisfies specification in ../example_rules/no_parallel_octaves.rules`.

#### Creating your own examples!
To understand how to write your own rules files, read the following Documentation section. Also take a look at the `example_rules` directory, particularly the `bach4part.rules` file, which contains the most complex logic so far, as examples for how to form proper rules files.

# Documentation
We assume a twelve-tone framework. We currently do not support rhythmic constructs--we assume every note is the same length, and there are no rests.


### Types and Literals:
We support the following types:
- `Integer`: an **integer literal** consists of any (whole) number. 
- `Boolean`: a **boolean literal** is either `true` or `false`.
- `Pitch`: the literal note value. A **pitch literal** is indicated by a number followed by a **p** or **P**. The number follows MIDI conventions, so `60p` is middle C (C4).
- `Interval`: the distance between two notes, in half steps. This can be between any two notes (e.g. two consecutive notes in a single voice, or two notes at the same time between different voices) An **interval literal** is indicated by a number followed by an **i** or **I**. For example, `4i` means a major third.
    - An interval literal may be annotated with `up` or `down`, which means the direction of the interval (ascending or descending) matters. For example, `4i up` means an ascending major third.
- `TimeStep`: the time unit. Currently all notes are 1 TimeStep length. A **time step literal** is indicated by a number followed by a **t** or **T**. For example, `0t` indicates the beginning of the piece (the first note is played at `0t`).
- `Voice`: one voice part or ``instrument''. Only one note per voice is allowed at any given time.
- `List`: an immutable list of any type. A list is comma-separated and surrounded by brackets `[]`. Lists cannot be empty. When indicating types, `List` should be followed by the type, so a list of intervals is indicated by `List of Interval`.
- `TimeSeries`: Timeseries represent a sequence of values, each associated with a time step in a time range. Built-in functions like `pitches-of`, `contour-of`, or `diads-of` all return values of type `TimeSeries`. When indicating types, `TimeSeries` should be followed by the type, so a TimeSeries of pitches should be indicated by `TimeSeries of Pitch`.


### Statements
There are 4 types of statements: configuration statements, specification statements, definition statements, and include statements. While it is not enforced, we a file should start with any include statements, then the configuration statements, then any definition statements, and finally all the specification statements.

#### Configuration statement
A configuration statement configures information about the piece (for checking, the piece that is passed in, and for generation, the piece that should be output). The following statements exists:
- `DECLARE VOICES:` this declares the names of the voice parts in the piece. The declaration should be followed with alphanumeric names, separated by commas.
- `DECLARE LENGTH:` this declares the number of notes in the piece (per voice--with the current restrictions this is the number of measures in the piece). The declaration should be followed by a number.

A well-formed rules file (that isn't just a ''header'' to be `INCLUDE`d in another file) must contain these statements.

#### Specification statement
A specification statement specifies the rules that the piece must follow. There are four types of specifications: require, disallow, prefer, and avoid. A require statement requires that a piece follows the given expression, and a disallow statement means a piece cannot satisfy the following expression. Prefer and avoid means that a piece maximizes/minimizes the following expression, respectively. An example syntax is `REQUIRE: pitches-of soprano at 0t = 60p`, which requires the soprano voice to begin with middle C.

Free variables in a specification statement are implicitly assumed to apply to all values of that type. The previous explanation of `REQUIRE: pitches-of soprano at 0t = 60p` assumed that `soprano` was a voice declared with `DECLARE VOICES:`. However, if we instead wrote `REQUIRE: pitches-of v at 0t = 60p`, where `v` is not an existing voice, then this statement requires that for ALL voices, the voice begins with middle C.

#### Definition statement
Definition statements allow users to bind expression to a name, or to define a custom function. Definitions should come before their usages in specifications. For example, `DEFINE: iv_min3 = 3i` means that `iv_min3` is bound to the interval of a minor third (`3i`). For functions, types are optionally declared since they can be inferred from the body. For example, `DEFINE: isConsonant(p1, p2) = interval-bt(p1, p2) in [ iv_uni, iv_min3, iv_maj3, iv_p4, iv_p5, iv_min6, iv_maj6, iv_oct ]` is how one could specify the idea of consonance for a certain style.  

#### Include statement
Includes includes all of the information from another file into the current file. There is no notion of namespace, so make sure that names and variable names do not collide with the ones in the current file. Filepaths are relative to where you are running the program from. For example, `INCLUDE: stdchord.rules` would include all of the constructs in `stdchord.rules`.

### Music-specific Constructs
We provide the following base functionality:
- `pitches-of`: takes in a `Voice` as input, and returns a `List of Pitch`, which represents of all of its pitches.
- `contour-of`: takes in a `Voice` as input, and returns an `List of Interval`, which represents the intervals between consecutive pitches in the voice.
- `diads-of`: takes in two `Voice`s as input, and returns an `List of Interval`, which represents the intervals between the two different voices.
- `interval-bt`: takes in two `Pitch`es as input, and returns an `Interval`, which represents the interval between the two pitches.
- `flatten`: takes in a `Pitch` or an `Interval` and returns a `Pitch` or `Interval`, respectively, where the notion of an octave have been removed (e.g. for pitches by moving the note to be in between `0p` and `11p` inclusive, so turns `60p` into `0p`, and for intervals, makes it fit within one octave, a major 9th turns into a major 2nd).

### Other Expressions and Operations
Most operations work as you would expect from math/other programming languages. We've highlighted a few key points to be aware of:

#### Logical constructs
- `=>` is the logical implies, and returns a `Boolean`. This is NOT an if-then-else statement, as we do not have control flow in this language
- `<=>` is the logical if and only if, and returns a `Boolean`

#### Arithmetic operators
The arithmetic operators `+` and `-` must both have the same literal type on either side (e.g. to indicate the next time step after `TimeStep t`, you have to write `t + 1t`, NOT `t + 1`).

One exception is that a `Pitch` and `Interval` can be added together to get another `Pitch`. 

#### Equality
The exact equals and not equals are indicated by `=` and `!=`, respectively. To check equality regardless of octave, use `is` and `is-not`. For example, `60p != 0p` but `60p is 0p`.

#### Interval Comparisons
Intervals

### Western Standard Terminology

Definitions and bindings for common concepts in Western music theory are included in the `western_stdlib` directory. To use these these definitions, add the line `INCLUDE: ../western_stdlib/stdlib.rules` to your rules file. The include statement should be added before any references to the definitions in the library. Below is an overview of the definitions provided by `western_stdlib`:

- `stdchord.rules`: provides standard chord definitions of chord qualities (e.g. major, minor, dominant seventh, etc.)
- `stdint.rules`: provides bindings for common interval names (e.g. unison, minor second, etc.) 
- `stdkey.rules`: provides standard keys and modes (e.g. major, minor, lydian, etc.) and functions to get the scale degree of a certain scale
- `stdmotion.rules`: defines ideas of contrary, oblique, parallel, and similar motion
- `stdpitch.rules`: provides bindings for midi pitch literals to human-readable names (e.g. `C4` instead of `60p`)

`stdlib.rules` is a "header" file that includes all of these files.

### Misc
- Single-line comments are supported. A comment begins with a `;`
- All variable names should be alphanumeric. Additionally, `#` and `_` are allowed
- The full grammar is provided in `grammar.bnf`
- Example well-formed rules files are in the `example_rules` folder