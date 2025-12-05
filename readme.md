# Overview
TODO write explanation of what this is

# Building and Running
### Install and Build
Make sure OCaml (> 5.1.1 definitely works) and `Z3` are installed. The `Z3` executable should be included in your `$PATH`. (Note that this is NOT the OCaml `z3` package).

You may also need to install the following packages with `opam` (`opam install <packagename>`):
- `dune`
- `llama_midi`
- `re`

To execute the program, run `dune exec -- satzart`. (You can also build and clean with `dune build` and `dune clean`, respectively). These commands should be run from within the `src` directory.

## End-to-end Example
Synthesizes a major scale:
`dune exec -- satzart ../example_rules/major_scale.rules -synth out.mid`

To understand how to write your own rules files, read the following Documentation section.

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
- `TimeSeries`: TODO. When indicating types, `TimeSeries` should be followed by the type, so a TimeSeries of pitches should be indicated by `TimeSeries of Pitch`.


### Statements
There are 4 types of statements: configuration statements, specification statements, definition statements, and include statements. While it is not enforced, we a file should start with any include statements, then the configuration statements, then any definition statements, and finally all the specification statements.

#### Configuration statement
A configuration statement configures information about the piece (for checking, the piece that is passed in, and for generation, the piece that should be output). The following statements exists:
- `DECLARE VOICES:` this declares the names of the voice parts in the piece. The declaration should be followed with alphanumeric names, separated by commas.
- `DECLARE LENGTH:` this declares the number of notes in the piece (per voice--with the current restrictions this is the number of measures in the piece). The declaration should be followed by a number.

TODO: other decls aren't really useful rn?
- `DECLARE KEY:`
- `DECLARE TIME UNIT TICKS:`

#### Specification statement
A specification statement specifies the rules that the piece must follow. There are four types of specifications: require, disallow, prefer, and avoid. A require statement requires that a piece follows the given expression, and a disallow statement means a piece cannot satisfy the following expression. Prefer and avoid means that a piece maximizes/minimizes the following expression, respectively. An example syntax is `REQUIRE: pitches-of soprano at 0t = 60p`, which requires the soprano voice to begin with middle C.

Free variables in a specification statement are implicitly assumed to apply to all values of that type. The previous explanation of `REQUIRE: pitches-of soprano at 0t = 60p` assumed that `soprano` was a voice declared with `DECLARE VOICES:`. However, if `soprano` was not bound to an existing voice, then this statement requires that for ALL voices, the voice begins with middle C.

#### Definition statement
Definition statements allow users to bind expression to a name, or to define a custom function. Definitions should come before their usages in specifications. For example, `DEFINE: m3 = 3i` means that `m3` is bound to the interval of a minor third (`3i`). For functions, types are optionally declared since they can be inferred from the body. For example, `DEFINE: isConsonant(p1, p2) = interval-bt(p1, p2) in [ unison, m3, M3, P4, P5, m6, M6, octave ]` is how one could specify the idea of consonance for a certain style.  

#### Include statement
Includes includes all of the information from another file into the current file. There is no notion of namespace, so make sure that names and variable names do not collide with the ones in the current file. Filepaths are relative to where you are running the program from. For example, `INCLUDE: std_intervals.rules` would include all of the constructs in `std_intervals.rules`.

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

#### Equality
The exact equals and not equals are indicated by `=` and `!=`, respectively. To check equality regardless of octave, use `is` and `is-not`. For example, `60p != 0p` but `60p is 0p`.

### Misc
- Single-line comments are supported. A comment begins with a `;`
- All variable names should be alphanumeric
- The full grammar is provided in `grammar.bnf`
- Example well-formed rules files are in the `example_rules` folder
