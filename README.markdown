# What it is

Soundflow is designed to simplify the process of translating music from brain to computer.
Rather than equating a keypress (or several) with a single note, a button can also add harmonies,
loop a track, split your keyboard, or change it altogether.

This is an evolving work, and more complex musical ideas will be added as it progresses.
Even in its current incarnation, it can vastly simplify the playing of many songs.

# Setting Up

You will need:

 * Haskell
 * cabal
 * Summit (https://github.com/RobotGymnast/Summit)
 * Game-Wrappers (https://github.com/RobotGymnast/Game-Wrappers)
 * imidi (https://github.com/RobotGymnast/midi-monad)
 * extensible-effects

You can set up the build environment by running

    scripts/setup.sh

Doing so is **mandatory** before committing any code, as this also sets up pre-commit hooks.
All scripts should be run from the project root directory.

# Building

When the environment has been successfully set up, the project can be built with

    scripts/build.sh

# Running

After a successful build, the application can be run from

    dist/build/Soundflow/Soundflow

Keys are configurable in `src/Config.hs`

# Documentation

Haddock documentation can be generated using

    scripts/docgen.sh

By default, the documentation is generated to `dist/docs/html/`

# Tests

To run the tests after a successful build, run

    scripts/test.sh

# Code Standards

The `Util` and `Wrappers` folders are for code which is *not project-specific*:
Direct library wraps go into `Wrappers/`, and useful generic functions and modules go in `Util/`.

Coding is a language. You are expressing ideas, so they should be as clear, concise, and elegant as possible.

 * Wrap to 120 characters
 * Functions ending with a single quote usually require a transformation function as one of their parameters
 * When indenting multi-lined bodies, align SOMETHING visually (e.g. operators)
   or just use a multiple of 2 spaces (at least 4)
 * Indent a `where` keyword by 2 spaces, and the declarations within it by 4
 * If a `where` clause has more than one line in it, the `where` keyword should be on a distinct line from any code
 * Do not have more than one embedded subscope (A `let` inside a `where` is acceptable, but to be used sparingly)
