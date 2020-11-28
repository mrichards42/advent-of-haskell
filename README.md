# Advent of Haskell

Using [Advent of Code][aoc] to learn Haskell.

## Running

This should do it ...

```sh
stack run all
```

... but I had a heck of a time struggling with linker issues, perhaps related
to one of [these][stack-error-1] [issues][stack-error-2]? So instead I'm
running this through ghci like so:

```sh
echo ":main all\n:quit" | stack repl
```

[aoc]: https://adventofcode.com
[stack-error-1]: https://github.com/commercialhaskell/stack/issues/4373
[stack-error-2]: https://github.com/commercialhaskell/stack/issues/3487
