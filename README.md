# advent-of-code-api

Haskell bindings for Advent of Code REST API.  Caches and throttles requests
automatically.

Specify your requests with 'AoC' and 'AoCOpts', and run them with
'runAoC'.

Example:

```haskell
-- fetch prompt for day 5, part 2
'runAoC' myOpts $ 'AoCPrompt' ('mkDay_' 5) 'Part2'

-- fetch input for day 8
'runAoC' myOpts $ 'AoCInput' ('mkDay_' 8)

-- submit answer "hello" for day 10, part 1
'runAoC' myOpts $ 'AoCSubmit' ('mkDay_' 10) 'Part1' "hello"
```

Please use responsibly.  All actions are rate-limited to a default of one
request every three minutes, with ability to adjust up to as fast as a
hard-coded limit of one request per minute.

Note that leaderboard API is not yet supported.
