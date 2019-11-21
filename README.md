[advent-of-code-api][]
======================

[![advent-of-code-api on Hackage](https://img.shields.io/hackage/v/advent-of-code-api.svg?maxAge=86400)](https://hackage.haskell.org/package/advent-of-code-api)
[![Build Status](https://travis-ci.org/mstksg/advent-of-code-api.svg?branch=master)](https://travis-ci.org/mstksg/advent-of-code-api)

Haskell bindings for Advent of Code REST API.  Caches and throttles requests
automatically, and parses responses into meaningful data types.

[advent-of-code-api]: https://hackage.haskell.org/package/advent-of-code-api

Specify your requests with `AoC` and `AoCOpts`, and run them with
`runAoC`.

Examples:

```haskell
-- Fetch prompts for day 5
runAoC myOpts $ AoCPrompt (mkDay_ 5)

-- Fetch input for day 8
runAoC myOpts $ AoCInput (mkDay_ 8)

-- Submit answer "hello" for Day 10, Part 1
runAoC myOpts $ AoCSubmit (mkDay_ 10) Part1 "hello"
```

Please use responsibly.  All actions are rate-limited to a default of one
request every three seconds, with ability to adjust up to as fast as a
hard-coded limit of one request per second.

The neatly exported bindings (handling cookies/authentication, cacheing,
throttling) are in *Advent*.

Session Keys
------------

Session keys are required for most commands, but if you enter a bogus key
you should be able to get at least Part 1 from `AoCPrompt`.  Session keys are
also not needed for daily and global leaderboards.

The session key can be found by logging in on a web client and checking
the cookies.  You can usually check these with in-browser developer
tools.

Servant API
-----------

A Servant API (for integrating with *servant* for features like mock servers,
documentation and low-level client methods) is also exported in *Advent.API*.
The Servant API also parses into meaningful types, but lacks management of
cookies/auth, cacheing, and throttling.  Please use especially responsibly.

