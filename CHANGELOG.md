Changelog
=========

Version 0.2.4.1
---------------

*November 21, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.4.1>

*   Export `DayInt` and `_DayInt` from *Advent* module

Version 0.2.4.0
---------------

*November 21, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.4.0>

*   Fixed caching behavior and documentation to reflect that Day 25 actually
    does have 2 stars, like normal.
*   Some extra smart constructors for moving between `Day` and `Integer`, in
    the form of a `Prism` and a pattern synonym.

Version 0.2.3.0
---------------

*November 21, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.3.0>

*   Add API commands for daily and global leaderboards.
*   In the process, the Servant API is reshuffled a bit: `Articles` has been
    generalized to `HTMLTags "article"`, to also support `HTMLTags "div"`.
    `FromArticle` is now `FromTags "article"`.
*   Move some of the data types to be in their own module, *Advent.Types*.

Version 0.2.2.1
---------------

*November 19, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.2.1>

*   Fixed prompt parser that would fail on 2016 Day 2 Part 2 because of a
    malformed `<span>...</title>` tag pair in the prompt HTML

Version 0.2.2.0
---------------

*November 9, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.2.0>

*   Rewrote submission response parser using megaparsec for better errors

Version 0.2.1.0
---------------

*November 5, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.1.0>

*   Export `Day` constructor from *Advent*

Version 0.2.0.0
---------------

*November 4, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.0.0>

*   Switch from libcurl to servant, which allows for shedding of external
    dependencies.
*   Support leaderboard API with data type.
*   Expose raw servant API and client functions, for those who want to build
    documentation or a mock server or low-level client.

Version 0.1.2.X
---------------

*   *December 8, 2018*: *BUGFIX* Switched from *taggy* to *tagsoup*, after observing that *taggy*
    had some issues parsing 2018's Day 8 challenge prompt.

    <https://github.com/mstksg/advent-of-code-api/releases/tag/v0.1.2.1>

*   *December 8, 2018*: *BUGFIX* Add CPP to deal with building issues on GHC 8.2

    <https://github.com/mstksg/advent-of-code-api/releases/tag/v0.1.2.2>

*   *December 8, 2018*: *BUGFIX* Fix cache directory to separate by year

    <https://github.com/mstksg/advent-of-code-api/releases/tag/v0.1.2.3>

Version 0.1.2.0
---------------

*December 7, 2018*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.1.2.0>

*   Fixed cache to store prompts at `.html` instead of `.yaml`
*   `SubIncorrect` and `SubWait` now include fields for wait times.
*   Re-implemented submission result parsers using *attoparsec*

Version 0.1.1.0
---------------

*December 7, 2018*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.1.1.0>

*   More robust parser for submission results.  Also now reports "hints" if
    possible.

Version 0.1.0.0
---------------

*December 5, 2018*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.1.0.0>

*   Initial Release
