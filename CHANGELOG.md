Changelog
=========

Version 0.2.11.0
----------------

*December 5, 2025*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.11.0>

*   Add endpoint for `<year>/stats` to get current star counts. Use with care,
    never cached.

Version 0.2.10.0
----------------

*December 1, 2025*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.10.0>

*   Quick change to make `global_score` optional for `LeaderboardMember` json.
    Will follow-up with more broad changes about the lack of global leaderboard
    and only 12 days for 2025.

Version 0.2.9.1
---------------

*December 11, 2023*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.9.1>

*   Re-export `AoCUserAgent` from `Advent`.

Version 0.2.9.0
---------------

*December 11, 2023*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.9.0>

*   All API requests now require providing a structured user agent data type,
    to follow
    <https://www.reddit.com/r/adventofcode/comments/z9dhtd/please_include_your_contact_info_in_the_useragent/>.
    This is not enforced in the raw servant API, but is enforced in the
    "regulated" `Advent` module.

Version 0.2.8.5
---------------

*December 11, 2023*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.8.5>

*   Compatibility with ghc-9.6 / mtl-2.3

Version 0.2.8.4
---------------

*December 14, 2022*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.8.4>

*   Whoops, the member id is a number now too.

Version 0.2.8.3
---------------

*December 13, 2022*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.8.3>

*   Properly adjust for AoC private leaderboard json: it uses a number instead
    of a string, but the parser was not properly fixed by hand.

Version 0.2.8.2
---------------

*December 9, 2022*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.8.2>

*   As of 2022 AoC, private leaderboard json payload uses a string instead of a
    number for owner_id.

Version 0.2.8.1
---------------

*November 30, 2021*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.8.1>

*   Account for new json schema for private leaderboard stats

Version 0.2.8.0
---------------

*December 14, 2020*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.8.0>

*   Add servant endpoint to query calendar page for a year to infer next puzzle
    and time to puzzle in seconds, backed by the `NextDayTime` data type.
*   Add `AoCNextDayTime` to `AoC` to support the above operation.

Version 0.2.7.1
---------------

*November 28, 2020*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.7.1>

*   Work with servant 0.17 and above.

Version 0.2.7.0
---------------

*December 4, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.7.0>

*   Throughout the library, change from `UTCTime` to `ZonedTime`, except for
    situations where the official site uses actual UTCTime.  The main change is
    in `challengeReleaseTime`.
*   `challengeReleaseTime` moved to *Advent.Types* but re-exported from
    *Advent*.
*   `dlbmTime` changed from `UTCTime` to `NominalDiffTime` `dlbmDecTime`, which
    is time from December 1st.  This is because we don't have information about
    the year from the HTML returned alone.  This fixes a bug where the time
    would always be in 1970.
*   To convert `dlbmDecTime` back into a useful time,added `dlbmCompleteTime`
    to get the actual time of completion (as a `ZonedTime`), and `dlbmTime` to
    get the `NominalDiffTime` representing how long the challenge took.

Version 0.2.6.0
---------------

*December 3, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.6.0>

*   Add `aocServerTime` to get the current time for AoC servers.
*   Fix cacheing rules for global leaderboard (was previously not saving or
    invalidating cache properly) also for prompt (will not invalidate
    part1-only caches if there is no session key)
*   **0.2.6.1 Bugfix**: Fix bug in prompt cache invalidation
*   **0.2.6.2 Bugfix**: HTML parser for articles (for prompt API calls) now
    more robust, adjusting for more malformed HTML from site.

Version 0.2.5.0
---------------

*December 2, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.5.0>

*   Add `runAoC_`, which is `runAoC` but throwing an IO exception instead of
    returning an `Either`.

Version 0.2.4.2
---------------

*November 23, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.4.2>

*   Added instances of `ToJSONKey Day`, `ToJSON Day`, `ToJSONKey Part`, `ToJSON
    Part`.

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
