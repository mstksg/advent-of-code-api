Changelog
=========

Version 0.2.0.0
---------------

*November 3, 2019*

<https://github.com/mstksg/advent-of-code-api/releases/tag/v0.2.0.0>

*   Switch from libcurl to servant, which allows for shedding of external
    dependencies.
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
