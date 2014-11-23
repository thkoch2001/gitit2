gitit2
======

gitit2 is a rewrite of [gitit] using the [Yesod] web framework.
My intention is that it will eventually replace gitit.

[gitit]: http://gitit.net

To install:

    cabal install

If you run gitit2 from the source directory, a default
`wikidata` directory will be automatically created, based
on the settings in `settings.yaml`.  You will then be able
to view your wiki at <http://localhost:3000>.

Why a rewrite?
--------------

The old code base was ugly, for a variety of reasons, including
the fact that it dates from an early phase in happstack's history
(when many things that are easy now were hard) and an early phase
in the author's Haskell experience.  Yesod provides abstractions,
such as type safe routes, that make many of the ugly parts go away.
The new code base should be much smaller, easier to modify, and
easier to be confident in.

gitit2 is implemented as a Yesod subsite, making it trivial
to embed a gitit wiki in a Yesod application.

What is still missing?
----------------------

The basic wiki functions have all been implemented. You can
create, edit, and modify pages.  You can search, view
history, and comment.  You can export in any format that
pandoc supports.

* ~~User login -- currently there is no access control.~~
* ~~Preview in edit.~~
* Loading plugins (the plugin API works, but there is not
  yet support for specifying plugins in the config file).
* Page locking (no-delete, no-edit).
* ~~Command-line option to specify a config file.~~
* Site initialization wizard.
* Documentation.
* Multiple options for treatment of math in HTML.
* Configurable session timeout.
* Option to show bird tracks in LHS.
* Log level options.
* Configurabel mime types file.
* 'absolute-urls' option for wikilinks?
* 'feed-refresh-time' option?
* Option to disable PDF export?
* Option for pandoc user data directory
* Option to disable sanitization?
* System for customizing CSS

[Yesod]: http://www.yesodweb.com/

Hacking gitit2
--------------

Here are some notes for those who want to modify or
add to the code.

* The library is all in one file, `Network/Gitit.hs`.
* The executable, which uses the library, is in `src/gitit.hs`.
* There are no longer any templates.  The HTML is produced
  by hamlet code in `Network/Gitit.hs`.
