slack-lambdabot-heroku
======================

[lambdabot](https://hackage.haskell.org/package/lambdabot) for
[Slack](https://slack.com) on [Heroku](https://heroku.com).

Building
--------

Clone the repo and run

```
$ cabal install
```

On OS X, you may need to install [PCRE](http://www.pcre.org) and specify an
extra include directory. For example, using [Homebrew](http://brew.sh):

```
$ brew install pcre
$ cabal install --extra-include-dirs /usr/local/include
```
