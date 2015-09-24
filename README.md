slack-lambdabot
===============

[Lambdabot](https://hackage.haskell.org/package/lambdabot) for
[Slack](https://slack.com)

Building
--------

Clone the repo and run

```
cabal sandbox init
cabal install --dependencies-only
cabal configure
cabal build
```

### OS X

On OS X, you may need to install [PCRE](http://www.pcre.org) and specify an
extra include directory. For example, using [Homebrew](http://brew.sh):

```
brew install pcre
```

Then, the install command above becomes

```
cabal install --dependencies-only --extra-include-dirs /usr/local/include
```

Running
-------

slack-lambdabot requires the `SLACK_API_TOKEN` environment variable to be set
to a Slack API token. Follow Slack's
[Bot Users](https://api.slack.com/bot-users) guide to create one.

Then, you should be able to run

```
SLACK_API_TOKEN=foo ./dist/bin/slack-lambdabot
```

Deploying to Heroku
-------------------

slack-lambdabot is easily deployed to [Heroku](https://heroku.com) using
[Haskell on Heroku](https://haskellonheroku.com/). Follow the instructions
there, but in general you should be able to run the following once

```
heroku create -b https://github.com/mietek/haskell-on-heroku
```

Then, follow the instructions in

  1. [Set up private storage](https://haskellonheroku.com/tutorial/#set-up-private-storage)
  2. [Deploy the buildpack](https://haskellonheroku.com/tutorial/#deploy-the-buildpack)
  3. [Build on a one-off PX dyno](https://haskellonheroku.com/tutorial/#deploy-the-buildpack)

Next, ensure your `SLACK_API_TOKEN` is set with

```
heroku config:set SLACK_API_TOKEN=foo
```

Now, subsequent deploys should be as simple as

```
git push heroku master
```
