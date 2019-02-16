boris
=====

> WARNING: currently not in buildable state - going through a big clean-up.


```
Boris is a retired Russian cosmonaut who is saddened by the state of
bloated, not-working, and quite frankly - ridiculous - programs which
have incorrectly assumed they were up to the task of running so called
"build" processes. So in his ample leisure time, Boris has taken to
running your builds for you.
```

<img src="https://cloud.githubusercontent.com/assets/722409/15855397/9f777ec2-2cf2-11e6-8eaf-33d9433b73df.png" width="300" align="right"/>


Working with Boris
------------------

There are three main ways to interact with boris:

 - command line

 - hipchat bot

 - web interface


### command line

```
boris build <project> <build> [<ref>]
boris discover <project>
boris cancel <build-id>
boris list
boris list <project>
boris list <project> <build>
boris status <build-id>
boris log <build-id>
boris ignore <project> <build>
```

#### build

Trigger a project build.

This is different to other systems where triggering may mean "check
if it needs to" or similar. With Boris asking for something to
build, will result in exactly one build (that may or may not succeed).

When triggering a build you need to specify:

 - the project you want to build

 - the build name you want to build

 - if the build can be run on multiple branches, you also need
   to tell it which of those branches to build, when asking for
   a build, Boris will not guess.

Examples:

```
boris build example dist-7-10
boris build example branches-7-10 topic/feature-1
boris build -t example branches-7-10 topic/feature-1
```


#### discover

Determine any outstanding builds for a project. This is closer to
other non-boris systems idea of "triggering" a build. This is what
github hooks for example would use to force builds on a commit.

Examples:

```
boris discover example
```

#### cancel

Make an attempt to cancel the build.

Basically it just sets a flag, and when the build next sends a
heartbeat it will shut itself down and fail if this flag is set.

Examples:

```
boris cancel 71
```

#### list

List all projects that Boris knows about.

Examples:

```
boris list
```


#### list <project>

List all builds that Boris has ever attempted for a specific project.

This isn't all that it *can* build, because that would require
scraping the git repository which we really don't want to do for this
type of query.

Examples:

```
boris list example
```


#### list <project> <build>

List all build ids that Boris has ever attempted for this specific
project and build.

Examples:

```
boris list example dist-7-10
boris list example branches-7-10
```

#### status <build-id>

Get the status of a specific build id.


Examples:

```
boris status 71
boris status 72
```


#### log <build-id>

Get the log of a specific build id.

Examples:

```
boris log 71
boris log 72 | grep INTERESTING
boris log 72 | less
```

#### ignore <project> <build>

Ignore any outstanding failures for a specific build.

Examples:

```
boris ignore example dist-7-10
```

### the bot

The bot is largely designed to mimic the command line interface, and
provides most functionality except dumping logs. We won't provide all
the details here as the implementation lives elsewhere and may change,
but as a starting point `/boris help` should get you part of the way
there.


### web interface

The [web interface](http://boris.ambiata.com) is a simple read-only
view to help with those with out easy access to command line
tooling. It is useful for discovering builds, retrying failed builds,
cancelling builds and viewing logs.


Telling Boris How To Build Your Project
---------------------------------------

Boris needs to be told two things:

 - What he is allowed to build?

 - How he should perform the build?


### What is Boris allowed to build?

Boris understands git, so permission to build is defined in terms of a
git ref spec (with wildcard support), mapping builds to the git refs
they can be applied to.

This is probably a lot easier to understand by example. So, if we have
a project for Boris to build, and it has two types of builds:

 1. "dist-*" wildcard that run any matching builds on the "master" branch; and

 2. "branches-*" wildcard that runs any matching builds on any branch starting
    with "topic/", for example, "topic/my-feature".

```toml
[boris]
  version = 1

[build.dist-*]
  git = "refs/heads/master"

[build.branches-*]
  git = "refs/heads/topic/*"

```

As Boris trusts your git repository more than some random repository
of gui-mutated cruft, so he needs you to place this config in a file
called 'boris-git.toml' in the root of your git
repository. Importantly, but not obviously until you understand how
Boris works, this file is *only read from the master branch*.

If you are setting up boris for an Ambiata project you may find
these guidelines for [simple projects](https://github.com/ambiata/engineering/blob/master/how/project/builds-simple.md)
and [multi level projects](https://github.com/ambiata/engineering/blob/master/how/project/builds-multi.md) useful.

For more detail on why, you can try [how Boris works](#how-boris-works)
below, but for now accept the simple justification that we need to
know this whitelist before we know the branch we are building so we
must be able to fetch it from a known (and trusted) location, storing
this information on each branch for example would create unresolvable
ambiguity.


### How he should perform the build?

Continuing the above example. Boris continues with the idea that your
repository is the best place to tell him how to perform a build.

So our two build types require definition:

```
[boris]
  version = 1

[build.dist-7-10]
  command = [
      ["./mafia", "build"]
    ]

[build.branches-7-10]
  command = [
      ["./mafia", "build"]
    ]
```

This tells Boris, just run `./mafia build` to perform the build.

Boris will succeed or fail based on the exit code of these commands, so
make sure your build sets exit codes properly.

If you want to run multiple commands, you can:

```
[boris]
  version = 1

[build.dist-7-10]
  command = [
      ["./mafia", "build"]
    , ["./bin/upload", "dist/build/example/example"]
    ]

[build.branches-7-10]
  command = [
      ["./mafia", "build"]
    ]
```

Here each nested array represents one command to run, the first
element in those arrays is the program to run, can be relative,
absolute, or a plain command on the path, the rest are the
arguments. No quoting is required for the arguments each will be
passed as a single atom, no mater spaces etc...

The next thing you might like to do is to add some lifecycle to
your build. For example, updating github checks, or notifying
on failures. These lifecycle hooks can be control from some
configuration:


```
[boris]
  version = 1

[build.dist-7-10]
  pre = [
      ["github-cli", "update-commit", "starting"]
    ]

  command = [
      ["./mafia", "build"]
    , ["./bin/upload", "dist/build/example/example"]
    ]

  success = [
      ["github-cli", "update-commit", "success"]
    ]

  failure = [
      ["github-cli", "update-commit", "failure"]
    , ["hipchat-hassle"]
    ]


[build.topic]
  command = [
      ["./mafia", "build"]
    ]
```

The full set of lifecycle hooks are:

  - `pre` always runs before the build

  - `command` runs the actual build

  - `post` runs after the build no mater if success or failure

  - `success` runs after the build only on success (after post if both specified)

  - `failure` runs after the build only on failure (after post if both specified)


Boris also currently has deeper integration with `master` for building
your project and `tsar` for lifecycle hooks. So if you are using those
tools you may be able to simplify your config down to:


```
[boris]
  version = 1

[build.dist-7-10]

[build.branches-7-10]
```

This implicitly expands to:

```
[boris]
  version = 1

[build.dist-7-10]
  pre = [["tsar", "pre"]]
  command = [["master", "build", "dist-7-10"]
  success = [["tsar", "success"]]
  failure = [["tsar", "failure"]]

[build.branches-7-10]
  pre = [["tsar", "pre"]]
  command = [["master", "build", "branches-7-10"]
  success = [["tsar", "success"]]
  failure = [["tsar", "failure"]]
```

Whilst this is nice, the tight integration/dependence does not sit
that well with Boris who hopes to provide a better mechanism for
templatizing away the boilerplate, so stay tuned for updates here.


How Boris Works
---------------

### build

When a build is triggered with Boris, the following happens:

 - Boris checks the project is known about (using the only non-repository
   driven configuration, which is a mapping from project name to git url).

 - Boris allocates the build an id. All builds have a globally unique
   id which makes it easy to directly reference any build.

 - Boris puts the build on the queue to be actioned (he can't/won't
   make any additional judgement calls on whether this is a valid
   build at this point because it needs access to the repository level
   configuration to do so.

 - One of the running Boris services will pick the build off the queue
   when it has some downtime.

 - It will acknowledge this build id to make sure no one else picks
   it up (this is when the build officially "starts" from a duration
   perspective).

 - It will clone the repository for the project.

 - Using the repository it will find boris-git.toml and check if it
   knows about the requested build.

 - Assuming it is a valid build it will use the ref spec to find what
   needs to build. Boris is a serious individual, who does not
   tolerate guessing. So unlike some un-named build tools this must be
   unambiguous. Meaning:

     - The refspec has no wildcards (i.e. specifies exactly on ref), and
       that ref exists.

     - The refspec has a wildcard, but still matches only one ref.

     - The refspec has a wildcard, it matches more than one, but you
       request a specific ref (i.e. you are running the topic build
       from the running example, and you include the exact branch you
       want to build.

     - In all other situations, Boris will fail the build because you
       have not been specific enough.

  - Now that there is a precise ref, that is resolved back to a commit.

  - The commit is checked out ready for the build.

  - The `pre` commands are run in order, stopping and failing on the
    first failure if there is one.

  - If `pre` succeeded, the `command` commands are run in order,
    stopping after the first failure if there is one.

  - The `post` commands are run in order (no matter if `command` succeeded).

  - If `command` succeeded, the `success` commands are run in order,
    stopping after the first failure if there is one.

  - If `command` failed, the `failure` commands are run in order,
    stopping after the first failure if there is one.

  - If any of pre/command/post/success/fail, then the build overall is
    marked as a failure, otherwise they all are ok and the build is
    marked as a success.


Sundry information:

  - There is a heartbeat process following the build, if a build fails
    to call in after a while (for example if the node dies, it will be
    cancelled and marked as a failure).


### discover

When a discover is triggered with Boris, the following happens:

 - Boris checks the project is known about (using the only non-repository
   driven configuration, which is a mapping from project name to git url).

 - Boris allocates the discover an id. All discovers have a globally
   unique id (unique with respect to builds as well)

 - Boris puts the build on the queue to be actioned (he can't/won't
   any additional judgement calls on whether this is a valid build at
   this point because it needs access to the repository level
   configuration to do so.

 - One of the running Boris services will pick the build off the queue
   when it has some downtime.

 - It will clone the repository for the project.

 - Using the repository it will find boris-git.toml.

 - For every build / resolved git-ref pairing, it will
   check if it has attempted a build for that commit.

 - If the commit has been seen, that pairing will be ignored.

 - If the commit has not been seen, that pairing will result in
   a triggered build.
