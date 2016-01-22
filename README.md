boris
=====

```
Boris is a retired russian cosmonaut who is saddened by the state of
bloated, not-working, and quite frankly - ridculous - programs which
have incorrectly assumed they were up to the task of running so called
"build" processes. So in his ample lesiure time, boris has taken to
running your builds for you.
```

Telling Boris How To Build Your Project
---------------------------------------

For now, boris is pretty naive. He likes to hang out with the hip
lego-building crowd, so he goes with the flow of just automatically
building all the [master](https://github.com/ambiata/master) builds
if he knows about your git repository.

To know about your github repository he needs to be given a file that
looks like this:

```
git@github.com:ambiata/p */master master dist
git@github.com:ambiata/p */pr/*/head master branches
git@github.com:ambiata/disorder */master master dist
git@github.com:ambiata/mismi */master master dist
PROJECT REFSPEC COMMAND [ARGS ...]
```

Asking Boris To Build Your Project
----------------------------------

Something, something....
