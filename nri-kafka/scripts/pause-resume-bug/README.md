# pause-resume-bug

There's a bug in librdkafka, fixed in 2.1.0, while hw-kafka is on 1.6.
The symptom is messages being skipped every once in a while in a slow consumer that has its
buffer filled up and has to pause/resume all the time.

See https://github.com/confluentinc/librdkafka/blob/c282ba2423b2694052393c8edb0399a5ef471b3f/CHANGELOG.md?plain=1#L90-L95

This directory contains sample code to check wether this bug is fixed in our library.

To use it run in a console:

```
cabal run pause-resume-bug-consumer -fpause-resume-bug
```

At the same time, run this in another terminal:

```
cabal run pause-resume-bug-producer -fpause-resume-bug
```

If all works well, no "ERROR" should be printed out. If the bug is present, you should see the error infrequently e.g. 3 times for 300 messages.
