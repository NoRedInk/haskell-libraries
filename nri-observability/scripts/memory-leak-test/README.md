# Memory Leak Test

Testing for memory leaks in the Observability library.

A commit in this repo caused multiple NRI service to leak memory.

This script is an attempt to reproduce the leak in a controlled environment.

## Running

```sh
cabal run memory-leak-test
```

At the end of execution you will see a summary of memory usage.

## Execution logs

### w/ waj's PR

#### ids: 100_000 (sequence)

```
10,450,960,160 bytes allocated in the heap
   159,580,640 bytes copied during GC
     5,992,320 bytes maximum residency (109 sample(s))
     2,179,200 bytes maximum slop
            81 MiB total memory in use (1 MiB lost due to fragmentation)
```
#### ids: 10_000 (sequence)

```
 1,045,693,312 bytes allocated in the heap
    36,096,416 bytes copied during GC
     1,654,336 bytes maximum residency (64 sample(s))
       885,664 bytes maximum slop
            71 MiB total memory in use (1 MiB lost due to fragmentation)
```

### w/o waj's PR

#### ids: 10_000 (sequence)
```
 901,761,584 bytes allocated in the heap
  38,123,856 bytes copied during GC
   1,630,936 bytes maximum residency (69 sample(s))
     810,240 bytes maximum slop
          70 MiB total memory in use (0 MiB lost due to fragmentation)
```

#### ids: 10_000 (mapConcurrently)

```
 1,212,006,112 bytes allocated in the heap
   331,789,960 bytes copied during GC
   378,212,016 bytes maximum residency (8 sample(s))
     3,235,704 bytes maximum slop
           706 MiB total memory in use (14 MiB lost due to fragmentation)
```