## Running


### As required
```
sbt "run 30"
sbt "run 45"
```
    → will return the number of days until a the entire permutation is exhausted
    
```
sbt "run 30 325"
```
   → will return the state of the machine in JSON format after 325 tick events (minutes)
   
### Tests   
   
```
sbt test
```
Should have started here ☺


### Bonus

```
sbt run
```
Rough eye candy


## Where to start

- `BallClock` is the main structure implementing a standard ball clock as described in the brief
- `Accumulator` (of which there are two subclasses, Tilting and Static) models each accumulating row. A Tilting row tilts when its contents reach capacity (including glued-on balls)
- `Ball` models a ball (to keep track of identity, effectively)
- `BallClockApp` is the main entry point, under control of ZIO
- `BallClockSpec` is the main test entry point, *not* under control of ZIO.Test but using ol'trusty ScalaTest+ScalaCheck and a thin ZIO adapter layer (*at this point, I'm not really sold on ZIO.Test's ergonomics and feel building it a `ZioFlatSpecLike` trait modeled after its `AsyncFlatSpecLike` would provide a much better service.*)

## Quick comments

- The code is built in an as pure FP way as I could; using ZIO (more for its Railroad-Structured Programming capabilities than anything else — `cats.IO[Either[E,A]]` would have worked fine)
- The asynchronous / parallelism capabilities of ZIO haven't been much used, except of course in the bonus "run as a real clock" mode.
- I'm not entirely happy with my use of the `zio.Schedule` object, with random fiber failure chatter in `HEAD^^` related to Schedule composition. To be dug further, as this feels like an unnecessary rough edge.
- It is possible to very quickly spawn non-standard ball clock geometries simulations 


