# Reinforcement Learning in Scala

This repo contains the source code for the demos to accompany my talk
'Reinforcement Learning in Scala'.

The slides are available
[here](https://slides.com/cb372/reinforcement-learning-in-scala).

The demos are available[here](https://cb372.github.io/rl-in-scala/).

## Running locally

The demos are implemented using Scala.js, so first you need to build the
JavaScript:

```
$ sbt fastOptJS
```

Next, start a simple web server of your choice. I use the Python one:

```
$ python -m SimpleHTTPServer
Serving HTTP on 0.0.0.0 port 8000 ...
```

Finally open the site in your browser:

```
$ open localhost:8000
```

## Pacman training

If you'd like to try your hand at making the Pacman agent smarter, the expected
workflow looks something like this:

1. Update
   [PacmanProblem.scala](src/main/scala/rl/pacman/core/PacmanProblem.scala) to
   improve the agent's state space, making it a more efficient learner.

2. Run the training harness:

   ```
   $ sbt run
   ```

   This will make the agent play a very large number of games of Pacman. It
   will run forever. Every 1 million time steps it will print out some stats to
   give an indicator of the agent's learning progress. Every five million time
   steps it will write the agent's Q-values to a JSON file in the
   `pacman-training` directory.

3. Once you have Q-values you are happy with, copy the JSON file to
   `data/pacman/Q.json`, overwriting the existing file.

4. Follow the steps above for running locally. Open the Pacman UI in your
   browser and watch your trained agent show those ghosts who's boss!

### Hints

If you make your state space too large, you'll have a number of problems:

* Your JSON file will probably be huge enough to crash your browser when the UI
  tries to load it.

* The agent will learn very slowly because it needs to explore so many states.

So the trick is to find a way of encoding enough information about the game
state without the number of states exploding.  e.g. if you were to track the
exact locations of Pacman and both ghosts, you already have 65 x 65 x 65 =
274,675 states to deal with.

Your state encoding should also make sense when combined with the reward
function.  For example, the environment gives a reward when Pacman eats food, so
intuitively the state should track food in some way.

If your agent is struggling to win games, you could try:

* Making the ghosts move more randomly by reducing their `smartMoveProb`

* Making a smaller grid, maybe with only one ghost
