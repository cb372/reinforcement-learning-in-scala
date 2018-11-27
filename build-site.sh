#!/usr/bin/env bash

set -e

sbt clean fullOptJS

echo "Copying reinforcement-learning-in-scala-opt.js"
mkdir -p ../rl-in-scala/js
cp target/scala-2.12/reinforcement-learning-in-scala-opt.js ../rl-in-scala/js

for file in *.html; do
  echo "Copying $file"
  sed -e "s/target\/scala-2.12\/reinforcement-learning-in-scala-fastopt.js/js\/reinforcement-learning-in-scala-opt.js/" $file > ../rl-in-scala/$file
done

echo "Copying Pacman data dir"
mkdir -p ../rl-in-scala/data
cp -R data/pacman ../rl-in-scala/data
