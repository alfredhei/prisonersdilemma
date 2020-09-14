# Prisoner's Dilemma strategies in R
A R script to simulate optimal strategy for prisoners dilemma under a given set of rules.

The rules are as follows:
For each iteration of the _game function_, two strategies are chosen at random from a list of common strategies (http://www.prisoners-dilemma.com/common-strategy/).
These two strategies playes the prisoners dilemma ten consecutive times. Both strategies know each others last moves, both only within the current game. 
The strategy with the highest score from those 10 games gets rewarded 1 point.

This is repeated 2*10^5 times.

When the script is done it prints a histogram showing the results for the strategies.
