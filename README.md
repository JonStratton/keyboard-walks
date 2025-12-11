# keyboard-walks
This project has moved. To get the latest version, pull from https://codeberg.org/JonStratton/keyboard-walks

Generate lists of keyboard-walks. 

## About
This script takes a single int as a command line argument. It will then generate two types of “keyboard walks”,
* Folds, for example, “1qa sdf”. Or
* Patterns, for example, “1qa 2ws 3ed”.
The script also simulates the pressing of the shift key per chunk of characters. For example “1qa @WS 3ed”.

## Examples
```
./keyboard-walks.lisp 8
Got Fold: (` 1 2 3 2 3 4 5)
Got Fold: (` 1 2 3 @ # $ %)
…
Got Pattern: (? : P ) . l o 9)
Got Pattern: (? : P ) > L O ()
```
