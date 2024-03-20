# Haskell Maze Game

This is a Haskell maze game project where you navigate through a labyrinth, aiming to reach the end while overcoming obstacles.

## Compilation

To compile the project, use the following command:

```bash
stack ghc Main.hs
```

## Running

You can run the game with the default map file or specify a custom map file.

- To run the game with the default map file:
```bash
./Main
```

- To run the game with a custom map file:
```bash
./Main <file>
```

## Running Tests

You can run tests using the following command:

```bash
./Main -t
```

## Map Structure

The map structure uses the following symbols:
- `S`: Represents the starting point.
- `F`: Represents the ending point.
- ` ` (Space): Represents empty space.
- `*`: Represents walls.
- `a`, `b`, `c`: Represent keys.
- `A`, `B`, `C`: Represent doors.
- `@`: Represents portals.

## Map Restrictions

- The outer boundaries of the map are composed of `*`.
- Each labyrinth has one start (`S`) and one end (`F`).
- Labyrinths contain either 0 or 2 portals.
