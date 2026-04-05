# KenKen Puzzle Game

A console-based KenKen puzzle game implementation in Haskell.

## Overview

KenKen is a grid-based numerical puzzle similar to Sudoku. Players must fill an N×N grid with numbers 1 through N such that:
- No number appears more than once in any row or column
- Numbers in each "cage" (outlined region) produce a target value using the specified mathematical operation

## Features

- **Interactive Gameplay**: Console-based interface with keyboard controls
- **Multiple Puzzle Sizes**: Support for puzzles from 3×3 to 9×9
- **Puzzle Management**: Load puzzles from files, save/load game states
- **Smart Solver**: Built-in solver with hint system
- **Validation**: Real-time error checking and constraint validation
- **Undo/Redo**: Full game history support

## Installation

### Prerequisites

- [Stack](https://docs.haskellstack.org/en/stable/README/) (Haskell build tool)
- GHC (Haskell compiler) - will be installed by Stack

### Building from Source

```bash
# Clone the repository
git clone https://github.com/yourusername/kenken-game.git
cd kenken-game

# Build the project
stack build

# Run the game
stack exec kenken-game-exe
```

## Usage

### Controls

- **Arrow Keys** or **WASD**: Move cursor
- **1-9**: Enter number in selected cell
- **0** or **Delete**: Clear selected cell
- **H**: Get hint
- **U**: Undo last move
- **R**: Redo
- **S**: Save game
- **L**: Load game
- **N**: New game
- **Q**: Quit

### Puzzle File Format

Puzzles are stored in `.ken` files with the following format:

```
size 4
cage 1 + 5 [(1,1),(1,2)]
cage 2 * 6 [(1,3),(2,3)]
cage 3 - 1 [(1,4),(2,4)]
```

Where:
- `size N`: Grid size (N×N)
- `cage ID OP TARGET [cells]`: Cage definition
  - `ID`: Unique cage identifier
  - `OP`: Operation (+, -, *, /, =)
  - `TARGET`: Target value
  - `[cells]`: List of (row,col) positions

## Project Structure

```
kenken-game/
├── app/           # Main executable
├── src/           # Library source code
│   └── KenKen/    # Game modules
├── test/          # Test suite
├── puzzles/       # Sample puzzle files
└── saves/         # Saved game states
```

## Development

### Running Tests

```bash
stack test
```

### Code Structure

- `Types.hs`: Core data types and structures
- `Grid.hs`: Grid manipulation functions
- `Cage.hs`: Cage constraint logic
- `Validation.hs`: Move and puzzle validation
- `Solver.hs`: Puzzle solving algorithm
- `Parser.hs`: File parsing and generation
- `Display.hs`: Console UI rendering
- `Game.hs`: Main game loop and input handling
- `FileIO.hs`: File operations

## Creating Custom Puzzles

1. Create a new `.ken` file in the `puzzles/` directory
2. Define the grid size
3. Add cages with operations and target values
4. Ensure all cells are covered by exactly one cage
5. Validate using the built-in validator

## Advanced Features

### Solver Algorithm

The solver uses:
- Backtracking with constraint propagation
- Minimum Remaining Values (MRV) heuristic
- Forward checking for early pruning

### Performance

- Efficient array-based grid representation
- Lazy evaluation for constraint checking
- Optimized solver for puzzles up to 9×9

## Contributing

1. Fork the repository
2. Create a feature branch
3. Commit your changes
4. Push to the branch
5. Create a Pull Request

## License

This project is licensed under the BSD-3 License - see the LICENSE file for details.

## Acknowledgments

- KenKen is a registered trademark of Nextoy, LLC
- Inspired by the original KenKen puzzle by Tetsuya Miyamoto