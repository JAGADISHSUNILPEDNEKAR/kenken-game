# KenKen Game Dependencies and Setup

## Required Dependencies

### Core Dependencies (package.yaml)
```yaml
dependencies:
- base >= 4.7 && < 5
- containers >= 0.6
- array >= 0.5
- megaparsec >= 9.0
- text >= 1.2
- mtl >= 2.2
- directory >= 1.3
- filepath >= 1.4
- time >= 1.9
- ansi-terminal >= 0.11
```

### Optional GUI Dependencies
```yaml
# For Brick TUI (recommended)
- brick >= 1.0
- vty >= 5.33
- microlens >= 0.4
- microlens-th >= 0.4

# Alternative: For Gloss GUI
# - gloss >= 1.13
```

## Installation Steps

1. **Install Stack** (Haskell build tool):
   ```bash
   curl -sSL https://get.haskellstack.org/ | sh
   ```

2. **Create project directory**:
   ```bash
   mkdir kenken-game
   cd kenken-game
   ```

3. **Initialize Stack project**:
   ```bash
   stack new kenken-game simple
   ```

4. **Replace the generated files with the provided code**

5. **Build the project**:
   ```bash
   stack build
   ```

6. **Run the game**:
   ```bash
   stack exec kenken-game-exe
   ```

## Development Tools (Optional)

- **HLS (Haskell Language Server)**: For IDE support
- **hlint**: For code suggestions
- **hoogle**: For documentation search

Install development tools:
```bash
stack install hlint hoogle
```