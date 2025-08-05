# Changelog for `sudoku-solver`

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to the
[Haskell Package Versioning Policy](https://pvp.haskell.org/).

## Unreleased

## 0.1.0.0 — YYYY-MM-DD

### Added

- data types:

    - `CellValue`
    - `Cell`
    - `Board`
    
- main functions:

    - solve
    - puzzle generators:
    
        - generateEasyBoard
        - generateMediumBoard
        - generateHardBoard

- application:

    - text-based user interface based on `brick`
