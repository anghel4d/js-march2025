import React, { useState, useEffect } from 'react';
import ReactDOM from 'react-dom/client';

// Types
type MirrorType = 0 | 1 | 2; // 0 = empty, 1 = /, 2 = \
type Direction = 0 | 1 | 2 | 3; // 0 = up, 1 = right, 2 = down, 3 = left
type Position = { row: number; col: number };
type Laser = { position: Position; direction: Direction; clue: number | null };
type Segment = { path: Position[]; length: number };
type LaserResult = { segments: Segment[]; product: number; exitPosition: Position | null };

const LaserMirrorPuzzle = () => {
  const gridSize = 5;
  const [grid, setGrid] = useState<MirrorType[][]>(
    Array(gridSize).fill(0).map(() => Array(gridSize).fill(0))
  );
  const [lasers, setLasers] = useState<Laser[]>([]);
  const [laserResults, setLaserResults] = useState<LaserResult[]>([]);
  const [showPaths, setShowPaths] = useState(true);
  const [finalScore, setFinalScore] = useState<number | null>(null);

  // Edge types
  type Edge = 'top' | 'right' | 'bottom' | 'left';

  // Initialize the puzzle once on mount
  useEffect(() => {
    initializePuzzle();
  }, []);

  // Recompute results + final score whenever grid or lasers change
  useEffect(() => {
    const results = lasers.map(laser => traceLaser(laser, grid));
    setLaserResults(results);
    calculateFinalScore(results);
  }, [grid, lasers]);

  // Convert an edge + position (1..5) to a grid coordinate just before it enters
  const getPositionFromEdge = (edge: Edge, position: number): Position => {
    const index = position - 1; // 0..4
    switch (edge) {
      case 'top':
        return { row: -1, col: index };
      case 'right':
        return { row: index, col: gridSize };
      case 'bottom':
        return { row: gridSize, col: gridSize - 1 - index };
      case 'left':
        return { row: gridSize - 1 - index, col: -1 };
    }
  };

  // Determine initial laser direction based on edge
  const getDirectionFromEdge = (edge: Edge): Direction => {
    switch (edge) {
      case 'top':
        return 2; // enters downward
      case 'right':
        return 3; // enters leftward
      case 'bottom':
        return 0; // enters upward
      case 'left':
        return 1; // enters rightward
    }
  };

  const initializePuzzle = () => {
    // Example edges/clues
    const edges: {
      edge: Edge;
      positions: { position: number; clue: number | null }[];
    }[] = [
      {
        edge: 'top',
        positions: [
          { position: 1, clue: null },
          { position: 2, clue: null },
          { position: 3, clue: 9 },
          { position: 4, clue: null },
          { position: 5, clue: null }
        ]
      },
      {
        edge: 'right',
        positions: [
          { position: 1, clue: null },
          { position: 2, clue: 75 },
          { position: 3, clue: null },
          { position: 4, clue: null },
          { position: 5, clue: null }
        ]
      },
      {
        edge: 'bottom',
        positions: [
          { position: 1, clue: null },
          { position: 2, clue: null },
          { position: 3, clue: 36 },
          { position: 4, clue: null },
          { position: 5, clue: null }
        ]
      },
      {
        edge: 'left',
        positions: [
          { position: 1, clue: null },
          { position: 2, clue: null },
          { position: 3, clue: null },
          { position: 4, clue: 16 },
          { position: 5, clue: null }
        ]
      }
    ];

    const initialLasers: Laser[] = [];
    edges.forEach(edgeInfo => {
      const { edge, positions } = edgeInfo;
      positions.forEach(({ position, clue }) => {
        initialLasers.push({
          position: getPositionFromEdge(edge, position),
          direction: getDirectionFromEdge(edge),
          clue
        });
      });
    });
    setLasers(initialLasers);
  };

  // Toggle mirror type on click
  const handleCellClick = (row: number, col: number) => {
    const newGrid = grid.map(r => [...r]);
    const oldMirror = newGrid[row][col];
    const nextMirror = ((oldMirror + 1) % 3) as MirrorType;
    newGrid[row][col] = nextMirror;

    // Check adjacency; revert if invalid
    if (nextMirror !== 0 && hasAdjacentMirror(row, col, newGrid)) {
      newGrid[row][col] = oldMirror;
      alert("Mirrors can't be placed in adjacent cells!");
      return;
    }

    setGrid(newGrid);
  };

  const hasAdjacentMirror = (row: number, col: number, testGrid: MirrorType[][]) => {
    const adjacentDeltas = [
      [-1, 0],
      [1, 0],
      [0, -1],
      [0, 1]
    ];
    return adjacentDeltas.some(([dr, dc]) => {
      const r2 = row + dr, c2 = col + dc;
      return (
        r2 >= 0 &&
        r2 < gridSize &&
        c2 >= 0 &&
        c2 < gridSize &&
        testGrid[r2][c2] !== 0
      );
    });
  };

  // Trace a single laser through the grid
  const traceLaser = (laser: Laser, currentGrid: MirrorType[][]): LaserResult => {
    let currentPos: Position = { ...laser.position };
    let currentDir: Direction = laser.direction;
    const segments: Segment[] = [];
    let currentPath: Position[] = [];
    let exitPosition: Position | null = null;

    // Move into the grid from the starting position
    step();
    currentPath = [{ ...currentPos }];

    function step() {
      switch (currentDir) {
        case 0:
          currentPos.row--;
          break; // up
        case 1:
          currentPos.col++;
          break; // right
        case 2:
          currentPos.row++;
          break; // down
        case 3:
          currentPos.col--;
          break; // left
      }
    }

    // Follow the path until exiting the grid
    while (
      currentPos.row >= 0 &&
      currentPos.row < gridSize &&
      currentPos.col >= 0 &&
      currentPos.col < gridSize
    ) {
      const cellMirror = currentGrid[currentPos.row][currentPos.col];
      if (cellMirror !== 0) {
        // Finish the current segment (include mirror cell)
        segments.push({ path: [...currentPath], length: currentPath.length });

        // Reflect the beam based on mirror type
        if (cellMirror === 1) {
          // '/' mirror: up↔right, down↔left
          currentDir = [1, 0, 3, 2][currentDir];
        } else {
          // '\' mirror: up↔left, down↔right
          currentDir = [3, 2, 1, 0][currentDir];
        }

        // Start a new segment from this mirror cell
        currentPath = [{ ...currentPos }];
      }

      step();

      if (
        currentPos.row >= 0 &&
        currentPos.row < gridSize &&
        currentPos.col >= 0 &&
        currentPos.col < gridSize
      ) {
        currentPath.push({ ...currentPos });
      } else {
        // The laser has exited the grid
        exitPosition = { ...currentPos };
        if (currentPath.length > 0) {
          segments.push({ path: [...currentPath], length: currentPath.length });
        }
      }
    }

    const product = segments.reduce((acc, seg) => acc * seg.length, 1);
    return { segments, product, exitPosition };
  };

  // Check if a known clue matches its product
  const isClueMatched = (index: number): boolean => {
    const laser = lasers[index];
    const result = laserResults[index];
    if (!laser || !result || laser.clue === null) return true;
    return result.product === laser.clue;
  };

  // Determine puzzle's final score if solved
  const calculateFinalScore = (results: LaserResult[]) => {
    // Only calculate if all known clues are matched
    const allKnownOk = lasers.every((laser, i) =>
      laser.clue === null || results[i].product === laser.clue
    );
    if (!allKnownOk) {
      setFinalScore(null);
      return;
    }

    // For unknown clues, sum based on exit side
    let topSum = 0,
      rightSum = 0,
      bottomSum = 0,
      leftSum = 0;
    lasers.forEach((laser, i) => {
      if (laser.clue === null) {
        const { exitPosition, product } = results[i];
        if (!exitPosition) return; // Laser didn't exit?
        const { row, col } = exitPosition;
        if (row < 0) {
          topSum += product;
        } else if (row >= gridSize) {
          bottomSum += product;
        } else if (col < 0) {
          leftSum += product;
        } else if (col >= gridSize) {
          rightSum += product;
        }
      }
    });

    setFinalScore(topSum * rightSum * bottomSum * leftSum);
  };

  // Render the puzzle grid
  const renderCell = (row: number, col: number) => {
    const mirror = grid[row][col];
    const content = mirror === 1 ? '/' : mirror === 2 ? '\\' : null;
    return (
      <div
        key={`cell-${row}-${col}`}
        className="cell"
        onClick={() => handleCellClick(row, col)}
      >
        {content && <div className="mirror">{content}</div>}
        {showPaths && renderLaserPaths(row, col)}
      </div>
    );
  };

  // Highlight laser paths that pass through a cell
  const renderLaserPaths = (row: number, col: number) => {
    const highlights = [];
    laserResults.forEach((res, i) => {
      res.segments.forEach(seg => {
        if (seg.path.some(p => p.row === row && p.col === col)) {
          highlights.push(
            <div key={`path-${i}-${row}-${col}`} className="laser-path" />
          );
        }
      });
    });
    return highlights;
  };

  // Get edge and position number from laser index
  const getEdgeAndPosition = (index: number): { edge: string; position: number } => {
    const edgeSize = 5;
    const edgeIndex = Math.floor(index / edgeSize);
    const positionInEdge = (index % edgeSize) + 1; // Convert to 1-indexed

    let edge = '';
    switch (edgeIndex) {
      case 0:
        edge = 'top';
        break;
      case 1:
        edge = 'right';
        break;
      case 2:
        edge = 'bottom';
        break;
      case 3:
        edge = 'left';
        break;
    }

    return { edge, position: positionInEdge };
  };

  // Render a clue for a given laser
  const renderClue = (laser: Laser, index: number) => {
    const clueValue =
      laser.clue !== null ? laser.clue : laserResults[index]?.product ?? '?';
    const matched = isClueMatched(index) ? 'matched' : '';
    return (
      <div className={`clue ${matched}`} title={`${getEdgeAndPosition(index).edge} edge, position ${getEdgeAndPosition(index).position}`}>
        {clueValue}
      </div>
    );
  };

  return (
    <div className="laser-mirror-puzzle">
      <h1>Laser-Mirror Puzzle (5×5)</h1>

      <div className="controls">
        <button onClick={() => setShowPaths(!showPaths)}>
          {showPaths ? 'Hide Paths' : 'Show Paths'}
        </button>
        <button
          onClick={() =>
            setGrid(Array(gridSize).fill(0).map(() => Array(gridSize).fill(0)))
          }
        >
          Reset Grid
        </button>
      </div>

      <div className="puzzle-container">
        {/* Top clues */}
        <div className="top-clues">
          {lasers.slice(0, 5).map((laser, i) => (
            <div key={`top-${i}`} className="clue-container">
              {renderClue(laser, i)}
            </div>
          ))}
        </div>

        <div className="grid-with-sides">
          {/* Left clues */}
          <div className="left-clues">
            {lasers.slice(15, 20).map((laser, idx) => (
              <div key={`left-${idx}`} className="clue-container">
                {renderClue(laser, idx + 15)}
              </div>
            ))}
          </div>

          {/* Main grid */}
          <div className="grid">
            {grid.map((rowArr, row) => (
              <div key={`row-${row}`} className="row">
                {rowArr.map((_, col) => renderCell(row, col))}
              </div>
            ))}
          </div>

          {/* Right clues */}
          <div className="right-clues">
            {lasers.slice(5, 10).map((laser, idx) => (
              <div key={`right-${idx}`} className="clue-container">
                {renderClue(laser, idx + 5)}
              </div>
            ))}
          </div>
        </div>

        {/* Bottom clues */}
        <div className="bottom-clues">
          {lasers.slice(10, 15).map((laser, idx) => (
            <div key={`bottom-${idx}`} className="clue-container">
              {renderClue(laser, idx + 10)}
            </div>
          ))}
        </div>
      </div>

      {finalScore !== null && (
        <div className="final-score">
          <h2>Solved!</h2>
          <p>Final Score: {finalScore}</p>
        </div>
      )}

      <div className="instructions">
        <h3>How to Play:</h3>
        <ul>
          <li>Click cells to cycle through mirror types: empty → / → \ → empty</li>
          <li>Mirrors can't be placed in adjacent cells</li>
          <li>Numbers around the grid are products of laser path segments</li>
        </ul>
      </div>

      {/* Basic styles */}
      <style jsx>{`
        .laser-mirror-puzzle {
          font-family: Arial, sans-serif;
          max-width: 600px;
          margin: 0 auto;
          padding: 20px;
        }
        h1 {
          text-align: center;
          color: #333;
        }
        .controls {
          display: flex;
          justify-content: center;
          margin-bottom: 20px;
          gap: 10px;
        }
        button {
          padding: 8px 16px;
          background-color: #4caf50;
          color: white;
          border: none;
          border-radius: 4px;
          cursor: pointer;
        }
        button:hover {
          background-color: #45a049;
        }
        .puzzle-container {
          display: flex;
          flex-direction: column;
          align-items: center;
        }
        .grid-with-sides {
          display: flex;
          align-items: stretch;
        }
        .grid {
          display: flex;
          flex-direction: column;
          border: 2px solid #333;
        }
        .row {
          display: flex;
        }
        .cell {
          width: 60px;
          height: 60px;
          border: 1px solid #ccc;
          display: flex;
          justify-content: center;
          align-items: center;
          position: relative;
          cursor: pointer;
        }
        .cell:hover {
          background-color: #f5f5f5;
        }
        .mirror {
          font-size: 36px;
          color: #ff8c00;
          font-weight: bold;
          transform: scale(2);
          z-index: 2;
        }
        .laser-path {
          position: absolute;
          top: 50%;
          left: 50%;
          width: 40px;
          height: 40px;
          transform: translate(-50%, -50%);
          background-color: rgba(0, 128, 255, 0.2);
          border-radius: 50%;
          z-index: 1;
        }
        .clue-container {
          width: 60px;
          height: 60px;
          display: flex;
          justify-content: center;
          align-items: center;
        }
        .clue {
          width: 30px;
          height: 30px;
          display: flex;
          justify-content: center;
          align-items: center;
          border-radius: 50%;
          background-color: #f0f0f0;
          font-weight: bold;
        }
        .clue.matched {
          background-color: #4caf50;
          color: white;
        }
        .top-clues,
        .bottom-clues {
          display: flex;
          margin: 10px 0;
        }
        .left-clues,
        .right-clues {
          display: flex;
          flex-direction: column;
        }
        .final-score {
          margin: 20px 0;
          padding: 10px;
          background-color: #dff0d8;
          border: 1px solid #d6e9c6;
          border-radius: 4px;
          text-align: center;
        }
        .instructions {
          margin-top: 20px;
          padding: 10px;
          background-color: #f8f9fa;
          border-radius: 4px;
        }
      `}</style>
    </div>
  );
};

export default LaserMirrorPuzzle;

ReactDOM.createRoot(document.getElementById('root')!).render(
  <LaserMirrorPuzzle />
);
