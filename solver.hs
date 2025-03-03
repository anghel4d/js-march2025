{-# LANGUAGE ScopedTypeVariables #-}
module LaserPuzzleSBV_Improved where

import Data.SBV
import Data.SBV.Control
import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------
--  PUZZLE SPECIFICATION
--------------------------------------------------------------------------------

-- | PuzzleSpec: size + 4 edges of product clues (Maybe Int).
data PuzzleSpec = PuzzleSpec
  { puzzleSize  :: Int
  , topClues    :: [Maybe Integer]
  , bottomClues :: [Maybe Integer]
  , leftClues   :: [Maybe Integer]
  , rightClues  :: [Maybe Integer]
  }

-- Example puzzle: 5x5 with known clues.
examplePuzzle :: PuzzleSpec
examplePuzzle = PuzzleSpec
  { puzzleSize = 5
  , topClues    = [Just 2,  Just 36, Just 9,  Just 16, Just 1 ]
  , leftClues   = [Just 10, Just 2,  Just 9,  Just 16, Just 1 ]
  , rightClues  = [Just 1,  Just 75, Just 3,  Just 4,  Just 1 ]
  , bottomClues = [Just 1,  Just 10, Just 36, Just 4,  Just 3 ]
  }

--------------------------------------------------------------------------------
--  ENCODING THE GRID AS A SINGLE SBV ARRAY
--  0 = no mirror, 1 = '/', 2 = '\'
--------------------------------------------------------------------------------

-- | Convert (row, col) to a single index in [0 .. n*n - 1].
idxOf :: SWord8 -> SWord8 -> SWord8 -> SWord8
idxOf n r c = r * n + c

-- | Get the cell value from the SBV array, given row & col in-bounds.
getCell :: SArray Word8 Word8 -> SWord8 -> SWord8 -> SWord8 -> SWord8
getCell arr n r c = readArray arr (idxOf n r c)

--------------------------------------------------------------------------------
--  ADJACENCY CONSTRAINTS
--------------------------------------------------------------------------------

addAdjacencyConstraints :: SArray Word8 Word8 -> Int -> Symbolic ()
addAdjacencyConstraints arr n = do
  let nLit = literal (fromIntegral n :: Word8)
  forM_ [0..n-1] $ \r ->
    forM_ [0..n-1] $ \c -> do
      let rS = literal (fromIntegral r :: Word8)
          cS = literal (fromIntegral c :: Word8)
          cellVal = getCell arr nLit rS cS
          neighsOrs = orthogonalNeighsOrs n r c
      forM_ neighsOrs $ \(nr, nc) -> do
        let nrS = literal (fromIntegral nr :: Word8)
            ncS = literal (fromIntegral nc :: Word8)
            neighVal = getCell arr nLit nrS ncS
        -- If this cell is a mirror, neighsOr must be 0:
        constrain $ (cellVal ./= 0) .=> (neighVal .== 0)

orthogonalNeighsOrs :: Int -> Int -> Int -> [(Int,Int)]
orthogonalNeighsOrs n r c =
  [ (r-1,c) | r > 0     ] ++
  [ (r+1,c) | r < n-1   ] ++
  [ (r,c-1) | c > 0     ] ++
  [ (r,c+1) | c < n-1   ]

--------------------------------------------------------------------------------
--  LASER DIRECTIONS & REFLECTIONS
--
--   0=Up, 1=Right, 2=Down, 3=Left
--------------------------------------------------------------------------------

reflectDir :: SWord8 -> SWord8 -> SWord8
reflectDir d mirrorVal =
  ite (mirrorVal .== 1) (slashReflect d)
  $ ite (mirrorVal .== 2) (backslashReflect d)
  $ d

slashReflect :: SWord8 -> SWord8
slashReflect d =
  ite (d .== 0) 1    -- up -> right
  $ ite (d .== 1) 0  -- right -> up
  $ ite (d .== 2) 3  -- down -> left
  $ ite (d .== 3) 2  -- left -> down
  $ d

backslashReflect :: SWord8 -> SWord8
backslashReflect d =
  ite (d .== 0) 3    -- up -> left
  $ ite (d .== 3) 0  -- left -> up
  $ ite (d .== 2) 1  -- down -> right
  $ ite (d .== 1) 2  -- right -> down
  $ d

stepForward :: SWord8 -> SWord8 -> SWord8 -> (SWord8, SWord8)
stepForward r c d =
  ( ite (d .== 0) (r-1)
    $ ite (d .== 2) (r+1)
    $ r
  , ite (d .== 1) (c+1)
    $ ite (d .== 3) (c-1)
    $ c
  )

outOfBounds :: SWord8 -> SWord8 -> SWord8 -> SBool
outOfBounds r c n = (r .>= n) .|| (c .>= n)

--------------------------------------------------------------------------------
--  PRODUCT-FACTOR PRUNING
--
--  If a beam clue is Just c, we can restrict each nonzero segment length to
--  be a divisor of c, also not exceeding n (or some plausible max).
--------------------------------------------------------------------------------

factorBasedPruning :: Maybe Integer -> Int -> [SWord8] -> Symbolic SInteger
factorBasedPruning Nothing _ segLens8 = do
  -- If no clue, just multiply them up as is, ignoring zeros
  let segLens32 = map sFromIntegral segLens8 :: [SInteger]
      totalProd = foldl (\acc x -> acc * ite (x .== 0) 1 x) 1 segLens32
  return totalProd

factorBasedPruning (Just c) n segLens8 = do
  let divs = possibleDivisors c n
  -- Constrain each segment length to be 0 or in the set of divisors
  forM_ segLens8 $ \s -> do
    constrain $ (s ./= 0) .=> sOr [s .== literal (fromIntegral d :: Word8) | d <- divs]
  -- Now compute the product as an SInteger
  let segLensI = map sFromIntegral segLens8 :: [SInteger]
      totalProd = foldl (\acc x -> acc * ite (x .== 0) 1 x) 1 segLensI
  -- Constrain the final product to c
  constrain $ totalProd .== literal c
  return totalProd

-- | Return all positive divisors of c up to maxLen= n or so.
--   If you want a bigger cap (like 2n), adjust accordingly.
possibleDivisors :: Integer -> Int -> [Integer]
possibleDivisors c maxLen =
  [ d | d <- [1..fromIntegral maxLen]
      , c `mod` d == 0
  ]

--------------------------------------------------------------------------------
--  BEAM UNROLLING, PROPER SEGMENT LENGTH
--
--  We'll store an array "dist[i]" = distance traveled in the *current* segment
--  at step i. If we "hit a mirror" or "go out of bounds" at step i, that
--  finishes this segment, and we record that length in "seg[i]". Then we reset
--  dist[i+1] to 1 for the next step (because entering a new cell is distance 1).
--
--  We'll keep "seg[i]" in an array of the same length. The final product
--  is the product of all seg[i] (ignoring zeros).
--------------------------------------------------------------------------------

maxStepsFor :: Int -> Int
maxStepsFor n = 4 * n

addAllLaserConstraints :: SArray Word8 Word8 -> PuzzleSpec -> Symbolic ()
addAllLaserConstraints arr spec = do
  let n = puzzleSize spec
      nW8 = literal (fromIntegral n :: Word8)

  -- top edge => row=0, col=i => direction=2 (Down)
  forM_ [0..n-1] $ \i -> do
    let clue = (topClues spec) !! i
        r0   = 0
        c0   = i
        dir  = 2  -- down
    addOneLaserConstraint arr n r0 c0 dir clue

  -- bottom edge => row=n-1, col=i => direction=0 (Up)
  forM_ [0..n-1] $ \i -> do
    let clue = (bottomClues spec) !! i
        r0   = n-1
        c0   = i
        dir  = 0  -- up
    addOneLaserConstraint arr n r0 c0 dir clue

  -- left edge => row=i, col=0 => direction=1 (Right)
  forM_ [0..n-1] $ \i -> do
    let clue = (leftClues spec) !! i
        r0   = i
        c0   = 0
        dir  = 1  -- right
    addOneLaserConstraint arr n r0 c0 dir clue

  -- right edge => row=i, col=n-1 => direction=3 (Left)
  forM_ [0..n-1] $ \i -> do
    let clue = (rightClues spec) !! i
        r0   = i
        c0   = n-1
        dir  = 3  -- left
    addOneLaserConstraint arr n r0 c0 dir clue

-- | Add constraints for a single laser starting at (r0,c0), direction=d0.
--   We'll keep two arrays of length = maxSteps:
--     dist[i] = distance traveled in the current segment at step i
--     seg[i]  = if we ended a segment at step i, seg[i] is that segment's length
--               otherwise 0
--   The final product of all seg[i] (ignoring 0s) must match the clue if present,
--   with factor-based pruning for small clues.
addOneLaserConstraint :: SArray Word8 Word8
                      -> Int             -- puzzle dimension
                      -> Int -> Int -> Int
                      -> Maybe Integer
                      -> Symbolic ()
addOneLaserConstraint arr n r0 c0 d0 maybeClue = do
  let steps = maxStepsFor n
      nW8 = literal (fromIntegral n :: Word8)

  -- Create symbolic arrays for row, col, dir, ended
  rowVars   <- mapM (\i -> sWord8 ("r_"   ++ pfx i)) [0..steps-1]
  colVars   <- mapM (\i -> sWord8 ("c_"   ++ pfx i)) [0..steps-1]
  dirVars   <- mapM (\i -> sWord8 ("dir_" ++ pfx i)) [0..steps-1]
  endedVars <- mapM (\i -> sBool  ("end_" ++ pfx i)) [0..steps-1]

  -- dist[i] = distance traveled in the *current* segment at step i
  distVars  <- mapM (\i -> sWord8 ("dist_"++ pfx i)) [0..steps-1]
  -- seg[i] = length of the segment that ended *at* step i, or 0 if no segment ended
  segVars   <- mapM (\i -> sWord8 ("seg_" ++ pfx i)) [0..steps-1]

  -- Step 0:
  let r0S = literal (fromIntegral r0 :: Word8)
      c0S = literal (fromIntegral c0 :: Word8)
      d0S = literal (fromIntegral d0 :: Word8)
  constrain $ head rowVars   .== r0S
  constrain $ head colVars   .== c0S
  constrain $ head dirVars   .== d0S
  constrain $ head endedVars .== sFalse

  -- Because the beam enters the grid from outside, let's say the initial
  -- distance for the *current* segment is 1. (We count stepping in as 1.)
  constrain $ head distVars  .== 1

  -- No segment has ended at step 0 (yet), so segVars[0] = 0
  constrain $ head segVars   .== 0

  forM_ [0..steps-2] $ \i -> do
    let r_i   = rowVars   !! i
        c_i   = colVars   !! i
        d_i   = dirVars   !! i
        e_i   = endedVars !! i
        dist_i= distVars  !! i
        seg_i = segVars   !! i

        r_ip1 = rowVars   !! (i+1)
        c_ip1 = colVars   !! (i+1)
        d_ip1 = dirVars   !! (i+1)
        e_ip1 = endedVars !! (i+1)
        dist_ip1 = distVars !! (i+1)
        seg_ip1  = segVars  !! (i+1)

    -- If ended[i], we remain ended
    constrain $ e_i .=> (
         r_ip1 .== r_i
      .&& c_ip1 .== c_i
      .&& d_ip1 .== d_i
      .&& e_ip1 .== sTrue
      .&& dist_ip1 .== dist_i
      .&& seg_ip1 .== 0
      )

    -- If not ended[i], then check out-of-bounds or reflection
    let outOf = outOfBounds r_i c_i nW8
        cellVal = getCell arr nW8 r_i c_i
        newDir  = reflectDir d_i cellVal
        (nr,nc) = stepForward r_i c_i newDir

        isMirror = cellVal ./= 0
        endNow   = outOf .|| isMirror  -- if we are outOf or see a mirror, this segment ends here.

    -- If we end at step i, seg[i+1] = dist_i, dist[i+1] resets to 1, ended stays false
    -- unless outOf bounds => that beam is done
    constrain $ sNot e_i .=> (
      ite endNow
        -- We ended a segment right now
        ( seg_ip1    .== dist_i
       .&& dist_ip1  .== 1
       .&& r_ip1     .== nr
       .&& c_ip1     .== nc
       .&& d_ip1     .== newDir
       .&& e_ip1     .== outOf  -- If outOf => ended = True, else false
        )
        -- Otherwise, we keep traveling inside the grid
        ( seg_ip1    .== 0
       .&& dist_ip1  .== dist_i + 1
       .&& r_ip1     .== nr
       .&& c_ip1     .== nc
       .&& d_ip1     .== newDir
       .&& e_ip1     .== sFalse
        )
      )

  -- The last step doesn't transition, so segVars[last] can be zero or record
  -- a segment if it ends at the final step. We'll do a minimal constraint:
  let lastIdx = steps - 1
  let e_last  = endedVars !! lastIdx
      seg_last= segVars   !! lastIdx
      dist_last = distVars !! lastIdx
  -- If the beam isn't ended at the last step, we might still end a segment:
  constrain $
    sNot e_last .=> (
      seg_last .== dist_last  -- we end a final segment
    )

  -- Now gather all segVars as the lengths of ended segments.
  -- We'll do factor-based pruning if there's a clue.
  totalProd :: SInteger <- factorBasedPruning maybeClue n (segVars)

  return ()

  where
    pfx i = show r0 ++ "_" ++ show c0 ++ "_" ++ show d0 ++ "_" ++ show i

--------------------------------------------------------------------------------
--  SOLVER WRAPPER
--------------------------------------------------------------------------------

solvePuzzleSBV :: PuzzleSpec -> IO ()
solvePuzzleSBV spec = runSMT $ do
  let n  = puzzleSize spec
      nW = fromIntegral n :: Word8

  -- Create SBV array for the grid
  gridArr :: SArray Word8 Word8 <- newArray Nothing (Just 0)

  -- Constrain each cell to be <= 2
  forM_ [0..(n*n - 1)] $ \idx -> do
    let idxS = literal (fromIntegral idx :: Word8)
    val <- readArray gridArr idxS
    constrain $ val .<= 2

  -- Add adjacency constraints
  addAdjacencyConstraints gridArr n

  -- Add perimeter laser constraints
  addAllLaserConstraints gridArr spec

  -- Check SAT
  query $ do
    cs <- checkSat
    case cs of
      Unsat -> io $ putStrLn "No solution!"
      Unk   -> io $ putStrLn "Solver said unknown!"
      Sat   -> do
        io $ putStrLn "Solution found!"
        -- Retrieve each cell value
        sol <- mapM (\idx -> getValue (readArray gridArr (literal (fromIntegral idx :: Word8))))
                    [0..(n*n -1)]
        io $ printSolution n sol

-- | Print the grid in ASCII
printSolution :: Int -> [Word8] -> IO ()
printSolution n vals = do
  putStrLn $ "Grid " ++ show n ++ "x" ++ show n ++ ":"
  let rowStrings =
        [ [ mirrorChar (vals !! (r*n + c))
          | c <- [0..n-1]
          ]
        | r <- [0..n-1]
        ]
  mapM_ putStrLn rowStrings
  where
    mirrorChar 0 = '.'
    mirrorChar 1 = '/'
    mirrorChar 2 = '\\'
    mirrorChar _ = '?'

--------------------------------------------------------------------------------
--  MAIN
--------------------------------------------------------------------------------

main :: IO ()
main = solvePuzzleSBV examplePuzzle