{- |
Module      :  Text.HTML.TagSoup.Position

Maintainer  :  tagsoup@henning-thielemann.de
Stability   :  provisional
Portability :  portable

Position in a file.

Cf. to Text.ParserCombinators.Parsec.Pos
-}

module Text.HTML.TagSoup.Position (
    Position(..), FileName, Row, Column,
    new, initialize,
    setRow, setColumn, setFileName,
    getRow, getColumn, getFileName,
    incRow, incColumn,
    updateOnChar, updateOnString,
    toReportText,
   ) where

import Data.List (foldl')

type FileName = String
type Row      = Int
type Column   = Int

{- |
Position in a file consisting of file name, row and column coordinates.
Upper left is (0,0), but show routines can display this with different offsets.
-}
data Position =
   Position {
      fileName :: FileName,
      row      :: !Row,
      column   :: !Column
   } deriving (Eq,Ord)


new :: FileName -> Row -> Column -> Position
new = Position

initialize :: FileName -> Position
initialize fn = new fn 0 0


-- * access functions

getFileName :: Position -> FileName
getFileName = fileName

getRow :: Position -> Row
getRow = row

getColumn :: Position -> Column
getColumn = column

incRow :: Row -> Position -> Position
incRow n p = p{row = getRow p + n}

incColumn :: Column -> Position -> Position
incColumn n p = p{column = getColumn p + n}

setFileName :: FileName -> Position -> Position
setFileName fn p = p{fileName = fn}

setRow :: Row -> Position -> Position
setRow n p = p{row = n}

setColumn :: Column -> Position -> Position
setColumn n p = p{column = n}


-- * update position according to read characters

updateOnString :: Position -> String -> Position
updateOnString pos string =
   foldl' updateOnChar pos string

updateOnChar   :: Position -> Char -> Position
updateOnChar pos@(Position _ r c) char =
   let (newRow, newColumn) =
          case char of
            '\n' -> (succ r, 0)
            '\t' -> (r, c + 8 - mod c 8)
            _    -> (r, succ c)
   in  setRow newRow $ setColumn newColumn pos


-- * update position according to read characters

{- |
Convert the file position to a format
that development environments can understand.
-}
toReportText :: Position -> String
toReportText (Position name r c) =
   concatMap (++":") [name, show (r+1), show (c+1)]

instance Show Position where
  showsPrec p (Position name r c) =
     showParen (p >= 10)
        (showString $ unwords $
            "Position.new" : show name : show r : show c : [])
