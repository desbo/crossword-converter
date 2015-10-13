import Crossword (parseCrossword)
import qualified Data.ByteString.Lazy as B (readFile)

main = do
  json <- B.readFile "14174.json"
  print $ parseCrossword json
