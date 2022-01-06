module Network.HTTP.Utils

import Data.String

export
tail : String -> String
tail "" = ""
tail str = assert_total (strTail str)

export
splitBy : Char -> String -> (String, String)
splitBy sep = mapSnd tail . break (== sep)
