

module FinancialTimeseries.Util.Directory where


import Data.Time (getCurrentTime, formatTime, defaultTimeLocale, iso8601DateFormat)

import qualified System.Directory as Dir



outputDirectory :: FilePath -> IO FilePath
outputDirectory fp = do
  now <- getCurrentTime
  let dir = fp ++ "/" ++ formatTime defaultTimeLocale (iso8601DateFormat (Just "%X%Z")) now        
  Dir.createDirectoryIfMissing True dir
  return dir
