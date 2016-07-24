module Labtech.Help ( help ) where

help :: [String]
help =
     [ "Available commands:"
     , "!help -> Print help"
     , "!upload [url] [filename] -> Downloads the provided url to the data directory with the provided filename."
     , "!list -> List all available files downloaded in the data directory."
     ]
