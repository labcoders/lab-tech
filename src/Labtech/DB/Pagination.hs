module Labtech.DB.Pagination where

type PageSize = Int

data PageSpec
  = PageSpec
    { pageInfoOffset :: !Int
    , pageInfoSize :: !PageSize
    }

-- | Computes which page number the given spec begins at.
--
-- For all @n@ and @m@, @pageInfoNum (nthPath n m) = n@.
pageInfoNum :: PageSpec -> Int
pageInfoNum p = pageInfoOffset p `div` pageInfoSize p

-- | Computes how many items are skipped from the beginning of the starting
-- page of the given specification.
pageInfoInternalOffset :: PageSpec -> Int
pageInfoInternalOffset p = pageInfoOffset p `mod` pageInfoSize p

firstPage :: PageSize -> PageSpec
firstPage n = PageSpec { pageInfoOffset = 0, pageInfoSize = n }

nextPage :: PageSpec -> PageSpec
nextPage p = p { pageInfoOffset = pageInfoOffset p + pageInfoSize p }

nthPage :: Int -> PageSize -> PageSpec
nthPage n s = PageSpec { pageInfoOffset = n * s, pageInfoSize = s }

-- | Renders the 'PageSpec' into a tuple that can be used for database queries.
pageInfoParams :: PageSpec -> (Int, Int)
pageInfoParams p = (pageInfoOffset p, pageInfoSize p)

-- | A page of data.
data Page a
  = Page
    { pageSpec :: !PageSpec
    -- ^ The specification of this page, which details the page size and
    -- offset.
    , pageCount :: !Int
    -- ^ The total count of pages according to the associated specification.
    , pageData :: [a]
    -- ^ The data in the page.
    -- @length (pageData p) = pageInfoSize (pageSpec p)@ for all pages @p@ that
    -- are not the last page in the book.
    }
