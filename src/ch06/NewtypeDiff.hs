data TwoFields =
  TwoFields Int
            Int

newtype Okay =
  ExactlyOne Int

newtype Param a b =
  Param (Either a b)

newtype Record = Record
  { getInt :: Int
  }
