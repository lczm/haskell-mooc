module Either where

readInt :: String -> Either String Int
readInt "0" = Right 0
readInt "1" = Right 1
readInt s = Left ("Unsupported string: " ++ s)

iWantAString :: Either Int String -> String
iWantAString (Right str)   = str
iWantAString (Left number) = show number

lectureParticipants :: [Either String Int]
lectureParticipants = [Right 10,
                       Right 13,
                       Left "easter vacation",
                       Right 17,
                       Left "lecturer was sick",
                       Right 3]

