import Data.List

type Displayable = String

-- Dummy types with unit arguments for simplicity
newtype Image = Image ()
newtype Video = Video ()
newtype ThreeDMedia = ThreeDMedia ()

-- Note that this is a homogeneous list of only ImageList
-- A heterogeneous list simulataneously solving the expression problem
-- is more complicated (is it possible?)
newtype ImageList = ImageList [Image]

class MediaDisplay a where
  displayMedia :: a -> Displayable
  
class MediaLike a where
  likeMedia :: a -> String
  
-- somewhere later, make the types instance of MediaDisplay independently
instance MediaDisplay Image where
  displayMedia _ = "Displaying image" 
  
instance MediaDisplay Video where
  displayMedia _ = "Displaying video"
  
instance MediaDisplay ImageList where
  displayMedia (ImageList xs) = (intercalate "\n" (map displayMedia xs)) ++ "\nDisplayed Image List"

-- somewhere later
instance MediaLike Image where
  likeMedia _ = "Liking image" 
  
instance MediaLike Video where
  likeMedia _ = "Liking video"
  
instance MediaLike ImageList where
  likeMedia (ImageList xs) = (intercalate "\n" (map likeMedia xs)) ++ "\nLiked Image List"


main :: IO ()
main = do
  putStrLn $ displayMedia (Image ())
  putStrLn $ displayMedia (Video ())
  putStrLn "---"
  putStrLn $ displayMedia (ImageList [Image(), Image (), Image ()])
  putStrLn "---"
  putStrLn $ likeMedia (ImageList [Image(), Image (), Image ()])