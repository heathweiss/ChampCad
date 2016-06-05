{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-} --for the example on ErrorT
{-# LANGUAGE FlexibleContexts #-}

{-ToDo:
Get rid of the anything to do with my custom state monad.

Try out a StateT example, then implement it for the Except work with CornerPoints.
-}
module Tests.BuilderMonadTest(builderMonadTest) where

import Builder.Monad( BuilderData(..), CornerPointsErrorForMonad(..),
                     combine, combine', combine'',  cornerPointsContainError, cornerPointsContainError',
                     eitherCornerPointsContainError')

import CornerPoints.CornerPoints((|@+++#@|), (|+++|), CornerPoints(..), (+++), (+++>))
import CornerPoints.Points(Point(..))
import CornerPoints.FaceConversions(upperFaceFromLowerFace, backFaceFromFrontFace )
import CornerPoints.FaceExtraction(extractFrontFace, extractTopFace, extractBottomFace)
import CornerPoints.Transpose(transposeX, transposeY, transposeZ)

import Stl.StlBase(Triangle(..), newStlShape)
import Stl.StlCornerPoints((|+++^|), Faces(..) )
import Stl.StlFileWriter(writeStlToFile)

import Control.Lens

import Test.HUnit hiding (State)

import Data.List(find)


--for the example on ErrorT
import qualified Data.Text as Txt
import qualified Data.Text.IO as T
import qualified Text.Read as TR
import Data.Map as Map hiding (map, filter)
import qualified Data.Map as M (map, filter)
import Control.Applicative

import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad
import Control.Monad.Trans
import Control.Monad.State.Lazy
import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer (WriterT, tell, execWriterT)
import Control.Monad.Reader

--all 3 for examples
import System.Random
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath hiding (combine)--((</>))

makeLenses ''BuilderData

--create a test cube
btmFace = BottomFace (Point 0 0 0) (Point 0 1 0) (Point 1 0 0) (Point 1 1 0 )
cube = btmFace +++ (upperFaceFromLowerFace $ transposeZ (+1) btmFace )

builderMonadTest = do

----------------------------------------------- CornerPointsError-------------------------------------------------

  {-See if [CornerPoints] has an error in it. It does.
    Should move this to CornerPoints test module.-}
  let cornerPointsListHasErrorTest = TestCase $ assertEqual
        "cornerPointsListHasErrorTest"
        (Left "illegal BottomFace +++ BottomFace operation")
        (let badAdd = [btmFace
                       +++
                       btmFace
                      ]
         in cornerPointsContainError badAdd
        )
  runTestTT cornerPointsListHasErrorTest

  {-See if [CornerPoints] has an error in it. It doesn't.
    Should move this to CornerPoints test module
  -}
  let cornerPointsListHasNoErrorTest = TestCase $ assertEqual
        "cornerPointsListHasNoErrorTest"
        (Right
          [CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0}, f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                       f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0}, f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
                       b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0}, b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                       b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0}, b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}
          ]
        )
        (let goodAdd = [btmFace
                        +++
                        (upperFaceFromLowerFace $ transposeZ (+1) btmFace )
                       ]
         in cornerPointsContainError goodAdd
        )
  runTestTT cornerPointsListHasNoErrorTest

  let combineWithIllegalOperationTest = TestCase $ assertEqual
       "combineWithIllegalOperationTest"
       (CornerPointsErrorData {_err = "illegal +++ operation",
                               _cPointsAllPriorToError = [CubePoints {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                                                          f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                                                          f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                                                          f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
                                                          b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                                                          b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                                                          b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0},
                                                          b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}]}
       )
       (let lclCubes  = [cube]
            lclData = CornerPointsData lclCubes lclCubes
            lclCubes' = lclCubes
                        |+++|
                        [cube]
            lclData' = CornerPointsData lclCubes' lclCubes'
        in  combine lclData lclData'
            
       )
  runTestTT combineWithIllegalOperationTest

  let combineWithLegalOperationTest = TestCase $ assertEqual
       "combineWithLegalOperationTest"
       (CornerPointsData {_cPointsCurr = [CubePoints {f1 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 0.0},
                                                      f2 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 1.0},
                                                      f3 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 1.0},
                                                      f4 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 0.0},
                                                      b1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                                                      b2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                                                      b3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                                                      b4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}}],
                          _cPointsAll = [CubePoints  {f1 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 0.0},
                                                      f2 = Point {x_axis = 0.0, y_axis = 2.0, z_axis = 1.0},
                                                      f3 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 1.0},
                                                      f4 = Point {x_axis = 1.0, y_axis = 2.0, z_axis = 0.0},
                                                      b1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                                                      b2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                                                      b3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                                                      b4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0}},
                                         CubePoints  {f1 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 0.0},
                                                      f2 = Point {x_axis = 0.0, y_axis = 1.0, z_axis = 1.0},
                                                      f3 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 1.0},
                                                      f4 = Point {x_axis = 1.0, y_axis = 1.0, z_axis = 0.0},
                                                      b1 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 0.0},
                                                      b2 = Point {x_axis = 0.0, y_axis = 0.0, z_axis = 1.0},
                                                      b3 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 1.0},
                                                      b4 = Point {x_axis = 1.0, y_axis = 0.0, z_axis = 0.0}}]})
       (let lclCubes  = [cube]
            lclData = CornerPointsData lclCubes lclCubes
            lclCubes' = lclCubes
                        |+++|
                        (map ((transposeY (+1)) . extractFrontFace) lclCubes)
            lclData' = CornerPointsData lclCubes' lclCubes'
        in  combine lclData lclData'
            
       )
  runTestTT combineWithLegalOperationTest


{--------------------------------- State example using a stack--------------------------------------
http://learnyouahaskell.com/for-a-few-monads-more#state
-}
type Stack = [Int]

pop :: State Stack Int 
pop =  state $ (\(x:xs) -> (x,xs))

--pushes without showing a
push :: Int -> State Stack ()
push a  = state $ \xs -> ((),a:xs)

--push on to the stack, and show a
pushAndShow :: Int -> State Stack Int
pushAndShow a = state $ \xs -> (a, a:xs)

--evalState pushAndShowTest []
pushAndShowTest :: State Stack Int
pushAndShowTest = do
  pushAndShow 3

stackManip :: State Stack Int
stackManip = do
  push 3
  a <- pop
  pop

stackStuff :: State Stack ()  
stackStuff = do  
        a <- pop  
        if a == 5  
            then push 5  
            else do  
                push 3  
                push 8  

moreStack :: State Stack ()
moreStack = do
  a <- stackManip
  if a == 100
     then stackStuff
     else return ()

stackyStack :: State Stack ()
stackyStack = do
  stackNow <- get
  if stackNow == [1,2,3]
     then put [3,2,1]
     else put stackNow

randomState :: (RandomGen g, Random a) => State g a
randomState = state random

threeCoins :: State StdGen (Bool,Bool,Bool)
threeCoins = do
  a <- randomState
  b <- randomState
  c <- randomState
  return (a,b,c)




{--------------------------------------- Control.Monad.Except vs Control.Monad.Trans.Except
throwError :: e -> m a 
throwE :: Monad m => e -> ExceptT e m a

catchError :: m a -> (e -> m a) -> m a 
catchE :: Monad m => ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a

run the example from Control.Monad.Except
-}
-- This is the type to represent length calculation error.
data LengthError = EmptyString  -- Entered string was empty.
          | StringTooLong Int   -- A string is longer than 5 characters.
                                -- Records a length of the string.
          | OtherError String   -- Other error, stores the problem description.

-- Converts LengthError to a readable message.
instance Show LengthError where
  show EmptyString = "The string was empty!"
  show (StringTooLong len) =
      "The length of the string (" ++ (show len) ++ ") is bigger than 5!"
  show (OtherError msg) = msg

-- For our monad type constructor, we use Either LengthError
-- which represents failure using Left LengthError
-- or a successful result of type a using Right a.
type LengthMonad = Either LengthError

runLengthMain :: IO ()
runLengthMain = do
  putStrLn "Please enter a string:"
  s <- getLine
  reportResult (calculateLength s)


-- Wraps length calculation to catch the errors.
-- Returns either length of the string or an error.
calculateLength :: String -> LengthMonad Int
calculateLength s = (calculateLengthOrFail s) `catchError` Left

-- Attempts to calculate length and throws an error if the provided string is
-- empty or longer than 5 characters.
-- The processing is done in Either monad.
calculateLengthOrFail :: String -> LengthMonad Int {- String ->  LengthMonad (Either LengthError Int)-}
calculateLengthOrFail [] = throwError EmptyString
calculateLengthOrFail s | len > 5 = throwError (StringTooLong len)
                        | otherwise = return len
  where len = length s

-- Prints result of the string length calculation.
reportResult :: LengthMonad Int -> IO ()
reportResult (Right len) = putStrLn ("The length of the string is " ++ (show len))
reportResult (Left e) = putStrLn ("Length calculation failed with error: " ++ (show e))




{------------------------------------ ErrorT example------------------------------------------
https://github.com/kqr/gists/blob/master/articles/gentle-introduction-monad-transformers.md

All sample code worked good with hand rolled ExceptT. Deleted it.
Now use Control.Monad.Trans.Except module. Works good.

-}
data LoginError = InvalidEmail
                | NoSuchUser
                | WrongPassword
  deriving (Eq, Show)

{------------------------------- main order of operations ---------------------------------
main: IO ()
  runExceptT :: ExceptT LoginError IO () -> IO (Either LoginError ())
  loginDialogue:: ExceptT LoginError IO ()    
  runExceptT loginDialogue :: IO (Either LoginError ())
-}

main' :: IO ()
main' = do
  runExceptT loginDialogue
  return ()

  
users :: Map Txt.Text Txt.Text
users = Map.fromList [("example.com", "qwerty123"), ("localhost", "password")]

{--------------------------- loginDialogue order of operations ------------------------
loginDialogue :: ExceptT LoginError IO ()
  userLogin :: ExceptT LoginError IO Txt.Text
  catchE :: ExceptT e m a -> (e -> ExceptT e' m a) -> ExceptT e' m a
  wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Txt.Text

  printError :: LoginError -> ExceptT LoginError IO a
-}
loginDialogue :: ExceptT LoginError IO ()
loginDialogue = do
  --if error, wrongPasswordHandler will call userLogin again internally for a second chance.
  --catchE will decide if it was an error
    --if not return a ExceptT IO Txt.Text
    --if so use wrongPasswordHandler to try logging in again.
      --if good return a ExceptT IO Txt.Text
      --if not return a ExceptT LoginError
  let retry =  userLogin `catchE` wrongPasswordHandler
  --token: will be a Right Txt.Text or a Left LoginError
  token     <- retry `catchE` printError
  --if token is Left LoginError, will this be called or will token be returned.
  lift $ T.putStrLn (Txt.append "Logged in with token: " token)

wrongPasswordHandler :: LoginError -> ExceptT LoginError IO Txt.Text
wrongPasswordHandler WrongPassword = do
  lift (T.putStrLn "Wrong password, one more chance.")
  userLogin
wrongPasswordHandler err = throwE err



userLogin :: ExceptT LoginError IO Txt.Text
userLogin = do
  token      <- getToken
  userpw     <- maybe
                  (throwE NoSuchUser)
                  return (Map.lookup token users)
  password   <- lift (T.putStrLn "Enter your password:" >> T.getLine)

  if userpw == password
     then return token
     else throwE WrongPassword

getToken :: ExceptT LoginError IO Txt.Text
getToken = do
  lift (T.putStrLn "Enter email address:")
  input <- lift T.getLine
  liftEither (getDomain input)

liftEither :: Monad m => Either e a -> ExceptT e m a
liftEither x = ExceptT (return x)

--test out lift Either
runLiftEither :: IO (Either String String)
runLiftEither = runExceptT $ do
  liftEither (Right "err")

getDomain :: Txt.Text -> Either LoginError Txt.Text
getDomain email =
  case Txt.splitOn "@" email of
    [name, domain] -> Right domain
    _              -> Left InvalidEmail

printError :: LoginError -> ExceptT LoginError IO a
printError err = do
  lift . T.putStrLn $ case err of
    WrongPassword -> "Wrong password. No more chances."
    NoSuchUser -> "No user with that email exists."
    InvalidEmail -> "Invalid email address entered."
  throwE err







{---------------------------------- MaybeT example----------------------------------------------------------
http://stackoverflow.com/questions/32579133/simplest-non-trivial-monad-transformer-example-for-dummies-iomaybe
-}

askfor2 :: String -> MaybeT IO String
askfor2 prompt = do
   liftIO $ putStr $ "What is your " ++ prompt ++ " (type END to quit)? "
   r <- liftIO getLine
   if r == "END"
     then MaybeT (return Nothing)    -- has type: MaybeT IO String
     else MaybeT (return (Just r))   -- has type: MaybeT IO String

howLongGettingLaid :: IO (Maybe Int)
howLongGettingLaid =
  runMaybeT $ do age <- fmap read (askfor2 "age")
                 firstLay <- fmap read (askfor2 "age when you firs got laid")
                 return $ age - firstLay

getLaidSurvey :: IO (Maybe String)
getLaidSurvey =
  runMaybeT $ do name <- askfor2 "name"
                 yearsOfSex <- fmap show (MaybeT howLongGettingLaid)
                 if (read yearsOfSex) <= 5
                    then return $ name ++ " you have had not had sex for enough years"
                    else return $ name ++ " you have had sex for " ++ yearsOfSex ++ " years"

                 
--throws an exception: Prelude.read: no parse
howLongGettingLaid2 :: MaybeT IO Int
howLongGettingLaid2 = do
  age <- fmap read (askfor2 "age")
  firstLay <- fmap read (askfor2 "age when you firs got laid")
  MaybeT $ return $  (Just (-)) <*> age <*> firstLay
  

getLaidSurvey2 :: IO (Maybe String)
getLaidSurvey2 =
  runMaybeT $ do name <- askfor2 "name"
                 yearsOfSex <- fmap show (howLongGettingLaid2)
                 if (read yearsOfSex) <= 5
                    then return $ name ++ " you have had not had sex for enough years"
                    else return $ name ++ " you have had sex for " ++ yearsOfSex ++ " years"
                    


survey2 :: IO (Maybe (String,String))
survey2 =
   runMaybeT $ do a <- askfor2 "name"
                  b <- askfor2 "favorite color"
                  return (a,b)


{-------------------------------------------------------monad transformer examples from real world haskell chap 18-----------------
-}
{--------------------------------------- count directories without transformer--------------------
First do it with monads, but no transformers
-}
runListDirectory :: IO [String]
runListDirectory = do
  listDirectory "/home/heath/temp"

{-
Gives a list of all directory and file names in the folder.
Does not list subdirectories
Does not give count of directories/files
-}
listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
       where notDots p = p /= "." && p /= ".."

runCountEntriesTrad :: IO [(FilePath, Int)]
runCountEntriesTrad = do
  countEntriesTrad "/home/heath/temp"

{-
gives the current path and the # of directories and files in that path.
Does not include: info about subdirectories, names of contained directories/files
-}
countEntriesTrad :: FilePath -> IO [(FilePath, Int)]
countEntriesTrad path = do
  contents <- listDirectory path
  rest     <- forM contents $ \name -> do
                let newName = path </> name
                isDir <- doesDirectoryExist newName
                if isDir
                   then countEntriesTrad newName
                   else return []
  return $ (path, length contents) : concat rest

{----------------------------------------- now use WriterT transformer to count directories --------------------------------
It uses listDirectory from 1st example
-}

{-execWriterT throw away result and gives what was recorded.-}
runCountEntries = do
  execWriterT $ countEntries "/home/heath/temp"

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \name -> do
    let newName = path </> name
    isDir <- liftIO . doesDirectoryExist $ newName
    when isDir $ countEntries newName


{--------------------------------WriterT example---------------------------------------
Still in Real World transformers chapter.
Shows 'local' of the Reader(T) monad.
-}
myName step = do
  name <- ask
  return (step ++ " my name is " ++ name)

localExample :: Reader String (String,String,String)
localExample = do
  a <- myName "First"
  b <- local (++ "dy") (myName "second")
  c <- myName "third"
  return (a,b,c)

runLocalExample = runReader localExample "Fred"


{-----------------------------Stacking Multiple Monad Transformers----------------------------------------
Still in Real World transformers chapter.

Continue with counting directories, but with a stack of transformers. ReaderT and StateT

run it in repl with:
runApp (constrainedCount 1 "/home/heath/temp") 3
-}


data AppConfig = AppConfig {cfgMaxDepth :: Int}
  deriving Show

data AppState = AppState {stDeepestReached :: Int}
  deriving Show

type App = ReaderT AppConfig (StateT AppState IO)

runApp :: App a -> Int -> IO (a,AppState)
runApp k maxDepth =
  let config = AppConfig maxDepth
      state  = AppState  0
  in runStateT (runReaderT k config) state

constrainedCount :: Int -> FilePath -> App [(FilePath,Int)]
constrainedCount curDepth path = do
  contents <- liftIO . listDirectory $ path
  cfg      <- ask
  rest     <- forM contents $ \name -> do
               let newPath = path </> name
               isDir <- liftIO $ doesDirectoryExist newPath
               if isDir && curDepth < cfgMaxDepth cfg
                  then do
                    let newDepth = curDepth + 1
                    st <- get
                    when (stDeepestReached st < newDepth) $
                      put st {stDeepestReached = newDepth}
                    constrainedCount newDepth newPath
                  else return []
  return $ (path, length contents) : concat rest


{-----------------------------------------------unlucky Except------------------------------------------------------
Use the Except monad.

exception is thrown if the value is 13.
Use the throw catch method from Except.
-}

data NumberError = Is13 {locationMsg :: String }

instance Show NumberError where
  show (Is13 msg) = msg ++" The number was 13!"

type NumberMonad = Either NumberError

{-
Given:
extraMsg: extra error info such as where error occured.
int: The vaule that is being set to.

Return:
Right: the number that was successfully set.
Left NumberError: the extra error msg.
-}
setNumberOrFail :: String -> Int -> NumberMonad Int
setNumberOrFail extraMsg 13  = throwError (Is13 extraMsg)
setNumberOrFail _ int = Right int

--wrapper around setNumberOrFail
setNumber :: String -> Int -> NumberMonad Int
setNumber extraMsg int = (setNumberOrFail extraMsg int) `catchError` Left

--display results in repl
reportNumberResult :: NumberMonad Int -> IO ()
reportNumberResult (Right int) = putStrLn ("The number is " ++ (show int))
reportNumberResult (Left err)  = putStrLn ("The number set failed with error: " ++ (show err))

--simple tests without do notation
goodNumberTest = setNumber "" 3
badNumberTest  = setNumber "bad bobo "13

{-Use the Either monad in do notations

-}
goodNumberDoTest :: Int -> NumberMonad Int
goodNumberDoTest int = do
  number1 <- setNumber "at number1 " int
  number2 <- setNumber "at number2 " (number1 + 1)
  number3 <- setNumber "at number3 " (number2 + 1)
  return number3

-- run goodNumberDoTest, using reportNumberResult to show results.
runGoodNumberDoTest :: Int -> IO ()
runGoodNumberDoTest int = reportNumberResult $ goodNumberDoTest int


{----------------now use ExceptT--------------
Lift IO to get a number into ExceptT
-}

exceptTIOTest :: ExceptT NumberError IO Int
exceptTIOTest = do
  --liftIO readIOInt'
  num1 <- (is13 readIOInt') `catchError` (badNumberHandler "line1")
  num2 <-(is13 readIOInt') `catchError` (badNumberHandler "line2")
  liftIO $ putStrLn $ show (num1,num2)
  return num1
  

badNumberHandler :: String -> NumberError -> ExceptT NumberError IO Int
badNumberHandler extraMsg ( (Is13 msg)) = do
  throwE (Is13 (extraMsg ++ msg))

is13 :: IO Int -> ExceptT NumberError IO Int
is13 inIO = do
  val <- liftIO inIO
  case val of
   13 -> ExceptT $ return (Left $ Is13 "error: ")
   val  -> ExceptT (return $ Right val )

is13' :: Int -> ExceptT NumberError IO Int
is13' inInt = do
  case inInt of
   13 -> ExceptT $ return (Left $ Is13 "error: ")
   val  -> ExceptT (return $ Right val )


 
{-Read in an interger.
Will use it to do a live test that will require ExceptT IO and NumberMonad.
Could not make it work first time due to mismatched types, so deleted.-}
readIOInt :: IO (NumberMonad Int)
readIOInt = do
  line <- getLine
  let q =
        case TR.readMaybe line of
          Just a -> Right a
          Nothing -> Right 13
  return q 

readIOInt' :: IO (Int)
readIOInt' = do
  line <- getLine
  let q =
        case TR.readMaybe line of
          Just a ->  a
          Nothing -> 13
  return q


{------------------------------  combine State Stack and ExceptT unlucky number --------------------------------
Use the State Stack Int example along with the unluck number Except example
-}



{-
run the function with: reportUnluckyA $ runExceptT (unluckyStack 1)
  -can use any number instead of 1. 13 will cause left result.
  -can use reportUnluckyA or reportUnluckyS to look at a or s of state.

Return:
if possiblyBadInt == 13
  A left stack with (13,[0] ) Note that stack0 got pushed on the stack.
  A NumberError with line position message.

if possiblyBadInt != 13
  A Right stack (2,[2,1,0])
-}
unluckyTest :: Int -> ExceptT NumberError (State Stack) Int
unluckyTest possiblyBadInt = do
  --is the # 13
    --yes throw error
    --no  push onto stack
  stack0 <- (pushUnluckNumber 0) `catchError` (unluckyNumberHandler "line0")
  stack1 <- (pushUnluckNumber possiblyBadInt)  `catchError` (unluckyNumberHandler "line1")
  stack2 <- (pushUnluckNumber 2) `catchError` (unluckyNumberHandler "line2")
  return stack2

{-

-}
pushUnluckNumber :: Int  -> ExceptT NumberError (State Stack) Int
pushUnluckNumber a = do
  if a == 13
     --then  ExceptT $ return $ Left (Is13 "")
     then throwE (Is13 "")  
     else lift $ state $ \xs -> (a, a:xs)
     

unluckyNumberHandler :: String -> NumberError -> ExceptT NumberError (State Stack) Int
unluckyNumberHandler extraMsg ( (Is13 msg)) = do
  throwE (Is13 (extraMsg ++ msg))


reportUnluckyA :: (State Stack (Either NumberError Int)) -> IO ()
reportUnluckyA a = print $ show $ evalState a []

reportUnluckyS :: (State Stack (Either NumberError Int)) -> IO ()
reportUnluckyS a = print $ show $ execState a []




{----------------------------------- CornerPoints using State----------------------------------------------}
type CpointsStack = [CornerPoints]
type CpointsList = [CornerPoints] 

{-
a single ' indicates that combine' is used: cx's for errors in the cPointsNew
a double '' used combine'' which does not cx for errors in cPointsNew. Use Except monad instead.
-}
pushCpoints' :: CpointsList -> State CpointsStack CpointsList
--pushCpoints cPoints = state $ \cubes' -> (cPoints, cPoints ++ cubes')
pushCpoints' cPointsNew = state $ \cPointsAll -> (cPointsNew,
                                                  combine' cPointsNew  cPointsAll)

pushCpoints'' :: CpointsList -> State CpointsStack CpointsList
--pushCpoints cPoints = state $ \cubes' -> (cPoints, cPoints ++ cubes')
pushCpoints'' cPointsNew = state $ \cPointsAll -> (cPointsNew,
                                                   combine'' cPointsNew  cPointsAll) 
--well shaped cubes
buildGoodShape' :: State  CpointsStack CpointsList
buildGoodShape' = do
  initialShape   <- pushCpoints' [cube]
  elongatedShape <- pushCpoints'
                    (initialShape
                     |+++|
                     (map ((transposeY (+1)) . extractFrontFace) initialShape)
                    )
  addATop        <- pushCpoints'
                    (elongatedShape
                    |+++|
                    (map ((transposeZ (+1)) . extractTopFace) elongatedShape))
  return addATop


writeGoodShape' ::  IO ()
writeGoodShape'  = do
  let triangles' = [FacesAll | x <- [1..]] |+++^| (execState buildGoodShape' [])
  writeStlToFile $ newStlShape "builder monad test" triangles'
  putStrLn "shape written to stl"

--the add top is nfg, so it should not be added.
buildBadShape' :: State  CpointsStack CpointsList
buildBadShape' = do
  initialShape   <- pushCpoints' [cube]
  elongatedShape <- pushCpoints'
                    (initialShape
                     |+++|
                     (map ((transposeY (+1)) . extractFrontFace) initialShape)
                    )
  addATop        <- pushCpoints'
                    (elongatedShape
                     |+++|
                     (--adding a cube to a cube is an error
                      (map ((transposeZ (+1)) . extractTopFace) elongatedShape)
                      |+++|
                      (map ((transposeZ (+1)) . extractBottomFace) elongatedShape)
                     )
                    )
  return addATop

writeBadShape' ::  IO ()
writeBadShape'  = do
  let triangles' = [FacesAll | x <- [1..]] |+++^| (execState buildBadShape' [])
  writeStlToFile $ newStlShape "builder monad test" triangles'
  putStrLn "bad shape written to stl"


{----------------------------------------CornerPoints with Except monad-----------------------------
uses Control.Monad.Except to build up [CornerPoints] in do notation.

It does not maintain state, so only the last [CornerPoints] is returned.
Unless an error occurrs along the way, in which case the Left error is returned,
as per standard Either monad functionality.
-}

--data type for an exception as required for the Except monad.
data BuilderError  = BuilderError {errMsg :: String }

--common pattern to show the exception
instance Show BuilderError where
  show (BuilderError errMsg') = show errMsg'

{-
Common pattern.
It still requires the type of data being handled. In this case: [CornerPoints].
BuilderMonad [CornerPoints] is the return type of the monad.
-}
type BuilderMonad = Either BuilderError

{-
Run the test funcion buildCPointsListInDo  in repl.
Uses reportBuildCpointsList to extract and show the results from the Either monad.
-}
runBuildCPointsListInDo = do
  reportBuildCpointsList $ buildCPointsListInDo

{-
Test out buildCpointsList.

Note:
If return goodCube: will still have the badCube failure if badCube has not been commented out.
Will return goodCube if badCube has been commented out.
This show how Monads have to execute in order, instead of lazily.

If comment out badCube, can return either goodCube and goodCube2
-}
buildCPointsListInDo :: BuilderMonad CpointsList
buildCPointsListInDo = do
  goodCube <- buildCpointsList "at good cube" [btmFace] [(upperFaceFromLowerFace $ transposeZ (+1) btmFace )]
  goodCube2 <- buildCpointsList "at good cube2" goodCube (map ((transposeZ (+1)) . extractTopFace)  goodCube )
  badCube <- buildCpointsList "at bad cube" goodCube2 [cube]
  return goodCube

--run buildCpointsList in repl.
runBuildCPointsList = do
  reportBuildCpointsList $ buildCpointsList "at cube + cube" [cube] [cube]

{-
given:
extraMsg : a failure msg to say where in the monad, the failure happened.
cPoints cPoints' : the 2 [CornerPoints] being added together

task:
Add the 2 [CornerPoints] together, with the possibility of failure.

return:
Right [CornerPoints] : the result of the addition, if no error
Left BuilderError : with info about where the error occurred, and what type of error.

Notes:
Is a wrapper around buildCpointsListOrFail, allowing `catchError` Left to be added.
catchError: 
-}
buildCpointsList :: String -> CpointsList -> CpointsList -> BuilderMonad CpointsList
buildCpointsList extraMsg cPoints cPoints' = 
  (buildCpointsListOrFail extraMsg cPoints cPoints') `catchError` Left



{-
See buildCpointsList, which is a wrapper around this with catchError
-}
buildCpointsListOrFail :: String -> CpointsList -> CpointsList -> BuilderMonad CpointsList
buildCpointsListOrFail extraMsg cPoints cPoints' =
  let isError :: CornerPoints -> Bool
      isError (CornerPointsError _) = True
      isError _                     = False

      hasError = find (isError) 

      cube = cPoints |+++| cPoints'
  in  case hasError cube of
        Nothing -> Right cube
        Just err -> throwError (BuilderError (extraMsg ++ " " ++ (errMessage err)) {-cube-})

{-
task:
Extract the data from the Either Monad, and show it.
Used to display tests in the repl.
-}
reportBuildCpointsList :: BuilderMonad CpointsList -> IO ()
reportBuildCpointsList (Right cPointsList) = print $ show cPointsList
reportBuildCpointsList (Left e) = putStrLn $ show e

{------------------------------- CornerPoints with ExceptT State---------------------------------}

reportCornerPointsExceptTStateA :: IO ()
reportCornerPointsExceptTStateA = do
  print $ show $ (evalState $ runExceptT cornerPointsWithExceptTStateTest )  []

reportCornerPointsExceptTStateS :: IO ()
reportCornerPointsExceptTStateS = do
  print $ show $ (execState $ runExceptT cornerPointsWithExceptTStateTest )  []

--run in repl with: reportCornerPointsExceptTStateA(S)
cornerPointsWithExceptTStateTest :: ExceptT BuilderError (State CpointsStack ) CpointsList
cornerPointsWithExceptTStateTest = do
  list1 <- buildCpointsList' "list 1" [btmFace] [(upperFaceFromLowerFace $ transposeZ (+1) btmFace )] --good cube
  list2 <- buildCpointsList' "list 2" list1 (map (extractTopFace . (transposeZ (+1))) list1 ) --good cube
  list3 <- buildCpointsList' "list 3" list2 [cube] --bad cube
  return list3

buildCpointsList' :: String -> CpointsList -> CpointsList -> ExceptT BuilderError (State CpointsStack ) CpointsList
buildCpointsList' extraMsg cPoints cPoints' = 
  (buildCpointsListOrFail' extraMsg cPoints cPoints') `catchError` badCPointsHandler
{-
See buildCpointsList, which is a wrapper around this with catchError
-}
buildCpointsListOrFail' :: String -> CpointsList -> CpointsList -> ExceptT BuilderError (State CpointsStack ) CpointsList
buildCpointsListOrFail' extraMsg cPoints cPoints' =
  let isError :: CornerPoints -> Bool
      isError (CornerPointsError _) = True
      isError _                     = False

      hasError = find (isError) 

      cubeList = cPoints |+++| cPoints'
  in  case hasError cubeList of
        Nothing -> lift $ state $ \cubeStack -> (cubeList, cubeList ++ cubeStack)
        Just err -> throwE (BuilderError (extraMsg ++ " " ++ (errMessage err)) {-cube-})
  
badCPointsHandler :: BuilderError -> ExceptT BuilderError (State CpointsStack ) CpointsList
badCPointsHandler error = do
  throwE error
