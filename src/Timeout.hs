module Timeout where

import qualified Data.Map as M
import Control.Monad.State
import Data.Maybe
import Debug.Trace

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Mod Expr Expr
  | Eq  Expr Expr
  | Neq Expr Expr
  | Gt  Expr Expr
  | Lt  Expr Expr
  | Geq Expr Expr
  | Leq Expr Expr
  | Neg Expr
  | Num Int
  | Var String
  deriving (Eq, Show)

data Statement
  = Halt
  | Label String
  | Goto  String
  | If Expr Statement
  | Set String Expr
  | In  String
  | Out Expr
  | Send String Expr Expr Expr
  | Seq [Statement]
  | NOP
  deriving (Eq, Show)

type Program = M.Map LineNum Statement
type Value   = Int
type LineNum = Int
type Name    = String

data PState = Birth | PState
  { vars   :: M.Map Name Value
  , labels :: M.Map Name LineNum
  , pos    :: LineNum
  , code   :: Program
  , past   :: PState
  } deriving (Show)

initialState p = PState
  { vars   = M.empty
  , labels = M.empty
  , pos    = 0
  , code   = p
  , past   = Birth
  }

initState = initialState . M.fromList . zip [0..]


untangle :: PState -> PState
untangle p = snd $ flip runState p interp

interp :: State PState ()
interp = do
  old <- get
  c   <- gets code
  l   <- gets pos

  let st = M.lookup l c

  case st of
    Just Halt -> return ()
    Nothing -> return ()
    _ -> do
      runStatement $ fromJust st

      l <- gets pos
      modify $ \s -> s { pos  = l+1 }
      modify $ \s -> s { past = old }

      interp



runStatement st = do
  l <- gets pos
  case st of
    NOP     -> return ()
    Label n -> modify $ \s -> s { labels = M.insert n l $ labels s }
    Goto  n -> modify $ \s -> s { pos    = fromJust $ M.lookup n $ labels s }
    Set n e -> runExpr e >>= \v -> modify $ \s -> s { vars = M.insert n v $ vars s }
    If e s  -> runExpr e >>= \v -> when (v/=0) $ runStatement s
    Send v p n c -> do -- TODO: Time-paradox
      v' <- (fromJust . M.lookup v) <$> gets vars
      (n', p') <- (,) <$> runExpr n <*> runExpr p
      (_, _, ps) <- findOccurance (Set v $ Num v') c n' p'
      modify $ const ps
      modify $ \s -> s { code = M.insert l NOP $ code s }
    Out e -> runExpr e >>= \v -> trace (show v) $ return ()

runExpr :: Expr -> State PState Int
runExpr e = case e of
  Add a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ a+b
  Sub a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ a-b
  Mul a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ a*b
  Div a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ a`div`b
  Pow a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ a^b
  Mod a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ a`mod`b
  Eq  a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ if a==b then 1 else 0
  Neq a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ if a/=b then 1 else 0
  Gt  a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ if a>b then 1 else 0
  Lt  a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ if a<b then 1 else 0
  Geq a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ if a>=b then 1 else 0
  Leq a b -> runExpr a >>= \a -> runExpr b >>= \b -> return $ if a<=b then 1 else 0
  Var a -> (fromJust . M.lookup a) <$> gets vars
  Num a -> return a

findOccurance :: Statement -> Expr -> Int -> Int -> State PState (Int, Int, PState)
findOccurance v c t pl = do
  this <- get
  p    <- gets past
  l    <- gets pos

  case p of
    Birth -> return (-1, 0, this)
    x     -> do
      r <- runExpr c
      let (o, pl', s) = fst $ runState (findOccurance v c t pl) p

      let n = o + (if r/=0 then 1 else 0)
      when (n==t && (o/=n || abs pl - abs pl' == 1)) $ runStatement v

      if n < t
         then return (n, pl', this)
         else if pl == pl'
                 then return (n, pl', if n==o then s else this)
                 else if pl > pl'
                         then return (n, pl'+1, this)
                         else error "NOT IMPLEMENTED"

run :: PState -> IO ()
run p = const (return ()) $! untangle p

testProg = initState
  [ Set "a" (Num 1)
  , Set "a" (Add (Var "a") (Num 2))
  , Send "a" (Num 0) (Num 0) (Eq (Var "a") (Num 0))
  , Out $ Var "a"
  ]

