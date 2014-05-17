module FeatherweightJava
(
        Klass(..), Method(..), Expr(..), KlassTable(..), Program, Object(..), KlassName,
        run
) where

-- Imports
import qualified Data.Map.Strict as M
import qualified Control.Monad as C
import qualified Data.Either as E
import Utils
import Data.Maybe
import Debug.Trace


-- Data types
type KlassName = String
type MethodName = String
type FieldName = String
type ObjectName = String

data Klass = Klass
        { kName :: KlassName
        , kParent :: KlassName
        , kFields :: [(FieldName, KlassName)]
        , kMethods :: M.Map MethodName Method
} deriving (Eq, Show)

data Method = Method
        { mName :: MethodName
        , mArgs :: [(ObjectName, KlassName)]
        , mExpr :: Expr
        , mRetType :: KlassName
} deriving (Eq, Show)

data Expr = Arg ObjectName
        | FieldCall Expr FieldName
        | MethodCall Expr MethodName [Expr]
        | NewObject KlassName [Expr]
        | TypeCast Expr KlassName deriving (Eq, Show)

data TypeRelation = Subtype | Supertype | Equal | None deriving (Show, Eq)

type KlassTable = M.Map KlassName Klass
type Program = (KlassTable, Expr)

data Object = Object
        { oClass :: KlassName
        , oFields :: [(FieldName, Object)]
} deriving (Eq, Show)

-- Typechecker
inh_tree :: KlassTable -> KlassName -> Either String [KlassName]
inh_tree kt name = C.liftM reverse $ inh_tree' kt name []

inh_tree' :: KlassTable -> KlassName -> [KlassName] -> Either String [KlassName]
inh_tree' kt "Object" acc = Right ("Object":acc)
inh_tree' kt name acc = do
        pName <- C.liftM kParent $ klassLookup kt name
        if name `elem` acc
        then Left ("Loop in inheritance tree of "++name)
        else inh_tree' kt pName (name:acc)

type_relation :: KlassTable -> KlassName -> KlassName -> Either String TypeRelation
type_relation kt x y = do
        xtree <- inh_tree kt x
        ytree <- inh_tree kt y
        return $ case (x `elem` ytree, y `elem` xtree) of
                (True, True)    -> Equal
                (True, False)   -> Supertype
                (False, True)   -> Subtype
                (False, False)  -> None

isSubtypeOf :: KlassTable -> KlassName -> KlassName -> Either String Bool
isSubtypeOf kt x y = C.liftM (\x -> x `elem` [Equal, Subtype]) (type_relation kt x y)

fields' :: KlassTable -> KlassName -> ((FieldName, ObjectName) -> a) -> Either String [a]
fields' kt "Object" f = Right []
fields' kt name f = do
        Klass _ pname fs _ <- klassLookup kt name
        pRes <- fields' kt pname f
        return $ pRes ++ (map f fs)

fields :: KlassTable -> KlassName -> Either String [KlassName]
fields kt name = fields' kt name snd

fieldNames :: KlassTable -> KlassName -> Either String [FieldName]
fieldNames kt name = fields' kt name fst

mtype :: KlassTable -> KlassName -> MethodName -> Either String (Maybe ([KlassName], KlassName))
mtype kt "Object" _ = Right Nothing
mtype kt kName name = do
        klass <- klassLookup kt kName
        case (M.lookup name $ kMethods klass) of
                Just method     -> return$ Just (map snd (mArgs method), mRetType method)
                Nothing         -> mtype kt (kParent klass) name

methodLookup' :: KlassTable -> KlassName -> MethodName -> Either String (Maybe Method)
methodLookup' kt "Object" _ = Right Nothing
methodLookup' kt kName name = do
        klass <- klassLookup kt kName
        case (M.lookup name $ kMethods klass) of
                Just method     -> return$ Just method
                Nothing         -> methodLookup' kt (kParent klass) name


klassLookup :: KlassTable -> KlassName -> Either String Klass
klassLookup kt name = case (M.lookup name kt) of
        Just klass      -> Right klass
        Nothing         -> Left ("There is no `"++name++"` in Class Table.")

fieldLookup :: KlassTable -> KlassName -> FieldName -> Either String KlassName
fieldLookup kt "Object" name = Left ("There is no field `"++name++"`.")
fieldLookup kt kName name = (klassLookup kt kName) >>=
        (\(Klass _ pname fs _) -> case (lookup name fs) of
                Just kName      -> Right kName
                Nothing         -> fieldLookup kt pname name)

methodLookup :: Klass -> MethodName -> Either String Method
methodLookup klass name = case (M.lookup name (kMethods klass)) of
        Nothing         -> Left ("Method `"++(kName klass)++"`.`"++name++"`")
        Just method     -> Right method

compare_types :: KlassTable -> [KlassName] -> [KlassName] -> Either String Bool
compare_types kt xs ys = Right $ ((length xs) == (length ys)) && (and $ E.rights $ map (uncurry $ isSubtypeOf kt) $ zip xs ys)

equal_types :: [KlassName] -> [KlassName] -> Bool
equal_types xs ys = ((length xs) == (length ys)) && (all (uncurry (==)) $ zip xs ys)

checkelist :: M.Map ObjectName KlassName -> KlassTable -> [Expr] -> Either String [KlassName]
checkelist args kt es = mapM (curry (checketype args) kt) es

checketype :: M.Map ObjectName KlassName -> Program -> Either String KlassName
checketype args (kt, e@(TypeCast expr name)) = (checketype args (kt,expr)) >> (return name)

checketype args (kt, e@(NewObject name es)) = do -- T-New
        fst <- fields kt name
        est <- checkelist args kt es
        compRes <- compare_types kt est fst
        if compRes
        then Right name
        else Left ("Argument types are not ok in constructor of `"++name++"`.")

checketype args (kt, e@(MethodCall expr name es)) = do -- T-Invk
        expr_type <- checketype args (kt, expr)
        est <- checkelist args kt es
        mtypes <- mtype kt expr_type name
        maybe method_not_found (\(argTypes, retType) ->
                (compare_types kt est argTypes) >>= (\res ->
                        if res then Right retType else wrong_arguments)) mtypes
        where   method_not_found = Left ("Method `"++name++"` not found.")
                wrong_arguments = Left ("Argument's types are not ok in `"++name++"` call.")

checketype args (kt, e@(FieldCall expr name)) = (checketype args (kt, expr)) >>= (\expr_type ->
        fieldLookup kt expr_type name) -- T-Field

checketype args (kt, e@(Arg name)) = case (M.lookup name args) of
        Just kName      -> Right kName
        Nothing         -> Left ("There is no `"++name++"` variable.")

checkmethod :: KlassTable -> KlassName -> MethodName -> Either String ()
checkmethod kt kName name = do
        klass <- klassLookup kt kName
        method <- methodLookup klass name
        let method_args = mArgs method
        let args = M.insert "this" kName (M.fromList method_args)
        expr_type <- checketype args (kt, mExpr method)
        mtypeRes <- mtype kt (kParent klass) name
        C.void $ case mtypeRes of
                Nothing                 -> isSubtypeOf kt expr_type (mRetType method)
                Just(mArgs', mRetType') -> do
                        subtypeCond <- isSubtypeOf kt expr_type (mRetType method)
                        rettypeCond <- return$ mRetType' == (mRetType method)
                        equalCond <- return$ equal_types mArgs' (map snd $ mArgs method)
                        return$ subtypeCond && rettypeCond && equalCond

checkclass :: KlassTable -> KlassName -> Either String ()
checkclass kt name = do
        klass <- klassLookup kt name
        mapM_ (checkmethod kt name) (M.keys $ kMethods klass)

typechecker :: Program -> IO Bool
typechecker prog@(kt, expr) = do
        expr_ok <- case (checketype M.empty example1) of
                Left err        -> putStrLn ("Error in expression: "++err) >> return False
                Right etype     -> putStrLn ("Expression type: "++etype++" - expression OK.") >> return True
        class_ok <- C.foldM (\ok name -> case (checkclass kt name) of
                        Left e  -> putStrLn ("Error in class `"++name++"`: "++e) >> return False
                        Right _ -> putStrLn ("Class `"++name++"` OK.") >> return (True && ok)
                ) True (M.keys kt)
        return$ expr_ok && class_ok

-- Evaluator -- assumes, that typechecker was run before
evaluate :: M.Map ObjectName Object -> Program -> Object
evaluate args prog@(kt, NewObject kName es) = Object kName os
        where   fs = getRight $ fieldNames kt kName
                os = map run_arg $ zip fs es
                run_arg (name, expr) = (name, evaluate args (kt, expr))

evaluate args prog@(kt, TypeCast expr name) = Object name os
        where   Object _ os = evaluate args (kt, expr)

evaluate args prog@(kt, FieldCall expr name) = fromJust $ lookup name os
        where   Object _ os = evaluate args (kt, expr)

evaluate args prog@(kt, MethodCall expr name es) = evaluate newArgs' (kt, mExpr)
        where   o@(Object kName os) = evaluate args (kt, expr)
                est = map (\e -> evaluate args (kt, e)) es
                Method _ mArgs mExpr mRetType = fromJust $ getRight $ methodLookup' kt kName name
                newArgs = foldl (\acc (name, val) -> M.insert name val acc) args (zip (map fst mArgs) est)
                newArgs' = M.insert "this" o newArgs

evaluate args prog@(kt, Arg name) = case (M.lookup name args) of
        Just obj        -> obj
        Nothing         -> error ("There is no `"++name++"` variable.")

-- Examples
example_kt_pair = M.fromList [("A", Klass "A" "Object" [] (M.fromList [])),                        ("B", Klass "B" "Object" [] (M.fromList [])),                        ("Pair", Klass "Pair" "Object" [("fst", "Object"), ("snd", "Object")] (M.fromList [("setfst", Method "setfst" [("newfst", "Object")] (NewObject "Pair" [Arg "newfst", FieldCall (Arg "this") "snd"]) "Pair")]))                ]
example1 = (example_kt_pair, expr)
        where   expr = MethodCall (NewObject "Pair" [NewObject "A" [], NewObject "B" []]) "setfst" [NewObject "B" []]

-- Main

run :: Program -> IO ()
run example = do
        res <- typechecker example
        if res
        then putStrLn $ "Result of evaluation: "++(show$ evaluate M.empty example)
        else return$ ()

main = run example1