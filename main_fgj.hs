-- Imports
import qualified Data.Map.Strict as M
import Data.Map.Strict((!))
import Utils
import System.Environment

--import Debug.Trace
import qualified FeatherweightJava as FJ

-- Data types
type KlassName = String
type MethodName = String
type FieldName = String
type ObjectName = String

data KlassType = SimpleKlass KlassName | ComplexKlass KlassGeneric deriving (Eq, Show)
data KlassGeneric = KlassGeneric KlassName [KlassType] deriving (Eq, Show)
data Generic = Generic KlassName KlassGeneric [(FieldName, KlassType)] [(KlassName, KlassGeneric)] (M.Map MethodName Method) deriving (Eq, Show)
data Method = Method MethodName [(ObjectName, KlassType)] Expr KlassType [(KlassName, KlassGeneric)] deriving (Eq, Show)

data Expr = Arg ObjectName
        | FieldCall Expr FieldName
        | MethodCall Expr MethodName [Expr] [KlassType]
        | NewObject KlassGeneric [Expr]
        | TypeCast Expr KlassGeneric deriving (Eq, Show)

type KlassTable = M.Map KlassName Generic
type Program = (KlassTable, Expr)
type Context = M.Map KlassName KlassGeneric
type Environment = M.Map ObjectName KlassType

-- Small utils
mk_SimpleClass :: KlassName -> KlassType
mk_SimpleClass name = SimpleKlass name

mk_ComplexClass :: KlassGeneric -> KlassType
mk_ComplexClass gen = ComplexKlass gen

mk_context :: [(KlassName, KlassGeneric)] -> M.Map KlassName KlassGeneric
mk_context = M.fromList

join_context :: M.Map KlassName KlassGeneric -> M.Map KlassName KlassGeneric -> M.Map KlassName KlassGeneric
join_context c1 c2 = mk_context $ (M.toList c1) ++ (M.toList c2)

get_generic :: KlassTable -> KlassName -> Generic
get_generic kt kName = kt ! kName

mk_type_mapper :: [KlassName] -> [KlassType] -> M.Map KlassName KlassType
mk_type_mapper type_variables kts = M.fromList (zip type_variables kts)

isSimpleKlass :: KlassType -> Bool
isSimpleKlass kType = case kType of
        SimpleKlass _   -> True
        _               -> False

free_variables :: KlassType -> [KlassName]
free_variables (SimpleKlass kName) = [kName]
free_variables (ComplexKlass (KlassGeneric _ kts)) = concatMap free_variables kts

is_defined :: KlassTable -> MethodName -> KlassName -> Bool
is_defined kt mName "Object" = False
is_defined kt mName kName = mName `elem` (M.keys gMethods)
        where   (Generic _ _ _ _ gMethods) = get_generic kt kName

get_method :: KlassTable -> KlassName -> MethodName -> Method
get_method kt kName mName = gMethods ! mName
        where   (Generic _ _ _ _ gMethods) = get_generic kt kName

-- Auxiliary functions, mostly specific functions from official publication about FJ
klass_type_substitute :: M.Map KlassName KlassType -> KlassType -> KlassType
klass_type_substitute mapper kType = case kType of
        ComplexKlass (KlassGeneric kName kts)   -> ComplexKlass (KlassGeneric kName (map (klass_type_substitute mapper) kts))
        SimpleKlass kName                       -> (case (M.lookup kName mapper) of
                Just kType'     -> kType'
                Nothing         -> kType)

klass_generic_substitute :: M.Map KlassName KlassType -> KlassGeneric -> KlassGeneric
klass_generic_substitute mapper kGeneric = kGeneric'
        where   ComplexKlass kGeneric' = klass_type_substitute mapper (ComplexKlass kGeneric)


expr_substitute :: M.Map KlassName KlassType -> Expr -> Expr
expr_substitute mapper expr = case expr of
        Arg x                           -> expr
        FieldCall expr0 fName            -> FieldCall (expr_substitute mapper expr0) fName
        MethodCall expr0 mName es kts    -> MethodCall (expr_substitute mapper expr0) mName (map (expr_substitute mapper) es) (map (klass_type_substitute mapper) kts)
        NewObject kGeneric es           -> NewObject (klass_generic_substitute mapper kGeneric) (map (expr_substitute mapper) es)
        TypeCast es kGeneric            -> TypeCast (expr_substitute mapper expr) (klass_generic_substitute mapper kGeneric)


fields :: KlassTable -> KlassGeneric -> [(FieldName, KlassType)]
fields kt (KlassGeneric "Object" _) = []
fields kt (KlassGeneric gName kts) = parent_fields ++ generic_fields
        where   (Generic _ gParent gFields gParams _) = get_generic kt gName
                type_mapper = mk_type_mapper (map fst gParams) kts
                parent_fields = fields kt (klass_generic_substitute type_mapper gParent)
                kts' = map (klass_type_substitute type_mapper) kts
                generic_fields = zip (map fst gFields) kts'


mtype :: KlassTable -> KlassGeneric -> MethodName -> ([(KlassName, KlassGeneric, KlassType)], KlassType)
mtype kt kGeneric mName = case (M.lookup mName gMethods) of
        Just m          -> (mArgs'', mRetType')
                where   (Method _ mArgs _ mRetType mCast) = m
                        mRetType' = klass_type_substitute type_mapper mRetType
                        mArgs' = uncurry zip3 (unzip mCast) (map snd mArgs)
                        mArgs'' = map (\(x,y,z) -> (x, klass_generic_substitute type_mapper y, klass_type_substitute type_mapper z)) mArgs'
        Nothing         -> mtype kt kGeneric' mName
        where   (KlassGeneric gName kts) = kGeneric
                (Generic _ gParent _ gParams gMethods) = get_generic kt gName
                type_mapper = mk_type_mapper (map fst gParams) kts
                kGeneric' = klass_generic_substitute type_mapper gParent


mbody :: KlassTable -> KlassGeneric -> MethodName -> [KlassType] -> ([KlassName], Expr)
mbody kt kGeneric mName mValues = case (M.lookup mName gMethods) of
        Just m          -> (mVars, mExpr')
                where   (Method _ mArgs mExpr mRetType mCast) = m
                        mVars = map fst mArgs
                        type_mapper' = mk_type_mapper (map fst mCast) mValues
                        new_type_mapper = M.union type_mapper type_mapper'
                        mExpr' = expr_substitute new_type_mapper mExpr
        Nothing         -> mbody kt kGeneric' mName mValues
        where   (KlassGeneric gName kts) = kGeneric
                (Generic _ gParent _ gParams gMethods) = get_generic kt gName
                type_mapper = mk_type_mapper (map fst gParams) kts
                kGeneric' = klass_generic_substitute type_mapper gParent


bound :: Context -> KlassType -> KlassGeneric
bound ctx kType = case kType of
        SimpleKlass name        -> ctx ! name
        ComplexKlass kGeneric   -> kGeneric


isSubclassOf :: KlassTable -> KlassName -> KlassName -> Bool
isSubclassOf kt k1 k2
        | k1 == k2              = True
        | k2 == pName           = True
        | k1 == "Object"        = False
        | otherwise             = isSubclassOf kt pName k2
        where   (Generic _ (KlassGeneric pName _) _ _ _) = get_generic kt k1


isSubtypeOf :: KlassTable -> Context -> KlassType -> KlassType -> Bool
isSubtypeOf kt ctx k1 k2
        | k1 == k2                                      = True
        | k1 == ComplexKlass (KlassGeneric "Object" []) = False
        | otherwise                                     = case k1 of
                SimpleKlass k1Name                      -> (kc == k2) || (isSubtypeOf kt ctx kc k2)
                        where   kc = ComplexKlass (ctx ! k1Name)
                ComplexKlass (KlassGeneric k1Name kts)  -> (kp == k2) || (isSubtypeOf kt ctx kp k2)
                        where   (Generic _ kGenericParent _ gParams _) = get_generic kt k1Name
                                type_mapper = mk_type_mapper (map fst gParams) kts
                                kp = ComplexKlass (klass_generic_substitute type_mapper kGenericParent)


isWellFormed :: KlassTable -> Context -> KlassType -> Bool
isWellFormed kt ctx (ComplexKlass (KlassGeneric "Object" _)) = True
isWellFormed kt ctx (SimpleKlass kName) = kName `elem` (M.keys ctx)
isWellFormed kt ctx (ComplexKlass (KlassGeneric gName kts)) = params_ok && subtypes_ok
        where   (Generic _ _ _ gParams _) = get_generic kt gName
                params_ok = and$ map (isWellFormed kt ctx) kts
                bounds = map snd gParams
                type_mapper = mk_type_mapper (map fst gParams) kts
                bounds' = map (ComplexKlass . (klass_generic_substitute type_mapper)) bounds
                subtypes_ok = and$ map (uncurry$ isSubtypeOf kt ctx) $ zip kts bounds'


downcast :: KlassTable -> KlassName -> KlassName -> Bool
downcast kt k1 k2
        | k1 == "Object"        = False
        | k2 == pName           = set_equal type_vars free_vars
        | otherwise             = downcast kt pName k2
        where   (Generic _ gParent _ gParams _) = get_generic kt k1
                (KlassGeneric pName kts) = gParent
                type_vars = map fst gParams
                free_vars = concatMap free_variables kts


-- Determining type
typeof :: KlassTable -> Context -> Environment -> Expr -> KlassType
typeof kt ctx env (Arg x) = env ! x

typeof kt ctx env (FieldCall expr0 fName) = lookupe fName expr0_fields
        where   expr0_type = typeof kt ctx env expr0
                expr0_fields = fields kt (bound ctx expr0_type)

typeof kt ctx env expr@(NewObject kGeneric es) = if and (klass_ok:subtypes)
        then ComplexKlass kGeneric else error ("Error when typechecking `"++(show expr)++"`.\n")
        where   klass_ok = isWellFormed kt ctx (ComplexKlass kGeneric)
                field_types = map snd $ fields kt kGeneric
                arg_types = map (typeof kt ctx env) es
                subtypes = map (uncurry $ isSubtypeOf kt ctx) $ zip arg_types field_types

typeof kt ctx env expr@(MethodCall expr0 mName es kts) = if everything_ok
        then ret_type' else error ("Error when typechecking `"++(show expr)++"`.\n")
        where   expr0_type = typeof kt ctx env expr0
                es_types = map (typeof kt ctx env) es
                (arg_types, ret_type) = mtype kt (bound ctx expr0_type) mName
                type_mapper = mk_type_mapper (map fst3 arg_types) kts
                type_vars_ok = and$ map (isWellFormed kt ctx) kts
                vars_subtypes_ok = and$ map (uncurry $ isSubtypeOf kt ctx) (zip kts (map (ComplexKlass . (klass_generic_substitute type_mapper) . snd3) arg_types))
                val_subtypes_ok = and$ map (uncurry $ isSubtypeOf kt ctx) (zip es_types (map (klass_type_substitute type_mapper . trd3) arg_types))
                everything_ok = type_vars_ok && vars_subtypes_ok && val_subtypes_ok
                ret_type' = klass_type_substitute type_mapper ret_type

typeof kt ctx env expr@(TypeCast expr0 kGeneric)
        | isSubtypeOf kt ctx bounded_expr0_type ret_type                = ret_type
        | isSubtypeOf kt ctx ret_type bounded_expr0_type && dcast_conds = ret_type
        | scast_conds                                                   = ret_type
        | otherwise                                                     = error "Impossible!"
        where   expr0_type = typeof kt ctx env expr0
                bounded_expr0_kgen = bound ctx expr0_type
                bounded_expr0_type = (ComplexKlass bounded_expr0_kgen)
                ret_type = (ComplexKlass kGeneric)
                (KlassGeneric kGenericName _) = kGeneric
                (KlassGeneric bounded_name bounded_vars) = bounded_expr0_kgen
                ret_type_ok = isWellFormed kt ctx ret_type
                dcast_conds = (downcast kt kGenericName bounded_name) && ret_type_ok
                scast_conds = ret_type_ok && (not (isSubclassOf kt kGenericName bounded_name)) && (not (isSubclassOf kt bounded_name kGenericName))


-- Erasure
type_erasure :: Context -> KlassType -> KlassName
type_erasure ctx kType = name
        where   (KlassGeneric name _) = bound ctx kType


fieldsmax :: KlassTable -> KlassName -> M.Map FieldName KlassName
fieldsmax kt' kName' = res
        where   res = M.fromList (fieldsmax' kt' kName')
                fieldsmax' kt "Object" = []
                fieldsmax' kt kName = parent_fields ++ klass_fields
                        where   (Generic _ (KlassGeneric pName _) gFields gParams _) = get_generic kt kName
                                parent_fields = fieldsmax' kt pName
                                ctx = mk_context gParams
                                klass_fields = zip (map fst gFields) (map (type_erasure ctx . snd) gFields)


mtypemax :: KlassTable -> MethodName -> KlassName -> ([FJ.KlassName], FJ.KlassName)

mtypemax kt mName kName = if is_defined kt mName pName then mtypemax kt mName pName else (mArgs', mRetType')
        where   (Generic gName gParent _ gParams _) = get_generic kt kName
                (Method _ mArgs _ mRetType mCast) = get_method kt kName mName
                (KlassGeneric pName _) = gParent
                ctx = mk_context (gParams ++ mCast)
                mRetType' = type_erasure ctx mRetType
                mArgs' = map (type_erasure ctx) (map snd mArgs)


klass_expr_substitute :: M.Map ObjectName FJ.Expr -> FJ.Expr -> FJ.Expr
klass_expr_substitute mapper expr@(FJ.Arg x) = if x `elem` (M.keys mapper) then mapper ! x else expr
klass_expr_substitute mapper (FJ.FieldCall expr0 fName) = FJ.FieldCall (klass_expr_substitute mapper expr0) fName
klass_expr_substitute mapper (FJ.MethodCall expr0 mName es) = FJ.MethodCall expr0' mName es'
        where   expr0' = klass_expr_substitute mapper expr0
                es' = map (klass_expr_substitute mapper) es
klass_expr_substitute mapper (FJ.NewObject kName es) = FJ.NewObject kName (map (klass_expr_substitute mapper) es)
klass_expr_substitute mapper (FJ.TypeCast expr0 kType) = FJ.TypeCast (klass_expr_substitute mapper expr0) kType


erasure :: KlassTable -> Context -> Environment -> Expr -> FJ.Expr
erasure kt ctx env expr@(Arg x) = FJ.Arg x

erasure kt ctx env expr@(FieldCall expr0 fName)
        | field_type == expr_type       = expr'
        | otherwise                     = FJ.TypeCast expr' expr_type
        where   expr_type = type_erasure ctx (typeof kt ctx env expr)
                expr0_type = type_erasure ctx (typeof kt ctx env expr0)
                field_type = (fieldsmax kt expr0_type) ! fName
                expr0' = erasure kt ctx env expr0
                expr' = FJ.FieldCall expr0' fName

erasure kt ctx env expr@(MethodCall expr0 mName es kts)
        | return_type == expr_type      = expr'
        | otherwise                     = FJ.TypeCast expr' expr_type
        where   expr_type = type_erasure ctx (typeof kt ctx env expr)
                expr0_type = type_erasure ctx (typeof kt ctx env expr0)
                expr0' = erasure kt ctx env expr0
                es' = map (erasure kt ctx env) es
                (_,return_type) = mtypemax kt mName expr0_type
                expr' = FJ.MethodCall expr0' mName es'

erasure kt ctx env expr@(NewObject kGeneric es) = FJ.NewObject kType' es'
        where   es' = map (erasure kt ctx env) es
                kType' = type_erasure ctx (ComplexKlass kGeneric)

erasure kt ctx env expr@(TypeCast expr0 kGeneric) = FJ.TypeCast expr0' kType'
        where   expr0' = erasure kt ctx env expr0
                kType' = type_erasure ctx (ComplexKlass kGeneric)


compile_method :: KlassTable -> Generic -> Context -> Method -> FJ.Method
compile_method kt gen ctx m@(Method mName mArgs mExpr mRetType mCast) = m'
        where   (Generic gName _ _ _ _) = gen
                (arg_types, mRetType') = mtypemax kt mName gName
                new_ctx = join_context ctx (mk_context mCast)
                env = M.fromList $ ("this", ComplexKlass (KlassGeneric gName (map mk_SimpleClass (M.keys ctx)))):mArgs
                var_map_fun (d,(x,t)) = if d == t then (x, FJ.Arg x) else (x, FJ.TypeCast (FJ.Arg x) t)
                var_mapper = M.fromList $ map var_map_fun (zip arg_types (map (\(x,t) -> (x, type_erasure new_ctx t)) mArgs))
                mExpr' = klass_expr_substitute var_mapper (erasure kt ctx env mExpr)
                mArgs' = zip (map fst mArgs) arg_types
                m' = FJ.Method mName mArgs' mExpr' mRetType'


compile_class :: KlassTable -> Generic -> FJ.Klass
compile_class kt gen@(Generic gName (KlassGeneric pName kts) gFields gParams gMethods) = klass
        where   ctx = mk_context gParams
                kFields = map (\(n,k) -> (n, type_erasure ctx k)) gFields
                kMethods = M.map (compile_method kt gen ctx) gMethods
                klass = FJ.Klass gName pName kFields kMethods

-- Examples
object_generic = KlassGeneric "Object" []
example_kt_pair = M.fromList [a_generic, b_generic, pair_generic]
        where   a_generic = ("A", Generic "A" object_generic [] [] M.empty)
                b_generic = ("B", Generic "B" object_generic [] [] M.empty)
                pair_fields = [("fst", SimpleKlass "X"), ("snd", SimpleKlass "Y")]
                pair_params = [("X", object_generic), ("Y", object_generic)]
                setfst_args = [("newfst", SimpleKlass "Z")]
                setfst_expr = NewObject (KlassGeneric "Pair" [SimpleKlass "Z", SimpleKlass "Y"]) [Arg "newfst", FieldCall (Arg "this") "snd"]
                setfst_ret_type = ComplexKlass (KlassGeneric "Pair" [SimpleKlass "Z", SimpleKlass "Y"])
                setfst_cast = [("Z", object_generic)]
                setfst_method = Method "setfst" setfst_args setfst_expr setfst_ret_type setfst_cast
                pair_methods = M.fromList [("setfst", setfst_method)]
                pair_generic = ("Pair", Generic "Pair" object_generic pair_fields pair_params pair_methods)

example_kt_list = M.fromList [a_generic, b_generic, c_generic, list_generic, empty_generic, node_generic]
        where   a_generic = ("A", Generic "A" object_generic [] [] M.empty)
                b_generic = ("B", Generic "B" object_generic [] [] M.empty)
                c_generic = ("C", Generic "C" object_generic [] [] M.empty)
                prepend_method = Method "prepend" [("new_val", ComplexKlass object_generic)] (NewObject (KlassGeneric "Node" []) [Arg "new_val", Arg "this"]) (ComplexKlass (KlassGeneric "Node" [])) []
                node_generic = ("Node", Generic "Node" (KlassGeneric "List" []) [("value", ComplexKlass (KlassGeneric "Object" [])), ("tail", ComplexKlass (KlassGeneric "List" []))] [] M.empty)
                empty_generic = ("EmptyList", Generic "EmptyList" (KlassGeneric "List" []) [] [] M.empty)
                list_generic = ("List", Generic "List" object_generic [] [] (M.fromList [("prepend", prepend_method)]))

example1 = (example_kt_pair, expr)
        where   expr = MethodCall (NewObject (KlassGeneric "Pair" [ComplexKlass (KlassGeneric "A" []), ComplexKlass (KlassGeneric "B" [])]) [NewObject (KlassGeneric "A" []) [],NewObject (KlassGeneric "B" []) []]) "setfst" [NewObject (KlassGeneric "B" []) []] [ComplexKlass (KlassGeneric "B" [])]

example2 = (example_kt_list, expr4)
        where   expr1 = NewObject (KlassGeneric "EmptyList" []) []
                expr2 = MethodCall expr1 "prepend" [NewObject (KlassGeneric "A" []) []] []
                expr3 = MethodCall expr2 "prepend" [NewObject (KlassGeneric "B" []) []] []
                expr4 = MethodCall expr3 "prepend" [NewObject (KlassGeneric "A" []) []] []


compile :: Program -> FJ.Program
compile prog@(kt, expr) = (kt', expr')
        where   kt' = M.map (compile_class kt) kt
                expr' = erasure kt M.empty M.empty expr

main = do
        args <- getArgs
        let example = if (length args >= 1) && (args !! 0 == "pair") then example1 else example2
        let ast@(kt, expr) = compile example
        putStrLn$ "=== AST: ===\n" ++ (concatMap (\(n,k) -> "Class `"++n++"`: "++(show k)++"\n") (M.toList kt)) ++ "===========\n"
        FJ.run ast

