import qualified Language.ECMAScript3.Parser as Parser
import Language.ECMAScript3.Syntax
import Control.Monad
import Control.Applicative
import Data.Map as Map (Map, insert, lookup, union, toList, empty, intersection, difference, filter, delete)
import Debug.Trace
import Value
import Kind

--
-- Evaluate functions
--

evalExpr :: StateT -> Expression -> StateTransformer Value
evalExpr env (VarRef (Id id)) = do
    val <-  stateLookup env id
    case val of
        Nil -> error $ "Variable " ++ show id ++ " not defiend." -- Cuidando do caso em que a variavel não foi definida previamente
        _ -> return val
evalExpr env (IntLit int) = return $ Int int
evalExpr env (StringLit word) = return $ String word
evalExpr env (BoolLit bool) = return $ Bool bool
evalExpr env (InfixExpr op expr1 expr2) = do
    v1 <- evalExpr env expr1
    v2 <- evalExpr env expr2
    infixOp env op v1 v2
evalExpr env (AssignExpr OpAssign (LVar var) expr) = do
    e <- stateLookup env var
    val <- evalExpr env expr
    case e of
        Nil ->
            setVar var (val, Global) -- Adicionamos nova variavel como Local
        _ ->
            setValue var val -- Nesse Caso apenas mudamos o valor da variavel e não o seu tipo
--
-- List Functions
--
evalExpr env (CallExpr (DotRef (VarRef expr) (Id function)) refList) = do
    list <- evalExpr env (VarRef expr)
    case function of
        "head" -> return (head (getList list))
        "tail" -> return (List $ tail (getList list))
        "concat" -> do 
                    first <- evalExpr env (VarRef expr)
                    second <- evalExpr env (head refList)
                    return (List ((getList first) ++ (getList second)))
        _ -> error "Função não definida"

-- Chamando um função já definida
evalExpr env (CallExpr expr args) = ST $ \s ->
    let (ST a) = return Nil
        (t, newS) = a s
        (ST g) = do
            val <- evalExpr env expr
            evalFunctionArgs env (getIds val) args
            returnStmt <- evalStmt env (BlockStmt (getStatements val))
            clearFunctionArgs env (getIds val)
            return returnStmt
        (resp,ign) = g newS
        fEnv = update ign s
        in (resp,fEnv)

------------------------------------------------------------------------------------
-------------------------------  Lista ---------------------------------------------
evalExpr env (ArrayLit []) = return $ (List [])
evalExpr env (ArrayLit list) = do
    a <- mapM (evalExpr env) list
    return $ (List a) 


evalStmt :: StateT -> Statement -> StateTransformer Value
evalStmt env EmptyStmt = return Nil
evalStmt env (VarDeclStmt []) = return Nil
evalStmt env (VarDeclStmt (decl:ds)) =
    varDecl env decl >> evalStmt env (VarDeclStmt ds)
evalStmt env (ExprStmt expr) = evalExpr env expr
evalStmt env (BreakStmt tipo) = return Break
evalStmt env (ReturnStmt Nothing) = return Nil
evalStmt env (ReturnStmt (Just expr)) = evalExpr env expr
-- If statements
evalStmt env (IfSingleStmt expr ifStmt) = evalStmt env (IfStmt expr ifStmt EmptyStmt) -- Chama a função que já resolve para o if com else
evalStmt env (IfStmt expr ifStmt elseStmt) = do
    val <- evalExpr env expr
    case val of
        (Bool bool) -> if bool then
                            evalStmt env ifStmt 
                       else 
                            evalStmt env elseStmt
        Nil -> return val
-- BlockStatements
evalStmt env (BlockStmt (a:[])) = evalStmt env a
evalStmt env (BlockStmt (a:as)) = 
    case a of
        (BreakStmt _) -> evalStmt env a
        (ReturnStmt _) -> evalStmt env a
        _ -> do evalStmt env a
                evalStmt env (BlockStmt as)
-----------------------
-- Laços While e For --
-----------------------
evalStmt env (WhileStmt expr whileStmt) = do
    val <- evalExpr env expr
    case val of
        (Bool bool) -> if bool then do
                            isBreak <-  evalStmt env whileStmt
                            case isBreak of
                                Break -> return Nil
                                _ -> evalStmt env (WhileStmt expr whileStmt)
                       else return Nil
evalStmt env (ForStmt start expr incr forStmt) = do
    evalForInit env start
    case expr of
        (Just a) -> do
            val <- evalExpr env a
            case val of
                (Bool bool) -> if bool then do
                                    isBreak <- evalStmt env forStmt 
                                    case isBreak of
                                        Break -> return Nil
                                        _ -> case incr of 
                                                (Just a) -> do evalExpr env a
                                                               evalStmt env (ForStmt NoInit expr incr forStmt)
                                                (Nothing) -> evalStmt env (ForStmt NoInit expr incr forStmt)
                                else evalStmt env EmptyStmt
        (Nothing) -> do
            evalStmt env (ForStmt start (Just (BoolLit True)) incr forStmt)
-----------------------
--      Fuction      --
-----------------------
evalStmt env (FunctionStmt (Id id) args functionBlock) = do
    val <-  stateLookup env id
    case val of 
        Nil -> setVar id ((Function (Id id) args functionBlock),Global)
        _ -> error $ "Variable " ++ show id ++ " already defiend."



-- Do not touch this one :)
evaluate :: StateT -> [Statement] -> StateTransformer Value
evaluate env [] = return Nil
evaluate env stmts = foldl1 (>>) $ map (evalStmt env) stmts




--
-- Operators
--

infixOp :: StateT -> InfixOp -> Value -> Value -> StateTransformer Value
infixOp env OpAdd  (Int  v1) (Int  v2) = return $ Int  $ v1 + v2
infixOp env OpSub  (Int  v1) (Int  v2) = return $ Int  $ v1 - v2
infixOp env OpMul  (Int  v1) (Int  v2) = return $ Int  $ v1 * v2
infixOp env OpDiv  (Int  v1) (Int  v2) = return $ Int  $ div v1 v2
infixOp env OpMod  (Int  v1) (Int  v2) = return $ Int  $ mod v1 v2
infixOp env OpLT   (Int  v1) (Int  v2) = return $ Bool $ v1 < v2
infixOp env OpLEq  (Int  v1) (Int  v2) = return $ Bool $ v1 <= v2
infixOp env OpGT   (Int  v1) (Int  v2) = return $ Bool $ v1 > v2
infixOp env OpGEq  (Int  v1) (Int  v2) = return $ Bool $ v1 >= v2
infixOp env OpEq   (Int  v1) (Int  v2) = return $ Bool $ v1 == v2
infixOp env OpEq   (Bool v1) (Bool v2) = return $ Bool $ v1 == v2
infixOp env OpNEq  (Bool v1) (Bool v2) = return $ Bool $ v1 /= v2
infixOp env OpLAnd (Bool v1) (Bool v2) = return $ Bool $ v1 && v2
infixOp env OpLOr  (Bool v1) (Bool v2) = return $ Bool $ v1 || v2

--
-- Environment and auxiliary functions
--

environment :: Map String (Value,Kind)
environment = Map.empty

stateLookup :: StateT -> String -> StateTransformer Value
stateLookup env var = ST $ \s ->
    -- this way the error won't be skipped by lazy evaluation
    case Map.lookup var (union s env) of
        Nothing -> (Nil,s)
        Just (val,kind) -> (val, s)

varDecl :: StateT -> VarDecl -> StateTransformer Value
varDecl env (VarDecl (Id id) maybeExpr) = do
    case maybeExpr of
        Nothing -> setVar id (Nil,Nulo)
        (Just expr) -> do
            val <- evalExpr env expr
            setVar id (val, Local)

setVar :: String -> (Value,Kind) -> StateTransformer Value
setVar var (val,kind) = ST $ \s -> (val, insert var (val, kind) s)

setValue :: String -> Value -> StateTransformer Value
setValue var val = ST $ \s -> (val, insert var (val, helper s var) s)

deleteVar :: String  -> StateTransformer Value
deleteVar var = ST $ \s -> (Nil, Map.delete var s)

helper :: StateT -> String -> Kind
helper env var = case Map.lookup var env of
                    Nothing -> Nulo
                    Just (value,tipo) -> tipo

--
-- Inicializa os argumentos das funções
--
evalFunctionArgs :: StateT ->  [Id] ->  [Expression] -> StateTransformer Value
evalFunctionArgs env ((Id a):[]) (b:[]) = do
    val <- evalExpr env b
    setVar a (val, Local)
evalFunctionArgs env ((Id a):as) (b:bs) = do
        val <- evalExpr env b
        setVar a (val, Local)
        evalFunctionArgs env as bs
evalFunctionArgs _ _ _ = error "Número de argumentos errados"

clearFunctionArgs :: StateT -> [Id] ->  StateTransformer Value
clearFunctionArgs env []     = return Nil
clearFunctionArgs env ((Id a):[]) = deleteVar  a
clearFunctionArgs env ((Id a):as) = do
    deleteVar  a
    clearFunctionArgs env as

-- Atualiza estado, dependendo das novos valores de variaveis
-- e novas variaveis globais
update :: StateT -> StateT -> StateT
update new old = newGlobal
               where disjuction = Map.difference old new
                     disjuction2 = Map.difference new old
                     upda  = Map.intersection new old
                     oldGlobal = Map.filter (\(val,kind) -> if kind == Global then True else False) disjuction2
                     newGlobal = Map.union disjuction (Map.union oldGlobal upda)

--
-- Evaluate the first expression in a for loop
--
evalForInit env (NoInit) = return Nil
evalForInit env (VarInit var) = (evalStmt env (VarDeclStmt var))    
evalForInit env (ExprInit expr) = evalExpr env expr
                       
--
-- Types and boilerplate
--

type StateT = Map String (Value,Kind)
data StateTransformer t = ST (StateT -> (t, StateT))

instance Monad StateTransformer where
    return x = ST $ \s -> (x, s)
    (>>=) (ST m) f = ST $ \s ->
        let (v, newS) = m s
            (ST resF) = f v
        in resF newS

instance Functor StateTransformer where
    fmap = liftM

instance Applicative StateTransformer where
    pure = return
    (<*>) = ap

--
-- Main and results functions
--
showEnviroment :: [(String,(Value,Kind))] -> [(String,Value)]
showEnviroment [] = []
showEnviroment ((var, (val,kind)):as) = (var, val) : showEnviroment as

showResult :: (Value, StateT) -> String
showResult (val, defs) =
    show val ++ "\n" ++ show (showEnviroment.toList $ union defs environment) ++ "\n"

getResult :: StateTransformer Value -> (Value, StateT)
getResult (ST f) = f Map.empty

main :: IO ()
main = do
    js <- Parser.parseFromFile "Main.js"
    let statements = unJavaScript js
    putStrLn $ "AST: " ++ (show $ statements) ++ "\n"
    putStr $ showResult $ getResult $ evaluate environment statements
