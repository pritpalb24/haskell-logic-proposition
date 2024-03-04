import qualified Data.Map.Strict as Map
import System.Environment (getArgs)

type VarId = String
type VarAsgn = Map.Map VarId Bool

-- represents a constant value
-- either true or false
data Prop = Const Bool
    | Var VarId
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Imply Prop Prop
    | Iff Prop Prop
    deriving (Eq, Read, Show)

-- finds distinct var names in a prop formula and returns a list of them
findVarIds :: Prop -> [VarId]
findVarIds (Const _) = [] -- if prop is const, return empty list
findVarIds (Var x) = [x] -- if prop is var, return a list with that varId
findVarIds (Not p) = findVarIds p -- if prop is not, recursively find varIds
-- for the last four, recursively find varIds from both
findVarIds (And p q) = findVarIds p ++ findVarIds q
findVarIds (Or p q) = findVarIds p ++ findVarIds q
findVarIds (Imply p q) = findVarIds p ++ findVarIds q
findVarIds (Iff p q) = findVarIds p ++ findVarIds q

-- generates all possible var assignments
genVarAsgns :: [VarId] -> [VarAsgn]
genVarAsgns [] = [Map.empty] -- empty list returns an empty map
-- fold over the list of VarIds
-- for each x create all possibilities
-- for each assign created, insert x with both Bool values
genVarAsgns xs = foldr(\x -> concatMap(\ assign -> [Map.insert x b assign | b <- [True, False]]))[Map.empty] xs

-- evalutes the prop formula with a var assignment and returns a Bool
eval :: Prop -> VarAsgn -> Bool
eval (Var x) asgns = case Map.lookup x asgns of -- searches for the value of the variable
    Just b -> b -- if found, return the value
    Nothing -> error "Var not found" --else, throw an error
eval (Const b) _ = b -- if prop is const, return the value
eval (Not p) asgns = not $ eval p asgns -- if prop is Not, evaluate and negate the result
eval (And p q) asgns = eval p asgns && eval q asgns -- if prop is And, evaluate p and q 
eval (Or p q) asgns = eval p asgns || eval q asgns -- if prop is Or, evaluate p or q
eval (Imply p q) asgns = not (eval p asgns) || eval q asgns -- if prop is Imply, evaluate not p or q
eval (Iff p q) asgns = eval p asgns == eval q asgns -- if prop is Iff, p and q should be equal

--takes in a formula and returns the truth value
sat :: Prop -> Bool
-- any searches for a True value
-- evaluates the formula from all possible Bool values
-- returns true if formula is satifiable or else returns false 
sat formula = any (eval formula) (genVarAsgns $ findVarIds formula)

-- turns String into proposition
readFormula :: String -> Prop
readFormula = read

-- takes in a String and returns depending on the sat function
-- if true returns "SAT", else returns "UNSAT"
checkFormula :: String -> String
checkFormula formulaString = if sat (readFormula formulaString) then "SAT" else "UNSAT"


main :: IO ()
main = do
    args <- getArgs -- get filename from args
    case args of
        [filePath] -> do
            contents <- readFile filePath -- read contents
            let formulas = lines contents --split formulas
            mapM_ (putStrLn . checkFormula) formulas -- check each one and print result
        _ -> putStrLn "Filename required" -- if not filename, ask for filename  


