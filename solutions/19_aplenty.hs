import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe, isJust)
import System.Environment (getArgs)

-- Workflows.

type WorkflowName = String

data Operator = LESS | GREATER deriving (Eq, Show)

data Goto = GotoName WorkflowName | ACCEPT | REJECT deriving (Eq, Show)

data Rule = Rule Char Operator Int Goto deriving (Eq, Show)

data Workflow = Workflow WorkflowName [Rule] Goto deriving (Eq, Show)

type WorkflowGetter = (WorkflowName -> Workflow)

get_workflow_named :: Map.Map WorkflowName Workflow -> WorkflowGetter
get_workflow_named workflow_map name = fromJust (Map.lookup name workflow_map)

-- Parts.

data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving Show

get_property :: Char -> Part -> Int
get_property property_char part
    | property_char == 'x' = x part
    | property_char == 'm' = m part
    | property_char == 'a' = a part
    | property_char == 's' = s part

-- Constraints.

type Constraint = (Int, Int)

data Constraints = Constraints 
    { cx :: Constraint
    , cm :: Constraint
    , ca :: Constraint
    , cs :: Constraint }

combinations :: Constraints -> Int
combinations ways =
    combs (cx ways) * combs (cm ways) * combs (ca ways) * combs (cs ways)
    where
        combs (min, max) = maximum [max - min + 1, 0]

-- Main.

main :: IO ()
main = do
    args <- getArgs
    input <- readFile(args !! 0)
    let (workflow_map, parts) = parse_input(lines input)

    let a1 = answer1 (get_workflow_named workflow_map) parts
    print(a1)

    let a2 = answer2 (get_workflow_named workflow_map)
    print(a2)

-- Answer 1.

-- Process each part by evaluating each rule in the workflow to determine
-- whether it results in ACCEPT (in which case the part contributes the
-- sum of its properties to the total), REJECT (which contributes nothing),
-- or the name of the next workflow (in which case continue processing the
-- part using that workflow).

answer1 :: WorkflowGetter -> [Part] -> Int
answer1 get_workflow = foldl (\s p -> s + process_part get_workflow "in" p) 0 

process_part :: WorkflowGetter -> WorkflowName -> Part -> Int
process_part get_workflow name part = case next_goto of
    ACCEPT -> x part + m part + a part + s part
    REJECT -> 0
    GotoName next -> process_part get_workflow next part
    where
        Workflow _ rules default_goto = get_workflow name
        matched_goto = find isJust (map (eval_rule part) rules)
        next_goto = fromMaybe default_goto (fromMaybe Nothing matched_goto)

eval_rule :: Part -> Rule -> Maybe Goto
eval_rule part (Rule rule_char r_op rule_constant rule_goto) =
    if predicate then Just rule_goto else Nothing
    where
        part_property = get_property rule_char part
        predicate 
            | r_op == LESS = part_property < rule_constant
            | r_op == GREATER = part_property > rule_constant

-- Answer 2.

-- The gist here is that you evaluate each successive rule in the initial
-- "in" workflow, descending recursively into any child workflows that are
-- invoked by its gotos and so on. The predicate of each rule constrains its 
-- child gotos, and the inverse of the rule constrains the next peer rule that
-- is evaluated which excludes duplicates. Upon reaching an ACCEPT state we 
-- evaluate how many combinations remain inside the constraints and contribute 
-- these to the total, REJECT counts nothing.

-- Note: The workflow graph is a tree. The property values of a part do not
-- change, so any cycle (if formed) would be infinite which is contrary to
-- the fact that all parts are either accepted or rejected.

answer2 :: WorkflowGetter -> Int
answer2 get_workflow = count_combs get_workflow initial_constraints "in"
    where
        unconstrained = (1, 4000)
        initial_constraints = Constraints 
            { cx = unconstrained
            , cm = unconstrained
            , ca = unconstrained
            , cs = unconstrained }

count_combs :: WorkflowGetter -> Constraints -> WorkflowName -> Int
count_combs get_workflow cur_ways w_name = 
    count_rule 0 get_workflow cur_ways default_goto rules
    where
        Workflow _ rules default_goto = get_workflow w_name

count_rule :: Int -> WorkflowGetter -> Constraints -> Goto -> [Rule] -> Int

count_rule acc get_workflow constraints default_goto [] = 
    case default_goto of
        ACCEPT -> acc + combinations constraints
        REJECT -> acc
        GotoName name -> acc + (count_combs get_workflow constraints name)

count_rule acc get_workflow constraints default_goto (rule : rules) = 
    case goto of
        GotoName name -> sub_combs name + remaining rules
        _ -> remaining rules
    where
        Rule _ _ _ goto = rule
        adj_constraints = adjust_rule constraints rule False
        inv_constraints = adjust_rule constraints rule True
        acc' = case goto of
            ACCEPT -> acc + combinations adj_constraints
            _ -> acc
        sub_combs = count_combs get_workflow adj_constraints
        remaining = count_rule acc' get_workflow inv_constraints default_goto

adjust_rule :: Constraints -> Rule -> Bool -> Constraints
adjust_rule constraints (Rule c op n _) invert
    | c == 'x' = constraints { cx = narrow (cx constraints) op n invert }
    | c == 'm' = constraints { cm = narrow (cm constraints) op n invert }
    | c == 'a' = constraints { ca = narrow (ca constraints) op n invert }
    | c == 's' = constraints { cs = narrow (cs constraints) op n invert }

narrow :: Constraint -> Operator -> Int -> Bool -> Constraint
narrow (min, max) operator num invert
    | (operator == LESS) && (not invert) = (min, minimum [max, num - 1])
    | (operator == GREATER) && (not invert) = (maximum [min, num + 1], max)
    | (operator == LESS) && invert = (maximum [min, num], max)
    | (operator == GREATER) && invert = (min, minimum [max, num])

-- Parse input.

parse_input :: [String] -> (Map.Map WorkflowName Workflow, [Part])
parse_input strs = (workflow_map, parts)
    where
        (wstrs, pstrs) = parse_segments strs
        parts = map parse_part pstrs
        workflows = map parse_workflow wstrs
        names = map (\(Workflow n cs g) -> n) workflows
        workflow_map = Map.fromList (zip names workflows)

parse_segments :: [String] -> ([String], [String])
parse_segments strs = (workflows, parts)
    where
        (blank_index, _) = (filter (\(n, s) -> s == "") (zip [0..] strs)) !! 0
        workflows = take blank_index strs
        parts = drop (blank_index + 1) strs

parse_workflow :: String -> Workflow
parse_workflow wstr = Workflow name rules default_goto
    where
        (name, s1) = split '{' wstr
        (s2, _) = split '}' s1
        strs = splitAll ',' s2
        default_goto = parse_goto (last strs)
        rules = map parse_rule (init strs)

parse_rule :: String -> Rule
parse_rule rstr = Rule rule_char rule_operator rule_int rule_goto
    where
        (predicate_str, goto_s) = split ':' rstr
        rule_char = predicate_str !! 0
        op_char = predicate_str !! 1
        rule_operator
            | op_char == '<' = LESS
            | op_char == '>' = GREATER
        rule_int = read (drop 2 predicate_str) :: Int
        rule_goto = parse_goto goto_s

parse_goto :: String -> Goto
parse_goto s
    | s == "A" = ACCEPT
    | s == "R" = REJECT
    | otherwise = GotoName s

parse_part :: String -> Part
parse_part pstr = Part { x = x, m = m, a = a, s = s }
    where
        e1 = splitAll ',' (filter (\c -> not (elem c ['{', '}'])) pstr) 
        e2 = map (\e -> read (snd (split '=' e)) :: Int) e1
        (x, m, a, s) = (e2 !! 0, e2 !! 1, e2 !! 2, e2 !! 3)

split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s

splitAll :: Char -> String -> [String]
splitAll ch "" = []
splitAll ch s = lhs : (splitAll ch rhs) where (lhs, rhs) = split ch s
