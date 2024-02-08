import Data.List
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import System.Environment (getArgs)

type WorkflowName = String
data Operator = LESS | GREATER deriving (Eq, Show)
data Goto = GotoName WorkflowName | ACCEPT | REJECT deriving (Eq, Show)
data Rule = Rule Char Operator Int Goto deriving (Eq, Show)
data Workflow = Workflow WorkflowName [Rule] Goto deriving (Eq, Show)
data Part = Part { x :: Int, m :: Int, a :: Int, s :: Int } deriving Show

main = do
    args <- getArgs
    input <- readFile(args !! 0)
    let (workflows, parts) = parse_input(lines input)
    let ns = map (\(Workflow n rs g) -> n) workflows
    let w_map = Map.fromList (zip ns workflows)

    let answer1 = process_parts w_map parts
    print(answer1)

    let ans2 = answer2 w_map
    print(ans2)

type Range = (Int, Int)
data Ways = Ways { rx :: Range, rm :: Range, ra :: Range, rs :: Range }

num_ways :: Ways -> Int
num_ways ways =
    num_range (rx ways)
    * num_range (rm ways)
    * num_range (ra ways)
    * num_range (rs ways)

num_range :: Range -> Int
num_range (min, max) = maximum [max - min + 1, 0]

answer2 w_map = count_ways w_map start_ways "in"
    where
        full_range = (1, 4000)
        start_ways = Ways { rx = full_range, rm = full_range, ra = full_range, rs = full_range }

count_ways :: Map.Map WorkflowName Workflow -> Ways -> WorkflowName -> Int
count_ways w_map cur_ways w_name = count_ways_rule 0 w_map cur_ways default_goto rules
    where
        Workflow _ rules default_goto = fromJust (Map.lookup w_name w_map)
        
count_ways_rule acc w_map cur_ways def_goto [] = case def_goto of
    ACCEPT -> acc + num_ways cur_ways
    REJECT -> acc
    GotoName name -> acc + (count_ways w_map cur_ways name)
count_ways_rule acc w_map cur_ways def_goto (rule : rules) = case goto of
    ACCEPT -> count_ways_rule (acc + num_ways adj_ways) w_map inv_ways def_goto rules
    REJECT -> count_ways_rule acc w_map inv_ways def_goto rules
    GotoName name -> (count_ways w_map adj_ways name) + (count_ways_rule acc w_map inv_ways def_goto rules)
    where
        Rule _ _ _ goto = rule
        adj_ways = adjust_rule cur_ways rule False
        inv_ways = adjust_rule cur_ways rule True

adjust_rule ways (Rule c op n _) invert
    | c == 'x' = ways { rx = adjust_range (rx ways) op n invert }
    | c == 'm' = ways { rm = adjust_range (rm ways) op n invert }
    | c == 'a' = ways { ra = adjust_range (ra ways) op n invert }
    | c == 's' = ways { rs = adjust_range (rs ways) op n invert }

adjust_range :: Range -> Operator -> Int -> Bool -> Range
adjust_range (min, max) operator num invert
    | (operator == LESS) && (not invert) = (min, minimum [max, num - 1])
    | (operator == GREATER) && (not invert) = (maximum [min, num + 1], max)
    | (operator == LESS) && invert = (maximum [min, num], max)
    | (operator == GREATER) && invert = (min, minimum [max, num])

process_parts w_map = foldl (\s p -> s + process_part w_map p "in") 0 

process_part :: Map.Map WorkflowName Workflow -> Part -> WorkflowName -> Int
process_part w_map part w_name = case w_next of
    GotoName next_name -> process_part w_map part next_name
    ACCEPT -> x part + m part + a part + s part
    REJECT -> 0
    where
        workflow = fromJust (Map.lookup w_name w_map)
        w_next = apply_workflow part workflow

apply_workflow :: Part -> Workflow -> Goto
apply_workflow part (Workflow _ rules default_goto) = case matched of
    Just (Just goto) -> goto
    Nothing -> default_goto
    where
        matched = find isJust (map (apply_rule part) rules)

apply_rule :: Part -> Rule -> Maybe Goto
apply_rule part (Rule r_char r_op r_num r_goto) =
    if predicate then Just r_goto else Nothing
    where
        p_num
            | r_char == 'x' = x part
            | r_char == 'm' = m part
            | r_char == 'a' = a part
            | r_char == 's' = s part
        predicate 
            | r_op == LESS = p_num < r_num
            | r_op == GREATER = p_num > r_num

-- Parse input.

parse_input strs = (workflows, parts)
    where
        (wstrs, pstrs) = parse_segments strs
        parts = map parse_part pstrs
        workflows = map parse_workflow wstrs

parse_workflow :: String -> Workflow
parse_workflow wstr = Workflow name rules default_goto
    where
        (name, s1) = split '{' wstr
        (s2, _) = split '}' s1
        strs = splitAll ',' s2
        default_goto = parse_goto (last strs)
        rules = map parse_rule (init strs)

parse_rule :: String -> Rule
parse_rule rstr = Rule r_char r_operator r_int r_goto
    where
        (pred_str, goto_s) = split ':' rstr
        r_char = pred_str !! 0
        op_char = pred_str !! 1
        r_operator
            | op_char == '<' = LESS
            | op_char == '>' = GREATER
        r_int = read (drop 2 pred_str) :: Int
        r_goto = parse_goto goto_s

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

parse_segments strs = (workflows, parts)
    where
        (blank_index, _) = (filter (\(n, s) -> s == "") (zip [0..] strs)) !! 0
        workflows = take blank_index strs
        parts = drop (blank_index + 1) strs

split :: Char -> String -> (String, String)
split ch s = (lhs, drop 1 rhs) where (lhs, rhs) = break (== ch) s

splitAll :: Char -> String -> [String]
splitAll ch "" = []
splitAll ch s = lhs : (splitAll ch rhs) where (lhs, rhs) = split ch s

removeChar :: Char -> String -> String
removeChar rem str = filter (rem /=) str
