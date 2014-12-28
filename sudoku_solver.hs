import System.Environment
import Control.Monad
import System.IO
import Data.Char
import Control.Concurrent
import Data.IORef
import Control.Concurrent.MVar
import Data.List
import Data.Time

data Unit = Unit { position:: (Int,Int),
                   possible_values:: Maybe [Int],
                   actual_value::Maybe Int,
                   square_no::Int}
            deriving(Show)
main = do
     start <- getCurrentTime
     putStrLn "Enter the name of the file"
     fileName <- getLine 
     s<-readFile fileName
     let xs = (zipWith add_value (create_tuples) (map (read.(:"")) (take 81 (read_csv s))::[Int])) in
        do 
          r <- recursive_search_module (eliminate_possible_values xs xs)
          print_answer (capture_actual_value r)
          stop <-getCurrentTime
          print $ diffUTCTime stop start

--we qualify each square on the 9x9 grid as a unit
--create 81 units for the grid
--Below modules have been used to generate the position, possible values and the square no for each unit on the grid 
create_tuples::[Unit]
create_tuples = map generate_unit [(x,y)|x<-[1..9],y<-[1..9]]

generate_unit::(Int,Int) -> Unit
generate_unit (a,b) = Unit {position = (a,b), possible_values = Just [1..9],actual_value = Nothing,square_no = allocate a b}

allocate::Int->Int->Int
allocate x y = if (x>=1&&x<=3) && (y>=1&&y<=3) then 1
               else if (x>=1&&x<=3) && (y>=4&&y<=6) then 2
               else if (x>=1&&x<=3) && (y>=7&&y<=9) then 3
               else if (x>=4&&x<=6) && (y>=1&&y<=3) then 4
               else if (x>=4&&x<=6) && (y>=4&&y<=6) then 5
               else if (x>=4&&x<=6) && (y>=7&&y<=9) then 6
               else if (x>=7&&x<=9) && (y>=1&&y<=3) then 7
               else if (x>=7&&x<=9) && (y>=4&&y<=6) then 8
               else 9

--The below modules have been created to add values that were read from the CSV file to the 81 units on the grid 
add_value::Unit->Int->Unit
add_value u 0 = Unit { position = position u,
                       possible_values = possible_values u,
                       actual_value = Nothing, 
                       square_no = square_no u}

add_value u x = Unit { position = position u,
                       possible_values = Nothing,
                       actual_value = Just x,
                       square_no = square_no u}

--remove is used to a value from a list of integers
remove::[Int]->Int->[Int]
remove (x:xs) y = if x==y then remove xs y else (x:remove xs y)
remove xs _ = []

my_maybe::[Int]-> Maybe [Int]
my_maybe (x:xs) = Just (x:xs)
my_maybe [] = Nothing

--Checks all the units with the given column number and deletes the given value from the possible values of those units
check_value_column::[Unit]->Int->Int->[Unit]
check_value_column (x:xs) y1 v = delete_values_from_column x y1 v : check_value_column xs y1 v
check_value_column [] _ _ = []

delete_values_from_column::Unit->Int->Int->Unit
delete_values_from_column u y v = Unit { position = position u,
                          possible_values = let (a,b) = (position u) in
                                            if (b==y) then
                                              case (possible_values u) of
                                                Just xs -> my_maybe (remove xs v)
                                                Nothing -> Nothing
                                            else 
                                              possible_values u,
                          actual_value = actual_value u,
                          square_no = square_no u}

--Checks all the units with the given row number and deletes the given value from the possible values of those units
check_value_row::[Unit]->Int->Int->[Unit]
check_value_row (x:xs) x1 v = delete_values_from_row x x1 v : check_value_row xs x1 v
check_value_row [] _ _ = []

delete_values_from_row::Unit->Int->Int->Unit
delete_values_from_row u x v = Unit { position = position u,
                          possible_values = let (a,b) = position u in
                                            if (a==x) then
                                            case possible_values u of
                                              Just xs -> my_maybe(remove xs v)
                                              Nothing -> Nothing
                                            else
                                              possible_values u,
                          actual_value = actual_value u,
                          square_no = square_no u}

--Checks all the units with the square number and deletes the given value from the possible values of those units
check_value_in_square::[Unit]->Int->Int->[Unit]
check_value_in_square (x:xs) n v = delete_values_from_sq x n v : check_value_in_square xs n v
check_value_in_square [] _ _ = []

delete_values_from_sq::Unit->Int->Int->Unit
delete_values_from_sq u n v = Unit { position = position u,
                       possible_values = let x = square_no u in
                                         if x == n then
                                         case possible_values u of 
                                           Just xs -> my_maybe (remove xs v)
                                           Nothing -> Nothing
                                         else
                                           possible_values u,
                       actual_value = actual_value u,
                       square_no = square_no u}

--
eliminate_possible_values::[Unit]->[Unit]->[Unit]
eliminate_possible_values (x:xs) ys = eliminate_possible_values xs (reset_grid_if_actual_value x ys)
eliminate_possible_values [] ys = ys

reset_grid_if_actual_value::Unit->[Unit]->[Unit]
reset_grid_if_actual_value u xs = let a = actual_value u in
           case a of 
             Just b -> let (x1,y1) = position u in
                        let s = square_no u in
                          (check_value_in_square (check_value_row (check_value_column xs y1 b) x1 b) s b)
             Nothing -> xs 

replace_all_single_values::[Unit]->[Unit]
replace_all_single_values (x:xs) = single_value x : replace_all_single_values xs
replace_all_single_values [] = []

single_value::Unit->Unit
single_value u = Unit{ position = position u,
                       possible_values = case possible_values u of
                                        Just xs -> if (length xs) == 1 then Nothing else Just xs
                                        Nothing -> Nothing,
                       actual_value = case possible_values u of 
                                        Just xs -> if (length xs) == 1 then Just (head xs) else actual_value u
                                        Nothing -> actual_value u,
                       square_no = square_no u}

change_existing::Unit->[Unit]->[Unit]
change_existing a (xs) = map (change a) xs
change u x =  let (x1,y1) = (position u) in 
	       let (x2,y2) = (position x) in 
                 if (x1==x2)&&(y1==y2) then 
                   Unit{ position = position u,
	                 possible_values = possible_values u,
		         actual_value = actual_value u ,
		         square_no = square_no u}
         	 else x


check_for_single_value::Unit->(Unit,Bool)
check_for_single_value u = case possible_values u of
			    Just xs -> if (length xs) == 1 then (single_value u,True) else (u,False)
			    Nothing -> (u,False)

recursive_search_module::[Unit]->IO [Unit]
recursive_search_module xs = do
		     v<-newEmptyMVar
                     forkIO $ putMVar v xs
                     y <- eliminate xs v 
                     return y

check_for_empty::[Unit]->[Bool]
check_for_empty xs = map c_f_e xs
c_f_e::Unit->Bool
c_f_e u = case possible_values u of
               Just xs -> True
               Nothing -> False


eliminate::[Unit]->MVar [Unit] -> IO [Unit]
eliminate (x:xs) v = do
                      let (a,flag) = (check_for_single_value x) in 
                       if(flag == True) then
                         do 
                          r <- takeMVar v
                          let updated_list = reset_grid_if_actual_value a (change_existing a r) in
                            do 
                             putMVar v updated_list 
                             eliminate updated_list v 
                       else 
                         do
                           eliminate xs v
eliminate [] v = do
                 r <- takeMVar v
                 if ((or (check_for_empty r)) == True) then
                   do
                    putMVar v r
                    (assign v r r)
                 else 
                   return r       


look_sq::Int->Int->(Int,Int)->Unit->Bool
look_sq val sq (x1,y1) u = let (x2,y2) = position u in
                            let s = square_no u in 
                              if (x1,y1)/=(x2,y2) && s==sq then
                               case possible_values u of 
                                  Just xs -> elem val xs
                                  Nothing -> case actual_value u of 
                                              Just v -> if (val == v) then True else False
                                              Nothing -> False
                              else 
                                False

look_pv_x::Int->(Int,Int)->Unit->Bool
look_pv_x val (x1,y1) u = let (x2,y2) = position u in 
                            if (x1,y1)/=(x2,y2) && (x1==x2) then
                              case possible_values u of
                                 Just xs -> elem val xs
                                 Nothing -> case actual_value u of
                                              Just v -> if (val == v) then True else False
                                              Nothing -> False
                            else
                              False

look_pv_y::Int->(Int,Int)->Unit->Bool
look_pv_y val (x1,y1) u = let (x2,y2) = position u in
                            if (x1,y1)/=(x2,y2) && (y1==y2) then
                              case possible_values u of
                                Just xs -> elem val xs
                                Nothing -> case actual_value u of
                                             Just v -> if (val == v) then True else False
                                             Nothing -> False

                            else
                              False

search_us u ys = let (x1,y1) = position u in
                  let sq = square_no u in 
                    case possible_values u of
                     Just xs -> (eliminate_possible_values_unit (x1,y1) sq xs ys) 
                     Nothing -> Nothing

eliminate_possible_values_unit::(Int,Int)->Int->[Int]->[Unit]->Maybe Int
eliminate_possible_values_unit (x1,y1) sq (x:xs) ys = if (or (map (look_sq x sq (x1,y1)) ys)) == False then
                                       Just x
                                      else
                                       (eliminate_possible_values_unit (x1,y1) sq xs ys)
eliminate_possible_values_unit (x1,y1) sq [] ys = Nothing 

assign::MVar [Unit]->[Unit]->[Unit]->IO [Unit]
assign v (u:us) ys = do
                     r <- takeMVar v 
                     case (search_us u r) of 
                      Just x -> do
                                 let unit = (assign_to_unit u x) in
                                   let updated_list = reset_grid_if_actual_value unit (change_existing unit r)
                                     in do 
                                          putMVar v updated_list
                                          assign v updated_list updated_list
                      Nothing -> do
                                 putMVar v r                                
                                 assign v us ys
assign v [] ys = do
                 r<-takeMVar v
                 putMVar v (eliminate_possible_values r r)
                 th <- newEmptyMVar
                 forkIO $ putMVar th r
                 e_th <- newEmptyMVar
                 forkIO $ putMVar e_th []
                 (trial_and_error r r th e_th)


                 
is_list_complete::[Unit]-> Bool
is_list_complete (x:xs) = case actual_value x  of
                           Just a -> is_list_complete xs
                           Nothing -> False
is_list_complete []  = True

get_values_from_row::[Unit]->Int->[Int]
get_values_from_row (y:ys) x = let (a1,b1) = position y in
                                if (a1==x) then
                                case actual_value y of
                                  Just v -> v:(get_values_from_row ys x)
                                  Nothing -> (get_values_from_row ys x)
				else (get_values_from_row ys x)
get_values_from_row _ x = []



correctness_of_rows::[Unit]->Bool
correctness_of_rows xs = and(map (\x -> if length x == 9 then True else False )(map nub ( map sort (map (get_values_from_row xs) [1..9]))))

get_values_from_column::[Unit]->Int->[Int]
get_values_from_column (y:ys) x = let (a1,b1) = position y in
                                if (b1==x) then
                                case actual_value y of
                                  Just v -> v:(get_values_from_column ys x)
                                  Nothing -> (get_values_from_column ys x)
                                else (get_values_from_column ys x)
get_values_from_column _ x = []



correctness_of_columns::[Unit]->Bool
correctness_of_columns xs = and(map (\x -> if length x == 9 then True else False ) (map nub(map sort(map (get_values_from_column xs) [1..9]))))

get_values_from_square::[Unit]->Int->[Int]
get_values_from_square (y:ys) x = let sq = square_no y in
                                if (sq==x) then
                                case actual_value y of
                                  Just v -> v:(get_values_from_square ys x)
                                  Nothing -> (get_values_from_square ys x)
                                else (get_values_from_square ys x)
get_values_from_square _ x = []



correctness_of_square::[Unit]->Bool
correctness_of_square xs = and(map (\x -> if length x == 9 then True else False ) (map nub(map sort(map (get_values_from_square xs) [1..9]))))

is_list_correct::[Unit]->Bool
is_list_correct xs = and [correctness_of_rows xs,correctness_of_columns xs,correctness_of_square xs]

trial_and_error::[Unit]->[Unit]->MVar [Unit]->MVar [Unit]->IO [Unit]
trial_and_error (unit:us) ys th e_th= do
                                  case possible_values unit of
                                   Just xs -> do
                                              lst <- (scan_list xs unit ys th e_th)
                                              s1 <- takeMVar e_th
                                              putMVar e_th s1 
					      if (is_list_complete s1)==False then (trial_and_error s1 s1 th e_th) 
                                              else 
                                               if (null lst) then
                                                  do
                                                  (trial_and_error us ys th e_th)
                                               else 
                                                  do
                                                  t <- takeMVar th
                                                  putMVar th t
                                                  return t
                                   Nothing -> (trial_and_error us ys th e_th)

trial_and_error [] ys th e_th = 
                           do 
                           n <- takeMVar th
                           putMVar th n
                           return n

scan_list::[Int]->Unit->[Unit]->MVar [Unit]->MVar [Unit]->IO [Unit]
scan_list (x:xs) unit ys th e_th=
                      do
                      (b,l) <- (select_value_trial x unit ys th e_th)
                      if(b==True) then
                        do
                        r<-takeMVar th
                        putMVar th l
                        return l
                      else 
                        do 
                        (scan_list xs unit ys th e_th)
scan_list [] unit ys th e_th= return []

select_value_trial::Int->Unit->[Unit]->MVar [Unit]->MVar [Unit]->IO (Bool,[Unit])
select_value_trial value unit ys th e_th = do
                                           let u = (assign_to_unit unit value) in 
                                            let upd_list = reset_grid_if_actual_value u (change_existing u ys)
                                             in do
                                              th2 <- newEmptyMVar
                                              forkIO $ putMVar th2 upd_list
                                              (bool,list) <- (run_assign_eliminate upd_list th2 e_th)
                                              if (bool==True) then
                                               do
                                               return (True,list)
                                              else
                                               return (False,[])

run_assign_eliminate:: [Unit]->MVar [Unit]->MVar [Unit]->IO (Bool,[Unit])
run_assign_eliminate list th2 e_th = 
                                 do
                                 r3<-takeMVar th2
                                 putMVar th2 r3
                                 y<-eliminate list th2
                                 if ((is_list_complete y)==True) then
                                  if ((is_list_correct y)==True) then
                                   do
                                   return (True,y)
                                  else
                                     return (False,[])
                                 else
                                   do
                                   r5<-takeMVar e_th
                                   putMVar e_th y
                                   return (False,[])
                                              
assign_to_unit::Unit->Int->Unit
assign_to_unit u value = Unit { position = position u,
                                actual_value = Just value,
                                possible_values = Nothing, 
                                square_no = square_no u }                                


test::[Unit]->[Unit]->[Unit]
test (x:xs) ys = case actual_value x of 
                   Just x -> test xs ys
                   Nothing -> let a = replace_all_single_values(eliminate_possible_values ys ys) in
                              test a a
test [] ys = ys

print_value::[Unit] -> [Int]
print_value (x:xs) = case actual_value x of 
                     Just a -> a : print_value xs
print_value [] = []  

capture_actual_value::[Unit]->[Int]
capture_actual_value (x:xs) = let y = (actual_value x) in 
			      case y of 
			      Just a -> a: capture_actual_value xs 
			      Nothing ->0:capture_actual_value xs
capture_actual_value [] = [] 
			 
print_answer::[Int]->IO()
print_answer (x:xs) = if (mod (length xs) 9)==0 
		      then 
                        do
			{	 
                         putChar (intToDigit x);
                         putChar '\n';
                         print_answer xs;
			}
                      else 
                        do
			{
                        putChar (intToDigit x);
                        putChar ' ';
                        print_answer xs;
			}
print_answer [] = do
                  putChar '\n'

read_csv::[Char]->[Char]
read_csv (x:xs) = if (x == ',') then read_csv xs else (x:read_csv xs)
read_csv _ = []
