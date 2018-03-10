(***************************************************************
*
* CSCI 305 - ML Programming Lab
*
* Dillon Tice
* Dillon.Tice@gmail.com
*
***************************************************************)
(* Warmup function *)
fun f [] = [] (* a *)
  | f (x::xs) = (x + 1) :: (f xs); (* b *)
  
(* Datatype *)
datatype 'element set =
   Empty
  | Set of 'element * 'element set;

(* Tells whether the given element e is a part of the given set by using tail recursion *)
fun isMember e Empty = false
  | isMember e (Set(x, tail)) = 
      if x = e then true else isMember e tail;

(* Converts a list to a set using tail recursion *)
fun list2Set [] = Empty
  | list2Set (x::xs) = let val z = list2Set xs 
						in if isMember x z 
							then
								z 
							else 
								Set(x, z)
						end;
(* Union operation for Set type. Returns a new set with all elements that are not in both sets to be united *)
fun union set1 Empty = set1
  | union Empty set1 = set1
  | union set1 (Set(x, tail)) = if isMember x set1 
								then
									union set1 tail
								else
									Set(x, union set1 tail);



(* Intersect operation for Set type *)
fun intersect set1 Empty = Empty
	| intersect Empty set1 = Empty
	| intersect set1 (Set(head, tail)) = if isMember head set1 then Set(head, intersect set1 tail) else intersect set1 tail;

(* Simple function to stringify the contents of a Set of characters *)
fun stringifyCharSet Empty = ""
  | stringifyCharSet (Set(y, ys)) = Char.toString(y) ^ " " ^ stringifyCharSet(ys);
  
(* Simple function to stringify the contents of a Set of ints *)
fun stringifyIntSet Empty = ""
  | stringifyIntSet (Set(w, ws)) = Int.toString(w) ^ " " ^ stringifyIntSet(ws);

(* Simple function to stringify the contents of a Set of strings *)
fun stringifyStringSet Empty = ""
  | stringifyStringSet (Set(z, zs)) = z ^ " " ^ stringifyStringSet(zs);

(* Simple function that prints a set of integers *)
fun print_int x = print ("{ " ^ stringifyIntSet(x) ^ "}\n");

(* Simple function that prints a set of strings *)
fun print_str x = print ("{ " ^ stringifyStringSet(x) ^ "}\n");

(* Simple function that prints a set of characters *)
fun print_chr x = print ("{ " ^ stringifyCharSet(x) ^ "}\n");

list2Set [1, 3, 2];
list2Set [#"a", #"b", #"c"];
list2Set [];
list2Set [6, 2, 2];
list2Set ["x", "y", "z", "x"];

(* Question 1 *)
f [3, 1, 4, 1, 5, 9];

(* Question 5 *)
val quest5 = isMember "one" (list2Set ["1", "2", "3", "4"]);
print ("\nQuestion 5: " ^ Bool.toString(quest5) ^ "\n");

(* Question 7 *)
val quest7 = list2Set ["it", "was", "the", "best", "of", "times,", "it", "was", "the", "worst", "of", "times"];
print "\nQuestion 7: ";
print_str quest7;
print "\n";

(* Question 9 *)
print "\nQuestion 9: ";
print_str (union (list2Set ["green", "eggs", "and"]) (list2Set ["ham"]));

(* Question 10 *)
print "\nQuestion 10: ";
print_str (intersect (list2Set ["stewed", "tomatoes", "and", "macaroni"]) (list2Set ["macaroni", "and", "cheese"]));
