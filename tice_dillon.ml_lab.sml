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
  | list2Set (x::xs) = Set(x, (list2Set xs));

(* Union operation for Set type. Returns a new set with all elements that are not in both sets to be united *)
fun union Empty Empty = Empty
  | union (Set (x)) (Set = (Set x);
  
val fuck = Set(#"F", Set(#"U", Set(#"C", Set(#"K", Empty))));
union Empty fuck;

(* Intersect operation for Set type *)
fun intersect set1 set2 = ;

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
