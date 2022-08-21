
(*
                                    Perface
Inspiration by: 
  - https://android.googlesource.com/platform/external/gcc-demangle/+/
       d6f70187df96e4f1968adbe9b52035dc474b7590/cp-demangle.c
  - https://opensource.apple.com/source/gcc/gcc-5482/libiberty/cp-demangle.c.auto.html

Reference:
  - https://fitzgeraldnick.com/2017/02/22/cpp-demangle.html (in Rust)
  - https://itanium-cxx-abi.github.io/cxx-abi/abi.html (Itanium C++ ABI’s name mangling rules)
  - https://docs.oracle.com/javase/10/docs/api/com/sun/jdi/doc-files/signature.html
  - https://docs.oracle.com/javase/specs/jvms/se7/html/jvms-4.html

That's all, thanks! 

*)


(* basic util *)
open List

let delta condition x y = if condition then x else y

let last xs = nth xs (length xs - 1)
let push xs x = xs @ [x]

let (|->) s t = fun x -> delta (x == s) t x

let rec drop n = function [] -> []
  | head :: tails as xs -> match n with 0 -> xs | _ -> (drop (n - 1) tails)
let tails xs = drop 1 xs

let rec take n = function [] -> []
  | head :: tails -> match n with 0 -> [] | _ -> head :: take (n - 1) tails
let lizard xs = take (length xs - 1) xs

let manifold f xs trans default = match xs with [] -> default
  | head :: tails -> fold_left (f trans) (trans head) tails

let glue = fun trans s t -> s ^ trans t
let string_of_chars xs = manifold glue xs Char.escaped ""

let subs s off pos = String.sub s off (pos - off + 1)
let mids s = subs s 1 (String.length s - 1)
let drops n s = subs s n (String.length s - 1)

let list_of_string s = let xs = ref [] in 
  for index = 0 to String.length s - 1 do
    xs := push !xs s.[index]
  done; !xs




(* demangle *)

module StringMap = Map.Make (String)

(* JVM - JDI Type Signatures *)

(*  
  +---------------------------+-----------------------+
  | L fully-qualified-class ; | fully-qualified-class |
  +---------------------------+-----------------------+
  | [ type                    | type[]                |
  +---------------------------+-----------------------+
  | ( arg-types ) ret-type    | method type           |
  +---------------------------+-----------------------+
*)

let jvm_mangle_encodings = StringMap . ( empty 
  |> add "Z" "boolean"
  |> add "B" "byte"
  |> add "C" "char"
  |> add "S" "short"
  |> add "I" "int"
  |> add "J" "long"
  |> add "F" "float"
  |> add "D" "double"
)

(*
let wrap s x = s ^ x ^ s
let wrap_char = wrap "'"
let wrap_string = wrap "\"" 

let generate_string maps = StringMap.fold 
  (fun k v s -> s ^ "| " ^ wrap_char k ^ " -> " ^ wrap_string v ^ "\n") maps "" 
*)

(* print_endline (generate_string jvm_mangle_encodings) *)

let is_primitive c = 
  c == 'B' || c == 'C' || 
  c == 'D' || c == 'F' || 
  c == 'I' || c == 'J' ||
  c == 'S' || c == 'Z'
let of_primitive s = StringMap.find s jvm_mangle_encodings

let dotify s = string_of_chars (map ('/' |-> '.') (list_of_string s))
let is_reference s = s.[0] == 'L' && s.[String.length s - 1] == ';'
let of_reference s = dotify (mids s)

let of_type s = let len = String.length s in match len with 
    1 -> if is_primitive s.[0] then of_primitive s else "__error"
  | _ -> if is_reference s then of_reference s else "__error"

let rec scan s xs = match String.length s with 0 -> xs | _ -> 
  let head = s.[0] in let consume n = drops n s in match head with 
    'L' -> let pos = String.index s ';' in 
           let aka = dotify (subs s 1 (pos - 1)) in
           scan (consume (pos + 1)) (push xs aka)
  | ___ -> match is_primitive head with 
      true -> scan (consume 1) (push xs (of_primitive (Char.escaped head)))
    | ____ -> scan (consume 1) (push xs "__error")

(* print_endline (String.concat ", " (scan "ILjava/lang/String;J" [])) *)


let of_method s = match s.[0] with 
    '(' -> let pos = String.index s ')' in
           let ret = of_type (drops (pos + 1) s) in
           ret
  | ___ -> "__error"
;;
print_endline (of_method "()I")


(* GCC - Itanium C++ ABI’s name mangling rules *)

let gcc_mangle_encodings = StringMap . ( empty 
  (* Compression *)
  |> add "St" "std::"
  |> add "Sa" "std::allocator"
  |> add "Sb" "std::basic_string"
  |> add "Ss" "std::string"
  |> add "Si" "std::istream"
  |> add "So" "std::ostream"
  |> add "Sd" "std::iostream"
  
  (* Builtin types *)
  |> add "v" "void"
  |> add "w" "wchar_t"
  |> add "b" "bool"

  |> add "c" "char" |> add "a" "signed char" |> add "h" "unsigned char"
  |> add "s" "short" |> add "t" "unsigned short"
  |> add "i" "int" |> add "j" "unsigned int"
  |> add "l" "long" |> add "m" "unsigned long"
  |> add "x" "long long" |> add "y" "unsigned long long"
  |> add "n" "__int128" |> add "o" "unsigned __int128"
  |> add "f" "float"
  |> add "d" "double"
  |> add "e" "long double"
  |> add "g" "__float128" (* __float80 *)
  |> add "z" "ellipsis" (* ... *)
)

(* 
                              Compression

   <substitution> ::= St # ::std::
   <substitution> ::= Sa # ::std::allocator
   <substitution> ::= Sb # ::std::basic_string
   <substitution> ::= Ss # ::std::basic_string < char, 
                                                 ::std::char_traits<char>, 
                                                 ::std::allocator<char> >
   <substitution> ::= Si # ::std::basic_istream<char,  std::char_traits<char> >
   <substitution> ::= So # ::std::basic_ostream<char,  std::char_traits<char> >
   <substitution> ::= Sd # ::std::basic_iostream<char, std::char_traits<char> >
*)

(*
              Builtin types

  <builtin-type> 
     ::= v  # void
     ::= w  # wchar_t
     ::= b  # bool
     ::= c  # char
     ::= a  # signed char
     ::= h  # unsigned char
     ::= s  # short
     ::= t  # unsigned short
     ::= i  # int
     ::= j  # unsigned int
     ::= l  # long
     ::= m  # unsigned long
     ::= x  # long long, __int64
     ::= y  # unsigned long long, __int64
     ::= n  # __int128
     ::= o  # unsigned __int128
     ::= f  # float
     ::= d  # double
     ::= e  # long double, __float80
     ::= g  # __float128
     ::= z  # ellipsis
     ::= Dd # IEEE 754r decimal floating point (64 bits)
     ::= De # IEEE 754r decimal floating point (128 bits)
     ::= Df # IEEE 754r decimal floating point (32 bits)
     ::= Dh # IEEE 754r half-precision floating point (16 bits)
     ::= DF <number> _ # ISO/IEC TS 18661 binary floating point type _FloatN (N bits)
     ::= DB <number> _        # C23 signed _BitInt(N)
     ::= DB <instantiation-dependent expression> _ # C23 signed _BitInt(N)
     ::= DU <number> _        # C23 unsigned _BitInt(N)
     ::= DU <instantiation-dependent expression> _ # C23 unsigned _BitInt(N)
     ::= Di # char32_t
     ::= Ds # char16_t
     ::= Du # char8_t
     ::= Da # auto
     ::= Dc # decltype(auto)
     ::= Dn # std::nullptr_t (i.e., decltype(nullptr))
     ::= u <source-name> [<template-args>] # vendor extended type
*)


(*
                       Type encodings

  <type> ::= <builtin-type>
         ::= <qualified-type>
         ::= <function-type>
         ::= <class-enum-type>
         ::= <array-type>
         ::= <pointer-to-member-type>
         ::= <template-param>
         ::= <template-template-param> <template-args>
         ::= <decltype>
         ::= P <type>        # pointer
         ::= R <type>        # l-value reference
         ::= O <type>        # r-value reference (C++11)
         ::= C <type>        # complex pair (C99)
         ::= G <type>        # imaginary (C99)
         ::= <substitution>  # See Compression below
*)

(* 
    Operator Encodings:

<operator-name> ::= nw  # new           
      ::= na  # new[]
      ::= dl  # delete        
      ::= da  # delete[]      
      ::= aw  # co_await      
      ::= ps  # + (unary)
      ::= ng  # - (unary)     
      ::= ad  # & (unary)     
      ::= de  # * (unary)     
      ::= co  # ~             
      ::= pl  # +             
      ::= mi  # -             
      ::= ml  # *             
      ::= dv  # /             
      ::= rm  # %             
      ::= an  # &             
      ::= or  # |             
      ::= eo  # ^             
      ::= aS  # =             
      ::= pL  # +=            
      ::= mI  # -=            
      ::= mL  # *=            
      ::= dV  # /=            
      ::= rM  # %=            
      ::= aN  # &=            
      ::= oR  # |=            
      ::= eO  # ^=            
      ::= ls  # <<            
      ::= rs  # >>            
      ::= lS  # <<=           
      ::= rS  # >>=           
      ::= eq  # ==            
      ::= ne  # !=            
      ::= lt  # <             
      ::= gt  # >             
      ::= le  # <=            
      ::= ge  # >=            
      ::= ss  # <=>           
      ::= nt  # !             
      ::= aa  # &&            
      ::= oo  # ||            
      ::= pp  # ++ (postfix in <expression> context)
      ::= mm  # -- (postfix in <expression> context)           
      ::= cm  # ,             
      ::= pm  # ->*           
      ::= pt  # ->            
      ::= cl  # ()            
      ::= ix  # []            
      ::= qu  # ?             
      ::= cv <type>  # (cast)
      ::= li <source-name>        # operator ""
      ::= v <digit> <source-name>  # vendor extended operator
*)

(*                 
               Virtual Tables and RTTI

<special-name> ::= TV <type>  # virtual table
     ::= TT <type>  # VTT structure (construction vtable index)
     ::= TI <type>  # typeinfo structure
     ::= TS <type>  # typeinfo name (null-terminated byte string)
*)





