
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
let subs' s pos = subs s 0 pos
let mids s = subs s 1 (String.length s - 2)
let drops n s = subs s n (String.length s - 1)
let (|+|) s n = s.[if n >= 0 then n else String.length s + n]

let list_of_string s = let xs = ref [] in 
  for index = 0 to String.length s - 1 do
    xs := push !xs s.[index]
  done; !xs




(* demangle *)


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

module StringMap = Map.Make (String)

let jvm_mangle_encodings = StringMap . ( empty 
  |> add "Z" "boolean"
  |> add "B" "byte"
  |> add "C" "char"
  |> add "S" "short"
  |> add "I" "int"
  |> add "J" "long"
  |> add "F" "float"
  |> add "D" "double"
  |> add "V" "void"
)

let unknown = "__unknown" (* unknown type *)

let is_primitive s = StringMap.mem s jvm_mangle_encodings
let of_primitive s = match is_primitive s with
  true -> StringMap.find s jvm_mangle_encodings | _ -> unknown

let dotify s = string_of_chars (map ('/' |-> '.') (list_of_string s))

let rec is_reference s = if String.length s >= 2 then match s.[0] with
    'L' -> s.[String.length s - 1] == ';'
  | '[' -> let s' = drops 1 s in is_primitive s' || is_reference s'
  | ___ -> false
else false

let rec of_reference s = if is_reference s then match s.[0] with 
    'L' -> dotify (mids s)
  | '[' -> let s' = drops 1 s in 
           let d' = delta (is_reference s') in
               d' of_reference of_primitive s' ^ "[]"
  | ___ -> unknown
else unknown

let is_type s = is_primitive s || is_reference s
let of_type s = let case = match String.length s with 
  1 -> of_primitive | _ -> of_reference in case s

let rec scan s xs = match String.length s with 0 -> xs | _ -> 
  let consume n = drops n s in 
  let rec trundle pos = let piece = subs' s pos in
    if piece |+| 1 == 'L' then subs' s (String.index s ';') else
    if is_type piece then piece else trundle (pos + 1) in
  let raw = trundle 0 in
  let len = String.length raw in 
  let aka = of_type raw in scan (consume len) (push xs aka)
let scan' s = scan s [] 

let of_method s name = if s.[0] == '(' then 
  let pos = String.index s ')' in
  let cat = String.concat ", " (scan' (subs s 1 (pos - 1))) in
  of_type (drops (pos + 1) s) ^ " " ^ name ^ "(" ^ cat ^ ")"
else unknown
let of_method' s = of_method s ""

;; print_endline (of_method "([[IZ[Ljava/lang/Object;)V" "test")





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





