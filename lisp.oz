functor

import
   Application Open System
   Dictionary at 'x-oz://system/adt/Dictionary.ozf'

define

LPar = &(
RPar = &)
Quote = &'

fun {SafeCar Obj}
   case Obj
   of cons(A _) then @A
   else nil
   end
end

fun {SafeCdr Obj}
   case Obj
   of cons(_ D) then @D
   else nil
   end
end

SymTable = {Dictionary.new}
fun {MakeSym Str} S in
   S = {String.toAtom Str}
   if {SymTable.member S} == false then
      {SymTable.put S sym(Str)}
   end
   {SymTable.get S}
end

SymT = {MakeSym "t"}
SymQuote = {MakeSym "quote"}

fun {MakeCons A D}
  cons({NewCell A} {NewCell D})
end

fun {Nreverse Lst} Rec in
   fun {Rec Lst Acc} Tmp in
      case Lst
      of cons(_ D) then
         Tmp = @D
         D := Acc
         {Rec Tmp Lst}
      else Acc
      end
   end
   {Rec Lst nil}
end

fun {IsSpace C}
   C == &\t orelse C == &\r orelse C == &\n orelse C == &\x20
end

fun {IsDelimiter C}
   C == LPar orelse C == RPar orelse C == Quote orelse {IsSpace C}
end

fun {MakeExpr Args Env}
   expr({SafeCar Args} {SafeCdr Args} Env)
end

fun {SkipSpaces Str}
   if Str == nil then ""
   elseif {IsSpace Str.1} then {SkipSpaces Str.2}
   else Str
   end
end

fun {MakeNumOrSym Str}
   try num({String.toInt Str})
   catch _ then {MakeSym Str}
   end
end

fun {ReadAtom Str} Rec in
  fun {Rec Str Acc}
    if Str == nil orelse {IsDelimiter Str.1} then
      {MakeNumOrSym {List.reverse Acc}}|Str
    else
      {Rec Str.2 Str.1|Acc}
    end
  end
  {Rec Str ""}
end

fun {ParseError S}
   error(S)|""
end

fun {Read Str} S in
   S = {SkipSpaces Str}
   if S == nil then
      {ParseError "empty input"}
   elseif S.1 == RPar then
      {ParseError {Append "invalid syntax: " S}}
   elseif S.1 == LPar then
      {ReadList S.2}
   elseif S.1 == Quote then
      local Elm Next
      in
         Elm|Next = {Read S.2}
         {MakeCons SymQuote {MakeCons Elm nil}}|Next
      end
   else
      {ReadAtom S}
   end
end

fun {ReadList Str} Rec in
   fun {Rec Str Acc} S Elm Next in
      S = {SkipSpaces Str}
      if S == nil then
         {ParseError "unfinishde parenthesis"}
      elseif S.1 == RPar then
         {Nreverse Acc}|Str.2
      else
         Elm|Next = {Read S}
         case Elm
         of error(_) then Elm|Next
         else {Rec Next {MakeCons Elm Acc}}
         end
      end
   end
   {Rec Str nil}
end

fun {PrintObj Obj}
   case Obj
   of nil then "nil"
   [] num(N) then {Int.toString N}
   [] sym(S) then S
   [] error(S) then {Append "<error: " {Append S ">"}}
   [] cons(_ _) then {PrintList Obj}
   end
end

fun {PrintList Obj} Rec in
   fun {Rec Obj Blank Acc}
     case Obj
     of cons(A D) then
        {Rec @D " " {Append Acc {Append Blank {PrintObj @A}}}}
     [] nil then
        {Append "(" {Append Acc ")"}}
     else
        {Append "(" {Append Acc {Append " . " {Append {PrintObj Obj} ")"}}}}
     end
   end
   {Rec Obj "" ""}
end

fun {FindVar Sym Env} Assoc X in
   fun {Assoc Alist}
      if Alist == nil then
         nil
      elseif {SafeCar {SafeCar Alist}} == Sym then
         {SafeCar Alist}
      else
         {Assoc {SafeCdr Alist}}
      end
   end
   if Env == nil then
      nil
   else
      X = {Assoc {SafeCar Env}}
      if X == nil then {FindVar Sym {SafeCdr Env}}
      else X
      end
   end
end

GEnv = {MakeCons nil nil}

proc {AddToEnv Sym Val Env}
   case Env
   of cons(A _) then
      A := {MakeCons {MakeCons Sym Val} @A}
   end
end

fun {Eval Obj Env} Bind in
   case Obj
   of nil then Obj
   [] num(_) then Obj
   [] error(_) then Obj
   [] sym(S) then
      Bind = {FindVar Obj Env}
      if Bind == nil then error({Append S " has no value"})
      else {SafeCdr Bind}
      end
   else error("noimpl")
   end
end

local
   class TextFile from Open.file Open.text end
   StdIn = {New TextFile init(name:stdin)}
in
   proc {Repl Prompt} Line in
      {System.printInfo Prompt}
      Line = {StdIn getS($)}
      if Line \= false then
        {System.showInfo {PrintObj {Eval {Read Line}.1 GEnv}}}
        {Repl Prompt}
      end
   end
   {AddToEnv SymT SymT GEnv}
   {Repl "> "}
   {Application.exit 0}
end

end
