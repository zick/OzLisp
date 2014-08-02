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
   of cons(A _) then A
   else nil
   end
end

fun {SafeCdr Obj}
   case Obj
   of cons(_ D) then D
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
      {ParseError "noimpl"}
   elseif S.1 == Quote then
      {ParseError "noimpl"}
   else
      {ReadAtom S}
   end
end

fun {PrintObj Obj}
  case Obj
  of nil then "nil"
  [] num(N) then {Int.toString N}
  [] sym(S) then S
  [] error(S) then {Append "<error: " {Append S ">"}}
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
        {System.showInfo {PrintObj {Read Line}.1}}
        {Repl Prompt}
      end
   end
   {Repl "> "}
   {Application.exit 0}
end

end
