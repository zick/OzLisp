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
SymIf = {MakeSym "if"}
SymLambda = {MakeSym "lambda"}
SymDefun = {MakeSym "defun"}
SymSetq = {MakeSym "setq"}

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

fun {Pairlis Lst1 Lst2} Rec in
   fun {Rec Lst1 Lst2 Acc}
      case Lst1|Lst2
      of cons(A1 D1)|cons(A2 D2) then
         {Rec @D1 @D2 {MakeCons {MakeCons @A1 @A2} Acc}}
      else
         {Nreverse Acc}
      end
   end
   {Rec Lst1 Lst2 nil}
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
   [] subr(_) then "<subr>"
   [] expr(_ _ _) then "<expr>"
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

fun {Eval Obj Env} Bind Op Args C Expr Sym Val in
   case Obj
   of nil then Obj
   [] num(_) then Obj
   [] error(_) then Obj
   [] sym(S) then
      Bind = {FindVar Obj Env}
      if Bind == nil then error({Append S " has no value"})
      else {SafeCdr Bind}
      end
   else
      Op = {SafeCar Obj}
      Args = {SafeCdr Obj}
      if Op == SymQuote then
        {SafeCar Args}
      elseif Op == SymIf then
         C = {Eval {SafeCar Args} Env}
         case C
         of error(_) then C
         [] nil then {Eval {SafeCar {SafeCdr {SafeCdr Args}}} Env}
         else {Eval {SafeCar {SafeCdr Args}} Env}
         end
      elseif Op == SymLambda then
         {MakeExpr Args Env}
      elseif Op == SymDefun then
         Expr = {MakeExpr {SafeCdr Args} Env}
         Sym = {SafeCar Args}
         {AddToEnv Sym Expr GEnv}
         Sym
      elseif Op == SymSetq then
         Val = {Eval {SafeCar {SafeCdr Args}} Env}
         Sym = {SafeCar Args}
         Bind = {FindVar Sym Env}
         case Bind
         of cons(_ D) then D := Val
         else {AddToEnv Sym Val GEnv}
         end
         Val
      else
         {Apply {Eval Op Env} {Evlis Args Env}}
      end
   end
end

fun {Evlis Lst Env} Rec X in
   fun {Rec Lst Acc} Elm in
      case Lst
      of cons(A D) then
         Elm = {Eval @A Env}
         case Elm
         of error(_) then Elm
         else {Rec @D {MakeCons Elm Acc}}
         end
      else Acc
      end
   end
   X = {Rec Lst nil}
   case X
   of error(_) then X
   else {Nreverse X}
   end
end

fun {Progn Body Env} Rec in
   fun {Rec Body Acc}
      case Body
      of cons(A D) then {Rec @D {Eval @A Env}}
      else Acc
      end
   end
   {Rec Body nil}
end

fun {Apply Fn Args}
   case Args
   of error(_) then Args
   else
      case Fn
      of error(_) then Fn
      [] subr(F) then {F Args}
      [] expr(A B E) then {Progn B {MakeCons {Pairlis A Args} E}}
      else error({Append {PrintObj Fn} " is not function"})
      end
   end
end

fun {SubrCar Args}
   {SafeCar {SafeCar Args}}
end

fun {SubrCdr Args}
   {SafeCdr {SafeCar Args}}
end

fun {SubrCons Args}
   {MakeCons {SafeCar Args} {SafeCar {SafeCdr Args}}}
end

fun {SubrEq Args} X Y in
   X = {SafeCar Args}
   Y = {SafeCar {SafeCdr Args}}
   case X|Y
   of num(N)|num(N) then SymT
   else
      if X == Y then SymT
      else nil
      end
   end
end

fun {SubrAtom Args}
   case {SafeCar Args}
   of cons(_ _) then nil
   else SymT
   end
end

fun {SubrNumberp Args}
   case {SafeCar Args}
   of num(_) then SymT
   else nil
   end
end

fun {SubrSymbolp Args}
   case {SafeCar Args}
   of sym(_) then SymT
   else nil
   end
end

fun {SubrAddOrMul Fn InitVal}
   fun {$ Args} Rec in
      fun {Rec Args Acc}
         case Args
         of cons(A D) then
            case @A
            of error(_) then @A
            [] num(N) then {Rec @D {Fn Acc N}}
            else error("wrong type")
            end
         else num(Acc)
         end
      end
      {Rec Args InitVal}
   end
end
SubrAdd = {SubrAddOrMul fun {$ X Y} X + Y end 0}
SubrMul = {SubrAddOrMul fun {$ X Y} X * Y end 1}

fun {SubrSubOrDivOrMod Fn}
   fun {$ Args} X Y in
      X = {SafeCar Args}
      Y = {SafeCar {SafeCdr Args}}
      case X|Y
      of num(N)|num(M) then num({Fn N M})
      else error("wrong type")
      end
   end
end
SubrSub = {SubrSubOrDivOrMod fun {$ X Y} X - Y end}
SubrDiv = {SubrSubOrDivOrMod fun {$ X Y} X div Y end}
SubrMod = {SubrSubOrDivOrMod fun {$ X Y} X mod Y end}

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
   {AddToEnv {MakeSym "car"} subr(SubrCar) GEnv}
   {AddToEnv {MakeSym "cdr"} subr(SubrCdr) GEnv}
   {AddToEnv {MakeSym "cons"} subr(SubrCons) GEnv}
   {AddToEnv {MakeSym "eq"} subr(SubrEq) GEnv}
   {AddToEnv {MakeSym "atom"} subr(SubrAtom) GEnv}
   {AddToEnv {MakeSym "numberp"} subr(SubrNumberp) GEnv}
   {AddToEnv {MakeSym "symbolp"} subr(SubrSymbolp) GEnv}
   {AddToEnv {MakeSym "+"} subr(SubrAdd) GEnv}
   {AddToEnv {MakeSym "*"} subr(SubrMul) GEnv}
   {AddToEnv {MakeSym "-"} subr(SubrSub) GEnv}
   {AddToEnv {MakeSym "/"} subr(SubrDiv) GEnv}
   {AddToEnv {MakeSym "mod"} subr(SubrMod) GEnv}
   {AddToEnv SymT SymT GEnv}
   {Repl "> "}
   {Application.exit 0}
end

end
