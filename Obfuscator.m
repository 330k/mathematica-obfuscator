(* ::Package:: *)

(* :Mathematica Version: 8.0 *)

(* :Name: Obfuscator` *)

(* :Title: Mathematica Code Obfuscator *)

(* :Author: Kei Misawa *)

(* :Summary:
  This package provides code obfuscation function.

 This software is released under the MIT License.
  http://opensource.org/licenses/mit-license.php
*)

(* :Context: Obfuscator` *)

(* :Package Version: 1.0 *)

(* :Copyright: Copyright 2021, Kei Misawa *)

(* :Keywords: code obfuscation *)

(* :Warning:  *)

(* :Sources: 
*)


BeginPackage["Obfuscator`"];


Obfuscate::usage="Obfuscate[expression] returns obfuscated Mathematica code string.
Use ToExpression in order to evaluate the obfuscated string, or copy and paste the code.";


Begin["`Private`"];


Attributes[Obfuscate]={HoldFirst,ReadProtected};
Obfuscate[exp_]:=Obfuscate[exp,1];

(* Numberize 1 *)
Obfuscate[exp_,1]:=HoldForm[Uncompress[FromCharacterCode[IntegerDigits[#,128]]]]&[
FromDigits[ToCharacterCode@Compress@Unevaluated[exp],128]];

(* Numberize 2 *)
Obfuscate[exp_,2]:=HoldForm[Uncompress[FromCharacterCode[IntegerDigits[#,80]+43]]]&[
FromDigits[ToCharacterCode@Compress@Unevaluated[exp]-43,80]];

(* Base64 *)
Obfuscate[exp_,3]:=HoldForm[ImportString[#,IntegerString[683248828,36]]]&[
StringReplace[ExportString[
StringReplace[ToString@InputForm[Unevaluated@exp],StartOfString~~"Unevaluated["~~a:Longest[___]~~"]":>a]
,"Base64"],"\n"->""]];

(* Base64 + Numberize 1 *)
Obfuscate[exp_,4]:=HoldForm[ImportString[FromCharacterCode[IntegerDigits[#,128]],IntegerString[683248828,36]]]&[
FromDigits[ToCharacterCode@ExportString[
StringReplace[ToString@InputForm[Unevaluated@exp],StartOfString~~"Unevaluated["~~a:Longest[___]~~"]":>a]
,"Base64"],128]];

(* Base64 + Numberize 2*)
Obfuscate[exp_,5]:=HoldForm[ImportString[FromCharacterCode[IntegerDigits[#,80]+43],IntegerString[683248828,36]]]&[
FromDigits[ToCharacterCode@ExportString[
StringReplace[ToString@InputForm[Unevaluated@exp],StartOfString~~"Unevaluated["~~a:Longest[___]~~"]":>a]
,"Base64"]-43,80]];


End[];


EndPackage[];
