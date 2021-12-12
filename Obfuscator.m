(* ::Package:: *)

(* :Mathematica Version: 8.0 *)

(* :Name: Obfuscation` *)

(* :Title: Mathematica Code Obfuscator *)

(* :Author: Kei Misawa *)

(* :Summary:
  This package provides code obfuscation function.

 This software is released under the MIT License.
  http://opensource.org/licenses/mit-license.php
*)

(* :Context: Obfuscation` *)

(* :Package Version: 1.0 *)

(* :Copyright: Copyright 2021, Kei Misawa *)

(* :Keywords: code obfuscation *)

(* :Warning:  *)

(* :Sources: 
*)


BeginPackage["Obfuscation`"];


Obfuscate::usage="Obfuscate[expression] returns obfuscated Mathematica code string.
Use ToExpression in order to evaluate the obfuscated string, or copy and paste the code.";


Begin["`Private`"];


Attributes[Obfuscate]={HoldFirst,ReadProtected};
Obfuscate[exp_]:=Obfuscate[exp,1];

Obfuscate[exp_,1]:="Uncompress[FromCharacterCode[IntegerDigits["<>
ToString[FromDigits[ToCharacterCode@Compress@Unevaluated[exp],128]]<>",128]]]";

Obfuscate[exp_,2]:="Uncompress[FromCharacterCode[IntegerDigits["<>
ToString[FromDigits[ToCharacterCode@Compress@Unevaluated[exp]-43,80]]<>",80]+43]]";


End[];


EndPackage[];
