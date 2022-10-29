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
Obfuscate[exp_,1]:=Defer[Uncompress[FromCharacterCode[IntegerDigits[#,128]]]]&[
 FromDigits[ToCharacterCode@Compress@Unevaluated[exp],128]
];
Obfuscate[str_String?SyntaxQ,1]:=Defer[ToExpression@Uncompress[FromCharacterCode[IntegerDigits[#,128]]]]&[
 FromDigits[ToCharacterCode@Compress@Unevaluated[str],128]
];

(* Numberize 2 *)
Obfuscate[exp_,2]:=Module[{n,min,max},
 n=ToCharacterCode@Compress@Unevaluated[exp];
 min=Min[n];
 max=Max[n];
 If[min>0,
  Defer[Uncompress[FromCharacterCode[IntegerDigits[#1,#2]+#3]]]&[FromDigits[n-min,max-min+1],max-min+1,min],
  Defer[Uncompress[FromCharacterCode[IntegerDigits[#1,#2]]]]&[FromDigits[n-min,max-min+1],max-min+1]
 ]
];
Obfuscate[str_String?SyntaxQ,2]:=Module[{n,min,max},
 n=ToCharacterCode@Compress@Unevaluated[str];
 min=Min[n];
 max=Max[n];
 If[min>0,
  Defer[ToExpression@Uncompress[FromCharacterCode[IntegerDigits[#1,#2]+#3]]]&[FromDigits[n-min,max-min+1],max-min+1,min],
  Defer[ToExpression@Uncompress[FromCharacterCode[IntegerDigits[#1,#2]]]]&[FromDigits[n-min,max-min+1],max-min+1]
 ]
];

(* Base64 *)
Obfuscate[exp_,3]:=Obfuscate[Evaluate[ToString[Unevaluated@exp,InputForm]],3];
Obfuscate[str_String,3]:=Defer[ImportString[#,IntegerString[29^^base64,29]]]&[
 StringReplace[ExportString[str,"Base64"],"\n"->""]
];

(* Base64 + Numberize 1 *)
Obfuscate[exp_,4]:=Obfuscate[Evaluate[ToString[Unevaluated@exp,InputForm]],4];
Obfuscate[str_String,4]:=Defer[ImportString[FromCharacterCode[IntegerDigits[#,128]],IntegerString[29^^base64,29]]]&[
 FromDigits[ToCharacterCode@StringReplace[ExportString[str,"Base64"],"\n"->""],128]
]

(* Base64 + Numberize 2*)
Obfuscate[exp_,5]:=Obfuscate[Evaluate[ToString[Unevaluated@exp,InputForm]],5];
Obfuscate[str_String,5]:=Module[{n,min,max},
 n=ToCharacterCode@StringReplace[ExportString[str,"Base64"],"\n"->""];
 min=Min[n];
 max=Max[n];
 If[min>0,
  Defer[ImportString[FromCharacterCode[IntegerDigits[#1,#2]+#3],IntegerString[29^^base64,29]]]&[FromDigits[n-min,max-min+1],max-min+1,min],
  Defer[ImportString[FromCharacterCode[IntegerDigits[#1,#2]],IntegerString[29^^base64,29]]]&[FromDigits[n-min,max-min+1],max-min+1]
 ]
];

(* GZIP + Numberize 1 *)
Obfuscate[exp_,6]:=Obfuscate[Evaluate[ToString[Unevaluated@exp,InputForm]],6];
Obfuscate[str_String,6]:=Defer[ImportString[FromCharacterCode[IntegerDigits[#,256]],IntegerString[36^^gz,36]]]&[
 FromDigits[ToCharacterCode@ExportString[str,"GZIP"],256]
]

(* GZIP + Numberize 2*)
Obfuscate[exp_,7]:=Obfuscate[Evaluate[ToString[Unevaluated@exp,InputForm]],7];
Obfuscate[str_String,7]:=Module[{n,min,max},
 n=ToCharacterCode@ExportString[str,"GZIP"];
 min=Min[n];
 max=Max[n];
 If[min>0,
  Defer[ImportString[FromCharacterCode[IntegerDigits[#1,#2]+#3],IntegerString[36^^gz,36]]]&[FromDigits[n-min,max-min+1],max-min+1,min],
  Defer[ImportString[FromCharacterCode[IntegerDigits[#1,#2]],IntegerString[36^^gz,36]]]&[FromDigits[n-min,max-min+1],max-min+1]
 ]
];

(* Encode *)
Obfuscate[exp_,8]:=Obfuscate[Evaluate[ToString[Unevaluated@exp,InputForm]],8];
Obfuscate[str_String,8]:=Module[{tmp1,tmp2},
 tmp1=FileNameJoin[{$TemporaryDirectory,"temp.txt"}];
 tmp2=FileNameJoin[{$TemporaryDirectory,"temp_enc.txt"}];
 Export[tmp1,str,"Text"];
 Encode[tmp1,tmp2];
 With[{encoded=Import[tmp2,"Text"]},
  DeleteFile/@{tmp1,tmp2};
  Defer[(BinaryWrite[#,encoded];Get[#])&[
   OpenWrite[BinaryFormat->True][[1]]
  ]]
 ]
];

(* Encode + Numberize *)
Obfuscate[exp_,9]:=Obfuscate[Evaluate[ToString[Unevaluated@exp,InputForm]],9];
Obfuscate[str_String,9]:=Module[{tmp1,tmp2,chars,min,max},
 tmp1=FileNameJoin[{$TemporaryDirectory,"temp.txt"}];
 tmp2=FileNameJoin[{$TemporaryDirectory,"temp_enc.txt"}];
 Export[tmp1,str,"Text"];
 Encode[tmp1,tmp2];
 chars=ToCharacterCode[Import[tmp2,"Text"]];
 min=Min[chars];
 max=Max[chars];
 DeleteFile/@{tmp1,tmp2};
 With[{num=FromDigits[chars-min,max-min+1],radix=max-min+1,offset=min},
  Defer[(BinaryWrite[#,IntegerDigits[num,radix]+offset];Get[#])&[
   OpenWrite[BinaryFormat->True][[1]]
  ]]
 ]
];


End[];


EndPackage[];
