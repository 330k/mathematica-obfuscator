# Summary
This package provides obfuscation function for _Mathematica_.

# Installation
 To install the package, 
+ download the zip file,
+ extract,
+ move "Obfuscator" directory to the directory _Mathematica_ can find such as `$UserBaseDirectory` or another one of `$Path`.

# Usage
 In your _Mathematica_ session,
+ evaluate ``<<Obfuscator` `` to load this package,
+ to obfuscate _Mathematica_ code, `Obfuscate[(* your Mathematica code here *)]`,
+ to evaluate the obfuscated code, copy and paste the returned string.

# Example

+ Code to be obfuscated: `4.*^-6 Count[RandomReal[1, {1*^6, 2}], a_ /; a.a < 1]`
+ Obfuscate: `Obfuscate[4.*^-6 Count[RandomReal[1, {1*^6, 2}], a_ /; a.a < 1]]`
+ Obfuscated code: `"Uncompress[FromCharacterCode[IntegerDigits[\
4681799545987174809762695842959710762296946230746297311013470597620197\
0035738025847466689144829325302193942109481053093982587869403703284666\
0592882958441376098663135539936564414610210943357614966862099944259869\
3851480102199669036062981733182848756146329941259780596301351298669624\
6675003600897843131469292076930487616405397961372968205878018828467435\
2331264464575929238350525,128]]]"`
+ Execute: `FromCharacterCode[IntegerDigits[\
4681799545987174809762695842959710762296946230746297311013470597620197\
0035738025847466689144829325302193942109481053093982587869403703284666\
0592882958441376098663135539936564414610210943357614966862099944259869\
3851480102199669036062981733182848756146329941259780596301351298669624\
6675003600897843131469292076930487616405397961372968205878018828467435\
2331264464575929238350525,128]]`
+ Output: `3.14017`

# License
 This software is released under the MIT License.
