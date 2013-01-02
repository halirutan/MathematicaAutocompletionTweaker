(* :Title: Code for CamelCase expansion in Mathematica *)

(* :Context: FE` *)

(* :Author: halirutan *)

(* :Summary: 
    This provides a way to use an improved autocomplete with Mathematica 9.
*)

(* :Package Version: 1.0 *)

(* :Mathematica Version: 9.0 *)

(* :Copyright: Copyright 2012, halirutan  *)

(* :History:
*)

(* :Keywords:
    code assist, autocomplete
*)

(* :Limitations:  What I try to achieve here has several limitations which are mainly because the frontend of 
    Mathematica 9 is clever and does many things without asking the kernel. Additionally, it is not possible to influence
    everything as I might wish it.
 *)

(* :Discussion:      
    - Completion of local variable names in Modules or Blocks is completely done inside the front-end where it is hidden
    and cannot be changed (this part wouldn't be possible in the kernel anyway, because it doesn't know local names). 
    - I wasn't able to reengineer whether I can influence how often the front-end calls
    the the kernel for expansion suggestions (it's not done on every keystroke).
    By *asking the kernel* I mean calling FE`FC or FE`CAFC which gives the list of valid expansions back.
    Wolfram have tried to make this fast and therefore when I type "Li" and the suggestion window pops up, as long as 
    I type something which matches something in the suggestion list, the kernel is not asked again. 
    Unfortunately, the FE seem to think that when I type a character which is not in the current suggestions, 
    the symbol doesn't exists at all and therefore doesn't ask the kernel *on this keystroke*. 
    Example: typing "Li" gives a long list of all valid expansions. Appending then "LiL" (e.g. for ListLinePlot) 
    closes the suggestion box but does not ask the kernel again whether "LiL" can be expanded to a valid symbol.
    As long as you don't work with CamelCase expansion this seems to be the correct assumption. However, since we will
    use CamelCase it would be nice when the FE would ask the kernel about a valid expansion the moment we type the
    last "L" in "LiL". The consequence is that you have to type "LiLi" to get the suggestionbox with ListLinePlot 
    (I'm not sure whether this behaviour is consistent through all OS's).
    - Mathematica tries to guess the context to be able to call a special *options expanding function* when we are in the
    posisition where usually options are. In Mathematica 9 this can lead to unexpected behavior. Some users
    use  Sequence regularely and an simple example which tricks Mathematica is "Plot[Sequence[x,{x,0,1}], Plo..]".
    Here Mathematica would not try to expand options because it thinks we are still in position 2 where options
    would not be approbriate. But one cannot change whether the front-end calls function or options expansion and
    an options expansion call always expects an OptionCompletionsListPacket as answer.
    *)
Begin["FE`"];

(* ::Section:: *)
(* Functions for creating possible name-matches *)

(**
    This splits a word into its CamelCase parts. So e.g. ListLinePlot would be split into
    List, Line and Plot. It makes a cut at every capital letter or number-digit.
*)
camelCaseParts[in_String] := If[#==={}, {in}, #]&@StringCases[in,_?(UpperCaseQ[#]||DigitQ[#]&)~~___?LowerCaseQ];

(**
    getMatch will be the main-function to calculate valid CamelCase expansions. It will always test two things:
    (1) Does a normal expansion exist? E.g. ListLin could be expanded to ListLinePlot
    (2) Does a CamelCase expansion exist? E.g. ListLP could be expanded to ListLinePlot
*)
getMatch[""]={};

(**
    This function is called when the pattern contains a backtick which suggests that we are dealing with contexts.
    We don't do CamelCase expansion on context names since for most like Developer, Internal, Experimental, ...
    this would be useless. Nevertheless, once you have a valid context prefix you can use CamelCase for the functions.
    Therefore, Developer`TPA should give you Developer`ToPackedArray as choice.
*)
getMatch[in_String/;Not[StringFreeQ[in,"`"]], ignoreCase_]:=Module[
    {exactUnfinishedMatches,camelMatches,pattern, packetPattern},
    
    pattern=StringSplit[StringReplace[in,(Longest[start___]~~"`"~~patt___):>start<>"` "<>patt]];
    exactUnfinishedMatches=Names[in<>"*", IgnoreCase -> ignoreCase];
    If[Length[pattern]===1,
        packetPattern = First[pattern];
        camelMatches={},
        packetPattern = First[pattern]<>StringTake[Last[pattern],1];
        camelMatches=Union@Flatten@StringCases[Names[packetPattern <> "*"],
            StartOfString~~First[pattern]~~(StringExpression@@(#~~___?LowerCaseQ&/@camelCaseParts[Last[pattern]]))~~___]
    ];
    {packetPattern, Union@Flatten@Join[exactUnfinishedMatches,camelMatches]}
];

(**
    This is called when we are not dealing with context names.
*)
getMatch[in_String, ignoreCase_]:=Module[
    {exactUnfinishedMatches,packetPattern, camelMatches},
    
    exactUnfinishedMatches=Names[in<>"*", IgnoreCase -> ignoreCase];
    packetPattern = StringTake[in,1];
    camelMatches=Union@Flatten@StringCases[Names[packetPattern <> "*"],
        StartOfString~~(StringExpression@@(#~~___?LowerCaseQ&/@camelCaseParts[in]))~~___];
    {packetPattern, Union@Flatten@Join[exactUnfinishedMatches,camelMatches]}
];


(**
    This finds possible expansions to Options. It get's a list of options in string-form like 
    {"CharacterEncoding :> $CharacterEncoding", "Method -> Automatic", "Path :> $Path"}
    extracts the head of each rule and checks which one are a valid expansion of "pattern"
*)
getOptionsMatch[_,{}]:={};
getOptionsMatch[pattern_String,opts_List]:=Module[{
    optnames=StringReplace[opts,start_~~(" -> "|" :> ")~~__:>start],
    exactUnfinishedMatches,
    camelMatches},

    exactUnfinishedMatches=StringCases[optnames,StartOfString~~pattern~~__];
    camelMatches=StringCases[optnames,StartOfString~~(StringExpression@@(#~~___?LowerCaseQ&/@camelCaseParts[pattern]))~~___];
    Union@Flatten@StringCases[opts,#~~__&/@(Union@Flatten@Join[exactUnfinishedMatches,camelMatches])]
];

(* ::Section:: *)
(* Injecting the new completion functions into the front-end functions *)


(**
    In Mathematica Version 9 the autocomplete is different on Linux and Windows. While Windows (and MacOSX) call
    FE`CAFC to ask the kernel for some valid suggestions for an input pattern, in Linux it's the function FE`FC.
    The function to expand Options is in all systems FC`OC. 
*)
If[ $VersionNumber == 9  && (StringMatchQ[$SystemID, "MacOSX*"] || StringMatchQ[$SystemID, "Windows*"]),

If[ DownValues[RecoverDefaultCompletion] === {},
	$originalCAFC = DownValues[CAFC];
	$originalOC = DownValues[OC];

	RecoverDefaultCompletion[] := Block[{},
	    Unprotect[CAFC];
	    DownValues[CAFC] = $originalCAFC;
	    Protect[CAFC];
	    Unprotect[OC];
	    DownValues[OC] = $originalOC;
	    Protect[OC];
	];
];

Unprotect[CAFC];
CAFC[nameString_, ignoreCase_:False]/;$Notebooks:=
    MathLink`CallFrontEnd[
        FrontEnd`CompletionsListPacket[Sequence@@getMatch[nameString, ignoreCase], Contexts["*"<>(nameString<>"*")]],
        NoResult
    ];
Protect[CAFC];

Unprotect[OC];
OC[nameString_,patternString_]/;$Notebooks:=Module[
    {opts=(ToString[#1]&)/@(InputForm[#1]&)/@Options[ToExpression[nameString]]},
    MathLink`CallFrontEnd[
        FrontEnd`OptionCompletionsListPacket[nameString,"",getOptionsMatch[patternString,opts]],
        NoResult
    ]
];
Protect[OC];

]; (* end of Mac and Windows implementation *)

If[ $VersionNumber == 9 && StringMatchQ[$SystemID, "Linux*"],

If[ DownValues[RecoverDefaultCompletion] === {},
	$originalFC = DownValues[FC];
	$originalOC = DownValues[OC];

	RecoverDefaultCompletion[] := Block[{},
	    Unprotect[FC];
	    DownValues[FC] = $originalFC;
	    Protect[FC];
	    Unprotect[OC];
	    DownValues[OC] = $originalOC;
	    Protect[OC];
	];
];

Unprotect[FC];
FC[nameString_, ignoreCase_:False]/;$Notebooks:=
    MathLink`CallFrontEnd[
        FrontEnd`CompletionsListPacket[Sequence@@getMatch[nameString, ignoreCase], Contexts["*"<>(nameString<>"*")]],
        NoResult
    ];
Protect[FC];

Unprotect[OC];
OC[nameString_,patternString_]/;$Notebooks:=Module[
    {opts=(ToString[#1]&)/@(InputForm[#1]&)/@Options[ToExpression[nameString]]},
    MathLink`CallFrontEnd[
        FrontEnd`OptionCompletionsListPacket[nameString,"",getOptionsMatch[patternString,opts]],
        NoResult
    ]
];
Protect[OC];

]; (* end of Linux section *)



(* ::Section:: *)
(* New completion for older versions *)

(**
    The next is not tested under Windows but in versions < 9 it seems Mathematica called on all OS's the function
    FE`FC when you hit Ctrl+K to get an expansion.
*)
If[ $VersionNumber < 9,

If[ DownValues[RecoverDefaultCompletion] === {},
    $originalFC = DownValues[FC];

	RecoverDefaultCompletion[] := Block[{},
	    Unprotect[FC];
	    DownValues[FC] = $originalFC;
	    Protect[FC];
	];
];

Unprotect[FC];
FC[nameString_, ignoreCase_:False]/;$Notebooks:=
    MathLink`CallFrontEnd[
        FrontEnd`CompletionsListPacket[Sequence@@getMatch[nameString, ignoreCase], Contexts["*"<>(nameString<>"*")]],
        NoResult
    ];
Protect[FC];
 
];

End[];
