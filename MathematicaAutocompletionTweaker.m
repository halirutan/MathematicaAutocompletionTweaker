Begin["FE`"];

AutocompletionTweaker::wrongvers = "This is currently only supported with Mathematica version 9.0";

If[ $VersionNumber != 9,
    Message[AutocompletionTweaker::wrongvers];
    Abort[]
];

camelParts[in_String]:=StringCases[in,_?(UpperCaseQ[#]||DigitQ[#]&)~~___?LowerCaseQ];

getMatch[""]={};
getMatch[in_String, ignoreCase_]:=Module[
    {exactUnfinishedMatches,packetPattern, camelMatches},
    
    exactUnfinishedMatches=Names[in<>"*", IgnoreCase -> ignoreCase];
    packetPattern = StringTake[in,1];
    camelMatches=StringCases[Names[packetPattern <> "*"],
        StartOfString~~(StringExpression@@(#~~___?LowerCaseQ&/@camelParts[in]))~~___];
    {StringTake[in,1], Union@Flatten@Join[exactUnfinishedMatches,camelMatches]}
];

getMatch[in_String/;Not[StringFreeQ[in,"`"]], ignoreCase_]:=Module[
    {exactUnfinishedMatches,camelMatches,pattern, packetPattern},
    
    pattern=StringSplit[StringReplace[in,(Longest[start___]~~"`"~~patt___):>start<>"` "<>patt]];
    exactUnfinishedMatches=Names[in<>"*", IgnoreCase -> ignoreCase];
    If[Length[pattern]===1,
        packetPattern = pattern;
        camelMatches={},
        packetPattern = First[pattern]<>StringTake[Last[pattern],1];
        camelMatches=StringCases[Names[packetPattern <> "*"],
            StartOfString~~First[pattern]~~(StringExpression@@(#~~___?LowerCaseQ&/@camelParts[Last[pattern]]))~~___]
    ];
    {packetPattern, Union@Flatten@Join[exactUnfinishedMatches,camelMatches]}
];

getOptionsMatch[_,{}]:={};
getOptionsMatch[pattern_String,opts_List]:=Module[{
    optnames=StringReplace[opts,start_~~(" -> "|" :> ")~~__:>start],
    exactUnfinishedMatches,
    camelMatches},

    exactUnfinishedMatches=StringCases[optnames,StartOfString~~pattern~~__];
    camelMatches=StringCases[optnames,StartOfString~~(StringExpression@@(#~~___?LowerCaseQ&/@camelParts[pattern]))~~___];
    Union@Flatten@StringCases[opts,#~~__&/@(Union@Flatten@Join[exactUnfinishedMatches,camelMatches])]
];

If[ StringMatchQ[$SystemID, "MacOSX*"] || StringMatchQ[$SystemID, "Windows*"],

Null

]; (* end of Mac and Windows implementation *)

If[ StringMatchQ[$SystemID, "Linux*"],

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

End[];