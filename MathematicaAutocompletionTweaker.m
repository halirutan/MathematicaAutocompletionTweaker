Begin["FE`"];

AutocompletionTweaker::wrongvers = "This is currently only supported with Mathematica version 9.0";

If[ $VersionNumber != 9,
    Message[AutocompletionTweaker::wrongvers];
    Abort[]
];

RecoverDefaultCompletion[] := Null;

camelParts[in_String]:=StringCases[in,_?(UpperCaseQ[#]||DigitQ[#]&)~~___?LowerCaseQ];

(*

(* Matching files will not work due to how the auto-completion of Mathematica is implemented !! *)

getFileMatch[in_String] := Module[{file = StringDrop[in, 2]},
  file = StringReplace[file, "`" :> $PathnameSeparator];
  (StringReplace[#,$PathnameSeparator:>"`"]<>"`")&/@
  If[file != "" && DirectoryQ[file],
    FileNames["*", {file}],
    FileNames[FileNameTake[file] <> "*", {DirectoryName[file]}]
    ]
  ]

*)

getMatch[""]={};

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

getMatch[in_String, ignoreCase_]:=Module[
    {exactUnfinishedMatches,packetPattern, camelMatches},
    
    exactUnfinishedMatches=Names[in<>"*", IgnoreCase -> ignoreCase];
    packetPattern = StringTake[in,1];
    camelMatches=StringCases[Names[packetPattern <> "*"],
        StartOfString~~(StringExpression@@(#~~___?LowerCaseQ&/@camelParts[in]))~~___];
    {StringTake[in,1], Union@Flatten@Join[exactUnfinishedMatches,camelMatches]}
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

(*

(* Matching files will not work due to how the auto-completion of Mathematica is implemented !! *)


FC[nameString_/;StringMatchQ[nameString,"``"~~___], _]/;$Notebooks:=
    MathLink`CallFrontEnd[
        FrontEnd`CompletionsListPacket["",
            {StringDrop[StringReplace[nameString, "`":>$PathnameSeparator],2]}, getFileMatch[nameString]],
        NoResult
    ];

*)

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
