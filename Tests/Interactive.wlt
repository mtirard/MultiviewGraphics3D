(* Axis-locked interactive figure: structure, front-end construction rules, fallbacks, camera store, setters. *)
BeginTestSection["Interactive"]

PrependTo[$ContextPath, "MultiviewGraphics3D`Private`"];
box = Graphics3D[{Red, Cuboid[{0, 0, 0}, {4, 2, 1}]}, Boxed -> False];
quad = MultiviewGraphics3D[box];
$writable = ViewPoint | ViewVertical | ViewCenter | ViewAngle | "ViewSize";
panes[e_] := Cases[e, _Graphics3D, Infinity];
lockedQ[p_] := MemberQ[Flatten[{Lookup[Options[p], Method, {}]}], "RotationControl" -> None];

(* ---- structure ---- *)

VerificationTest[Head[quad], DynamicModule, TestID -> "Is-DynamicModule"]
VerificationTest[Length[panes[quad]], 4, TestID -> "Quad-four-panes"]
VerificationTest[Count[panes[quad], _?lockedQ], 3, TestID -> "Only-Free-rotates"]
VerificationTest[
 With[{n = MultiviewGraphics3D[box, "Net"]}, {Length[panes[n]], Count[panes[n], _?lockedQ]}], {6, 6},
 TestID -> "Net-all-locked"]
VerificationTest[Count[panes[quad], _?(Lookup[Options[#], SphericalRegion] === True &)], 4, TestID -> "SphericalRegion"]
VerificationTest[Sort[Cases[quad, Style[s_String, ___] :> s, Infinity]], Sort[{"Top", "Free", "Front", "Right"}], TestID -> "Labels"]

(* ---- front-end construction rules (prototypes/common.wl mvBuildProblems) ---- *)

VerificationTest[
 Flatten@Cases[ToBoxes[quad], Graphics3DBox[_, o___] :> Select[{o}, ! MatchQ[#, _Rule | _RuleDelayed] &], {0, Infinity}], {},
 TestID -> "Box-options-are-rules"]
VerificationTest[
 Union@Cases[Cases[quad, _Dynamic, Infinity],
   s_Symbol /; StringMatchQ[SymbolName[Unevaluated[s]], __ ~~ "$" ~~ DigitCharacter ..] :> SymbolName[Unevaluated[s]], Infinity, Heads -> True],
 {}, TestID -> "No-temporaries-in-Dynamic"]
VerificationTest[With[{wr = $writable}, Cases[quad, HoldPattern[(o : wr) -> Dynamic[_]] :> o, Infinity]], {},
 TestID -> "Every-camera-Dynamic-has-setter"]
VerificationTest[
 With[{wr = $writable}, Union[Cases[quad, HoldPattern[(o : wr) -> Dynamic[_, _]] :> o, Infinity]]],
 Sort[{ViewPoint, ViewVertical, ViewCenter, "ViewSize"}], TestID -> "Linked-options"]

(* ---- fallbacks: every Dynamic's first argument evaluates with the module variables unbound ---- *)

VerificationTest[
 With[{vals = Cases[quad /. HoldPattern[DynamicModule[_, body_, ___]] :> body, Dynamic[x_, ___] :> x, Infinity]},
  AllTrue[vals, VectorQ[#, NumericQ] || NumericQ[#] || MatchQ[#, {{_?NumericQ, _?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}] &]],
 True, TestID -> "Fallbacks-unbound"]

VerificationTest[
 With[{p = MultiviewGraphics3D[box, ViewProjection -> "Perspective"]},
  {Cases[p, HoldPattern[ViewAngle -> Dynamic[_, _]], Infinity] =!= {},
   Count[panes[p], _?(KeyExistsQ[Association[Options[#]], "ViewSize"] &)]}],
 {True, 3}, TestID -> "Perspective-Free-zooms-with-ViewAngle"]

(* static render of the built figure with bound variables *)
VerificationTest[
 ImageQ@Rasterize[(quad /. HoldPattern[DynamicModule[{Set[a_, x1_], Set[b_, x2_], Set[c_, x3_], Set[d_, x4_]}, body_, ___]] :>
      (body /. {a -> x1, b -> x2, c -> x3, d -> x4})) //. Dynamic[x_, ___] :> x],
 True, TestID -> "Static-render"]

(* ---- camera store ---- *)

VerificationTest[Cases[quad, _CurrentValue, Infinity], {}, TestID -> "Top-level-no-store"]

VerificationTest[
 Block[{anchorBox = ("box" &), storeCell = ("cell" &)}, $storeOrdinal["box"] = 0;
  Union[Cases[{MultiviewGraphics3D[box], MultiviewGraphics3D[box]}, {"cell", {TaggingRules, "MultiviewGraphics3D", k_String}} :> k, Infinity]]],
 Sort[{ToString[{Hash["box"], 1}], ToString[{Hash["box"], 2}]}], TestID -> "Store-key-per-call"]

VerificationTest[
 Block[{anchorBox = ("box" &), storeCell = ("cell" &)}, $storeOrdinal["box"] = 0;
  Cases[MultiviewGraphics3D[box, PreserveImageOptions -> False], {"cell", {TaggingRules, __}}, Infinity]],
 {}, TestID -> "PreserveImageOptions-False"]

VerificationTest[storeGet[None, "Zoom", NumericQ, 7], 7, TestID -> "Store-local-default"]

(* ---- setters ---- *)

VerificationTest[Module[{x = {0, 0, 1.}}, setVector[x, None, "k", {0, 0, 1. + 10^-12}]; x], {0, 0, 1.}, TestID -> "Vector-tolerance"]
VerificationTest[Module[{x = {0, 0, 1.}}, setVector[x, None, "k", {0, 1., 1.}]; x], {0, 1., 1.}, TestID -> "Vector-change"]
VerificationTest[Module[{x}, setVector[x, None, "k", {1, 2, 3}]; x], {1, 2, 3}, TestID -> "Vector-from-unbound"]

front = {{1, 0, 0}, {0, 0, 1}};
VerificationTest[Module[{w = {0, 0, 0}}, setPan[w, front, None, {{0.5, 0.5, 0.5}, {0.6, 0.5}}]; w], {0.1, 0, 0},
 SameTest -> Equal, TestID -> "Pan-moves-in-view-plane"]
VerificationTest[
 Module[{w = {0.1, 0.2, -0.3}, w0}, w0 = w; setPan[w, front, None, {{0.5, 0.5, 0.5}, {1, 1}/2 + front . w}]; w === w0],
 True, TestID -> "Pan-write-back-no-change"]
VerificationTest[Module[{w = {0, 0, 0}}, setPan[w, front, None, {0.5, 0.5, 0.5}]; w], {0, 0, 0}, TestID -> "Pan-other-form-ignored"]

VerificationTest[Module[{vs = 2.}, setZoom[vs, None, 2. + 10^-12]; setZoom[vs, None, "x"]; vs], 2., TestID -> "Zoom-tolerance"]
VerificationTest[Module[{vs = 2.}, setZoom[vs, None, 1.5]; vs], 1.5, TestID -> "Zoom-change"]
VerificationTest[Module[{vs = 2.}, setAngle[vs, 1, 3, None, 2 ArcTan[2./(2 3)]]; vs], 2., SameTest -> Equal, TestID -> "Angle-round-trip"]

(* ---- figure-level features ---- *)

VerificationTest[
 With[{f = MultiviewGraphics3D[Legended[Graphics3D[Cuboid[], PlotLabel -> "T"], "leg"]]}, {Head[f], Head[First[f]], Head[First[First[f]]]}],
 {Legended, Labeled, DynamicModule}, TestID -> "Wrappers-and-title"]

EndTestSection[]
