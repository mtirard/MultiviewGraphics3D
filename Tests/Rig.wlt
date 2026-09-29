(* Rig-locked interactive figure: one rig rotation r turns every pane's camera (glass box); pan and zoom linked as in
   axis-locked mode. Rig maths in display coordinates (FINDINGS p6). *)
BeginTestSection["Rig"]

PrependTo[$ContextPath, "MultiviewGraphics3D`Private`"];
box = Graphics3D[{Red, Cuboid[{0, 0, 0}, {4, 2, 1}]}, Boxed -> False];
rig = MultiviewGraphics3D[box, "CameraInteraction" -> "RigLocked"];
$writable = ViewPoint | ViewVertical | ViewCenter | ViewAngle | "ViewSize";
panes[e_] := Cases[e, _Graphics3D, Infinity];
lockedQ[p_] := MemberQ[Flatten[{Lookup[Options[p], Method, {}]}], "RotationControl" -> None];
rotationQ[m_] := MatrixQ[m, NumericQ] && Norm[m . Transpose[m] - IdentityMatrix[3]] < 10^-9 && Abs[Det[m] - 1] < 10^-9;

(* ---- rig maths ---- *)

VerificationTest[rigFrame[{0, -1, 0}, {0, 0, 1}], {{1, 0, 0}, {0, 0, 1}, {0, -1, 0}}, SameTest -> Equal, TestID -> "Frame-Front"]

(* The rotation taking a pane's base camera to a new camera: base direction -> new direction, base up -> new up. *)
VerificationTest[
 With[{m = rigFromCamera[IdentityMatrix[3], {{0, -1, 0}, {0, 0, 1}}, {{1, 0, 0}, {0, 0, 1}}]},
  {rotationQ[m], m . {0, -1, 0}, m . {0, 0, 1}}],
 {True, {1, 0, 0}, {0, 0, 1}}, SameTest -> Equal, TestID -> "From-camera"]

(* A non-orthogonal new up (as the front end writes it) is orthogonalized. *)
VerificationTest[
 rotationQ[rigFromCamera[IdentityMatrix[3], {{0, -1, 0}, {0, 0, 1}}, {{1, -1, 1}, {0.2, 0.1, 1}}]], True, TestID -> "Orthogonalized"]

(* Degenerate camera (direction along up): keep r. *)
VerificationTest[
 rigFromCamera[RotationMatrix[0.3, {0, 0, 1}], {{0, -1, 0}, {0, 0, 1}}, {{0, 0, 2}, {0, 0, 1}}], RotationMatrix[0.3, {0, 0, 1}],
 TestID -> "Degenerate-keeps-r"]

(* Round trip (p6): every pane's displayed camera gives back r. *)
VerificationTest[
 With[{r = RotationMatrix[0.7, Normalize[{1, 2, 3}]]},
  AllTrue[{Front, Above, Right, {1, -1, 1}}, With[{v = paneView[#]}, Norm[rigFromCamera[r, {v["Direction"], v["Vertical"]}, {r . v["Direction"], r . v["Vertical"]}] - r] < 10^-12] &]],
 True, TestID -> "Round-trip"]

VerificationTest[
 Module[{r = IdentityMatrix[3]}, setRig[r, None, IdentityMatrix[3] + 10^-12]; r], IdentityMatrix[3], TestID -> "Rig-tolerance"]
VerificationTest[
 Module[{r = IdentityMatrix[3]}, setRig[r, None, RotationMatrix[0.1, {0, 0, 1}]]; r], RotationMatrix[0.1, {0, 0, 1}], TestID -> "Rig-change"]
VerificationTest[Module[{r = IdentityMatrix[3]}, setRig[r, None, {{1, 2}, {3, 4}}]; r], IdentityMatrix[3], TestID -> "Rig-bad-ignored"]

(* ---- structure ---- *)

VerificationTest[{Head[rig], Length[panes[rig]], Count[panes[rig], _?lockedQ]}, {DynamicModule, 4, 0}, TestID -> "All-panes-rotate"]
VerificationTest[
 With[{wr = $writable}, {Cases[rig, HoldPattern[(o : wr) -> Dynamic[_]] :> o, Infinity],
   Union[Cases[rig, HoldPattern[(o : wr) -> Dynamic[_, _]] :> o, Infinity]]}],
 {{}, Sort[{ViewPoint, ViewVertical, ViewCenter, "ViewSize"}]}, TestID -> "Linked-options-with-setters"]
VerificationTest[
 Union@Cases[Cases[rig, _Dynamic, Infinity],
   s_Symbol /; StringMatchQ[SymbolName[Unevaluated[s]], __ ~~ "$" ~~ DigitCharacter ..] :> SymbolName[Unevaluated[s]], Infinity, Heads -> True],
 {}, TestID -> "No-temporaries-in-Dynamic"]
VerificationTest[
 Flatten@Cases[ToBoxes[rig], Graphics3DBox[_, o___] :> Select[{o}, ! MatchQ[#, _Rule | _RuleDelayed] &], {0, Infinity}], {},
 TestID -> "Box-options-are-rules"]

(* Rig lighting (the default here) follows r; Camera lighting is static. *)
VerificationTest[Count[panes[rig], _?(MatchQ[Lookup[Options[#], Lighting], _Dynamic] &)], 4, TestID -> "Rig-lighting-dynamic"]
VerificationTest[
 Count[panes[MultiviewGraphics3D[box, "CameraInteraction" -> "RigLocked", "LightingAttachment" -> "Camera"]],
  _?(MatchQ[Lookup[Options[#], Lighting], _Dynamic] &)], 0, TestID -> "Camera-lighting-static"]

(* ---- fallbacks with the module variables unbound: System functions only, valid values ---- *)

dynBodies = Cases[rig /. HoldPattern[DynamicModule[_, body_, ___]] :> body, Dynamic[x_, ___] :> x, Infinity];
VerificationTest[
 AllTrue[dynBodies, VectorQ[#, NumericQ] || NumericQ[#] || MatchQ[#, {{_?NumericQ, _?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}}] ||
    MatchQ[#, {(_AmbientLight | _DirectionalLight) ..}] &],
 True, TestID -> "Fallbacks-unbound"]
VerificationTest[
 Union[Cases[Cases[rig, Dynamic[x_, ___] :> Hold[x], Infinity], s_Symbol /; Context[s] === "MultiviewGraphics3D`Private`" &&
     ! StringMatchQ[SymbolName[s], "r" | "w" | "vs" | (__ ~~ "$")] :> SymbolName[s], Infinity, Heads -> True]],
 {}, TestID -> "Fallbacks-System-only"]

(* displayed ViewPoint with r bound follows the rig *)
VerificationTest[
 With[{rot = RotationMatrix[Pi/2, {0, 0, 1}]},
  Cases[rig, HoldPattern[ViewPoint -> Dynamic[e_, _]] :> Normalize[(Hold[e] /. HoldPattern[MultiviewGraphics3D`Private`r] :> rot) // ReleaseHold], Infinity]],
 Normalize /@ (RotationMatrix[Pi/2, {0, 0, 1}] . # & /@ {{0, 0, 1}, {1, -1, 1}, {0, -1, 0}, {1, 0, 0}}),
 SameTest -> Equal, TestID -> "ViewPoint-follows-rig"]

(* store keys *)
VerificationTest[
 Block[{anchorBox = ("box" &), storeCell = ("cell" &)}, $storeOrdinal["box"] = 0;
  MemberQ[MultiviewGraphics3D[box, "CameraInteraction" -> "RigLocked"], "Rig", Infinity]],
 True, TestID -> "Store-has-rig"]

(* Graphics3D style options passed to the function reach every pane, over the input's own, with no messages (Q23, issue 1). *)
VerificationTest[
 Union[Lookup[Options[#], {Boxed, Axes}] & /@
   panes[MultiviewGraphics3D[box, "ThreeView", Boxed -> True, Axes -> True, "CameraInteraction" -> "RigLocked"]]],
 {{True, True}}, TestID -> "Style-options-every-pane"]

EndTestSection[]
