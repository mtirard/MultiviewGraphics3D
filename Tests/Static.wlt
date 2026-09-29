(* Static figure: cell placement, view labels, and the assembled Graphics of Insets (Q5, Q18, Q20, Q22-Q25). *)
BeginTestSection["Static"]

PrependTo[$ContextPath, "MultiviewGraphics3D`Private`"];

(* ---- placement: y up, row 1 on top; each row has a label strip above its cells ---- *)

VerificationTest[
 cellPlacement[<|"ColumnWidths" -> {2, 1}, "RowHeights" -> {1, 3}|>, 0.5, 0.25],
 <|"Centers" -> {{{1, 4.25}, {3, 4.25}}, {{1, 1.5}, {3, 1.5}}},
  "LabelPositions" -> {{{1, 4.875}, {3, 4.875}}, {{1, 3.125}, {3, 3.125}}},
  "Size" -> {3.5, 5}|>,
 SameTest -> Equal, TestID -> "Placement"]

VerificationTest[
 cellPlacement[<|"ColumnWidths" -> {1}, "RowHeights" -> {1}|>, 0.5, 0],
 <|"Centers" -> {{{0.5, 0.5}}}, "LabelPositions" -> {{{0.5, 1}}}, "Size" -> {1, 1}|>,
 SameTest -> Equal, TestID -> "Placement-no-labels"]

(* ---- automatic view labels (design-decisions: Automatic view labels) ---- *)

VerificationTest[
 automaticLabel[#, False, {1, -1, 1}] & /@ {Above, Below, Front, Back, Left, Right},
 {"Top", "Bottom", "Front", "Rear", "Left", "Right"}, TestID -> "Named-labels"]
VerificationTest[automaticLabel[{2, 2, -2}, False, {1, -1, 1}], "Isometric", TestID -> "Corner-vector"]
VerificationTest[automaticLabel[{1, 2, 3}, False, {1, -1, 1}], None, TestID -> "Other-vector"]
VerificationTest[automaticLabel["Free", True, {1, -1, 1}], "Free", TestID -> "Free-interactive"]
VerificationTest[automaticLabel["Free", False, {1, -1, 1}], "Isometric", TestID -> "Free-static-default"]
VerificationTest[automaticLabel["Free", False, {1, 2, 3}], None, TestID -> "Free-static-other"]

VerificationTest[viewLabel[Front, None, False, {1, -1, 1}], None, TestID -> "Labels-None"]
VerificationTest[viewLabel[Front, <|Front -> "F"|>, False, {1, -1, 1}], "F", TestID -> "Labels-assoc"]
VerificationTest[viewLabel[Back, <|Front -> "F"|>, False, {1, -1, 1}], None, TestID -> "Labels-assoc-missing"]
VerificationTest[viewLabel[Left, ToString, False, {1, -1, 1}], "Left", TestID -> "Labels-function"]

(* ---- the assembled figure ---- *)

box = Graphics3D[{Red, Cuboid[{0, 0, 0}, {4, 2, 1}]}, Boxed -> False];
net = MultiviewGraphics3D[box, "Net", "CameraInteraction" -> None];

VerificationTest[Head[net], Graphics, TestID -> "Static-is-Graphics"]
VerificationTest[Length[Cases[net, Inset[Deploy[_Graphics3D], ___], Infinity]], 6, TestID -> "Net-six-deployed-panes"]
VerificationTest[
 Sort[Cases[net, Text[Style[s_String, ___], ___] :> s, Infinity]], Sort[{"Top", "Left", "Front", "Right", "Rear", "Bottom"}],
 TestID -> "Net-labels"]
VerificationTest[
 Union[Cases[net, Graphics3D[_, o___] :> Lookup[{o}, PlotInteractivity], Infinity]], {False},
 TestID -> "Panes-no-interactivity"]

(* one scale: ViewSize / smaller inset side (points) is the same k / (points per unit) in every pane (FINDINGS p40) *)
VerificationTest[
 Length[Union[Round[Cases[net, Inset[Deploy[p_Graphics3D], _, _, sz_] :> Lookup[Options[p], "ViewSize"]/Min[sz], Infinity], 10.^-9]]],
 1, TestID -> "One-scale"]

(* figure-level features, reapplied once *)
VerificationTest[
 With[{f = MultiviewGraphics3D[Framed[Legended[Graphics3D[Cuboid[], PlotLabel -> "T", Background -> Yellow], "leg"]],
     "ThreeView", "CameraInteraction" -> None]},
  {Head[f], Head[First[f]], Head[First[First[f]]], OptionValue[Graphics, Options[First[First[f]]], {PlotLabel, Background}],
   Union[Cases[f, Graphics3D[_, o___] :> Lookup[{o}, Background], Infinity]]}],
 {Framed, Legended, Graphics, {"T", Yellow}, {None}},
 TestID -> "Figure-features"]

VerificationTest[
 MultiviewGraphics3D[Disk[], "Net", "CameraInteraction" -> None], $Failed,
 {MultiviewGraphics3D::input}, TestID -> "Bad-input-message"]

VerificationTest[
 MultiviewGraphics3D[box, {{"Free", "Free"}}, "CameraInteraction" -> None], $Failed,
 {MultiviewGraphics3D::free}, TestID -> "Bad-layout-message"]

(* Graphics3D style options passed to the function reach every pane, over the input's own, with no messages (Q23, issue 1). *)
VerificationTest[
 Union[Cases[MultiviewGraphics3D[box, "ThreeView", Boxed -> True, Axes -> True, "CameraInteraction" -> None],
   Graphics3D[_, o___] :> Lookup[{o}, {Boxed, Axes}], Infinity]],
 {{True, True}}, TestID -> "Style-options-every-pane"]

EndTestSection[]
