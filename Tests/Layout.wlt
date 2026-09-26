(* Layout expansion: layout spec + projection convention -> matrix of pane specs. *)
BeginTestSection["Layout"]

(* ---- named layouts, third angle ---- *)

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["Net", "ThirdAngle"],
 {{None, Above, None, None}, {Left, Front, Right, Back}, {None, Below, None, None}},
 TestID -> "Net-ThirdAngle"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["ThreeView", "ThirdAngle"],
 {{Above, None}, {Front, Right}},
 TestID -> "ThreeView-ThirdAngle"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["QuadView", "ThirdAngle"],
 {{Above, "Free"}, {Front, Right}},
 TestID -> "QuadView-ThirdAngle"]

(* ---- named layouts, first angle ---- *)

(* Q12: Back stays at the right end of the side row in both conventions. *)
VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["Net", "FirstAngle"],
 {{None, Below, None, None}, {Right, Front, Left, Back}, {None, Above, None, None}},
 TestID -> "Net-FirstAngle"]

(* SOLIDWORKS "Standard 3 Views" (prior-art/projection-conventions.md): Front upper left, Top below, Left right. *)
VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["ThreeView", "FirstAngle"],
 {{Front, Left}, {Above, None}},
 TestID -> "ThreeView-FirstAngle"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["QuadView", "FirstAngle"],
 {{Front, Left}, {Above, "Free"}},
 TestID -> "QuadView-FirstAngle"]

(* ---- isometric: corner vectors, arranged as seen from above (+y back row on top); convention ignored ---- *)

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["Isometric", "ThirdAngle"],
 {{{-1, 1, 1}, {1, 1, 1}}, {{-1, -1, 1}, {1, -1, 1}}},
 TestID -> "Isometric"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{"Isometric", Above}, "FirstAngle"],
 MultiviewGraphics3D`Private`layoutMatrix["Isometric", "ThirdAngle"],
 TestID -> "Isometric-Above-same-as-plain"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{"Isometric", Below}, "ThirdAngle"],
 {{{-1, 1, -1}, {1, 1, -1}}, {{-1, -1, -1}, {1, -1, -1}}},
 TestID -> "Isometric-Below"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{"Isometric", All}, "ThirdAngle"],
 {{{-1, 1, 1}, {1, 1, 1}, {-1, 1, -1}, {1, 1, -1}}, {{-1, -1, 1}, {1, -1, 1}, {-1, -1, -1}, {1, -1, -1}}},
 TestID -> "Isometric-All"]

(* ---- user matrices: placed as written, convention ignored (Q17) ---- *)

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{{Front, {1, 2, 3}}, {None, "Free"}}, "FirstAngle"],
 {{Front, {1, 2, 3}}, {None, "Free"}},
 TestID -> "Matrix-as-written"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{{Front, Top}}, "ThirdAngle"],
 Failure["InvalidPaneSpec", <|"MessageTemplate" :> MultiviewGraphics3D::pane, "MessageParameters" -> {Top}|>],
 TestID -> "Matrix-bad-pane"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{{"Free", Front}, {"Free", Back}}, "ThirdAngle"],
 Failure["MultipleFree", <|"MessageTemplate" :> MultiviewGraphics3D::free, "MessageParameters" -> {2}|>],
 TestID -> "Matrix-two-Free"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{{Front, Back}, {Left}}, "ThirdAngle"],
 Failure["InvalidLayout", <|"MessageTemplate" :> MultiviewGraphics3D::layout, "MessageParameters" -> {{{Front, Back}, {Left}}}|>],
 TestID -> "Matrix-ragged"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{{None, None}}, "ThirdAngle"],
 Failure["EmptyLayout", <|"MessageTemplate" :> MultiviewGraphics3D::empty, "MessageParameters" -> {}|>],
 TestID -> "Matrix-all-None"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix[{{Front, {0, 0, 0}}}, "ThirdAngle"],
 Failure["InvalidPaneSpec", <|"MessageTemplate" :> MultiviewGraphics3D::pane, "MessageParameters" -> {{0, 0, 0}}|>],
 TestID -> "Matrix-zero-vector"]

VerificationTest[
 MultiviewGraphics3D`Private`layoutMatrix["Cube", "ThirdAngle"],
 Failure["InvalidLayout", <|"MessageTemplate" :> MultiviewGraphics3D::layout, "MessageParameters" -> {"Cube"}|>],
 TestID -> "Unknown-name"]

EndTestSection[]
