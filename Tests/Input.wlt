(* Input normalization: input -> <|"Graphic" -> core Graphics3D, "Wrappers" -> {{head, {args}}, ...} (outermost first),
   "PlotLabel" -> label | None, "Background" -> colour | None|>, or a Failure. Design Q22-Q25, FINDINGS p4/p4b. *)
BeginTestSection["Input"]

norm = MultiviewGraphics3D`Private`normalizeInput;
cube = Graphics3D[{Red, Cuboid[]}, Boxed -> False];

VerificationTest[norm[cube], <|"Graphic" -> cube, "Wrappers" -> {}, "PlotLabel" -> None, "Background" -> None|>,
 TestID -> "Plain-Graphics3D"]

(* ---- figure-level options are taken out of the pane graphic ---- *)

VerificationTest[
 norm[Graphics3D[Cuboid[], PlotLabel -> "T", Background -> Yellow, Boxed -> False]],
 <|"Graphic" -> Graphics3D[Cuboid[], Boxed -> False], "Wrappers" -> {}, "PlotLabel" -> "T", "Background" -> Yellow|>,
 TestID -> "PlotLabel-Background"]

(* Figure size options and camera/framing options on the input are dropped (Q23, Q24). *)
VerificationTest[
 norm[Graphics3D[Cuboid[], ImageSize -> 300, ImagePadding -> 5, ImageMargins -> 2, ViewPoint -> {1, 2, 3},
   ViewVertical -> {1, 0, 0}, ViewAngle -> 0.3, ViewCenter -> {0, 0, 0}, ViewProjection -> "Orthographic",
   SphericalRegion -> True, ViewVector -> {{5, 5, 5}, {0, 0, 0}}, ViewMatrix -> Automatic, ViewRange -> All,
   Axes -> True]]["Graphic"],
 Graphics3D[Cuboid[], Axes -> True],
 TestID -> "Drop-size-and-camera"]

(* ---- wrappers ---- *)

VerificationTest[
 norm[Legended[cube, "leg"]],
 <|"Graphic" -> cube, "Wrappers" -> {{Legended, {"leg"}}}, "PlotLabel" -> None, "Background" -> None|>,
 TestID -> "Legended"]

VerificationTest[
 norm[Framed[Labeled[Style[Legended[cube, "leg"], FontSize -> 9], "L", Top], FrameStyle -> Red]]["Wrappers"],
 {{Framed, {FrameStyle -> Red}}, {Labeled, {"L", Top}}, {Style, {FontSize -> 9}}, {Legended, {"leg"}}},
 TestID -> "Nested-wrappers-outermost-first"]

(* A real legend from PlotLegends. *)
VerificationTest[
 With[{r = norm[Plot3D[{x y, -x y}, {x, 0, 1}, {y, 0, 1}, PlotLegends -> "Expressions", PlotPoints -> 5]]},
  {Head[r["Graphic"]], r["Wrappers"][[All, 1]]}],
 {Graphics3D, {Legended}},
 TestID -> "PlotLegends"]

(* ---- other heads ---- *)

VerificationTest[Head[norm[Graph3D[{1 -> 2, 2 -> 3}]]["Graphic"]], Graphics3D, TestID -> "Graph3D"]
VerificationTest[Head[norm[DiscretizeRegion[Ball[]]]["Graphic"]], Graphics3D, TestID -> "MeshRegion"]
VerificationTest[Head[norm[Region[Cuboid[]]]["Graphic"]], Graphics3D, TestID -> "Region"]
VerificationTest[norm[Ball[]]["Graphic"], Graphics3D[Ball[]], TestID -> "Ball"]
VerificationTest[norm[Cuboid[]]["Graphic"], Graphics3D[Cuboid[]], TestID -> "Bare-3D-primitive"]
VerificationTest[
 With[{p = Polyhedron[{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}, {0, 0, 1}}, {{1, 2, 3}, {1, 2, 4}, {1, 3, 4}, {2, 3, 4}}]},
  norm[p]["Graphic"] === Graphics3D[p]],
 True, TestID -> "Polyhedron"]
VerificationTest[
 With[{r = Raster3D[RandomReal[1, {2, 2, 2}]]}, norm[r]["Graphic"] === Graphics3D[r]],
 True, TestID -> "Raster3D"]
VerificationTest[
 Head[norm[Molecule[{"O", "H", "H"}, {Bond[{1, 2}], Bond[{1, 3}]}]]["Graphic"]], Graphics3D, TestID -> "Molecule"]
(* Image3D: Show, plus Image3D's own look (no box). Native Image3D panes lose linked zoom and orthographic projection (i20). *)
VerificationTest[
 With[{g = norm[Image3D[RandomReal[1, {3, 3, 3}]]]["Graphic"]}, {Head[g], Boxed /. Options[g, Boxed]}],
 {Graphics3D, False}, TestID -> "Image3D"]

(* A wrapper around a converted head. *)
VerificationTest[
 With[{r = norm[Labeled[Ball[], "B"]]}, {r["Graphic"], r["Wrappers"]}],
 {Graphics3D[Ball[]], {{Labeled, {"B"}}}},
 TestID -> "Wrapper-around-region"]

(* ---- not 3D ---- *)

VerificationTest[norm[Disk[]],
 Failure["NotGraphics3D", <|"MessageTemplate" :> MultiviewGraphics3D::input, "MessageParameters" -> {Disk[]}|>],
 TestID -> "2D-primitive"]
VerificationTest[norm[Graphics[Disk[]]]["Tag"], "NotGraphics3D", TestID -> "2D-Graphics"]
VerificationTest[norm["abc"]["Tag"], "NotGraphics3D", TestID -> "String"]
VerificationTest[norm[Legended[Disk[], "x"]]["Tag"], "NotGraphics3D", TestID -> "Wrapped-2D"]

EndTestSection[]
