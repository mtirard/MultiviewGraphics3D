(* Geometry: AbsoluteOptions values -> drawn box, display scale, zoom constants (FINDINGS rounds 9-10); pane sizes from
   the projected box; cell sizes by the Q18 alignment rule. Sizes are in display units (normalized BoxRatios). *)
BeginTestSection["Geometry"]

PrependTo[$ContextPath, "MultiviewGraphics3D`Private`"];
geo = geometryFromOptions[{{{0, 3}, {0, 1}, {-1, 1}}, {{0.5, 0.5}, {0, 0}, {0, 1}}, {6, 2, 3}}];

VerificationTest[geo["PlotRange"], {{-0.5, 3.5}, {0, 1}, {-1, 2}}, SameTest -> Equal, TestID -> "Drawn-range"]
VerificationTest[geo["BoxRatios"], {1, 1/3, 1/2}, SameTest -> Equal, TestID -> "BoxRatios-normalized"]
VerificationTest[geo["Center"], {1.5, 0.5, 0.5}, SameTest -> Equal, TestID -> "Center"]
(* display units per ordinary unit, per axis *)
VerificationTest[geo["Scale"], {1/4, 1/3, 1/6}, SameTest -> Equal, TestID -> "Scale"]
(* k = ordinary units per display unit along the least-stretched axis; W0 = Norm[BoxRatios] *)
VerificationTest[geo["K"], 3, SameTest -> Equal, TestID -> "K"]
VerificationTest[geo["W0"], Norm[{1, 1/3, 1/2}], SameTest -> Equal, TestID -> "W0"]
VerificationTest[geo["ViewSize0"], 3 Norm[{1, 1/3, 1/2}], SameTest -> Equal, TestID -> "ViewSize0"]

(* ---- pane sizes: {width, height} of the box projected on the view plane ---- *)

cube = geometryFromOptions[{{{0, 1}, {0, 1}, {0, 1}}, {{0, 0}, {0, 0}, {0, 0}}, {1, 1, 1}}];
VerificationTest[paneSize[geo, Front], {1, 1/2}, SameTest -> Equal, TestID -> "Front-face"]
VerificationTest[paneSize[geo, Right], {1/3, 1/2}, SameTest -> Equal, TestID -> "Right-face"]
VerificationTest[paneSize[geo, Above], {1, 1/3}, SameTest -> Equal, TestID -> "Top-face"]
VerificationTest[paneSize[geo, Below], {1, 1/3}, SameTest -> Equal, TestID -> "Bottom-face"]
VerificationTest[paneSize[cube, {1, -1, 1}], {Sqrt[2], 4/Sqrt[6]}, SameTest -> Equal, TestID -> "Isometric-cube"]
(* a vector along an axis gives that face *)
VerificationTest[paneSize[geo, {0, -2, 0}], {1, 1/2}, SameTest -> Equal, TestID -> "Axis-vector"]

(* ---- cell sizes (Q18): columns as wide as their widest pane, rows as tall as their tallest; None is empty ---- *)

VerificationTest[
 cellSizes[{{None, {1, 2}}, {{3, 1}, {1, 1}}}],
 <|"ColumnWidths" -> {3, 1}, "RowHeights" -> {2, 1}|>,
 TestID -> "Cells"]

(* Net of a 1 x 1/3 x 1/2 box: exact glass-box widths and heights. *)
VerificationTest[
 cellSizes[Map[If[# === None, None, paneSize[geo, #]] &,
   MultiviewGraphics3D`Private`layoutMatrix["Net", "ThirdAngle"], {2}]],
 <|"ColumnWidths" -> {1/3, 1, 1/3, 1}, "RowHeights" -> {1/3, 1/2, 1/3}|>,
 SameTest -> Equal, TestID -> "Net-cells"]

(* ---- the one front-end call ---- *)

VerificationTest[
 With[{g = graphicGeometry[Plot3D[x y, {x, 0, 2}, {y, 0, 1}]]}, {g["BoxRatios"], Round[g["PlotRange"], 0.01]}],
 {{1., 1., 0.4}, {{-0.04, 2.04}, {-0.02, 1.02}, {-0.04, 2.04}}},
 TestID -> "graphicGeometry-Plot3D"]

EndTestSection[]
