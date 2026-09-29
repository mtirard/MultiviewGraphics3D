(* ::Package:: *)

BeginPackage["MultiviewGraphics3D`"];

MultiviewGraphics3D::usage = "MultiviewGraphics3D[g] shows the 3D graphic g from several standard views.";

MultiviewGraphics3D::layout = "`1` is not a valid layout. Use a rectangular matrix of pane specs, \"Net\", \"ThreeView\", \"QuadView\", \"Isometric\", or {\"Isometric\", Above | Below | All}.";
MultiviewGraphics3D::pane = "`1` is not a valid pane spec. Use Above, Below, Front, Back, Left, Right, a nonzero 3-vector, \"Free\" or None.";
MultiviewGraphics3D::free = "The layout has `1` \"Free\" panes; at most one is allowed.";
MultiviewGraphics3D::empty = "The layout has no panes.";
MultiviewGraphics3D::input = "`1` is not a 3D graphic, a 3D region or a molecule.";

Options[MultiviewGraphics3D] = {
   "CameraInteraction" -> "AxisLocked", "ProjectionConvention" -> "ThirdAngle", "LightingAttachment" -> Automatic,
   "ViewLabels" -> Automatic, LabelStyle -> {}, PreserveImageOptions -> True,
   ViewPoint -> {1, -1, 1}, ViewProjection -> "Orthographic", ImageSize -> Automatic};

Begin["`Private`"];

(* The function's own option. opts also holds Graphics3D options, which OptionValue[MultiviewGraphics3D, ...] would
   report with ::nodef. *)
ownOption[opts_, name_] := OptionValue[MultiviewGraphics3D, FilterRules[opts, Options[MultiviewGraphics3D]], name];

(* ::Section:: *)
(* Layouts *)

(* A failure that carries its message, so the top level can issue it. *)
layoutFailure[tag_, name_, params___] := Failure[tag,
   <|"MessageTemplate" :> MessageName[MultiviewGraphics3D, name], "MessageParameters" -> {params}|>];

$namedViews = Above | Below | Front | Back | Left | Right;

realVectorQ[v_] := VectorQ[v, NumericQ] && Length[v] == 3 && AllTrue[v, Element[#, Reals] &] && Norm[v] != 0;

paneSpecQ[None | "Free" | $namedViews] := True;
paneSpecQ[v_] := realVectorQ[v];

(* Named layouts. Third angle: views are pulled onto the near walls of the glass box; first angle: pushed onto the
   far walls (prior-art/projection-conventions.md). Back sits at the right end of the side row in both (Q12).
   First-angle three-view follows SOLIDWORKS "Standard 3 Views": Front, Top below it, Left to its right. *)
$namedLayouts = <|
   {"Net", "ThirdAngle"} -> {{None, Above, None, None}, {Left, Front, Right, Back}, {None, Below, None, None}},
   {"Net", "FirstAngle"} -> {{None, Below, None, None}, {Right, Front, Left, Back}, {None, Above, None, None}},
   {"ThreeView", "ThirdAngle"} -> {{Above, None}, {Front, Right}},
   {"ThreeView", "FirstAngle"} -> {{Front, Left}, {Above, None}},
   {"QuadView", "ThirdAngle"} -> {{Above, "Free"}, {Front, Right}},
   {"QuadView", "FirstAngle"} -> {{Front, Left}, {Above, "Free"}}|>;

(* Corner views arranged as seen from above: the +y (back) corners in the top row. *)
isometricCorners[z_] := {{{-1, 1, z}, {1, 1, z}}, {{-1, -1, z}, {1, -1, z}}};

layoutMatrix["Isometric" | {"Isometric", Above}, _] := isometricCorners[1];
layoutMatrix[{"Isometric", Below}, _] := isometricCorners[-1];
layoutMatrix[{"Isometric", All}, _] := Join[isometricCorners[1], isometricCorners[-1], 2];
layoutMatrix[name_String, conv_] /; KeyExistsQ[$namedLayouts, {name, conv}] := $namedLayouts[{name, conv}];
layoutMatrix[m : {__List}, _] /; Equal @@ Length /@ m && Length[First[m]] > 0 := checkPanes[m];
layoutMatrix[spec_, _] := layoutFailure["InvalidLayout", "layout", spec];

(* A user's matrix is placed as written (Q17), after checking its panes. *)
checkPanes[m_] := Module[{panes = Flatten[m, 1], bad, nFree},
   bad = SelectFirst[panes, ! paneSpecQ[#] &, Missing[]];
   nFree = Count[panes, "Free"];
   Which[
    ! MissingQ[bad], layoutFailure["InvalidPaneSpec", "pane", bad],
    nFree > 1, layoutFailure["MultipleFree", "free", nFree],
    AllTrue[panes, # === None &], layoutFailure["EmptyLayout", "empty"],
    True, m]];

(* ::Section:: *)
(* Views *)

(* Camera direction (centre -> camera), ViewVertical, and the box axes shown {horizontally, vertically}.
   Orientations verified with coloured markers in prototype p2. *)
$viewTable = <|
   Front -> {{0, -1, 0}, {0, 0, 1}, {1, 3}}, Back -> {{0, 1, 0}, {0, 0, 1}, {1, 3}},
   Right -> {{1, 0, 0}, {0, 0, 1}, {2, 3}}, Left -> {{-1, 0, 0}, {0, 0, 1}, {2, 3}},
   Above -> {{0, 0, 1}, {0, 1, 0}, {1, 2}}, Below -> {{0, 0, -1}, {0, -1, 0}, {1, 2}}|>;

viewAssoc[{d_, v_, f_}] := <|"Direction" -> d, "Vertical" -> v, "FaceAxes" -> f|>;

paneView[s : $namedViews] := viewAssoc[$viewTable[s]];
(* The Free pane's start comes from the ViewPoint option; this is the default. *)
paneView["Free"] := paneView[{1, -1, 1}];
(* Oblique views keep z up, unless they look along z. *)
paneView[v_?realVectorQ] := With[{d = Normalize[v]},
   viewAssoc[{d, Which[d[[;; 2]] != {0, 0}, {0, 0, 1}, d[[3]] > 0, {0, 1, 0}, True, {0, -1, 0}], None}]];

(* ::Section:: *)
(* Input *)

(* Wrappers that are removed and reapplied once around the whole figure (Q22, Q25). *)
$figureWrappers = Legended | Labeled | Framed | Style;

(* Options of the input that the figure owns (Q22, Q24) or that set the camera, which the panes set (Q23). *)
$figureOptions = PlotLabel | Background;
$droppedOptions = ImageSize | ImagePadding | ImageMargins | ViewPoint | ViewVertical | ViewAngle | ViewCenter |
   ViewProjection | SphericalRegion | ViewVector | ViewMatrix | ViewRange;

(* Anything Show can take as 3D: decided by head or predicate so nothing needs Quiet (p4b). *)
toGraphics3D[g_Graphics3D] := g;
toGraphics3D[x_?(GraphQ[#] || MeshRegionQ[#] || BoundaryMeshRegionQ[#] || MatchQ[#, _Region] &)] := Show[x];
(* Show drops Image3D's display defaults; restore its look (i20). Native Image3D panes reject "ViewSize" and render
   distant perspective instead of orthographic. *)
toGraphics3D[img_Image3D] := Show[img, Boxed -> False];
toGraphics3D[m_?MoleculeQ] := MoleculePlot3D[m];
toGraphics3D[r_Raster3D] := Graphics3D[r];
toGraphics3D[r_?(RegionQ[#] && RegionEmbeddingDimension[#] === 3 &)] := Graphics3D[r];
toGraphics3D[_] := $Failed;

normalizeInput[g_] := normalizeInput[g, {}];
normalizeInput[(h : $figureWrappers)[x_, args___], wrappers_] := normalizeInput[x, Append[wrappers, {h, {args}}]];
normalizeInput[x_, wrappers_] := With[{g = toGraphics3D[x]},
   If[g === $Failed,
    layoutFailure["NotGraphics3D", "input", x],
    <|"Graphic" -> Graphics3D[First[g], Sequence @@ FilterRules[Options[g], Except[$figureOptions | $droppedOptions]]],
     "Wrappers" -> wrappers,
     "PlotLabel" -> OptionValue[Graphics3D, Options[g], PlotLabel],
     "Background" -> Replace[FilterRules[Options[g], Background], {{Background -> b_, ___} :> b, _ -> None}]|>]];

(* ::Section:: *)
(* Geometry *)

(* The one front-end call: AbsoluteOptions returns BoxRatios unnormalized and the padding as absolute amounts. *)
graphicGeometry[g_Graphics3D] := geometryFromOptions[
   {PlotRange, PlotRangePadding, BoxRatios} /. AbsoluteOptions[g, {PlotRange, PlotRangePadding, BoxRatios}]];

(* Panes set PlotRange to the drawn range with PlotRangePadding -> None: an explicit PlotRange drops the default
   padding, which broke the zoom law (FINDINGS p16). Zoom constants: orthographic ViewSize = k W, with W the visible
   width in display units; ViewSize0 = k W0 reproduces Automatic framing (p15-p21). *)
geometryFromOptions[{pr_, pad_, br_}] := Module[{drawn, brn = br/Max[br], widths},
   drawn = MapThread[#1 + {-1, 1} #2 &, {pr, pad}];
   widths = Subtract @@@ Reverse /@ drawn;
   <|"PlotRange" -> drawn, "BoxRatios" -> brn, "Center" -> Mean /@ drawn, "Scale" -> brn/widths,
    "K" -> Min[widths/brn], "W0" -> Norm[brn], "ViewSize0" -> Min[widths/brn] Norm[brn]|>];

(* Screen frame {right, up} for a camera direction and ViewVertical, in display coordinates. *)
screenFrame[dir_, vert_] := With[{right = Normalize[Cross[vert, dir]]}, {right, Cross[dir, right]}];

(* {width, height} of the display box projected onto the view plane: the face for axis views. ViewVertical is in
   scaled coordinates, so its display direction is vert * BoxRatios. *)
paneSize[geo_, spec_] := With[{v = paneView[spec], br = geo["BoxRatios"]},
   With[{f = screenFrame[v["Direction"], Normalize[v["Vertical"] br]]},
    Max[#] - Min[#] & /@ Transpose[Tuples[Transpose[{-br, br}/2]] . Transpose[f]]]];

(* Q18: each column as wide as its widest pane, each row as tall as its tallest; None cells are empty. *)
cellSizes[sizes_] := With[{wh = Replace[sizes, None -> {0, 0}, {2}]},
   <|"ColumnWidths" -> Max /@ Transpose[wh[[All, All, 1]]], "RowHeights" -> Max /@ wh[[All, All, 2]]|>];

(* ::Section:: *)
(* Lighting *)

(* Plot3D & co. bake camera lighting into the surface style, which overrides the Lighting option (FINDINGS round 2).
   Shim: delete the inner Lighting rules and return the first inner light list (the plot's own tinted lights), or None.
   ReplaceAll, not DeleteCases at every level: DeleteCases rebuilds the whole primitive tree, and ToBoxes of the result
   was 50 times slower (p45, p46). *)
stripInnerLighting[g_Graphics3D] := With[{inner = Cases[First[g], HoldPattern[Lighting -> l_List] :> l, Infinity]},
   {Graphics3D[First[g] /. HoldPattern[Lighting -> _List] :> Sequence[], Sequence @@ Options[g]],
    If[inner === {}, None, First[inner]]}];

(* Named lighting (Automatic, "Neutral", ...) as a list of light specs. Needs the front end. *)
resolveLighting[spec_] := Lighting /. AbsoluteOptions[Graphics3D[{}, Lighting -> spec], Lighting];

(* ImageScaled[p] lights shine from direction p in (screen right, screen up, towards the viewer) (p2c). *)
cameraToDisplay[p_, view_, geo_] := With[{d = view["Direction"]},
   p . Append[screenFrame[d, Normalize[view["Vertical"] geo["BoxRatios"]]], d]];

(* Lighting of one pane. "Camera": the lights as given, so camera-frame lights follow each pane's camera. "Object" and
   "Rig": camera-frame lights are fixed as they fall in the reference view (p2b), then rotated with the rig r (display
   frame). A world direction w from the centre c is DirectionalLight[col, {c + w/scale, c}] (round 2). Lights already
   fixed in the scene stay as given. *)
paneLighting["Camera", lights_, ___] := lights;
paneLighting["Object" | "Rig", lights_, ref_, geo_, r_ : IdentityMatrix[3]] := With[{c = geo["Center"], s = geo["Scale"]},
   Replace[lights, {
     {"Ambient", col_} :> AmbientLight[col],
     {"Directional", col_, ImageScaled[p_]} :> DirectionalLight[col, {c + (r . cameraToDisplay[p, ref, geo])/s, c}],
     {"Directional", col_, pos_} :> DirectionalLight[col, pos],
     {"Point", col_, pos_, rest___} :> PointLight[col, pos, rest],
     {"Spot", col_, pos_, rest___} :> SpotLight[col, pos, rest]}, {1}]];

(* ::Section:: *)
(* View labels *)

$drafting = <|Above -> "Top", Below -> "Bottom", Front -> "Front", Back -> "Rear", Left -> "Left", Right -> "Right"|>;

cornerQ[v_] := Normalize[v] == Normalize[Sign[v]] && FreeQ[Sign[v], 0];

automaticLabel[s : $namedViews, _, _] := $drafting[s];
automaticLabel["Free", True, _] := "Free";
automaticLabel["Free", False, vp_] := If[cornerQ[vp], "Isometric", None];
automaticLabel[v_List, _, _] := If[cornerQ[v], "Isometric", None];

viewLabel[_, None, _, _] := None;
viewLabel[spec_, Automatic, interactive_, vp_] := automaticLabel[spec, interactive, vp];
viewLabel[spec_, labels_Association, _, _] := Lookup[labels, Key[spec], None];
viewLabel[spec_, f_, _, _] := f[spec];

(* ::Section:: *)
(* Static figure *)

(* Cell centres and label positions, y up, row 1 on top; each row has a label strip of height strip above its cells. *)
cellPlacement[cells_, gap_, strip_] := Module[{w = cells["ColumnWidths"], h = cells["RowHeights"], x, tops, height},
   x = Most[Accumulate[Prepend[w + gap, 0]]] + w/2;
   height = Total[h + strip] + gap (Length[h] - 1);
   tops = height - Most[Accumulate[Prepend[h + strip + gap, 0]]];
   <|"Centers" -> MapThread[Function[{t, hr}, {#, t - strip - hr/2} & /@ x], {tops, h}],
    "LabelPositions" -> Table[{cx, t - strip/2}, {t, tops}, {cx, x}],
    "Size" -> {Total[w] + gap (Length[w] - 1), height}|>];

$gapPoints = 12;
$labelPoints = 20;
$pointsPerUnit = 160;
$namedSizes = <|Tiny -> 100, Small -> 180, Medium -> 360, Large -> 576|>;

(* Points per display unit for a requested figure width. *)
pointsPerUnit[size_, totalWidth_, nCols_] := With[{w = Replace[size, {{x_, _} :> x, s_Symbol :> Lookup[$namedSizes, s, Automatic]}]},
   If[NumericQ[w], (w - $gapPoints (nCols - 1))/totalWidth, $pointsPerUnit]];

$viewDistance = Norm[{1.3, -2.4, 2}];

(* One static pane. ctx: geometry, lights, attachment, reference view, Free ViewPoint and projection, style options. *)
staticPane[g_, spec_, size_, ctx_] := Module[{geo = ctx["Geometry"], free = spec === "Free", v, proj},
   v = paneView[If[free, ctx["ViewPoint"], spec]];
   proj = If[free, ctx["ViewProjection"], "Orthographic"];
   Deploy@Graphics3D[First[g],
     ViewPoint -> If[free && proj =!= "Orthographic", ctx["ViewPoint"], $viewDistance v["Direction"]],
     ViewVertical -> v["Vertical"], ViewProjection -> proj,
     If[proj === "Orthographic", "ViewSize" -> geo["K"] Min[size],
      ViewAngle -> 2 ArcTan[Min[size]/(2 Norm[ctx["ViewPoint"]])]],
     SphericalRegion -> False, PlotRange -> geo["PlotRange"], PlotRangePadding -> None,
     Lighting -> paneLighting[ctx["Attachment"], ctx["Lights"], ctx["Reference"], geo],
     Background -> None, PlotInteractivity -> False, ImageSize -> Automatic,
     Sequence @@ ctx["Style"],
     Sequence @@ FilterRules[Options[g], Except[Lighting | PlotRange | PlotRangePadding | PlotInteractivity]]]];

(* What both the static and the interactive figure need. The rig never moves in static or axis-locked output, so
   "Rig" lighting is "Object" lighting there. *)
prepare[input_, opts_, rigMoves_] := Module[{g, inner, att, vp},
   {g, inner} = stripInnerLighting[input["Graphic"]];
   vp = ownOption[opts, ViewPoint];
   att = Replace[ownOption[opts, "LightingAttachment"],
     {Automatic -> If[rigMoves, "Rig", "Camera"], "Rig" /; ! rigMoves -> "Object"}];
   <|"Graphic" -> g, "Geometry" -> graphicGeometry[g], "Attachment" -> att, "ViewPoint" -> vp, "Reference" -> paneView[vp],
    "ViewProjection" -> ownOption[opts, ViewProjection], "Style" -> paneStyle[opts],
    "Lights" -> Which[inner =!= None, inner, att === "Camera", OptionValue[Graphics3D, Options[g], Lighting],
      True, resolveLighting[OptionValue[Graphics3D, Options[g], Lighting]]]|>];

staticFigure[input_, matrix_, opts_] := Module[{ctx = prepare[input, opts, False], g, geo, vp, sizes, ppu, labels, strip, place, figure},
   g = ctx["Graphic"]; geo = ctx["Geometry"]; vp = ctx["ViewPoint"];
   sizes = Map[If[# === None, None, paneSize[geo, # /. "Free" -> vp]] &, matrix, {2}];
   ppu = pointsPerUnit[ownOption[opts, ImageSize],
     Total[cellSizes[sizes]["ColumnWidths"]], Length[First[matrix]]];
   labels = Map[If[# === None, None, viewLabel[#, ownOption[opts, "ViewLabels"], False, vp]] &, matrix, {2}];
   strip = If[AllTrue[Flatten[labels], # === None &], 0, $labelPoints];
   place = cellPlacement[cellSizes[Map[If[# === None, None, ppu #] &, sizes, {2}]], $gapPoints, strip];
   figure = Graphics[
     MapThread[
      Function[{spec, size, label, center, lpos},
       If[spec === None, {},
        {Inset[staticPane[g, spec, size, ctx], center, Center, ppu size],
         If[label === None, {}, Text[labelStyle[label, opts], lpos]]}]],
      {matrix, sizes, labels, place["Centers"], place["LabelPositions"]}, 2],
     PlotRange -> Transpose[{{0, 0}, place["Size"]}], PlotRangePadding -> None, ImagePadding -> 2,
     ImageSize -> First[place["Size"]] + 4,
     PlotLabel -> figureOption[PlotLabel, input, opts], Background -> figureOption[Background, input, opts]];
   rewrap[figure, input]];

labelStyle[label_, opts_] := Style[label, Sequence @@ Flatten[{ownOption[opts, LabelStyle]}]];

(* PlotLabel and Background passed to the function win over the input's. *)
figureOption[o_, input_, opts_] := Replace[OptionValue[Graphics3D, FilterRules[opts, Options[Graphics3D]], o],
   None -> input[SymbolName[o]]];

(* Reapply the input's wrappers once, innermost first. *)
rewrap[figure_, input_] := Fold[#2[[1]][#1, Sequence @@ #2[[2]]] &, figure, Reverse[input["Wrappers"]]];

(* ::Section:: *)
(* Camera store *)

(* Keeps the explored camera when a surrounding Dynamic or Manipulate rebuilds the output (design-decisions, "Keeping
   the camera across rebuilds"). The store is in the TaggingRules of the evaluation cell, keyed per call by
   {Hash[EvaluationBox[]], ordinal}. At top level EvaluationBox[] is $Failed: local state only, so evaluating again
   resets the view (Q21). *)
$storeOrdinal[_] = 0;
anchorBox[] := If[$FrontEnd === Null, $Failed, EvaluationBox[]];
storeCell[] := EvaluationCell[];

(* {store reference or None, anchor}. The ordinal counter is global state changed during a build, so it is hidden
   from a surrounding Dynamic with Refresh[..., None] (i17 mM). *)
storeRef[preserve_] := With[{box = If[preserve === False, $Failed, anchorBox[]]},
   If[box === $Failed, {None, None},
    With[{k = Refresh[++$storeOrdinal[box], None]},
     {{storeCell[], {TaggingRules, "MultiviewGraphics3D", ToString[{Hash[box], k}]}}, box}]]];

storeGet[None, _, _, default_] := default;
storeGet[{obj_, path_}, key_, test_, default_] := With[{v = CurrentValue[obj, Append[path, key]]}, If[test[v], v, default]];

(* Only the cell that owns the store writes to it: a Copy Graphic paste must not (i18 mN). *)
storePut[None, _, _] := Null;
storePut[{obj_, path_}, key_, v_] := If[EvaluationCell[] === obj, CurrentValue[obj, Append[path, key]] = v];

(* ::Section:: *)
(* Setters *)

(* The front end writes every camera option back on every frame, also unchanged ones, and only to about 1e-16 (i2).
   Setters ignore changes below the tolerance, so an unchanged write-back neither updates the other panes nor the store. *)
$tolerance = 10^-9;
$panPattern = {{_?NumericQ, _?NumericQ, _?NumericQ}, {_?NumericQ, _?NumericQ}};

SetAttributes[{setVector, setPan, setZoom, setAngle}, HoldFirst];
setVector[x_, r_, key_, v_] := If[VectorQ[v, NumericQ] && ! (VectorQ[x, NumericQ] && Max[Abs[v - x]] <= $tolerance),
   x = v; storePut[r, key, v]];

(* A pan writes the screen part of ViewCenter as an image fraction (p11). All panes share one offset w (display frame,
   image widths); a pan moves w within the panning pane's view plane, frame f = {right, up}. *)
setPan[w_, f_, r_, v_] := If[MatchQ[v, $panPattern],
   With[{w0 = If[VectorQ[w, NumericQ], w, {0, 0, 0}]}, With[{wn = w0 + (v[[2]] - {1, 1}/2 - f . w0) . f},
     If[Max[Abs[wn - w0]] > $tolerance, w = wn; storePut[r, "Pan", wn]]]]];

(* Orthographic zoom is "ViewSize" = k W (FINDINGS round 10). *)
setZoom[vs_, r_, v_] := If[NumericQ[v] && ! (NumericQ[vs] && Abs[v - vs] <= $tolerance), vs = v; storePut[r, "Zoom", v]];

(* Perspective zoom writes ViewAngle = 2 ArcTan[W / (2 |vp|)] (p18), with W = vs / k. *)
setAngle[vs_, k_, d_, r_, a_] := If[NumericQ[a] && NumericQ[d], setZoom[vs, r, 2 k d Tan[a/2]]];

(* ::Section:: *)
(* Interactive figure *)

freeFrame[vp_, vv_, br_] := screenFrame[Normalize[vp], Normalize[vv br]];

$interactiveCell = 220;   (* points per pane when ImageSize is Automatic *)

(* Options of one live pane in axis-locked mode. vpF, vvF, w, vs are DynamicModule variables (held). Every Dynamic has an
   inline fallback made only of System functions (p32): on a rebuild, and in a pasted copy, the variables can be unbound. *)
SetAttributes[livePaneOptions, HoldRest];
livePaneOptions[spec_, ctx_, r_, vpF_, vvF_, w_, vs_] := With[{geo = ctx["Geometry"]},
  With[{br = geo["BoxRatios"], k = geo["K"], vs0 = geo["ViewSize0"],
    method = FilterRules[Flatten[{OptionValue[Graphics3D, Options[ctx["Graphic"]], Method]} /. Automatic -> {}],
      Except["RotationControl"]]},
   If[spec === "Free",
    With[{vp0 = N[ctx["ViewPoint"]], vv0 = paneView[ctx["ViewPoint"]]["Vertical"], proj = ctx["ViewProjection"],
      f0 = freeFrame[ctx["ViewPoint"], paneView[ctx["ViewPoint"]]["Vertical"], br], a0 = 2 ArcTan[vs0/(2 k Norm[ctx["ViewPoint"]])]},
     Flatten[{
       ViewPoint -> Dynamic[If[VectorQ[vpF, NumericQ], vpF, vp0], setVector[vpF, r, "ViewPoint", #] &],
       ViewVertical -> Dynamic[If[VectorQ[vvF, NumericQ], vvF, vv0], setVector[vvF, r, "ViewVertical", #] &],
       ViewCenter -> Dynamic[
         With[{f = If[VectorQ[vpF, NumericQ] && VectorQ[vvF, NumericQ],
             With[{d = Normalize[vpF]}, With[{rt = Normalize[Cross[Normalize[vvF br], d]]}, {rt, Cross[d, rt]}]], f0]},
          {{1, 1, 1}/2, {1, 1}/2 + f . If[VectorQ[w, NumericQ], w, {0, 0, 0}]}],
         setPan[w, freeFrame[vpF, vvF, br], r, #] &],
       If[proj === "Orthographic",
        "ViewSize" -> Dynamic[If[NumericQ[vs], vs, vs0], setZoom[vs, r, #] &],
        ViewAngle -> Dynamic[If[NumericQ[vs] && VectorQ[vpF, NumericQ], 2 ArcTan[vs/(2 k Norm[vpF])], a0],
          setAngle[vs, k, Norm[vpF], r, #] &]],
       ViewProjection -> proj, If[method === {}, {}, Method -> method]}]],
    With[{v = paneView[spec]},
     With[{f = screenFrame[v["Direction"], Normalize[v["Vertical"] br]]},
      {ViewPoint -> $viewDistance v["Direction"], ViewVertical -> v["Vertical"],
       ViewCenter -> Dynamic[{{1, 1, 1}/2, {1, 1}/2 + f . If[VectorQ[w, NumericQ], w, {0, 0, 0}]}, setPan[w, f, r, #] &],
       "ViewSize" -> Dynamic[If[NumericQ[vs], vs, vs0], setZoom[vs, r, #] &],
       ViewProjection -> "Orthographic", Method -> Prepend[method, "RotationControl" -> None]}]]]]];

SetAttributes[livePane, HoldRest];
livePane[spec_, ctx_, side_, r_, vpF_, vvF_, w_, vs_] := With[{g = ctx["Graphic"], geo = ctx["Geometry"]},
   Graphics3D[First[g],
    Sequence @@ livePaneOptions[spec, ctx, r, vpF, vvF, w, vs],
    SphericalRegion -> True, PlotRange -> geo["PlotRange"], PlotRangePadding -> None,
    Lighting -> paneLighting[ctx["Attachment"], ctx["Lights"], ctx["Reference"], geo],
    Background -> None, ImageSize -> side,
    Sequence @@ ctx["Style"],
    Sequence @@ FilterRules[Options[g], Except[Lighting | PlotRange | PlotRangePadding | Method]]]];

(* Axis-locked figure: a Grid (a Graphics of Insets captures the clicks, i19), label rows above pane rows. Linked pan and
   zoom always on (Q9); only the Free pane rotates (Q16). *)
(* Side of the square panes, and label rows (Style or "") with a flag for whether any label is shown. *)
paneSide[matrix_, opts_] := With[{nc = Length[First[matrix]],
    w = Replace[ownOption[opts, ImageSize], {{x_, _} :> x, s_Symbol :> Lookup[$namedSizes, s, Automatic]}]},
   If[NumericQ[w], (w - $gapPoints (nc - 1))/nc, $interactiveCell]];
liveLabels[matrix_, opts_, vp_] := With[{labels = Map[If[# === None, None,
        viewLabel[#, ownOption[opts, "ViewLabels"], True, vp]] &, matrix, {2}]},
   {Map[If[# === None, "", labelStyle[#, opts]] &, labels, {2}], ! AllTrue[Flatten[labels], # === None &]}];

axisLockedFigure[input_, matrix_, opts_] := Module[{ctx = prepare[input, opts, False], ref, box, side, labels, grid},
   {ref, box} = storeRef[ownOption[opts, PreserveImageOptions]];
   side = paneSide[matrix, opts];
   labels = liveLabels[matrix, opts, ctx["ViewPoint"]];
   With[{r = ref, anchor = box, c = ctx, m = matrix, sd = side,
     rows = First[labels], hasLabels = Last[labels],
     vp0 = N[ctx["ViewPoint"]], vv0 = paneView[ctx["ViewPoint"]]["Vertical"], vs0 = ctx["Geometry"]["ViewSize0"],
     bg = figureOption[Background, input, opts]},
    grid = DynamicModule[{vpF = vp0, vvF = vv0, w = {0., 0., 0.}, vs = vs0},
      Grid[
       Flatten[MapThread[
         Function[{labelRow, specRow},
          {If[hasLabels, labelRow, Nothing],
           Map[If[# === None, Spacer[sd], livePane[#, c, sd, r, vpF, vvF, w, vs]] &, specRow]}],
         {rows, m}], 1],
       Spacings -> {1, 0.4}, Background -> bg],
      Initialization :> (
        If[anchor =!= None, $storeOrdinal[anchor] = 0];
        vpF = storeGet[r, "ViewPoint", VectorQ[#, NumericQ] &, vp0];
        vvF = storeGet[r, "ViewVertical", VectorQ[#, NumericQ] &, vv0];
        w = storeGet[r, "Pan", VectorQ[#, NumericQ] &, {0., 0., 0.}];
        vs = storeGet[r, "Zoom", NumericQ, vs0])]];
   rewrap[With[{pl = figureOption[PlotLabel, input, opts]}, If[pl === None, grid, Labeled[grid, pl, Top]]], input]];

(* Graphics3D style options passed to the function apply to every pane (Q23); camera, framing and figure options do not. *)
paneStyle[opts_] := FilterRules[FilterRules[opts, Options[Graphics3D]],
   Except[$droppedOptions | $figureOptions | LabelStyle | Lighting]];

(* ::Section:: *)
(* Rig-locked figure *)

(* Orthonormal camera frame {right, up, back}; back points from the centre towards the camera (display coordinates). *)
rigFrame[d_, u_] := With[{back = Normalize[d]}, With[{right = Normalize[Cross[u, back]]}, {right, Cross[back, right], back}]];

(* The rig rotation taking a pane's base camera {d0, u0} to camera {d, u}: F^T . F0. A degenerate camera (d along u)
   keeps r. Round trip exact to 1e-12 (p6). *)
rigFromCamera[r_, {d0_, u0_}, {d_, u_}] :=
  If[Norm[Cross[d, u]] < 10^-6 Norm[d] Norm[u], r, Transpose[rigFrame[d, u]] . rigFrame[d0, u0]];

SetAttributes[{setRig, setRigViewPoint, setRigVertical}, HoldFirst];
setRig[r_, ref_, m_] := If[MatrixQ[m, NumericQ] && Dimensions[m] == {3, 3} &&
    ! (MatrixQ[r, NumericQ] && Max[Abs[m - r]] <= $tolerance), r = m; storePut[ref, "Rig", m]];

(* Both ViewPoint and ViewVertical drive the rig: the front end writes both on rotation, and linking only ViewPoint
   leaves the roll unlinked (i13). base = {d0, u0} in display coordinates; ViewVertical is in scaled coordinates. *)
setRigViewPoint[r_, ref_, base_, vp_] := With[{m = If[MatrixQ[r, NumericQ], r, IdentityMatrix[3]]},
   If[VectorQ[vp, NumericQ], setRig[r, ref, rigFromCamera[m, base, {vp, m . base[[2]]}]]]];
setRigVertical[r_, ref_, base_, br_, vv_] := With[{m = If[MatrixQ[r, NumericQ], r, IdentityMatrix[3]]},
   If[VectorQ[vv, NumericQ], setRig[r, ref, rigFromCamera[m, base, {m . base[[1]], Normalize[vv br]}]]]];

(* Rig lighting: camera-frame lights fixed as they fall in the reference view, then turned with r. Returned as parts so
   the Dynamic can rebuild the lights with System functions only: {fixed lights, colours, display directions}. *)
rigLightParts[lights_, ref_, geo_] := {
   paneLighting["Object", DeleteCases[lights, {"Directional", _, ImageScaled[_]}], ref, geo],
   Cases[lights, {"Directional", col_, ImageScaled[_]} :> col],
   Cases[lights, {"Directional", _, ImageScaled[p_]} :> cameraToDisplay[p, ref, geo]]};

(* Options of one live pane in rig-locked mode. r, w, vs are DynamicModule variables (held). *)
SetAttributes[rigPaneOptions, HoldRest];
rigPaneOptions[spec_, ctx_, ref_, r_, w_, vs_] := With[{geo = ctx["Geometry"], free = spec === "Free"},
  With[{br = geo["BoxRatios"], k = geo["K"], vs0 = geo["ViewSize0"], v = paneView[If[free, ctx["ViewPoint"], spec]],
    proj = If[free, ctx["ViewProjection"], "Orthographic"],
    dist = If[free && ctx["ViewProjection"] =!= "Orthographic", Norm[N[ctx["ViewPoint"]]], $viewDistance]},
   With[{d0 = v["Direction"], u0 = Normalize[v["Vertical"] br], a0 = 2 ArcTan[vs0/(2 k dist)]},
    With[{rt0 = screenFrame[d0, u0][[1]], up0 = screenFrame[d0, u0][[2]]},
     Flatten[{
       ViewPoint -> Dynamic[dist Normalize[If[MatrixQ[r, NumericQ], r, IdentityMatrix[3]] . d0],
         setRigViewPoint[r, ref, {d0, u0}, #] &],
       ViewVertical -> Dynamic[(If[MatrixQ[r, NumericQ], r, IdentityMatrix[3]] . u0)/br,
         setRigVertical[r, ref, {d0, u0}, br, #] &],
       ViewCenter -> Dynamic[
         With[{m = If[MatrixQ[r, NumericQ], r, IdentityMatrix[3]]},
          {{1, 1, 1}/2, {1, 1}/2 + {m . rt0, m . up0} . If[VectorQ[w, NumericQ], w, {0, 0, 0}]}],
         setPan[w, With[{m = If[MatrixQ[r, NumericQ], r, IdentityMatrix[3]]}, {m . rt0, m . up0}], ref, #] &],
       If[proj === "Orthographic",
        "ViewSize" -> Dynamic[If[NumericQ[vs], vs, vs0], setZoom[vs, ref, #] &],
        ViewAngle -> Dynamic[If[NumericQ[vs], 2 ArcTan[vs/(2 k dist)], a0], setAngle[vs, k, dist, ref, #] &]],
       ViewProjection -> proj,
       If[ctx["Attachment"] === "Rig",
        With[{parts = rigLightParts[ctx["Lights"], ctx["Reference"], geo], c = geo["Center"], s = geo["Scale"]},
         With[{fixed = parts[[1]], cols = parts[[2]], dirs = parts[[3]]},
          Lighting -> Dynamic[With[{m = If[MatrixQ[r, NumericQ], r, IdentityMatrix[3]]},
             Join[fixed, MapThread[DirectionalLight[#1, {c + (m . #2)/s, c}] &, {cols, dirs}]]]]]],
        Lighting -> paneLighting[ctx["Attachment"], ctx["Lights"], ctx["Reference"], geo]]}]]]]];

SetAttributes[rigPane, HoldRest];
rigPane[spec_, ctx_, side_, ref_, r_, w_, vs_] := With[{g = ctx["Graphic"], geo = ctx["Geometry"]},
   Graphics3D[First[g],
    Sequence @@ rigPaneOptions[spec, ctx, ref, r, w, vs],
    SphericalRegion -> True, PlotRange -> geo["PlotRange"], PlotRangePadding -> None, Background -> None, ImageSize -> side,
    Sequence @@ ctx["Style"],
    Sequence @@ FilterRules[Options[g], Except[Lighting | PlotRange | PlotRangePadding]]]];

(* Rig-locked figure: every pane turns with one rig rotation (a glass box); pan and zoom linked. *)
rigLockedFigure[input_, matrix_, opts_] := Module[{ctx = prepare[input, opts, True], ref, box, grid},
   {ref, box} = storeRef[ownOption[opts, PreserveImageOptions]];
   With[{rf = ref, anchor = box, c = ctx, m = matrix, sd = paneSide[matrix, opts], labels = liveLabels[matrix, opts, ctx["ViewPoint"]],
     vs0 = ctx["Geometry"]["ViewSize0"], bg = figureOption[Background, input, opts], id = N[IdentityMatrix[3]]},
    grid = DynamicModule[{r = id, w = {0., 0., 0.}, vs = vs0},
      Grid[
       Flatten[MapThread[
         Function[{labelRow, specRow},
          {If[Last[labels], labelRow, Nothing], Map[If[# === None, Spacer[sd], rigPane[#, c, sd, rf, r, w, vs]] &, specRow]}],
         {First[labels], m}], 1],
       Spacings -> {1, 0.4}, Background -> bg],
      Initialization :> (
        If[anchor =!= None, $storeOrdinal[anchor] = 0];
        r = storeGet[rf, "Rig", MatrixQ[#, NumericQ] && Dimensions[#] == {3, 3} &, id];
        w = storeGet[rf, "Pan", VectorQ[#, NumericQ] &, {0., 0., 0.}];
        vs = storeGet[rf, "Zoom", NumericQ, vs0])]];
   rewrap[With[{pl = figureOption[PlotLabel, input, opts]}, If[pl === None, grid, Labeled[grid, pl, Top]]], input]];

(* ::Section:: *)
(* Top level *)

issueFailure[f_Failure] := (Cases[Normal[f[[2]]], HoldPattern["MessageTemplate" :> m_] :> Message[m, Sequence @@ f[[2]]["MessageParameters"]]]; $Failed);

MultiviewGraphics3D[g_, opts : OptionsPattern[{MultiviewGraphics3D, Graphics3D}]] := MultiviewGraphics3D[g, "QuadView", opts];
MultiviewGraphics3D[g_, layout : Except[_Rule | _RuleDelayed], opts : OptionsPattern[{MultiviewGraphics3D, Graphics3D}]] :=
  Module[{input = normalizeInput[g], matrix},
   matrix = layoutMatrix[layout, ownOption[{opts}, "ProjectionConvention"]];
   Which[
    FailureQ[input], issueFailure[input],
    FailureQ[matrix], issueFailure[matrix],
    ownOption[{opts}, "CameraInteraction"] === None, staticFigure[input, matrix, {opts}],
    ownOption[{opts}, "CameraInteraction"] === "RigLocked", rigLockedFigure[input, matrix, {opts}],
    True, axisLockedFigure[input, matrix, {opts}]]];

End[];
EndPackage[];
