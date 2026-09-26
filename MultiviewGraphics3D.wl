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
   Shim: delete the inner Lighting rules and return the first inner light list (the plot's own tinted lights), or None. *)
stripInnerLighting[g_Graphics3D] := With[{inner = Cases[First[g], HoldPattern[Lighting -> l_List] :> l, Infinity]},
   {Graphics3D[DeleteCases[First[g], HoldPattern[Lighting -> _], Infinity], Sequence @@ Options[g]],
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

staticFigure[input_, matrix_, opts_] := Module[{g, inner, geo, att, lights, vp, sizes, ppu, labels, strip, place, figure},
   {g, inner} = stripInnerLighting[input["Graphic"]];
   geo = graphicGeometry[g];
   vp = OptionValue[MultiviewGraphics3D, opts, ViewPoint];
   att = Replace[OptionValue[MultiviewGraphics3D, opts, "LightingAttachment"], {Automatic -> "Camera", "Rig" -> "Object"}];
   lights = Which[inner =!= None, inner, att === "Camera", OptionValue[Graphics3D, Options[g], Lighting],
     True, resolveLighting[OptionValue[Graphics3D, Options[g], Lighting]]];
   sizes = Map[If[# === None, None, paneSize[geo, # /. "Free" -> vp]] &, matrix, {2}];
   ppu = pointsPerUnit[OptionValue[MultiviewGraphics3D, opts, ImageSize],
     Total[cellSizes[sizes]["ColumnWidths"]], Length[First[matrix]]];
   labels = Map[If[# === None, None, viewLabel[#, OptionValue[MultiviewGraphics3D, opts, "ViewLabels"], False, vp]] &, matrix, {2}];
   strip = If[AllTrue[Flatten[labels], # === None &], 0, $labelPoints];
   place = cellPlacement[cellSizes[Map[If[# === None, None, ppu #] &, sizes, {2}]], $gapPoints, strip];
   figure = Graphics[
     MapThread[
      Function[{spec, size, label, center, lpos},
       If[spec === None, {},
        {Inset[staticPane[g, spec, size,
           <|"Geometry" -> geo, "Lights" -> lights, "Attachment" -> att, "Reference" -> paneView[vp], "ViewPoint" -> vp,
            "ViewProjection" -> OptionValue[MultiviewGraphics3D, opts, ViewProjection],
            "Style" -> paneStyle[opts]|>], center, Center, ppu size],
         If[label === None, {}, Text[Style[label, Sequence @@ Flatten[{OptionValue[MultiviewGraphics3D, opts, LabelStyle]}]], lpos]]}]],
      {matrix, sizes, labels, place["Centers"], place["LabelPositions"]}, 2],
     PlotRange -> Transpose[{{0, 0}, place["Size"]}], PlotRangePadding -> None, ImagePadding -> 2,
     ImageSize -> First[place["Size"]] + 4,
     PlotLabel -> Replace[OptionValue[Graphics3D, FilterRules[opts, Options[Graphics3D]], PlotLabel], None -> input["PlotLabel"]],
     Background -> Replace[OptionValue[Graphics3D, FilterRules[opts, Options[Graphics3D]], Background], None -> input["Background"]]];
   Fold[#2[[1]][#1, Sequence @@ #2[[2]]] &, figure, Reverse[input["Wrappers"]]]];

(* Graphics3D style options passed to the function apply to every pane (Q23); camera, framing and figure options do not. *)
paneStyle[opts_] := FilterRules[FilterRules[opts, Options[Graphics3D]],
   Except[$droppedOptions | $figureOptions | LabelStyle | Lighting]];

(* ::Section:: *)
(* Top level *)

issueFailure[f_Failure] := (Cases[Normal[f[[2]]], HoldPattern["MessageTemplate" :> m_] :> Message[m, Sequence @@ f[[2]]["MessageParameters"]]]; $Failed);

MultiviewGraphics3D[g_, opts : OptionsPattern[{MultiviewGraphics3D, Graphics3D}]] := MultiviewGraphics3D[g, "QuadView", opts];
MultiviewGraphics3D[g_, layout : Except[_Rule | _RuleDelayed], opts : OptionsPattern[{MultiviewGraphics3D, Graphics3D}]] :=
  Module[{input = normalizeInput[g], matrix},
   matrix = layoutMatrix[layout, OptionValue[MultiviewGraphics3D, {opts}, "ProjectionConvention"]];
   Which[
    FailureQ[input], issueFailure[input],
    FailureQ[matrix], issueFailure[matrix],
    (* interactive modes are not built yet: static for now *)
    True, staticFigure[input, matrix, {opts}]]];

End[];
EndPackage[];
