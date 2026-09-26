(* Lighting: the input's lights are kept; "LightingAttachment" says what camera-frame lights are fixed to (Q7, Q26).
   Lights are lists {"Ambient", col} / {"Directional", col, pos}. Camera-frame lights have pos ImageScaled[p], with p in
   (screen right, screen up, towards the viewer) (FINDINGS p2c). Other lights are already fixed in the scene and stay
   as given. *)
BeginTestSection["Lighting"]

PrependTo[$ContextPath, "MultiviewGraphics3D`Private`"];
geo = geometryFromOptions[{{{0, 4}, {0, 2}, {0, 1}}, {{0, 0}, {0, 0}, {0, 0}}, {1, 1/2, 1/4}}];   (* scale 1/4 on every axis *)

(* ---- Plot3D-style inner lighting (FINDINGS round 2) ---- *)

inner = {{"Ambient", GrayLevel[0.3]}, {"Directional", Red, ImageScaled[{0, 0, 2}]}};
VerificationTest[
 stripInnerLighting[Graphics3D[{Directive[Red, Specularity[1], Lighting -> inner], Polygon[{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}],
    {Lighting -> inner, Point[{0, 0, 0}]}}, Boxed -> False]],
 {Graphics3D[{Directive[Red, Specularity[1]], Polygon[{{0, 0, 0}, {1, 0, 0}, {0, 1, 0}}], {Point[{0, 0, 0}]}}, Boxed -> False], inner},
 TestID -> "Strip-inner"]

VerificationTest[stripInnerLighting[Graphics3D[Cuboid[]]], {Graphics3D[Cuboid[]], None}, TestID -> "No-inner"]

(* ---- camera frame -> display frame, for a view ---- *)

(* In Front: towards the viewer = -y, screen right = +x, screen up = +z. *)
VerificationTest[cameraToDisplay[{0, 0, 2}, paneView[Front], geo], {0, -2, 0}, SameTest -> Equal, TestID -> "Front-towards-viewer"]
VerificationTest[cameraToDisplay[{1, 1, 0}, paneView[Front], geo], {1, 0, 1}, SameTest -> Equal, TestID -> "Front-right-up"]
(* ViewVertical is in scaled coordinates; the frame uses its display direction. *)
VerificationTest[cameraToDisplay[{0, 1, 0}, paneView[{1, 0, 1}], geo] . {1, 0, 1}, 0, SameTest -> Equal, TestID -> "Up-orthogonal-to-view"]

(* ---- per-attachment lighting of a pane ---- *)

lights = {{"Ambient", Gray}, {"Directional", Red, ImageScaled[{0, 0, 2}]}};
ref = paneView[Front];

VerificationTest[paneLighting["Camera", lights, ref, geo], lights, TestID -> "Camera-unchanged"]

VerificationTest[
 paneLighting["Object", lights, ref, geo],
 {AmbientLight[Gray], DirectionalLight[Red, {{2, -7, 1/2}, {2, 1, 1/2}}]},
 SameTest -> Equal, TestID -> "Object"]

(* Lights already fixed in the scene stay where they are, in every attachment. *)
VerificationTest[
 paneLighting[#, {{"Directional", Red, {1, 2, 3}}, {"Point", Blue, {0, 0, 5}}, {"Spot", Green, {{0, 0, 5}, {0, 0, 0}}, Pi/8}},
    ref, geo] & /@ {"Object", "Rig"},
 ConstantArray[{DirectionalLight[Red, {1, 2, 3}], PointLight[Blue, {0, 0, 5}], SpotLight[Green, {{0, 0, 5}, {0, 0, 0}}, Pi/8]}, 2],
 TestID -> "Scene-lights-untouched"]

(* Rig: the object lighting rotated with the rig (display-frame rotation). 90 degrees about z takes -y to +x. *)
VerificationTest[
 paneLighting["Rig", lights, ref, geo, RotationMatrix[Pi/2, {0, 0, 1}]],
 {AmbientLight[Gray], DirectionalLight[Red, {{2 + 8, 1, 1/2}, {2, 1, 1/2}}]},
 SameTest -> Equal, TestID -> "Rig-rotated"]

VerificationTest[paneLighting["Rig", lights, ref, geo], paneLighting["Object", lights, ref, geo], TestID -> "Rig-at-rest"]

(* ---- front end: resolve named lighting; object lighting reproduces the reference view exactly (p2b) ---- *)

VerificationTest[
 MatchQ[resolveLighting[Automatic], {({"Ambient", _} | {"Directional", _, ImageScaled[{_, _, _}]}) ..}],
 True, TestID -> "Resolve-Automatic"]

VerificationTest[
 Module[{g = Graphics3D[{Cuboid[{0, 0, 0}, {4, 2, 1}], Sphere[{1, 1, 1}, 0.7]}, Boxed -> False], gg, v, cam, img},
  gg = graphicGeometry[g]; v = paneView[{1, -1, 1}];
  cam = {ViewPoint -> 3 v["Direction"], ViewVertical -> v["Vertical"], ImageSize -> 200};
  img[l_] := Rasterize[Show[g, Lighting -> l, Sequence @@ cam]];
  ImageDistance[img[Automatic], img[paneLighting["Object", resolveLighting[Automatic], v, gg]]] < 10^-3],
 True, TestID -> "Object-reproduces-reference"]

EndTestSection[]
