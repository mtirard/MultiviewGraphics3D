(* View table: pane spec -> camera direction (centre -> camera, unit), ViewVertical, and the box axes shown
   horizontally and vertically (None for oblique views). Orientations as verified in FINDINGS p2. *)
BeginTestSection["Views"]

view = MultiviewGraphics3D`Private`paneView;

VerificationTest[view[Front], <|"Direction" -> {0, -1, 0}, "Vertical" -> {0, 0, 1}, "FaceAxes" -> {1, 3}|>, TestID -> "Front"]
VerificationTest[view[Back], <|"Direction" -> {0, 1, 0}, "Vertical" -> {0, 0, 1}, "FaceAxes" -> {1, 3}|>, TestID -> "Back"]
VerificationTest[view[Right], <|"Direction" -> {1, 0, 0}, "Vertical" -> {0, 0, 1}, "FaceAxes" -> {2, 3}|>, TestID -> "Right"]
VerificationTest[view[Left], <|"Direction" -> {-1, 0, 0}, "Vertical" -> {0, 0, 1}, "FaceAxes" -> {2, 3}|>, TestID -> "Left"]
VerificationTest[view[Above], <|"Direction" -> {0, 0, 1}, "Vertical" -> {0, 1, 0}, "FaceAxes" -> {1, 2}|>, TestID -> "Above"]
VerificationTest[view[Below], <|"Direction" -> {0, 0, -1}, "Vertical" -> {0, -1, 0}, "FaceAxes" -> {1, 2}|>, TestID -> "Below"]

VerificationTest[view[{2, -2, 2}], <|"Direction" -> {1, -1, 1}/Sqrt[3], "Vertical" -> {0, 0, 1}, "FaceAxes" -> None|>,
 TestID -> "Vector-normalized"]

(* A vector along z would make ViewVertical {0,0,1} degenerate: use the Above/Below verticals. *)
VerificationTest[KeyTake[view[{0, 0, 5}], {"Direction", "Vertical"}], KeyTake[view[Above], {"Direction", "Vertical"}],
 TestID -> "Vector-up"]
VerificationTest[view[{0, 0, -0.5}], <|"Direction" -> {0., 0., -1.}, "Vertical" -> {0, -1, 0}, "FaceAxes" -> None|>,
 TestID -> "Vector-down"]

(* Axis-aligned vectors are not the named views: FaceAxes stays None (framing is the user's vector, Q16). *)
VerificationTest[view[{1, 0, 0}]["FaceAxes"], None, TestID -> "Vector-axis-no-face"]

(* The Free pane's start comes from options, so paneView gives the default isometric start. *)
VerificationTest[view["Free"], <|"Direction" -> {1, -1, 1}/Sqrt[3], "Vertical" -> {0, 0, 1}, "FaceAxes" -> None|>,
 TestID -> "Free-default"]

(* Screen frames must be right-handed and consistent: screen right x screen up = towards the camera. *)
VerificationTest[
 AllTrue[{Front, Back, Left, Right, Above, Below},
  With[{v = view[#]}, With[{rt = Cross[-v["Direction"], v["Vertical"]]}, Cross[rt, v["Vertical"]] == v["Direction"]]] &],
 True, TestID -> "Frames-right-handed"]

EndTestSection[]
