#!/usr/bin/env wolframscript
(* Run all Tests/*.wlt (or those named on the command line) against the package. Messages count as failures
   (TestReport records them per test). Exit code 1 if anything fails. *)
root = ParentDirectory[DirectoryName[$InputFileName]];
Get[FileNameJoin[{root, "MultiviewGraphics3D.wl"}]];
files = If[Length[$ScriptCommandLine] > 1, FileNameJoin[{root, "Tests", #}] & /@ Rest[$ScriptCommandLine],
   FileNames["*.wlt", FileNameJoin[{root, "Tests"}]]];
reports = TestReport /@ files;
MapThread[Function[{f, r},
   Print[FileNameTake[f], ": ", r["TestsSucceededCount"], " passed, ", r["TestsFailedCount"], " failed"];
   Scan[Print["  FAIL ", #["TestID"], "\n    expected: ", ToString[#["ExpectedOutput"], InputForm],
       "\n    actual:   ", ToString[#["ActualOutput"], InputForm],
       If[#["ActualMessages"] =!= {}, "\n    messages: " <> ToString[#["ActualMessages"], InputForm], ""]] &,
    Join @@ Values[r["TestsFailed"]]]], {files, reports}];
Exit[If[AllTrue[reports, #["AllTestsSucceeded"] &], 0, 1]]
