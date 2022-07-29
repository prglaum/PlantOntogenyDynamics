(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 11.3' *)

(***************************************************************************)
(*                                                                         *)
(*                                                                         *)
(*  Under the Wolfram FreeCDF terms of use, this file and its content are  *)
(*  bound by the Creative Commons BY-SA Attribution-ShareAlike license.    *)
(*                                                                         *)
(*        For additional information concerning CDF licensing, see:        *)
(*                                                                         *)
(*         www.wolfram.com/cdf/adopting-cdf/licensing-options.html         *)
(*                                                                         *)
(*                                                                         *)
(***************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1088,         20]
NotebookDataLength[     30105,        681]
NotebookOptionsPosition[     30416,        676]
NotebookOutlinePosition[     30760,        691]
CellTagsIndexPosition[     30717,        688]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{
Cell[BoxData[
 StyleBox[
  RowBox[{
   RowBox[{"Simon", " ", "&"}], " ", "Glaum", " ", "et", " ", 
   RowBox[{"al", ".", " ", "2022"}]}], "Section"]], "Input",
 CellChangeTimes->{{3.8670026336664543`*^9, 3.867002635120226*^9}, 
   3.867003057639945*^9},
 CellLabel->"In[25]:=",ExpressionUUID->"0234ed9f-bc8b-4d04-a637-d7d9bcd162bb"],

Cell[BoxData[{
 RowBox[{
  StyleBox[
   RowBox[{"Manipulatable", " ", "Parameters"}], "Subsubsection"], 
  StyleBox[" ", "Subsubsection"]}], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"rF", ":", " ", 
   RowBox[{"Intrinsic", " ", "reproduction", " ", 
    RowBox[{"(", "seed", ")"}], " ", "rate", " ", "of", " ", "fecund", " ", 
    "plant", " ", "individuals", " ", 
    RowBox[{"(", "F", ")"}]}]}], "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"g12", ":", " ", 
   RowBox[{"Germination", " ", "rate", " ", "of", " ", "seeds", " ", 
    RowBox[{"(", "S1", ")"}], " ", "into", " ", "seedlings", " ", 
    RowBox[{"(", "S2", ")"}]}]}], "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"g2F", ":", " ", 
   RowBox[{"Maturation", " ", "rate", " ", "of", " ", "seedlings", " ", 
    RowBox[{"(", "S2", ")"}], " ", "into", " ", "fecund", " ", "adults", " ", 
    
    RowBox[{"(", "F", ")"}]}]}], "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"aF", ":", " ", 
   RowBox[{
   "Attack", " ", "rate", " ", "of", " ", "the", " ", "herbivore", " ", 
    RowBox[{"(", "H", ")"}], " ", "on", " ", "the", " ", "fecund", " ", 
    "adult", " ", "plant", " ", "population", " ", 
    RowBox[{"(", "F", ")"}]}]}], "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"a2", ":", " ", 
   RowBox[{
   "Attack", " ", "rate", " ", "of", " ", "the", " ", "herbivore", " ", 
    RowBox[{"(", "H", ")"}], " ", "on", " ", "the", " ", "seedling", " ", 
    "population", " ", 
    RowBox[{"(", "S2", ")"}]}]}], "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"c", ":", " ", 
   RowBox[{
   "Conversion", " ", "rate", " ", "of", " ", "eaten", " ", "adult", " ", 
    "plants", " ", 
    RowBox[{"(", "cFH", ")"}], " ", "or", " ", "seedlings", " ", 
    RowBox[{"(", "c2H", ")"}], " ", "into", " ", "herbivores"}]}], 
  "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"\[Alpha]F", ",", "\[Alpha]g1", ",", "\[Alpha]g2", ",", 
   RowBox[{"\[Alpha]FS", ":", " ", 
    RowBox[{
    "Strength", " ", "of", " ", "density", " ", "dependence", " ", 
     "affecting", " ", "seed", " ", "production"}]}], ",", " ", 
   RowBox[{"seed", " ", "germination"}], ",", " ", 
   RowBox[{"seedling", " ", "maturation"}], ",", " ", 
   RowBox[{"and", " ", "seed", " ", "survival", " ", "respectively"}]}], 
  "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"\[Epsilon]", " ", ":", 
   RowBox[{
   "Parameter", " ", "mitigating", " ", "density", " ", "dependent", " ", 
    "attenuation", " ", "of", " ", "seeds", " ", "on", " ", "germination"}]}],
   "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"dF", ",", " ", "dS", ",", " ", 
   RowBox[{"dH", ":", 
    RowBox[{
    "Death", " ", "rates", " ", "for", " ", "the", " ", "flowering", " ", 
     "plant"}]}], ",", " ", 
   RowBox[{"seeds", "/", "seedlings"}], " ", ",", " ", 
   RowBox[{"and", " ", "herbivore"}]}], 
  "Subsubsection"], "\[IndentingNewLine]", 
 StyleBox[
  RowBox[{"hF", ",", 
   RowBox[{"h2", ":", 
    RowBox[{
    "Handling", " ", "times", " ", "for", " ", "herbivory", " ", "on", " ", 
     "reproductive", " ", "adults", " ", 
     RowBox[{"(", "hF", ")"}], " ", "and", " ", "seedling", " ", "predation", 
     " ", 
     RowBox[{"(", "h2", ")"}]}]}]}], "Subsubsection"]}], "Text", "Input",
 CellChangeTimes->{{3.867861819022106*^9, 3.867861948855137*^9}, {
   3.8678620083295507`*^9, 3.867862016830741*^9}, {3.867862108742972*^9, 
   3.867862143817544*^9}, {3.8678621898151293`*^9, 3.867862401642597*^9}, {
   3.867862541506315*^9, 3.867862702934711*^9}, {3.8678627475173492`*^9, 
   3.8678627672021713`*^9}, 3.8681124449626265`*^9, {3.868112653920602*^9, 
   3.8681126753809357`*^9}, {3.8681127569075346`*^9, 3.868112758107332*^9}, 
   3.8681132522814627`*^9},ExpressionUUID->"369efd5f-0d34-465b-bf22-\
6c972ee046a8"],

Cell[CellGroupData[{

Cell[BoxData[
 RowBox[{
  RowBox[{"(*", 
   RowBox[{"Clear", " ", "Variable", " ", "Names"}], "*)"}], 
  "\[IndentingNewLine]", 
  RowBox[{
   RowBox[{"Clear", "[", 
    RowBox[{"F", ",", "S2", ",", "S1", ",", " ", "H", ",", " ", "t"}], "]"}], 
   "\[IndentingNewLine]", 
   RowBox[{"(*", 
    RowBox[{"Run", " ", "time", " ", "of", " ", "simulation"}], "*)"}], 
   "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{"endtime", "=", "1000"}], ";"}], "\[IndentingNewLine]", 
   "\[IndentingNewLine]", 
   RowBox[{"Manipulate", "[", "\[IndentingNewLine]", 
    RowBox[{
     RowBox[{
      RowBox[{"Model", "=", 
       RowBox[{"NDSolve", "[", 
        RowBox[{
         RowBox[{"{", 
          RowBox[{
           RowBox[{
            RowBox[{
             RowBox[{"F", "'"}], "[", "t", "]"}], "\[Equal]", 
            RowBox[{
             RowBox[{
              RowBox[{"(", 
               RowBox[{"g2", "*", 
                RowBox[{"S2", "[", "t", "]"}]}], ")"}], "/", 
              RowBox[{"(", 
               RowBox[{"1", "+", 
                RowBox[{"\[Alpha]2", "*", 
                 RowBox[{"(", 
                  RowBox[{
                   RowBox[{"F", "[", "t", "]"}], "+", 
                   RowBox[{"\[Epsilon]", 
                    RowBox[{"(", 
                    RowBox[{"S2", "[", "t", "]"}], ")"}]}]}], ")"}]}]}], 
               ")"}]}], "-", 
             RowBox[{
              RowBox[{"(", 
               RowBox[{"aF", "*", 
                RowBox[{"F", "[", "t", "]"}], "*", 
                RowBox[{"H", "[", "t", "]"}]}], ")"}], "/", 
              RowBox[{"(", 
               RowBox[{"1", "+", 
                RowBox[{"aF", "*", "hF", "*", 
                 RowBox[{"F", "[", "t", "]"}]}], "+", 
                RowBox[{"a2", "*", "h2", "*", 
                 RowBox[{"S2", "[", "t", "]"}]}]}], ")"}]}], "-", 
             RowBox[{"dF", "*", 
              RowBox[{"F", "[", "t", "]"}]}]}]}], ",", 
           RowBox[{
            RowBox[{
             RowBox[{"S2", "'"}], "[", "t", "]"}], "\[Equal]", 
            RowBox[{
             RowBox[{
              RowBox[{"(", 
               RowBox[{"g1", "*", 
                RowBox[{"S1", "[", "t", "]"}]}], ")"}], "/", 
              RowBox[{"(", 
               RowBox[{"1", "+", 
                RowBox[{"\[Alpha]1", "*", 
                 RowBox[{"(", 
                  RowBox[{
                   RowBox[{"F", "[", "t", "]"}], "+", 
                   RowBox[{"\[Epsilon]", 
                    RowBox[{"(", 
                    RowBox[{
                    RowBox[{"S1", "[", "t", "]"}], "+", 
                    RowBox[{"S2", "[", "t", "]"}]}], ")"}]}]}], ")"}]}]}], 
               ")"}]}], "-", 
             RowBox[{
              RowBox[{"(", 
               RowBox[{"g2", "*", 
                RowBox[{"S2", "[", "t", "]"}]}], ")"}], "/", 
              RowBox[{"(", 
               RowBox[{"1", "+", 
                RowBox[{"\[Alpha]2", "*", 
                 RowBox[{"(", 
                  RowBox[{
                   RowBox[{"F", "[", "t", "]"}], "+", 
                   RowBox[{"\[Epsilon]", 
                    RowBox[{"(", 
                    RowBox[{"S2", "[", "t", "]"}], ")"}]}]}], ")"}]}]}], 
               ")"}]}], "-", 
             RowBox[{
              RowBox[{"(", 
               RowBox[{"a2", "*", 
                RowBox[{"S2", "[", "t", "]"}], "*", 
                RowBox[{"H", "[", "t", "]"}]}], ")"}], "/", 
              RowBox[{"(", 
               RowBox[{"1", "+", 
                RowBox[{"aF", "*", "hF", "*", 
                 RowBox[{"F", "[", "t", "]"}]}], "+", 
                RowBox[{"a2", "*", "h2", "*", 
                 RowBox[{"S2", "[", "t", "]"}]}]}], ")"}]}], "-", 
             RowBox[{"dS", "*", 
              RowBox[{"S2", "[", "t", "]"}]}]}]}], ",", 
           RowBox[{
            RowBox[{
             RowBox[{"S1", "'"}], "[", "t", "]"}], "\[Equal]", 
            RowBox[{
             RowBox[{"Max", "[", 
              RowBox[{
               RowBox[{
                RowBox[{"F", "[", "t", "]"}], "*", 
                RowBox[{"(", 
                 RowBox[{"rF", "-", 
                  RowBox[{"\[Alpha]F", "*", 
                   RowBox[{"F", "[", "t", "]"}]}]}], ")"}]}], ",", "0"}], 
              "]"}], "-", 
             RowBox[{
              RowBox[{"(", 
               RowBox[{"g1", "*", 
                RowBox[{"S1", "[", "t", "]"}]}], ")"}], "/", 
              RowBox[{"(", 
               RowBox[{"1", "+", 
                RowBox[{"\[Alpha]1", "*", 
                 RowBox[{"(", 
                  RowBox[{
                   RowBox[{"F", "[", "t", "]"}], "+", 
                   RowBox[{"\[Epsilon]", 
                    RowBox[{"(", 
                    RowBox[{
                    RowBox[{"S1", "[", "t", "]"}], "+", 
                    RowBox[{"S2", "[", "t", "]"}]}], ")"}]}]}], ")"}]}]}], 
               ")"}]}], "-", 
             RowBox[{
              RowBox[{"S1", "[", "t", "]"}], "*", 
              RowBox[{"(", 
               RowBox[{
                RowBox[{"\[Alpha]FS", "*", 
                 RowBox[{"F", "[", "t", "]"}]}], "+", "dS"}], ")"}]}]}]}], 
           ",", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{
             RowBox[{"H", "'"}], "[", "t", "]"}], "\[Equal]", 
            RowBox[{
             RowBox[{"H", "[", "t", "]"}], "*", 
             RowBox[{"(", 
              RowBox[{
               RowBox[{
                RowBox[{"(", 
                 RowBox[{"c", "*", "aF", "*", 
                  RowBox[{"F", "[", "t", "]"}]}], ")"}], "/", 
                RowBox[{"(", 
                 RowBox[{"1", "+", 
                  RowBox[{"aF", "*", "hF", "*", 
                   RowBox[{"F", "[", "t", "]"}]}], "+", 
                  RowBox[{"a2", "*", "h2", "*", 
                   RowBox[{"S2", "[", "t", "]"}]}]}], ")"}]}], "+", 
               RowBox[{
                RowBox[{"(", 
                 RowBox[{"c", "*", "a2", "*", 
                  RowBox[{"S2", "[", "t", "]"}]}], ")"}], "/", 
                RowBox[{"(", 
                 RowBox[{"1", "+", 
                  RowBox[{"aF", "*", "hF", "*", 
                   RowBox[{"F", "[", "t", "]"}]}], "+", 
                  RowBox[{"a2", "*", "h2", "*", 
                   RowBox[{"S2", "[", "t", "]"}]}]}], ")"}]}], "-", "dH"}], 
              ")"}]}]}], ",", "\[IndentingNewLine]", 
           RowBox[{
            RowBox[{"F", "[", "0", "]"}], "\[Equal]", "1"}], ",", 
           RowBox[{
            RowBox[{"S2", "[", "0", "]"}], "\[Equal]", "1"}], ",", 
           RowBox[{
            RowBox[{"S1", "[", "0", "]"}], "\[Equal]", "1"}], ",", 
           RowBox[{
            RowBox[{"H", "[", "0", "]"}], "\[Equal]", "1"}]}], "}"}], ",", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"F", "[", "t", "]"}], ",", 
           RowBox[{"S2", "[", "t", "]"}], ",", 
           RowBox[{"S1", "[", "t", "]"}], ",", 
           RowBox[{"H", "[", "t", "]"}]}], "}"}], ",", 
         RowBox[{"{", 
          RowBox[{"t", ",", "0", ",", "endtime"}], "}"}], ",", 
         RowBox[{"StartingStepSize", "\[Rule]", 
          RowBox[{"1", "/", "30"}]}], ",", "\[IndentingNewLine]", 
         RowBox[{"Method", "\[Rule]", 
          RowBox[{"{", 
           RowBox[{"\"\<FixedStep\>\"", ",", 
            RowBox[{"Method", "\[Rule]", "\"\<ExplicitRungeKutta\>\""}]}], 
           "}"}]}], ",", 
         RowBox[{"Method", "\[Rule]", 
          RowBox[{"{", "\"\<StiffnessSwitching\>\"", "}"}]}], ",", 
         RowBox[{"MaxSteps", "\[Rule]", "Infinity"}]}], "]"}]}], ";", 
      "\[IndentingNewLine]", 
      RowBox[{"(*", 
       RowBox[{"Line", " ", "Thickness"}], "*)"}], "\[IndentingNewLine]", 
      RowBox[{"th", "=", ".01"}], ";", "\[IndentingNewLine]", 
      RowBox[{"(*", 
       RowBox[{"Plot", " ", "Model"}], "*)"}], "\[IndentingNewLine]", 
      RowBox[{"Plot", "[", 
       RowBox[{
        RowBox[{"Evaluate", "[", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"F", "[", "t", "]"}], ",", 
            RowBox[{"S2", "[", "t", "]"}], ",", 
            RowBox[{"S1", "[", "t", "]"}], ",", 
            RowBox[{"H", "[", "t", "]"}]}], "}"}], "/.", "Model"}], "]"}], 
        ",", "\[IndentingNewLine]", 
        RowBox[{"{", 
         RowBox[{"t", ",", "00", ",", "endtime"}], "}"}], ",", 
        RowBox[{"PlotRange", "\[Rule]", 
         RowBox[{"{", 
          RowBox[{"All", ",", 
           RowBox[{"{", 
            RowBox[{"0", ",", "Full"}], "}"}]}], "}"}]}], ",", 
        RowBox[{"PlotStyle", "\[Rule]", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Thickness", "[", "th", "]"}], ",", 
             RowBox[{"Darker", "[", "Green", "]"}]}], "}"}], ",", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Thickness", "[", "th", "]"}], ",", 
             RowBox[{"Lighter", "[", "Green", "]"}]}], "}"}], ",", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Thickness", "[", "th", "]"}], ",", "Brown"}], "}"}], 
           ",", 
           RowBox[{"{", 
            RowBox[{
             RowBox[{"Thickness", "[", "th", "]"}], ",", "Red"}], "}"}]}], 
          "}"}]}], ",", 
        RowBox[{"AxesStyle", "\[Rule]", 
         RowBox[{"{", 
          RowBox[{
           RowBox[{"Directive", "[", "20", "]"}], ",", 
           RowBox[{"Directive", "[", "20", "]"}]}], "}"}]}], ",", 
        RowBox[{"PlotLegends", "\[Rule]", 
         RowBox[{"{", 
          RowBox[{
          "\"\<Fecund Adults\>\"", ",", "\"\<Seedling\>\"", ",", 
           "\"\<Seed Bank\>\"", ",", "\"\<Herbivore\>\""}], "}"}]}], ",", 
        RowBox[{
        "PlotLabel", "\[Rule]", 
         "\"\<Three-Stage Plant and Herbivore Model\>\""}], ",", 
        RowBox[{"LabelStyle", "\[Rule]", 
         RowBox[{"{", 
          RowBox[{"\"\<Subitem\>\"", ",", "Black"}], "}"}]}]}], "]"}]}], 
     "\[IndentingNewLine]", "\[IndentingNewLine]", ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"g1", ",", "0.5"}], "}"}], ",", "0", ",", "1", ",", "0.02", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"g2", ",", "0.2"}], "}"}], ",", "0", ",", "1", ",", "0.02", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"\[Alpha]1", ",", "0.06"}], "}"}], ",", "0", ",", "1", ",", 
       "0.02", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"\[Alpha]2", ",", "0.06"}], "}"}], ",", "0", ",", "1", ",", 
       "0.02", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"\[Alpha]F", ",", "0.06"}], "}"}], ",", "0", ",", "1", ",", 
       "0.02", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"a2", ",", "1"}], "}"}], ",", "0", ",", "1.5", ",", "0.03", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"aF", ",", ".2"}], "}"}], ",", "0", ",", "1.5", ",", "0.03", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"h2", ",", "1"}], "}"}], ",", "0", ",", "2", ",", "0.04", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"hF", ",", "1"}], "}"}], ",", "0", ",", "2", ",", "0.04", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"rF", ",", "0.42"}], "}"}], ",", ".00", ",", "3.5", ",", 
       "0.07", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"dS", ",", "0.1"}], "}"}], ",", "0", ",", "1", ",", "0.02", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"dF", ",", "0.1"}], "}"}], ",", "0", ",", "1", ",", "0.02", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"dH", ",", "0.1"}], "}"}], ",", "0", ",", "1", ",", "0.02", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"c", ",", "0.6"}], "}"}], ",", "0", ",", "2", ",", "0.04", 
       ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"\[Epsilon]", ",", "0.2"}], "}"}], ",", "0", ",", "1", ",", 
       "0.02", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     RowBox[{"{", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"\[Alpha]FS", ",", "0.001"}], "}"}], ",", "0", ",", "0.05", 
       ",", "0.002", ",", 
       RowBox[{"Appearance", "\[Rule]", "\"\<Open\>\""}]}], "}"}], ",", 
     "\[IndentingNewLine]", "\[IndentingNewLine]", 
     RowBox[{"ControlPlacement", "\[Rule]", 
      RowBox[{"{", 
       RowBox[{
       "Left", ",", "Left", ",", "Left", ",", "Left", ",", "Left", ",", 
        "Left", ",", "Left", ",", "Left", ",", "Left", ",", "Right", ",", 
        "Right", ",", "Right", ",", "Right", ",", "Right", ",", "Right", ",", 
        "Right", ",", "Right", ",", "Right", ",", " ", "Right"}], "}"}]}]}], 
    "\[IndentingNewLine]", "]"}]}]}]], "Input",
 CellChangeTimes->{{3.867002651953624*^9, 3.867002681996505*^9}, {
   3.867002714025893*^9, 3.8670027147484417`*^9}, {3.867002873691001*^9, 
   3.8670029874067717`*^9}, {3.867003055340267*^9, 3.8670030606124268`*^9}, {
   3.8678612081933928`*^9, 3.867861212418014*^9}, {3.867861250695252*^9, 
   3.8678612531006002`*^9}, {3.867861474880604*^9, 3.8678615304191732`*^9}, {
   3.86786159186695*^9, 3.8678615933743067`*^9}, {3.867861627396193*^9, 
   3.867861631593347*^9}, {3.8678616867894382`*^9, 3.867861688632517*^9}, {
   3.8681126234088182`*^9, 3.86811262438272*^9}, {3.868112718673035*^9, 
   3.8681127428151913`*^9}, 3.8681127939852247`*^9},
 CellLabel->"In[10]:=",ExpressionUUID->"1d321382-80d6-46f5-9b2e-040162952b12"],

Cell[BoxData[
 TagBox[
  StyleBox[
   DynamicModuleBox[{$CellContext`a2$$ = 1, $CellContext`aF$$ = 
    0.2, $CellContext`c$$ = 0.6, $CellContext`dF$$ = 0.1, $CellContext`dH$$ = 
    0.1, $CellContext`dS$$ = 0.1, $CellContext`g1$$ = 0.5, $CellContext`g2$$ =
     0.2, $CellContext`h2$$ = 1, $CellContext`hF$$ = 1, $CellContext`rF$$ = 
    0.42000000000000004`, $CellContext`\[Alpha]1$$ = 
    0.06, $CellContext`\[Alpha]2$$ = 0.06, $CellContext`\[Alpha]F$$ = 
    0.06, $CellContext`\[Alpha]FS$$ = 0.001, $CellContext`\[Epsilon]$$ = 0.2, 
    Typeset`show$$ = True, Typeset`bookmarkList$$ = {}, 
    Typeset`bookmarkMode$$ = "Menu", Typeset`animator$$, Typeset`animvar$$ = 
    1, Typeset`name$$ = "\"untitled\"", Typeset`specs$$ = {{{
       Hold[$CellContext`g1$$], 0.5}, 0, 1, 0.02}, {{
       Hold[$CellContext`g2$$], 0.2}, 0, 1, 0.02}, {{
       Hold[$CellContext`\[Alpha]1$$], 0.06}, 0, 1, 0.02}, {{
       Hold[$CellContext`\[Alpha]2$$], 0.06}, 0, 1, 0.02}, {{
       Hold[$CellContext`\[Alpha]F$$], 0.06}, 0, 1, 0.02}, {{
       Hold[$CellContext`a2$$], 1}, 0, 1.5, 0.03}, {{
       Hold[$CellContext`aF$$], 0.2}, 0, 1.5, 0.03}, {{
       Hold[$CellContext`h2$$], 1}, 0, 2, 0.04}, {{
       Hold[$CellContext`hF$$], 1}, 0, 2, 0.04}, {{
       Hold[$CellContext`rF$$], 0.42}, 0., 3.5, 0.07}, {{
       Hold[$CellContext`dS$$], 0.1}, 0, 1, 0.02}, {{
       Hold[$CellContext`dF$$], 0.1}, 0, 1, 0.02}, {{
       Hold[$CellContext`dH$$], 0.1}, 0, 1, 0.02}, {{
       Hold[$CellContext`c$$], 0.6}, 0, 2, 0.04}, {{
       Hold[$CellContext`\[Epsilon]$$], 0.2}, 0, 1, 0.02}, {{
       Hold[$CellContext`\[Alpha]FS$$], 0.001}, 0, 0.05, 0.002}}, 
    Typeset`size$$ = {498., {118., 124.}}, Typeset`update$$ = 0, 
    Typeset`initDone$$, Typeset`skipInitDone$$ = 
    True, $CellContext`g1$20179$$ = 0, $CellContext`g2$20180$$ = 
    0, $CellContext`\[Alpha]1$20181$$ = 0, $CellContext`\[Alpha]2$20182$$ = 
    0, $CellContext`\[Alpha]F$20183$$ = 0, $CellContext`a2$20184$$ = 
    0, $CellContext`aF$20185$$ = 0, $CellContext`h2$20186$$ = 
    0, $CellContext`hF$20187$$ = 0, $CellContext`rF$20188$$ = 0}, 
    DynamicBox[Manipulate`ManipulateBoxes[
     1, StandardForm, 
      "Variables" :> {$CellContext`a2$$ = 1, $CellContext`aF$$ = 
        0.2, $CellContext`c$$ = 0.6, $CellContext`dF$$ = 
        0.1, $CellContext`dH$$ = 0.1, $CellContext`dS$$ = 
        0.1, $CellContext`g1$$ = 0.5, $CellContext`g2$$ = 
        0.2, $CellContext`h2$$ = 1, $CellContext`hF$$ = 1, $CellContext`rF$$ = 
        0.42, $CellContext`\[Alpha]1$$ = 0.06, $CellContext`\[Alpha]2$$ = 
        0.06, $CellContext`\[Alpha]F$$ = 0.06, $CellContext`\[Alpha]FS$$ = 
        0.001, $CellContext`\[Epsilon]$$ = 0.2}, "ControllerVariables" :> {
        Hold[$CellContext`g1$$, $CellContext`g1$20179$$, 0], 
        Hold[$CellContext`g2$$, $CellContext`g2$20180$$, 0], 
        Hold[$CellContext`\[Alpha]1$$, $CellContext`\[Alpha]1$20181$$, 0], 
        Hold[$CellContext`\[Alpha]2$$, $CellContext`\[Alpha]2$20182$$, 0], 
        Hold[$CellContext`\[Alpha]F$$, $CellContext`\[Alpha]F$20183$$, 0], 
        Hold[$CellContext`a2$$, $CellContext`a2$20184$$, 0], 
        Hold[$CellContext`aF$$, $CellContext`aF$20185$$, 0], 
        Hold[$CellContext`h2$$, $CellContext`h2$20186$$, 0], 
        Hold[$CellContext`hF$$, $CellContext`hF$20187$$, 0], 
        Hold[$CellContext`rF$$, $CellContext`rF$20188$$, 0]}, 
      "OtherVariables" :> {
       Typeset`show$$, Typeset`bookmarkList$$, Typeset`bookmarkMode$$, 
        Typeset`animator$$, Typeset`animvar$$, Typeset`name$$, 
        Typeset`specs$$, Typeset`size$$, Typeset`update$$, Typeset`initDone$$,
         Typeset`skipInitDone$$}, 
      "Body" :> ($CellContext`Model = 
        NDSolve[{
          Derivative[
            1][$CellContext`F][$CellContext`t] == ($CellContext`g2$$ \
$CellContext`S2[$CellContext`t])/(
             1 + $CellContext`\[Alpha]2$$ ($CellContext`F[$CellContext`t] + \
$CellContext`\[Epsilon]$$ $CellContext`S2[$CellContext`t])) - \
(($CellContext`aF$$ $CellContext`F[$CellContext`t]) \
$CellContext`H[$CellContext`t])/(
            1 + ($CellContext`aF$$ $CellContext`hF$$) \
$CellContext`F[$CellContext`t] + ($CellContext`a2$$ $CellContext`h2$$) \
$CellContext`S2[$CellContext`t]) - $CellContext`dF$$ \
$CellContext`F[$CellContext`t], 
           Derivative[
            1][$CellContext`S2][$CellContext`t] == ($CellContext`g1$$ \
$CellContext`S1[$CellContext`t])/(
             1 + $CellContext`\[Alpha]1$$ ($CellContext`F[$CellContext`t] + \
$CellContext`\[Epsilon]$$ ($CellContext`S1[$CellContext`t] + \
$CellContext`S2[$CellContext`t]))) - ($CellContext`g2$$ \
$CellContext`S2[$CellContext`t])/(
            1 + $CellContext`\[Alpha]2$$ ($CellContext`F[$CellContext`t] + \
$CellContext`\[Epsilon]$$ $CellContext`S2[$CellContext`t])) - \
(($CellContext`a2$$ $CellContext`S2[$CellContext`t]) \
$CellContext`H[$CellContext`t])/(
            1 + ($CellContext`aF$$ $CellContext`hF$$) \
$CellContext`F[$CellContext`t] + ($CellContext`a2$$ $CellContext`h2$$) \
$CellContext`S2[$CellContext`t]) - $CellContext`dS$$ \
$CellContext`S2[$CellContext`t], 
           Derivative[1][$CellContext`S1][$CellContext`t] == 
           Max[$CellContext`F[$CellContext`t] ($CellContext`rF$$ - \
$CellContext`\[Alpha]F$$ $CellContext`F[$CellContext`t]), 
              0] - ($CellContext`g1$$ $CellContext`S1[$CellContext`t])/(
            1 + $CellContext`\[Alpha]1$$ ($CellContext`F[$CellContext`t] + \
$CellContext`\[Epsilon]$$ ($CellContext`S1[$CellContext`t] + \
$CellContext`S2[$CellContext`t]))) - $CellContext`S1[$CellContext`t] \
($CellContext`\[Alpha]FS$$ $CellContext`F[$CellContext`t] + \
$CellContext`dS$$), 
           Derivative[
            1][$CellContext`H][$CellContext`t] == \
$CellContext`H[$CellContext`t] ((($CellContext`c$$ $CellContext`aF$$) \
$CellContext`F[$CellContext`t])/(
              1 + ($CellContext`aF$$ $CellContext`hF$$) \
$CellContext`F[$CellContext`t] + ($CellContext`a2$$ $CellContext`h2$$) \
$CellContext`S2[$CellContext`t]) + (($CellContext`c$$ $CellContext`a2$$) \
$CellContext`S2[$CellContext`t])/(
              1 + ($CellContext`aF$$ $CellContext`hF$$) \
$CellContext`F[$CellContext`t] + ($CellContext`a2$$ $CellContext`h2$$) \
$CellContext`S2[$CellContext`t]) - $CellContext`dH$$), $CellContext`F[0] == 
           1, $CellContext`S2[0] == 1, $CellContext`S1[0] == 
           1, $CellContext`H[0] == 1}, {
           $CellContext`F[$CellContext`t], 
           $CellContext`S2[$CellContext`t], 
           $CellContext`S1[$CellContext`t], 
           $CellContext`H[$CellContext`t]}, {$CellContext`t, 
           0, $CellContext`endtime}, StartingStepSize -> 1/30, 
          Method -> {"FixedStep", Method -> "ExplicitRungeKutta"}, 
          Method -> {"StiffnessSwitching"}, MaxSteps -> 
          Infinity]; $CellContext`th = 0.01; Plot[
         Evaluate[
          ReplaceAll[{
            $CellContext`F[$CellContext`t], 
            $CellContext`S2[$CellContext`t], 
            $CellContext`S1[$CellContext`t], 
            $CellContext`H[$CellContext`t]}, $CellContext`Model]], \
{$CellContext`t, 0, $CellContext`endtime}, PlotRange -> {All, {0, Full}}, 
         PlotStyle -> {{
            Thickness[$CellContext`th], 
            Darker[Green]}, {
            Thickness[$CellContext`th], 
            Lighter[Green]}, {
            Thickness[$CellContext`th], Brown}, {
            Thickness[$CellContext`th], Red}}, AxesStyle -> {
           Directive[20], 
           Directive[20]}, 
         PlotLegends -> {
          "Fecund Adults", "Seedling", "Seed Bank", "Herbivore"}, PlotLabel -> 
         "Three-Stage Plant and Herbivore Model", 
         LabelStyle -> {"Subitem", Black}]), 
      "Specifications" :> {{{$CellContext`g1$$, 0.5}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`g2$$, 0.2}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`\[Alpha]1$$, 0.06}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`\[Alpha]2$$, 0.06}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`\[Alpha]F$$, 0.06}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`a2$$, 1}, 0, 1.5, 0.03, Appearance -> 
         "Open"}, {{$CellContext`aF$$, 0.2}, 0, 1.5, 0.03, Appearance -> 
         "Open"}, {{$CellContext`h2$$, 1}, 0, 2, 0.04, Appearance -> 
         "Open"}, {{$CellContext`hF$$, 1}, 0, 2, 0.04, Appearance -> 
         "Open"}, {{$CellContext`rF$$, 0.42}, 0., 3.5, 0.07, Appearance -> 
         "Open"}, {{$CellContext`dS$$, 0.1}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`dF$$, 0.1}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`dH$$, 0.1}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`c$$, 0.6}, 0, 2, 0.04, Appearance -> 
         "Open"}, {{$CellContext`\[Epsilon]$$, 0.2}, 0, 1, 0.02, Appearance -> 
         "Open"}, {{$CellContext`\[Alpha]FS$$, 0.001}, 0, 0.05, 0.002, 
         Appearance -> "Open"}}, 
      "Options" :> {
       ControlPlacement -> {
         Left, Left, Left, Left, Left, Left, Left, Left, Left, Right, Right, 
          Right, Right, Right, Right, Right, Right, Right, Right}}, 
      "DefaultOptions" :> {}],
     ImageSizeCache->{1024., {260., 266.}},
     SingleEvaluation->True],
    Deinitialization:>None,
    DynamicModuleValues:>{},
    SynchronousInitialization->True,
    UndoTrackedVariables:>{Typeset`show$$, Typeset`bookmarkMode$$},
    UnsavedVariables:>{Typeset`initDone$$},
    UntrackedVariables:>{Typeset`size$$}], "Manipulate",
   Deployed->True,
   StripOnInput->False],
  Manipulate`InterpretManipulate[1]]], "Output",
 CellChangeTimes->{
  3.867003022347678*^9, 3.867861224369117*^9, 3.867861257029714*^9, {
   3.867861516256311*^9, 3.8678615336167803`*^9}, 3.867861598375546*^9, 
   3.867861637799446*^9, 3.867861692197214*^9, 3.868112249285221*^9, 
   3.868112630426119*^9, {3.8681127713148227`*^9, 3.868112795645343*^9}},
 CellLabel->"Out[12]=",ExpressionUUID->"5a487be1-dfb2-4200-8a86-61f838b3f64a"]
}, Open  ]]
},
WindowSize->{1049, 607},
WindowMargins->{{88, Automatic}, {-16, Automatic}},
FrontEndVersion->"11.3 for Microsoft Windows (64-bit) (March 6, 2018)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[1488, 33, 332, 7, 49, "Input",ExpressionUUID->"0234ed9f-bc8b-4d04-a637-d7d9bcd162bb"],
Cell[1823, 42, 3868, 85, 286, "Text",ExpressionUUID->"369efd5f-0d34-465b-bf22-6c972ee046a8"],
Cell[CellGroupData[{
Cell[5716, 131, 14718, 363, 637, "Input",ExpressionUUID->"1d321382-80d6-46f5-9b2e-040162952b12"],
Cell[20437, 496, 9963, 177, 545, "Output",ExpressionUUID->"5a487be1-dfb2-4200-8a86-61f838b3f64a"]
}, Open  ]]
}
]
*)

(* NotebookSignature GuTkNvPCRK8HdDKEoYuHz5QX *)
