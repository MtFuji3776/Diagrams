module PGFSurface where

import Diagrams.Backend.PGF
import Diagrams.Backend.PGF.Surface
import Diagrams.Prelude
import Parts
import DiagramLanguage
import System.Texrunner


preamble1 :: String
preamble1 = "\\documentclass[dvipdfmx,uplatex]{jsarticle}\n"
    ++ "\\usepackage[utf8]{inputenc}\n"
    ++ "\\usepackage{otf}\n"
    ++ "\\usepackage{booktabs}\n"
    ++ "\\usepackage{array}\n"
    ++ "\\usepackage{graphicx}\n"
    ++ "\\usepackage{mathpazo}\n"
    ++ "\\usepackage{amsmath}\n"
    ++ "\\usepackage{amsthm}\n"
    ++ "\\usepackage{amssymb}\n"
    ++ "\\usepackage{amsfonts}\n"
    -- ++ "\\usepackage{enumitem}\n"
    -- ++ "\\usepackage{tikz}\n"
    -- ++ "\\usepackage[epsilon]{backnaur}\n"
    -- ++ "\\usepackage{bussproofs}\n"
    -- ++ "\\usepackage{media9}\n"
    ++ "\\usepackage[dvipdfmx]{hyperref}\n"
    ++ "\\usepackage{pxjahyper}\n"
    -- ++ "\\usetikzlibrary{matrix}\n"
    -- ++ "\\usetikzlibrary{cd}\n"
    ++ "\\usepackage{pgfcore}\n"
    -- ++ "\\usepackage{CJKutf8}"
    -- ++ "\\newtheorem{dfn}{Def}\n"
    -- ++ "\\newtheorem{thm}{Thm}\n"
    -- ++ "\\newtheorem{cor}{Cor}\n"
    -- ++ "\\newtheorem{prop}{Prop}\n"
    -- ++ "\\newtheorem{rk}{remark}\n"
    -- ++ "\\newtheorem{claim}{claim}\n"
    -- ++ "\\newtheorem{recall}{recall}\n"
    -- ++ "\\newtheorem{ques}{Q.}\n"
    -- ++ "\\newenvironment{diagram}{\n"
    -- ++ "\\begin{center}\\begin{tikzpicture}[-stealth]}{\\end{tikzpicture}\n"
    -- ++ "\\end{center}}\n"
    -- ++ "\\newenvironment{dialan}[1]\n"
    -- ++ "{\\begin{diagram}\\node[matrix]}{\\end{diagram}}\n"
    -- ++ "\\newcommand{\\vertical}[2]{\\draw[-Butt Cap] (0,-0.2) -- (0,#1.2) node[above]{\\small #2};}\n"
    -- ++ "\\newcommand{\\obj}[4]{\\node #1 at (#2,#3) {#4};}\n"
    -- ++ "\\newcommand{\\nolabel}[1]{\\fill #1 circle(2pt);}\n"
    -- ++ "\\newcommand{\\anglebrace}[1]{\\langle #1 \\rangle}\n"
    -- ++ "\\newcommand{\\reticle}[2]{\\draw[-Butt Cap] (#1 - 0.1,#2) -- (#1 + 0.1,#2);\n"
    -- ++ "                         \\draw[-Butt Cap] (#1 , #2 - 0.1) -- (#1 , #2 + 0.1);\n"
    -- ++ "                         \\fill[white] (#1 - 0.05,#2 - 0.05) rectangle (#1 + 0.05,#2 + 0.05);}\n"
    -- ++ "\\title{}\n"
    -- ++ "\\author{}\n"
    -- ++ "\\date{\\today}\n"

preamble2 :: String
preamble2 = "\\documentclass{article}\n"
         ++ "\\usepackage{pgfcore}\n"
         ++ "\\usepackage{mathpazo}\n"
        --  ++ "\\usepackage{amsmath}\n"
        --  ++ "\\usepackage{amssymb}"

uplatexSurface = Surface
    {   _texFormat = LaTeX
    ,   _command   = "pdflatex"
    ,   _arguments = []--"$dvipdf='dvipdfmx %O %S'"]
    ,   _pageSize  = Just $ \(V2 w h) ->
                    "\\pdfpagewidth=" ++ show w ++ "bp\n"
                ++  "\\pdfpageheight=" ++ show h ++ "bp\n"
                ++ "\\textheight=" ++ show h ++ "bp\n"
    ,   _preamble  = preamble2
    ,   _beginDoc  = "\\begin{document}"
    ,   _endDoc    = "\\end{document}"
    }

pgfTest = renderOnlinePGF' "test1.tex" (def & surface .~ uplatexSurface) --(mkSizeSpec2D (Just 400) (Just 300))

