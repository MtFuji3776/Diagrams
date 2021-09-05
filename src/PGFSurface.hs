module PGFSurface where

import Diagrams.Backend.PGF
import Diagrams.Backend.PGF.Surface
import Diagrams.Prelude
import Parts hiding(sizeSpec)
import DiagramLanguage hiding(open)
import System.Texrunner
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import qualified Data.Text.IO as TIO
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as Byte


preamble1 :: String
preamble1 = "\\documentclass{ltjsarticle}\n"
    -- ++ "\\usepackage[utf8]{inputenc}\n"
    ++ "\\usepackage{luatexja-otf}\n"
    ++ "\\usepackage[]{luatexja,luatexja-fontspec}"
    ++ "\\usepackage{booktabs}\n"
    ++ "\\usepackage{array}\n"
    ++ "\\usepackage{graphicx}\n"
    ++ "\\usepackage{mathpazo}\n"
    ++ "\\usepackage{amsmath}\n"
    ++ "\\usepackage{amsthm}\n"
    ++ "\\usepackage{amssymb}\n"
    ++ "\\usepackage{amsfonts}\n"
    ++ "\\usepackage[noheadfoot,top=0mm,bottom=0mm,hmargin=-5mm]{geometry}\n"
    -- ++ "\\usepackage{enumitem}\n"
    -- ++ "\\usepackage{tikz}\n"
    -- ++ "\\usepackage[epsilon]{backnaur}\n"
    -- ++ "\\usepackage{bussproofs}\n"
    -- ++ "\\usepackage{media9}\n"
    -- ++ "\\usepackage[dvipdfmx]{hyperref}\n"
    -- ++ "\\usepackage{pxjahyper}\n"
    -- ++ "\\usetikzlibrary{matrix}\n"
    -- ++ "\\usetikzlibrary{cd}\n"
    ++ "\\usepackage{pgfcore}\n"
    ++ "\\usepackage{color}\n"
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

preamble3 :: String
preamble3 = "\\documentclass[ja = standard,pdflatex]{bxjsarticle}\n"
         ++ "\\usepackage{pgfcore}\n"
         ++ "\\usepackage{mathpazo}\n"
         ++ "\\usepackage{CJK}"
         ++ "\\usepackage[colorlinks]{hyperref}"

lualatexSurface :: Surface
lualatexSurface = def & command .~ "lualatex"
                      & preamble .~ preamble1
                      & pageSize ?~ (\(V2 w h) -> "\\pagewidth=" ++ show w ++ "bp\n"
                                              ++ "\\pageheight=" ++ show h ++ "bp\n"
                                              ++ "\\textheight=" ++ show h ++ "bp\n")
                      

-- lualatexSurface = Surface
--     {   _texFormat = LaTeX
--     ,   _command   = "lualatex"
--     ,   _arguments = []--"$dvipdf='dvipdfmx %O %S'"]
--     ,   _pageSize  = Just $ \(V2 w h) ->
--                     "\\pdfpagewidth=" ++ show w ++ "bp\n"
--                 ++  "\\pdfpageheight=" ++ show h ++ "bp\n"
--                 ++ "\\textheight=" ++ show h ++ "bp\n"
--     ,   _preamble  = preamble1
--     ,   _beginDoc  = "\\begin{document}"
--     ,   _endDoc    = "\\end{document}"
--     }

pdflatexSurface = def & command .~ "pdflatex"
                      & preamble .~ preamble3

pgfTest s = renderOnlinePGF' "test1.pdf" (def & surface .~ s & standalone .~ False) --(mkSizeSpec2D (Just 400) (Just 300))

luaSurafaceSize w h = def & surface .~ lualatexSurface
                          & sizeSpec .~ (mkSizeSpec2D (Just w) (Just h))

ds = "/Users/fujimotomakoto/Documents/latexs/DailyStrategy/"

renderPGFLua filepath  = renderOnlinePGF' filepath (def & surface .~ lualatexSurface & sizeSpec .~ (mkSizeSpec2D (Just 400) (Just 300))) 

renderPDF' w h = renderOnlinePGF' "/Users/fujimotomakoto/haskell_testing/diagrams/img/test.pdf" $ luaSurafaceSize w h
renderPDF = renderPDF' 400 300

renderTex' w h = renderOnlinePGF' "/Users/fujimotomakoto/Documents/latexs/Notes/Free/Whiteboard/img/test.tex" $ luaSurafaceSize w h
renderTex = renderTex' 300 225

easyRender' w h name = renderOnlinePGF' ("/Users/fujimotomakoto/Documents/latexs/DailyStrategy/202012/img/" ++ name) (luaSurafaceSize w h)
easyRender = easyRender' 300 225 

render' w h path = renderOnlinePGF' path (luaSurafaceSize w h)
render = render' 200 150

renderdom name = renderOnlinePGF' (ds ++ "Work/DomainTheory/img/" ++ name) (luaSurafaceSize 400 300)

rendersato name = renderOnlinePGF' ("/Users/fujimotomakoto/Documents/latexs/DailyStrategy/Work/SatoDrill/img/" ++ name) (luaSurafaceSize 300 225)

dbFilePath :: String
dbFilePath = "/Users/fujimotomakoto/haskell_testing/diagrams/src/test.db"

filePathTex :: String
filePathTex = "/Users/fujimotomakoto/haskell_testing/diagrams/src/temp.tex"

filePathPdf :: String
filePathPdf = "/Users/fujimotomakoto/haskell_testing/diagrams/src/temp.pdf"

renderIntoDb' :: Double -> Double -> (OnlineTex (Diagram PGF)) -> IO ()
renderIntoDb' w h d = do
    renderOnlinePGF' filePathTex (luaSurafaceSize w h) d
    renderOnlinePGF' filePathPdf (luaSurafaceSize w h) d
    tex <- T.pack <$> readFile filePathTex
    pdf <- Byte.readFile filePathPdf
    conn <- open dbFilePath
    execute conn "INSERT INTO test_ (textData,pdfData,titleOfDiagram,dateCreated,dateLastUpdate) VALUES (?,?,'notitle',datetime('now','localtime'),'unset')" ((tex,pdf) :: (Text,ByteString))
    close conn

renderIntoDb = renderIntoDb' 200 150