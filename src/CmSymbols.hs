module CmSymbols where

import Parts
import Diagrams.Prelude

-- SVGFontファイルからフォントを取得する際の変換器
    -- SVGフォントのファイルではUnicodeが16進数文字列で書かれる
    -- 一方でHaskellではUnicode文字の16進数を10進数で書き表す:'\12345'みたいな感じで。
    -- なのでフォントファイルの記号名をファイルから読み取って16進数文字列で書くと、それを10進数文字に変換する関数が必要
-- encodeは文字列を読み取って一旦Int値にする関数
encode =
    let trans n [] = n
        trans n (x:xs) = trans (digitToInt x + 16 * n) xs
    in trans 0

-- encodeが出力したInt値をChar値に変換
utfInHask = chr . encode

getMathFont filepath unics = do
    x <- loadFont filepath
    let opts = def{textFont = x}
        txt = map utfInHask unics
    return $ textSVG_ opts txt

getMathFont' filepath unics = do
    x <- loadFont filepath
    let opts = def{textFont = x}
    return $ textSVG_ opts unics

-- ==========================================   cmsy5.svgのコーナー   ===================================
-- Unicode名を文字列で渡すと、対応する文字のIO Diagram B値を返す
mathSymbol = getMathFont "cmsy5.svg" 
    -- unics = do
    -- cmsy5 <- loadFont "cmsy5.svg"
    -- let opts = def{textFont = cmsy5}
    --     txt  = map utfInHask unics
    -- return $ textSVG_ opts txt

forall_ = mathSymbol ["f038"]

exists_ = mathSymbol ["f039"]

cup_ = mathSymbol ["f05b"]

cap_ = mathSymbol ["f05c"]

top_ = mathSymbol ["f03e"]

bot_ = mathSymbol ["f03f"]

-- ×記号
times_ = mathSymbol ["f0a3"]

bigotimes_ = mathSymbol ["f0ad"]


-- ==========================================   cmr9.svgのコーナー   =======================================

-- 数字はmathrmの書体が気に入っている
    -- 英数字のみUnicodeと入力記号が一致するようなので、それらに関してはgetMathFontを介さない方が良さそうだ
    -- エンコードしないバージョンのgetMathFont'という関数を作った
mathNumber = getMathFont' "cmr9.svg"



-- ========================================== cmmi10.svgのコーナー

mathAlphabet = getMathFont' "cmmi10.svg"
