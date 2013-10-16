{-# LANGUAGE OverloadedStrings #-}

module BetaCodeSpec where


import           Data.Monoid
import qualified Data.Text     as T

import           Text.BetaCode

-- import           Control.Exception (evaluate)
import           Test.Hspec
-- import           Test.QuickCheck


shouldBeBetaFor :: T.Text -> T.Text -> Expectation
shouldBeBetaFor a b = fromBetaIgnore a `shouldBe` b


spec :: Spec
spec = describe "betaCode" $ do

    it "should translate 'a' to alpha." $
        "a" `shouldBeBetaFor` "α"
    it "should translate 'B' to beta." $
        "B" `shouldBeBetaFor` "β"
    it "should translate '*B' to capital beta." $
        "*B" `shouldBeBetaFor` "Β"

    it "should translate medial sigma correctly." $ do
        "as1a" `shouldBeBetaFor` "ασα"
        " asa" `shouldBeBetaFor` " ασα"
    it "should translate final sigma correctly." $ do
        "aas2" `shouldBeBetaFor` "αας"
        " aas" `shouldBeBetaFor` " αας"

    it "should translate punctuation characters." $ do
        "." `shouldBeBetaFor` "."
        "_" `shouldBeBetaFor` "—"

    it "should translate the smooth breathing example." $
        "E)N" `shouldBeBetaFor` "ἐν"
    it "should translate the rough breathing examples." $ do
        "O("  `shouldBeBetaFor` "ὁ"
        "OI(" `shouldBeBetaFor` "οἱ"
    it "should translate the acute accent example." $
        "PRO/S" `shouldBeBetaFor` "πρός"
    it "should translate the circumflex accept example." $
        "TW=N" `shouldBeBetaFor` "τῶν"
    it "should translate the grave accent example." $
        "PRO\\S" `shouldBeBetaFor` "πρὸς"
    it "should translate the diaeresis example." $
        "PROI+E/NAI" `shouldBeBetaFor` "προϊέναι"
    it "should translate the iota subscript example." $
        "TW=|" `shouldBeBetaFor` "τῷ"
    it "should translate the macron example." $
        "MAXAI/RA&S" `shouldBeBetaFor` "μαχαίρᾱς"
    it "should NOT translate the breve example." $
        "MA/XAIRA'" `shouldBeBetaFor` "μάχαιρα᾽"

    it "should recognize periods." $
        "EN." `shouldBeBetaFor` "εν."
    it "should recognize commas." $
        "EN," `shouldBeBetaFor` "εν,"
    it "should NOT recognize colons (ano stigme)." $
        "EN:" `shouldBeBetaFor` "εν:"
    it "should recognize question marks." $
        "EN;" `shouldBeBetaFor` "εν;"
    it "should recognize an apostrophe as a Greek koronis." $
        "'EN" `shouldBeBetaFor` "᾽εν"
    it "should recongize a hyphen." $
        "EN-EN" `shouldBeBetaFor` "εν‐εν"
    it "should recongize a dash." $
        "EN_EN" `shouldBeBetaFor` "εν—εν"

    it "should return incorrect lines with an error marker." $
        let input  = "(/hfaiste"
            output = "ERROR " <> input
        in  input `shouldBeBetaFor` output

