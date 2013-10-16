{-# LANGUAGE OverloadedStrings #-}


module Text.BetaCode where


import           Control.Applicative
import           Control.Error
import           Data.Char
import           Data.Monoid

import           Data.Attoparsec.Text
import qualified Data.Text              as T
import           Data.Text.Lazy         (toStrict)
import           Data.Text.Lazy.Builder


-- BetaCode conversion

diacritic :: Parser Char
diacritic = choice [ char' ')'  '\x0313'  --   ̓  )   Smooth breathing    ἐν  E)N
                   , char' '('  '\x0314'  --   ̔  (   Rough breathing ὁ, οἱ   O(, OI(
                   , char' '/'  '\x0301'  --  ́   /   Acute accent    πρός    PRO/S
                   , char' '='  '\x0342'  --   ͂  =   Circumflex accent   τῶν TW=N
                   , char' '\\' '\x0300'  --  ̀   \   Grave accent    πρὸς    PRO\S
                   , char' '+'  '\x0308'  --   ̈  +   Diaeresis   προϊέναι    PROI+E/NAI
                   , char' '|'  '\x0345'  --   ͅ  |   Iota subscript  τῷ  TW=|
                   , char' '&'  '\x0304'  --   ̄  &   Macron  μαχαίρᾱς    MAXAI/RA&S
                   -- , char' '\'' '\x0306'  --   ̆  '   Breve   μάχαιρᾰ MA/XAIRA'
                   , char' '?'  '\x0323'  -- combining dot below
                   ]

punct :: Parser Char
punct = choice [ char  '.'            -- .	.	Period
               , char  ','            -- ,	,	Comma
               , char' ':'  '\x00b7'  -- ·	:	Colon (Ano Stigme)
               , char  ';'            -- ;	;	Question Mark
               , char' '\'' '\x2019'  -- ’	'	Apostrophe
               , char' '-'  '\x2010'  -- ‐	-	Hyphen
               , char' '_'  '\x2014'  -- —	_	Dash
               ]

lowercase :: Parser Char
lowercase = choice [ beta  'A'  'a'  '\x03b1'  -- Α    *A  Alpha   α   A
                   , beta  'B'  'b'  '\x03b2'  -- Β    *B  Beta    β   B
                   , beta  'G'  'g'  '\x03b3'  -- Γ    *G  Gamma   γ   G
                   , beta  'D'  'd'  '\x03b4'  -- Δ    *D  Delta   δ   D
                   , beta  'E'  'e'  '\x03b5'  -- Ε    *E  Epsilon ε   E
                   , beta  'V'  'v'  '\x03dd'  -- Ϝ    *V  Digamma ϝ   V
                   , beta  'Z'  'z'  '\x03b6'  -- Ζ    *Z  Zeta    ζ   Z
                   , beta  'H'  'h'  '\x03b7'  -- Η    *H  Eta η   H
                   , beta  'Q'  'q'  '\x03b8'  -- Θ    *Q  Theta   θ   Q
                   , beta  'I'  'i'  '\x03b9'  -- Ι    *I  Iota    ι   I
                   , beta  'K'  'k'  '\x03ba'  -- Κ    *K  Kappa   κ   K
                   , beta  'L'  'l'  '\x03bb'  -- Λ    *L  Lambda  λ   L
                   , beta  'M'  'm'  '\x03bc'  -- Μ    *M  Mu  μ   M
                   , beta  'N'  'n'  '\x03bd'  -- Ν    *N  Nu  ν   N
                   , beta  'C'  'c'  '\x03be'  -- Ξ    *C  Xi  ξ   C
                   , beta  'O'  'o'  '\x03bf'  -- Ο    *O  Omicron ο   O
                   , beta  'P'  'p'  '\x03c0'  -- Π    *P  Pi  π   P
                   , beta  'R'  'r'  '\x03c1'  -- Ρ    *R  Rho ρ   R
                   , beta' "S1" "s1" '\x03c3'  -- Σ    *S  Medial Sigma    σ   S, S1
                   , beta' "S2" "s2" '\x03c2'  -- Σ    *S  Final Sigma ς   S, S2, J
                   , beta' "S3" "s3" '\x03f2'  -- Ϲ    *S (*S3)    Lunate Sigma    ϲ   S (S3)
                   , beta  'J'  'j'  '\x03c2'  -- Σ    *S  Final Sigma ς   S, S2, J
                   , sigma
                   -- , beta  'S'  's'  '\x03c3'  -- Σ    *S  Medial Sigma    σ   S, S1
                   -- , beta  'S'  's'  '\x03c2'  -- Σ    *S  Final Sigma ς   S, S2, J
                   -- , beta  'S'  's'  '\x03f2'  -- Ϲ    *S (*S3)    Lunate Sigma    ϲ   S (S3)
                   , beta  'T'  't'  '\x03c4'  -- Τ    *T  Tau τ   T
                   , beta  'U'  'u'  '\x03c5'  -- Υ    *U  Upsilon υ   U
                   , beta  'F'  'f'  '\x03c6'  -- Φ    *F  Phi φ   F
                   , beta  'X'  'x'  '\x03c7'  -- Χ    *X  Chi χ   X
                   , beta  'Y'  'y'  '\x03c8'  -- Ψ    *Y  Psi ψ   Y
                   , beta  'W'  'w'  '\x03c9'  -- Ω    *W  Omega   ω   W
                   ]

char' :: Char -> Char -> Parser Char
char' c d = char c *> pure d

beta :: Char -> Char -> Char -> Parser Char
beta c d e = (char c <|> char d) *> pure e

beta' :: T.Text -> T.Text -> Char -> Parser Char
beta' c d e = (string c <|> string d) *> pure e

sigma :: Parser Char
sigma = do
    (char 'S' <|> char 's')
    eow <- endOfWord
    pure $ if eow
               then '\x03c2'
               else '\x03c3'

diacritics :: Parser Builder
diacritics = fromString <$> many' diacritic

upperseq :: Parser Builder
upperseq = char '*' *> ((<>) <$> (flip (<>) <$> diacritics
                                            <*> (singleton . toUpper <$> lowercase))
                             <*> diacritics)

lowerseq :: Parser Builder
lowerseq = (<>) <$> (singleton <$> lowercase) <*> diacritics

endOfWord :: Parser Bool
endOfWord = eow . maybe ' ' id <$> peekChar
    where eow '.'  = True
          eow ','  = True
          eow ':'  = True
          eow ';'  = True
          eow '\'' = True
          eow '-'  = True
          eow '_'  = True
          eow x    = isSpace x

betaCode :: Parser T.Text
betaCode =   toStrict . toLazyText . mconcat
         <$> (many' (space' <|> upperseq <|> lowerseq <|> punct') <* endOfInput)
    where space' = singleton <$> space
          punct' = singleton <$> punct

fromBeta :: T.Text -> Either T.Text T.Text
fromBeta t = fmapL (const errMsg) $ parseOnly betaCode t
    where errMsg = "ERROR " <> t

fromBetaIgnore :: T.Text -> T.Text
fromBetaIgnore = either id id . fromBeta

