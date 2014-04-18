import Control.Monad
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment

-- Define a Parser for all non-alpha-numeric symbols we want
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

-- Define a Parser for any n consecutive spaces 
spaces :: Parser ()
spaces = skipMany1 space
	
-- Define a Data Type for LispVal 	
data LispVal = Atom String
		| List [LispVal]
		| DottedList [LispVal] LispVal
		| Number Integer
		| String String
		| Bool Bool

parseString :: Parser LispVal
parseString = do
            char '"'
            x <- many (noneOf "\"")
            char '"'
            return $ String x

parseAtom :: Parser LispVal
parseAtom = do 
                first <- letter <|> symbol
                rest <- many (letter <|> digit <|> symbol)
                let atom = first:rest
                return $ case atom of 
                           "#t" -> Bool True
                           "#f" -> Bool False
                           _    -> Atom atom

-- Define a Parser for numbers						   
parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit

parseExpr :: Parser LispVal
parseExpr = parseAtom
         <|> parseString
         <|> parseNumber

-- Define the "read expression"	function	 
readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
     Left err -> "No match: " ++ show err
     Right _ -> "Found value"

-- Define the main IO monad		
main :: IO ()
main = do 
	args <- getArgs
	putStrLn (readExpr (args !! 0))