import Data.Maybe
import Data.List
import Data.Char

-- supported tags
tags :: [String]
tags = ["html", "head", "title", "body", "h1", "h2", "h3", "p", "ul", "li", "a", "div", "br", "hr"]

hasTags :: String -> Bool
hasTags (x:xs) = 
                if x == '/' 
                    then  xs `elem` tags 
            else (x:xs) `elem` tags

split :: Eq a => a -> [a] -> [[a]]
split d [] = []
split d s = x : split d (drop 1 y) where (x,y) = span (/= d) s

takeTagName :: String -> String
takeTagName input = do
                    let tagAttributes = split ' ' input
                    if length tagAttributes == 1
                        then init $ head tagAttributes
                    else if last (last tagAttributes) == '>' && elemIndex '<' input == Nothing
                        then (head tagAttributes)
                    else input

checkTags :: [String] -> Bool
checkTags [] = True
checkTags (x:xs) = if (hasTags x && checkTags xs) then True else error "invalid tags"

addCloseTag :: String -> String
addCloseTag input = ['/'] ++ input

performCheckStructure :: [String] -> [String] -> Bool
-- base case exhausted all the tags inside the list and the stack is also empty then the structure must be valid
performCheckStructure [] []= True
-- recursively run through all the list of tags and at the end the stack should be empty to make sure each tag has matching closing tag
performCheckStructure (x:xs) stack = do
                            if (head x) == '/' 
                                then 
                                    let topStack = last stack
                                        currentStack = init stack
                                        
                                    in (addCloseTag topStack) == x && performCheckStructure xs currentStack
                            else if x == "br" || x == "hr" 
                                then
                                    if True && performCheckStructure xs stack then True else error "mismatched tags"
                                else
                                    let newStack = (stack ++ [x]) in
                                    if True && performCheckStructure xs newStack then True else error "mismatched tags"

checkStructure :: [String] -> Bool
checkStructure input = performCheckStructure input []

checkInsideHTML :: [String] -> Bool
checkInsideHTML input = if head input == "html" && last input == "/html" then True else error "all tags need to be inside <html> tag except <html> tag itself"

checkParagraphNotInParagraph :: [String] -> Bool
checkParagraphNotInParagraph [] = True
checkParagraphNotInParagraph (x:xs) = if x == "p" && head xs == "p" then error "p tag cannot be nested in another p tag" else True && checkParagraphNotInParagraph xs

checkDivNotInParagraph :: [String] -> Bool
checkDivNotInParagraph [] = True
checkDivNotInParagraph (x:xs) = if x == "p" && head xs == "div" then error "Div incorrectly nested inside p tag" else True && checkDivNotInParagraph xs

checkHeadTagBeforeBody :: [String] -> Bool
checkHeadTagBeforeBody input = if elemIndex "head" input < elemIndex "body" input then True else error "head tag cannot come after body tag"

checkTagOccurrence :: String -> [String] -> Int -> Bool
checkTagOccurrence tag input occurrence = (length $ filter (==tag) input) == occurrence

checkHTMLTagOccurrence :: [String] -> Bool
checkHTMLTagOccurrence input = if checkTagOccurrence "html" input 1 then True else error "duplicate html tags"

checkBodyTagOccurrence :: [String] -> Bool
checkBodyTagOccurrence input = if checkTagOccurrence "body" input 1 then True else error "duplicate body tags"

checkHeadTagOccurrence :: [String] -> Bool
checkHeadTagOccurrence input = checkTagOccurrence "head" input 0 || checkTagOccurrence "head" input 1

checkTitleTagOccurrence :: [String] -> Bool
checkTitleTagOccurrence input = checkTagOccurrence "title" input 0 || checkTagOccurrence "title" input 1

checkTagsOccurrence :: [String] -> Bool
checkTagsOccurrence input = checkHTMLTagOccurrence input && checkBodyTagOccurrence input && checkHeadTagOccurrence input && checkTitleTagOccurrence input

filterTagsNotInBody :: [String] -> [String]
filterTagsNotInBody input = filter (\x -> x /= "html" && x /= "/html" && x /= "head" && x /= "/head" && x /= "title" && x /= "/title") input

checkTagsInsideBody :: [String] -> Bool
checkTagsInsideBody input = do 
                            -- filter out html head and title then check that all the rest of the tags are inside the body tag
                            let filtered = filterTagsNotInBody input
                            if ((head filtered) == "body" && last filtered == "/body") then True else error "tags other than <html>, <title>, <head> and <body> should be children of either body, not children of <html>"

checkTitleTagInsideHead :: [String] -> Bool
checkTitleTagInsideHead input = do 
                                    let endTitleIndex = elemIndex "/title" input
                                        endHeadIndex = elemIndex "/head" input
                                    if (endTitleIndex /= Nothing) && 
                                        (endHeadIndex /= Nothing) && 
                                        checkTagOccurrence "title" input 1 && 
                                        checkTagOccurrence "head" input 1
                                        -- since elemIndex has a type of Maybe and we already check that it is not Nothing then use fromJust to convert it to actual type
                                        then if (fromJust endTitleIndex) + 1 == fromJust endHeadIndex then True else error "only title tag can be children of head tag"
                                    else True

-- takeWhile but also include the character that it stops on
takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs
                                        else []

tokenize :: String -> [String]
tokenize "" = []
tokenize (x:xs) = 
    case x of
        '<' -> takeTagName (takeWhileInclusive (/='>') xs) : tokenize xs
        _ -> tokenize xs

checks :: [[String] -> Bool]
checks = [checkInsideHTML, checkTags, checkStructure, checkParagraphNotInParagraph, checkDivNotInParagraph, checkHeadTagBeforeBody, checkTagsOccurrence, checkTagsInsideBody, checkTitleTagInsideHead]

runChecks :: [String] -> [[String] -> Bool] -> Bool
-- exhausted all the tests and have not stopped the program with error message, so the tags list must be valid
runChecks _ [] = True
runChecks tags (f:fs) = f tags && runChecks tags fs


parseMessage :: String -> IO()
parseMessage "" = putStrLn "empty file not valid"
parseMessage message = do
                        let tags = tokenize message
                        let stack = []
                        let result = runChecks tags checks

                        if result then putStrLn "valid" else putStrLn "not valid"

main :: IO()
main = do
   fileContent <- readFile "file.html"
   parseMessage fileContent