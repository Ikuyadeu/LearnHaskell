-- $ echo '"&<>' | runghc escape.hs
-- &quot;&amp&lt&gt;

import HTMLEscapedString

main :: IO ()
main = do
	rawString <- getLine
	let escapedString = escape rawString
	putHTMLEscapedStrLn escapedString
