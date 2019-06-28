import System.IO  
import Words

main :: IO ()
main = do
        handle <- openFile "../wordlist_utf8.txt" ReadMode
        contents <- hGetContents handle
        let singlewords = words contents
        let all_anas = all_anagrams singlewords
        print (take 50 all_anas)
        print (length all_anas)
        hClose handle   
