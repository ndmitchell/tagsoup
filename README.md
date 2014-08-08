# TagSoup [![Hackage version](https://img.shields.io/hackage/v/tagsoup.svg?style=flat)](http://hackage.haskell.org/package/tagsoup) [![Build Status](http://img.shields.io/travis/ndmitchell/tagsoup.svg?style=flat)](https://travis-ci.org/ndmitchell/tagsoup)

TagSoup is a library for parsing HTML/XML. It supports the HTML 5 specification, and can be used to parse either well-formed XML, or unstructured and malformed HTML from the web. The library also provides useful functions to extract information from an HTML document, making it ideal for screen-scraping.

This document gives two particular examples of scraping information from the web, while a few more may be found in the [Sample](https://github.com/ndmitchell/tagsoup/blob/master/TagSoup/Sample.hs) file from the source repository. The examples we give are:

* Obtaining the Hit Count from Haskell.org
* Obtaining a list of Simon Peyton-Jones' latest papers
* A brief overview of some other examples

The intial version of this library was written in Javascript and has been used for various commercial projects involving screen scraping. In the examples general hints on screen scraping are included, learnt from bitter experience. It should be noted that if you depend on data which someone else may change at any given time, you may be in for a shock!

This library was written without knowledge of the Java version of [TagSoup](http://home.ccil.org/~cowan/XML/tagsoup/). They have made a very different design decision: to ensure default attributes are present and to properly nest parsed tags. We do not do this - tags are merely a list devoid of nesting information.


#### Acknowledgements

Thanks to Mike Dodds for persuading me to write this up as a library. Thanks to many people for debugging and code contributions, including: Gleb Alexeev, Ketil Malde, Conrad Parker, Henning Thielemann, Dino Morelli, Emily Mitchell, Gwern Branwen.


## Potential Bugs

There are two things that may go wrong with these examples:

* _The Websites being scraped may change._ There is nothing I can do about this, but if you suspect this is the case let me know, and I'll update the examples and tutorials. I have already done so several times, its only a few minutes work.
* _The `openURL` method may not work._ This happens quite regularly, and depending on your server, proxies and direction of the wind, they may not work. The solution is to use `wget` to download the page locally, then use `readFile` instead. Hopefully a decent Haskell HTTP library will emerge, and that can be used instead.


## Haskell Hit Count

Our goal is to develop a program that displays the Haskell.org hit count. This example covers all the basics in designing a basic web-scraping application.

### Finding the Page

We first need to find where the information is displayed, and in what format. Taking a look at the [front web page](http://www.haskell.org/haskellwiki/Haskell), when not logged in, we see:

    <ul id="f-list">
        <li id="lastmod"> This page was last modified on 9 September 2013, at 22:38.</li>
        <li id="viewcount">This page has been accessed 6,985,922 times.</li>
        <li id="copyright">Recent content is available under <a href="/haskellwiki/HaskellWiki:Copyrights" title="HaskellWiki:Copyrights">a simple permissive license</a>.</li>
        <li id="privacy"><a href="/haskellwiki/HaskellWiki:Privacy_policy" title="HaskellWiki:Privacy policy">Privacy policy</a></li>
        <li id="about"><a href="/haskellwiki/HaskellWiki:About" title="HaskellWiki:About">About HaskellWiki</a></li>
        <li id="disclaimer"><a href="/haskellwiki/HaskellWiki:General_disclaimer" title="HaskellWiki:General disclaimer">Disclaimers</a></li>
    </ul>

So we see the hit count is available. This leads us to rule 1:

**Rule 1:** Scrape from what the page returns, not what a browser renders, or what view-source gives.

Some web servers will serve different content depending on the user agent, some browsers will have scripting modify their displayed HTML, some pages will display differently depending on your cookies. Before you can start to figure out how to start scraping, first decide what the input to your program will be. There are two ways to get the page as it will appear to your program.

#### Using the HTTP package

We can write a simple HTTP downloader with using the [HTTP package](http://hackage.haskell.org/package/HTTP):

    import Network.HTTP
    
    openURL x = getResponseBody =<< simpleHTTP (getRequest x)
    
    main = do src <- openURL "http://www.haskell.org/haskellwiki/Haskell"
              writeFile "temp.htm" src

Now open `temp.htm`, find the fragment of HTML containing the hit count, and examine it.

#### Using the `tagsoup` Program

Tagsoup installs both as a library and a program. The program contains all the examples mentioned on this page, along with a few other useful functions. In order to download a URL to a file:

    $ tagsoup grab http://www.haskell.org/haskellwiki/Haskell > temp.htm

### Finding the Information

Now we examine both the fragment that contains our snippet of information, and the wider page. What does the fragment has that nothing else has? What algorithm would we use to obtain that particular element? How can we still return the element as the content changes? What if the design changes? But wait, before going any further:

**Rule 2:** Do not be robust to design changes, do not even consider the possibility when writing the code.

If the user changes their website, they will do so in unpredictable ways. They may move the page, they may put the information somewhere else, they may remove the information entirely. If you want something robust talk to the site owner, or buy the data from someone. If you try and think about design changes, you will complicate your design, and it still won't work. It is better to write an extraction method quickly, and happily rewrite it when things change.

So now, lets consider the fragment from above. It is useful to find a tag which is unique just above your snippet - something with a nice "id" property, or a "class" - something which is unlikely to occur multiple times. In the above example, "viewcount" as the id seems perfect.

    haskellHitCount = do
        src <- openURL "http://haskell.org/haskellwiki/Haskell"
        let count = fromFooter $ parseTags src
        putStrLn $ "haskell.org has been hit " ++ count ++ " times"
        where fromFooter = filter isDigit . innerText . take 2 . dropWhile (~/= "<li id=viewcount>")

Now we start writing the code! The first thing to do is open the required URL, then we parse the code into a list of `Tag`s with `parseTags`. The `fromFooter` function does the interesting thing, and can be read left to right:

* First we throw away everything (`dropWhile`) until we get to an `li` tag containing `id=viewcount`. The `(~==)` operator is different from standard equality, allowing additional attributes to be present. We write `"<li id=viewcount>"` as syntactic sugar for `TagOpen "li" [("id","viewcount")]`. If we just wanted any open tag with the given id we could have written `(~== TagOpen "" [("id","viewcount")])` and this would have matched. Any empty strings in the second element of the match are considered as wildcards.
* Next we take two elements, the `<li>` tag and the text node immediately following.
* We call the `innerText` function to get all the text values from inside, which will just be the text node following the `viewcount`.
* We keep only the numbers, getting rid of the surrounding text and the commas.

This code may seem slightly messy, and indeed it is - often that is the nature of extracting information from a tag soup.

**Rule 3:** TagSoup is for extracting information where structure has been lost, use more structured information if it is available.


## Simon's Papers

Our next very important task is to extract a list of all Simon Peyton Jones' recent research papers off his [home page](http://research.microsoft.com/en-us/people/simonpj/). The largest change to the previous example is that now we desire a list of papers, rather than just a single result.

As before we first start by writing a simple program that downloads the appropriate page, and look for common patterns. This time we want to look for all patterns which occur every time a paper is mentioned, but no where else. The other difference from last time is that previous we grabbed an automatically generated piece of information - this time the information is entered in a more freeform way by a human.

First we spot that the page helpfully has named anchors, there is a current work anchor, and after that is one for Haskell. We can extract all the information between them with a simple `take`/`drop` pair:

    takeWhile (~/= "<a name=haskell>") $
    drop 5 $ dropWhile (~/= "<a name=current>") tags

This code drops until you get to the "current" section, then takes until you get to the "haskell" section, ensuring we only look at the important bit of the page. Next we want to find all hyperlinks within this section:

    map f $ sections (~== "<A>") $ ...

Remember that the function to select all tags with name "A" could have been written as `(~== TagOpen "A" [])`, or alternatively `isTagOpenName "A"`. Afterwards we map each item with an `f` function. This function needs to take the tags starting just after the link, and find the text inside the link.

    f = dequote . unwords . words . fromTagText . head . filter isTagText

Here the complexity of interfacing to human written markup comes through. Some of the links are in italic, some are not - the `filter` drops all those that are not, until we find a pure text node. The `unwords . words` deletes all multiple spaces, replaces tabs and newlines with spaces and trims the front and back - a neat trick when dealing with text which has spacing at the source code but not when displayed. The final thing to take account of is that some papers are given with quotes around the name, some are not - dequote will remove the quotes if they exist.

For completeness, we now present the entire example:
    
    spjPapers :: IO ()
    spjPapers = do
            tags <- fmap parseTags $ openURL "http://research.microsoft.com/en-us/people/simonpj/"
            let links = map f $ sections (~== "<A>") $
                        takeWhile (~/= "<a name=haskell>") $
                        drop 5 $ dropWhile (~/= "<a name=current>") tags
            putStr $ unlines links
        where
            f :: [Tag] -> String
            f = dequote . unwords . words . fromTagText . head . filter isTagText
    
            dequote ('\"':xs) | last xs == '\"' = init xs
            dequote x = x

## Other Examples

Several more examples are given in the Example file, including obtaining the (short) list of papers from my site, getting the current time and a basic XML validator. All can be invoked using the `tagsoup` executable program. All use very much the same style as presented here - writing screen scrapers follow a standard pattern. We present the code from two for enjoyment only.

### My Papers

    ndmPapers :: IO ()
    ndmPapers = do
            tags <- fmap parseTags $ openURL "http://community.haskell.org/~ndm/downloads/"
            let papers = map f $ sections (~== "<li class=paper>") tags
            putStr $ unlines papers
        where
            f :: [Tag] -> String
            f xs = fromTagText (xs !! 2)

### UK Time

    currentTime :: IO ()
    currentTime = do
        tags <- fmap parseTags $ openURL "http://www.timeanddate.com/worldclock/city.html?n=136"
        let time = fromTagText (dropWhile (~/= "<strong id=ct>") tags !! 1)
        putStrLn time