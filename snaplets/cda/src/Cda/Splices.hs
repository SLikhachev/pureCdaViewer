{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Cda.Splices (
    cdaDocument
    ) where

-- import  Debug.Trace
import  qualified Data.Text as T
import  Text.XML.Expat.Tree
-- import Text.XML.Expat.Format
import  Text.XML.Expat.Proc
-- import qualified Data.ByteString.Lazy as L
import  Data.Maybe ( fromJust, maybe, catMaybes )
import  Control.Monad
import  Control.Monad.State
import  Data.Monoid

import  Snap.Snaplet.Heist
import  Heist
import  qualified Heist.Interpreted as I
import  qualified Text.XmlHtml as X
import  Application

----------------------------
strM:: T.Text -> Maybe T.Text
strM s =  if (T.length s) > 0 then Just s else Nothing
----------------------------
tagTextM:: T.Text -> UNode T.Text -> Maybe T.Text
tagTextM stag node  = findChild stag node >>= (strM . textContent) 
----------------------------
attrTextM:: T.Text -> T.Text -> UNode T.Text ->  Maybe T.Text
attrTextM stag sattr node = flip getAttribute sattr =<< findChild stag node
----------------------------
-- mapSplices f vs = liftM mconcat $ mapM f vs
-- mapSplices :: (Monad n) => (a -> Splice n n) -> [a] -> Splice n n
pSpl:: Monad n => T.Text -> T.Text -> Splices (I.Splice n)
pSpl tag text = tag ## (I.textSplice text)

pLSpl:: Monad n => T.Text -> T.Text -> [Splices (I.Splice n)]
pLSpl tag text = [pSpl tag text]

noSpl:: Monad n => I.Splice n
noSpl = return mempty

-------- Dcument General -----
docTitle, docDate:: Monad n => UNode T.Text -> Splices (I.Splice n)
docTitle node = do
  let 
    defTitle = Just ("Clinical document"::T.Text)
    tagTitle = tagTextM "title" node
    codeName = attrTextM "code" "displayName" node
  pSpl "cdaTitle" $ fromJust $ tagTitle `mplus` codeName `mplus` defTitle
----------------------------
docDate n = maybe mempty (pSpl "cdaDate") (attrTextM  "effectiveTime" "value" n)

docAuthors:: UNode T.Text -> SnapletISplice App
docAuthors node = maybe noSpl (dAuth . findElements  "assignedAuthor") aa
  where
    aa = findChild "author" node
    dAuth [] = noSpl
    dAuth (a: as) = ( I.runChildrenWith (docAuthor a)) -- <> pRole as

docAuthor:: Monad n => UNode T.Text -> Splices (I.Splice n)
docAuthor prn = mconcat (aName ++ aAddr ++ aTele)
  where 
    aName = maybe [] get_name (findChild "assignedPerson" prn)
    aAddr = maybe [] get_addr (findChild "addr" prn)
    aTele = maybe [] (pLSpl "aTele") (attrTextM  "telecom" "value" prn)

-------- Patients ----------

docPatients:: UNode T.Text -> SnapletISplice App
docPatients node = maybe noSpl (pRole . findElements  "patientRole") rt
  where
    rt = findChild "recordTarget" node
    pRole [] = noSpl
    pRole (e: es) = ( I.runChildrenWith (docPatient e))

docPatient:: Monad n => UNode T.Text -> Splices (I.Splice n)
docPatient prn = mconcat (pName ++ pAddr ++ pTele)
  where 
    pName = maybe [] pNspl (findChild "patient" prn)
    pAddr = maybe [] pAspl (findChild "addr" prn)
    pTele = maybe [] (pLSpl "pTele") (attrTextM  "telecom" "value" prn)
    pNspl p = 
      (get_name p) ++ (get_birth p) ++ (get_ethnos p) ++ (get_race p) ++ (get_gender p) 
    pAspl p = (get_addr p)

get_name, get_birth, get_gender, get_race, get_ethnos, get_addr:: Monad n => 
  UNode T.Text -> [Splices (I.Splice n)]
get_name prn = maybe [] pname (findChild "name" prn)
  where 
    tags = ["prefix", "given", "family", "suffix"]
    pname n = [ pSpl e pn |
       e <- tags, let pn = ( maybe "" id (tagTextM e n) ) ]

get_birth prn = maybe [] (pLSpl "pBirth") (attrTextM  "birthTime" "value" prn)
       
get_gender prn = maybe [] g (attrTextM  "administrativeGenderCode" "code" prn)
  where
    gs = pLSpl "pGender"
    g sex = case sex of
      "F" -> gs "Female" 
      "M" -> gs "Male"
      _ -> gs "N/A"

get_race prn = maybe [] (pLSpl "pRace") (attrTextM  "raceCode" "displayName" prn)
get_ethnos prn = maybe [] (pLSpl "pEthnos") (attrTextM  "ethnicGroupCode" "displayName" prn)            

get_addr nadr = [ pSpl e pn | e <- tags, let pn = ( maybe "N/A" id (tagTextM e nadr) ) ]
  where 
    tags = ["streetAddressLine", "city", "state", "postalCode", "country"]
    
--------- Sections -----------------

-- drop page*pageItems take pageItems
data Slist = Slist { 
  sLlength:: Int,
  sPageBy:: Int,
  sPage:: Int
} deriving Show

sectionsList:: UNode T.Text -> [UNode T.Text]
sectionsList node = catMaybes $ map fSect cc
 where
    fSect = findChild "section"
    getSBody s = (findChild "structuredBody" <=< findChild "component") s
    cc = maybe [] (findElements "component") (getSBody node)

docSections:: UNode T.Text -> Int -> Int -> State Slist [UNode T.Text]
docSections node pageBy page = do
  let 
    fSect = sectionsList node
    sectS = length fSect
    sp = [0, 5, 10, 20]
    spBy = if (elem pageBy sp) && (pageBy < sectS) then pageBy else 0
    tpAg = if page < 1 || page > 100 then 1 else page   
    reduce = (take spBy) . (drop $ (tpAg-1)*spBy)
    pAge = if (spBy == 0) then id else reduce
  put Slist { sLlength = sectS, sPageBy = spBy, sPage = tpAg }
  return $ pAge fSect

----------------------------
getSTitle:: UNode T.Text -> T.Text
getSTitle = (maybe "No section title" id) . tagTextM "title"

sTitles:: [UNode T.Text] -> [T.Text]
sTitles ns = map getSTitle ns
-----------------------------
docSectionsXText:: [UNode T.Text] -> [X.Node]
docSectionsXText sectList = fmap mkSection sectList
{-  where
    sl = concatMap getTxt sectList
    getTxt n = maybe [] ltx (findChild "text" n)
    ltx n = n:[]
-}
-----------------------------
mkSection:: UNode T.Text -> X.Node
mkSection sn = 
  X.Element "li" [] [
    ( X.Element "div" [("class", "collapsible-header")] [( X.TextNode (getSTitle sn) )] ),
    ( X.Element "div" [("class", "collapsible-body")] skids)
  ] 
  where
    skids = (fmap expatTrans textChildren) ++ (nestedSection sn)
    textChildren = maybe [] ltx (findChild "text" sn)
    ltx (Element _ _ tkids) = tkids

----------------------------------------
nestedSection:: UNode T.Text -> [X.Node]
nestedSection sn = mkNsect (findElements "component" sn)
  where
    mkNsect [] = []
    mkNsect (c:cs) = (tNsect $ getSc c) ++ (mkNsect cs)
    getSc = findChild "section"

tNsect:: Maybe (UNode T.Text) -> [X.Node]
tNsect ms = maybe [] nestS ms
  where
    nsT _ns = [ X.Element "span" [("class", "next-section-title")] [ X.TextNode (getSTitle _ns) ] ]
    nestS sn = [  X.Element "div" [("class", "next-section")] ((nsT sn) ++ (nkids sn)) ]

    nkids nd = (fmap expatTrans $ textChildren nd) ++ (nestedSection nd)
    textChildren nd = maybe [] ltx (findChild "text" nd)
    ltx (Element _ _ tkids) = tkids

-- TODO make the return type of List for concat (many nodes from one)
expatTrans:: UNode T.Text -> X.Node 
expatTrans node = let fc = fmap expatTrans in
  case node of
    (Element "paragraph" _ c) -> X.Element "p" [("class", "tsect-par")] (fc c)
    (Element "caption" _ c) -> X.Element "p" [("class", "tsect-capt")] (fc c)
    (Element "content" attr c) -> X.Element "span" ( attr ++ [("class", "tsect-cnt")] )  (fc c)
    
    (Element "pre" _ c) -> X.Element "pre" [("class", "tsect-pre")] (fc c)
    (Element "br" _ _) -> X.Element "br" [] []
    (Element "sup" _ c) -> X.Element "sup" [("class", "tsect-sup")] (fc c)
    (Element "sub" _ c) -> X.Element "sub" [("class", "tsect-sub")] (fc c)
    (Element "footnote" _ c) -> X.Element "span" [("class", "tsect-footnt")] (fc c)
    
    (Element "list" attr c) -> cdaList (getAttribute node "styleCode" ) c
    (Element "item" attr c) -> X.Element "li" [("class", "collection-item")] (fc c)
    
    (Element "table" attr c) -> X.Element "table" [("class", "tsect-table striped bordered")] (fc c)
    (Element "thead" attr c) -> X.Element "thead" [] (fc c)
    (Element "tfoot" attr c) -> X.Element "tfoot" [] (fc c)
    (Element "tbody" attr c) -> X.Element "tbody" [] (fc c)
    (Element "colgroup" attr c) -> X.Element "colgroup" attr (fc c)
    (Element "col" attr c) -> X.Element "col" attr (fc c)
    (Element "tr" attr c) -> X.Element "tr" attr (fc c)
    (Element "th" attr c) -> X.Element "th" attr (fc c)
    (Element "td" attr c) -> X.Element "td" attr (fc c)

    (Text txt) -> X.TextNode (clrText txt)

    otherwise -> X.TextNode T.empty
------------------------------
clrText:: T.Text -> T.Text    
clrText txt = T.dropAround isSp txt 
  where
    isSp c = if c == '\SP' || c == '\LF' || c == '\CR' || c == '\HT' then True else False  
------------------------------
cdaList:: Maybe T.Text -> [UNode T.Text] -> X.Node
cdaList ltype lchlds = 
  let
    lst = setLtype ltype 
    lattr = setLattr ltype
    lchilds = fmap expatTrans lchlds
    -- capt = head lchlds
  in
  X.Element lst lattr lchilds
-------------------------------
setLtype:: Maybe T.Text -> T.Text
setLtype ltype = maybe "ul" (chooseList . T.toLower) ltype
  where 
    chooseList l = if l == "disc" || l == "circle" || l == "square" then "ul" else "ol"
--------------------------------
setLattr:: Maybe T.Text -> [(T.Text, T.Text)]
setLattr ltype = [("class", cls)] 
  where
    cls = "tsec-list collection " `T.append` maybe "" T.toLower ltype 
{-
sectSpl:: UNode T.Text -> I.Splice AppHandler
sectSpl un = return [ (head . docSectionsXText . docSections) un]
-}

------ Pgination --------------------------------

pagination:: State Slist (SnapletISplice App)
pagination = do
  sL <- get
  return $ if (sPageBy sL) == 0 then (I.textSplice T.empty) else pSplices sL
  
pSplices:: Slist -> SnapletISplice App
pSplices (Slist sLength pageBy page) = I.runChildrenWith (shevLeft <> shevRight <> pList)
  where
    weff = "waves-effect"
    pAttr = "/?page="
    dis = "disabled"
    tside = "#top-side"
    act = "active"
    -- curr = T.pack (show page)
    next = T.pack (show $ page+1)
    prev = T.pack (show $ page-1)
    lastp = quot sLength pageBy

    sLeft = if page == 1 then (dis, tside) else (weff, pAttr `T.append` prev)
    sRight = if page == lastp then (dis, tside) else (weff, pAttr `T.append` next)
    
    shevLeft = (pSpl "shevLeft" (fst sLeft)) <> (pSpl "prevPage" (snd sLeft))
    shevRight = (pSpl "shevRight" (fst sRight)) <> (pSpl "nextPage" (snd sRight))
    
    pList = "pPages" ## return $ liPspl page [1..lastp]
    -- plist = [1..lastp]

liPspl:: Int -> [Int] -> [X.Node]
liPspl page pages = fmap makeLi pages
  where
    makeLi n = X.Element "li" [("class", (waf n) )] [ (anc n) ]
    waf n = "waves-effect" `T.append` (activ n)
    activ n = if n == page then " active" else T.empty
    anc n = X.Element "a" [("href", pref n)] [X.TextNode (pN n)]
    pref n = if n == page then "#top-side" else "?page=" `T.append` T.pack (show n)
    pN n = T.pack (show n)

----- Final splaces for Document ----------------

cdaDocument::HasHeist App => UNode T.Text -> Int -> Int -> Splices (SnapletISplice App)
cdaDocument node pageBy page = dTitle <> dDate <> dAuth <> patsS <> sectS <> sPages
  where
    ----- Cda Header
    dTitle = docTitle node
    dDate = docDate node
    dAuth = "cdaAuthors" ## (docAuthors node)

    ----- Patients
    patsS = "cdaPatients" ## (docPatients node)

{-   ------ Sections List
    stitle t = pSpl "sTitle" t
    cs = I.mapSplices (I.runChildrenWith . stitle) ts
    ts = sTitles $ docSections node
    dcntS = "cdaContent" ## cs
-}   
    ------ Sections content
    --sect = "cdaSection1" ## (sectSpl node)
    (nl, sl) = runState (docSections node pageBy page) (Slist 0 0 0)
    ss = docSectionsXText nl
    nss = I.mapSplices (I.runChildrenWith . sSect) ss
    sSect s = "cdaSection" ## return [s]
    sectS = "cdaSections" ## nss

    ------- Pagination 

    sPages = "pagination" ## evalState pagination sl
