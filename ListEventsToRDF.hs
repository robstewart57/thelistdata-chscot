{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Posn
import Text.XML.HaXml.Combinators
import Text.XML.HaXml
import Data.RDF
import Data.RDF.TriplesGraph
import Text.RDF.RDF4H.TurtleSerializer
import Network.HTTP.Base
import System.IO
import Data.Maybe
import Network.HTTP
import Text.RDF.RDF4H.TurtleParser
import Network.URI hiding (URI)
import Data.List.Split
import System.Environment (getArgs)

-- 1) Reads The List xml data
-- 2) Processes the file, generating triples
-- 3) Write the RDF graph in memory to a turtle file
main :: IO ()
main = do
  args <- getArgs
  case length args of
   0 -> error showUsage
   _ -> do
    let fname = args !! 0
    f <- readFile fname
    let (Document _ _ root _) = xmlParse fname f
    let rootElem = CElem root noPos
        events =  (tag "ives:IvesMessage" /> elm) rootElem
    triples <- mapM eventTriples events
    let prefixes = (PrefixMappings listPrefixMappings)
        (rdfGraph::TriplesGraph) = mkRdf (concat triples) Nothing prefixes
    outh <- openFile "rdf_output.ttl" WriteMode
    hWriteRdf (TurtleSerializer Nothing prefixes) outh rdfGraph
    hClose outh

showUsage = "Usage: ./ListEventsToRDF <list_data.xml>"

-- Maps over the elements for each event, creating a triple
eventTriples :: Content a -> IO Triples
eventTriples event = do
  let id = eventID event
      subj = unode (s2b $ thelist ++ id)
      ks = Map.keys mappings
      topLevelTriples = concatMap (\k -> genTriple subj event k mappings ) ks
      venTriple = venuTriple event
      tagTriples = eventTagTriples event
  putStrLn $ "Processing event: " ++ id
  locationTriples <- processEventLocation event
  return ([venTriple] ++ tagTriples ++ locationTriples ++ topLevelTriples)
  
-- Generates a triple to be added to the RDF graph.
genTriple :: forall a. Subject -> Content a -> Node -> Map.Map Node (FilterLookup a) -> [Triple]
genTriple subj event k mapping = 
  let obj = objectFromXml event mapping k
  in case obj of
   Nothing -> []
   Just obj -> [triple subj k obj]


-- Dictates wether the xml element data should be transformed into a URI resource or a literal
data FilterLookup a = URI (CFilter a) | Literal (CFilter a) | TypedLiteral String (CFilter a)

-- Only adds a node if there exists data for each given element in the xml
objectFromXml :: Content a -> Map.Map Node (FilterLookup a) -> Node -> Maybe Node
objectFromXml event map k =
  let (Just filterLookup) = Map.lookup k map
  in case filterLookup of
   (URI filt) ->
    let s = extractTxt filt event
    in case s of
     "" -> Nothing
     _ -> Just (unode $ s2b (thelist ++ urlEncode s))
   (Literal filt) ->
    let s = extractTxt filt event
    in case s of
     "" -> Nothing 
     _ -> Just (lnode (plainL $ s2b s))

extractTxt :: CFilter a -> Content a -> String
extractTxt xmlFilter event =
  let c = xmlFilter event
  in case length c of
   1 -> let (CString _ s _) = head c
         in s
   _ -> ""


-- | Finds nearby pubs to the venue, within 100 metres
findNearbyPubs venue vid = do
  let latFilter  = tag "venue" /> tag "latitude" /> txt
      longFilter = tag "venue" /> tag "longitude" /> txt
      lat  = extractTxt latFilter venue
      long = extractTxt longFilter venue
      query = "http://linkedgeodata.org/page/near/" ++ lat ++ "," ++ long ++ "/1000/class/Pub.n3"
  rdf <- httpCallForRdf query
  case rdf of
   Left e -> error (show e)
   Right g -> do
    let subj = (unode . s2b) vid
        pred = unode . s2b $ thelistProp ++ "neabyPub"
        linkTriples = map (triple subj pred . subjectOf)  (triplesOf g)
    return (linkTriples ++ triplesOf g)


-- |Takes a generated uri and makes simple HTTP request,
-- asking for RDF N3 serialization. Returns either 'ParseFailure' or 'RDF'
httpCallForRdf :: String -> IO (Either ParseFailure TriplesGraph)
httpCallForRdf uri = do
 let h1 = mkHeader HdrUserAgent "hsparql-client"
     h2 = mkHeader HdrAccept "text/rdf+n3"
     request = Request { rqURI = fromJust $ parseURI uri
                          , rqHeaders = [h1,h2]
                          , rqMethod = GET
                          , rqBody = ""
                          }
 response <- simpleHTTP request >>= getResponseBody
 return $ parseString (TurtleParser Nothing Nothing) (B.pack response)

lodeProp = "http://linkedevents.org/ontology/"
geonamesProp = "http://www.geonames.org/ontology#"
wg84posProp = "http://www.w3.org/2003/01/geo/wgs84_pos#"
linkedGeoData = "http://linkedgeodata.org/ontology/"

thelist = "http://www.list.co.uk/onto/resource#"
thelistProp = "http://www.list.co.uk/onto/prop#"

listPrefixMappings :: Map.Map B.ByteString B.ByteString
listPrefixMappings = Map.fromList $
  [ (s2b "list", s2b thelist)
  , (s2b "listProp", s2b thelistProp)
  , (s2b "lode", s2b lodeProp)
  , (s2b "gn", s2b geonamesProp)
  , (s2b "wgs84_pos", s2b wg84posProp)
  , (s2b "lgdo", s2b linkedGeoData)
  ]


-- TODO - it is this mapping that needs scaling up to match every element in The List xml data!  
mappings :: forall a. Map.Map Node (FilterLookup a)
mappings = Map.fromList
  [ (unode (s2b (lodeProp++"atTime")), Literal (tag "Event" /> tag "Schedule" /> tag "Performance" /> tag "StartDateTime" /> txt)) -- TODO typed literal as date
  , (unode (s2b (thelist++"title")), Literal (tag "Event" /> tag "Title" /> tag "MainTitle" /> txt))
  , (unode (s2b (thelist++"description")), Literal (tag "Event" /> tag "Description" /> tag "Plain" /> txt))
  ]


venuTriple :: forall t. Content t -> Triple
venuTriple event = 
  let id = venuID event
      s = unode (s2b (thelist ++ (eventID event)))
      p = unode (s2b (lodeProp ++ "atPlace"))
      o = unode (s2b id)
  in triple s p o

venuID :: forall t. Content t -> String
venuID event =
  let filter = tag "Event" /> tag "Venue"
      (CElem (Elem _ attrs _) _) = head $ filter event
      (_, vid) = attrs !! 1
  in show vid

eventID :: forall t. Content t -> String
eventID event =
  let filt = tag "Event"
      (CElem (Elem _ attrs _) _) = head $ filt event
      (_, eid) = attrs !! 0
  in show eid

eventTagTriples :: forall t. Content t -> [Triple]
eventTagTriples event = 
  let filt = tag "Event" /> tag "Property" /> txt
      propsS = extractTxt filt event
      props = parseProperties propsS
  in map (eventTagTriple event) props

eventTagTriple :: forall t. Content t -> String -> Triple
eventTagTriple event prop =
  let s = (unode (s2b (thelist ++ eventID event)))
      p = (unode (s2b (thelist ++ "tag")))
      o = lnode (plainL $ s2b prop)
  in triple s p o

parseProperties :: String -> [String]
parseProperties s = splitOn "," s

processEventLocation :: forall t. Content t -> IO [Triple]
processEventLocation event = do
  let vid = venuID event
  let uri      = vid ++ "?format=xml"
      request  = replaceHeader HdrUserAgent "user-agent" (getRequest uri)
  response <- simpleHTTP request >>= getResponseBody
  let (Document _ _ root _) = xmlParse "" response
  let rootElem = CElem root noPos
      venue = tag "venue" rootElem
      ks = Map.keys locationMappings
      subj = unode (s2b vid)
      triples = concatMap (\k -> genTriple subj (head venue) k locationMappings ) ks
  return triples

-- Uncomment these lines when LinkedGeoData API is working :-/
--  pubTriples <- findNearbyPubs (head venue) vid
--  return (pubTriples ++ triples)


locationMappings :: forall a. Map.Map Node (FilterLookup a)
locationMappings = Map.fromList
  [ (unode (s2b (thelist++"venueName")), Literal (tag "venue" /> tag "venue_name" /> txt))
  , (unode (s2b (thelist++"altName")), Literal (tag "venue" /> tag "alt_names" /> txt))
  , (unode (s2b (thelist++"previousName")), Literal (tag "venue" /> tag "previous_names" /> txt))
  , (unode (s2b (thelist++"address")), Literal (tag "venue" /> tag "address" /> txt))
  , (unode (s2b (thelist++"town")), Literal (tag "venue" /> tag "town" /> txt))
  , (unode (s2b (thelist++"region")), Literal (tag "venue" /> tag "region" /> txt))
  , (unode (s2b (thelist++"countryCode")), Literal (tag "venue" /> tag "country_code" /> txt))
  , (unode (s2b (thelist++"postalCode")), Literal (tag "venue" /> tag "postal_code" /> txt))
  , (unode (s2b (wg84posProp++"lat")), Literal (tag "venue" /> tag "latitude" /> txt))
  , (unode (s2b (wg84posProp++"long")), Literal (tag "venue" /> tag "longitude" /> txt))
  , (unode (s2b (thelist++"radius")), Literal (tag "venue" /> tag "radius" /> txt))
  , (unode (s2b (thelist++"vidType")), Literal (tag "venue" /> tag "vid_type" /> txt))
  ]