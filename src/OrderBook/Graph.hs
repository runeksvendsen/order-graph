{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module OrderBook.Graph
( -- * Read input data; build graph; match orders
  withBidsAsksOrder
, readOrdersFile
, buildBuyGraph
, GraphInfo(..)
, IBuyGraph
, matchOrders
  -- * Process output of order matching
, LiquidityInfo(..)
, SideLiquidity(..)
, PriceRange(..)
, toLiquidityInfo
, toSideLiquidity
  -- * Re-exports
, module Export
)
where

import Prelude hiding (log)
import OrderBook.Graph.Internal.Prelude hiding (log)

import OrderBook.Graph.Build (CompactOrderList, Tagged)
import qualified OrderBook.Graph.Types.Book as Book
import qualified Data.Graph.Digraph as DG

import Data.Text (Text)
import Data.List (sort, sortOn, sortBy, nub, groupBy)
import qualified Data.Aeson as Json
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T


-- Exports
import OrderBook.Graph.Types as Export
import OrderBook.Graph.Build as Export (build, buildFromOrders, SellOrderGraph)
import OrderBook.Graph.Match as Export (unlimited, BuyOrder, match, arbitrages)
import OrderBook.Graph.Run as Export (runArb, runMatch)
import OrderBook.Graph.Types.Path as Export
import OrderBook.Graph.Exchange as Export (invertSomeSellOrder)
import Data.Ord (comparing)
-- TMP
import Control.Exception (SomeException(SomeException), catch, throwIO)
import OrderBook.Graph.Types.SortedOrders (compactOrderListHead)
import OrderBook.Graph.Types.SortedOrders (Tagged(unTagged))
import qualified OrderBook.Graph.Types.SomeSellOrder as SO
import Unsafe.Coerce (unsafeCoerce)

-- |
withBidsAsksOrder
    :: Currency -- ^ Numeraire
    -> Currency -- ^ Cryptocurrency
       -- | arg1: buy order, arg2: sell order
    -> (forall src dst. (KnownSymbol src, KnownSymbol dst) => BuyOrder dst src
                                                           -> BuyOrder src dst
                                                           -> r
       )
    -> r
withBidsAsksOrder numeraire crypto f =
    case someSymbolVal (toS numeraire) of
        SomeSymbol (Proxy :: Proxy numeraire) ->
            case someSymbolVal (toS crypto) of
                SomeSymbol (Proxy :: Proxy crypto) ->
                    f (buyOrder :: BuyOrder crypto numeraire)
                      (buyOrder :: BuyOrder numeraire crypto)
  where
    buyOrder = unlimited

readOrdersFile
    :: (Json.FromJSON numType, Fractional numType, Real numType)
    => ([Char] -> IO ())
    -> Rational
    -> [Char]
    -> IO [OrderBook numType]
readOrdersFile log maxSlippage filePath = do
    log $ "Reading order books from " ++ show filePath ++ "..."
    books <- decodeFileOrFail filePath
    -- Log venues
    log ("Venues:") >> logVenues (nub $ map Book.bookVenue books)
    let orders = concatMap Book.fromOrderBook books
    log $ "Order book count: " ++ show (length books)
    log $ "Order count: " ++ show (length orders)
    forM (map (Book.trimSlippageOB maxSlippage) books) $ \(trimmedBook, warningM) -> do
        -- print warning in case of input orderbook depth < 'maxSlippage'
        forM_ warningM (\warning -> log $ "WARNING: " <> toS warning)
        return trimmedBook
  where
    throwError file str = error $ file ++ ": " ++ str
    decodeFileOrFail :: (Json.FromJSON numType, Ord numType) => FilePath -> IO [OrderBook numType]
    decodeFileOrFail file =
        either (throwError file) return =<< Json.eitherDecodeFileStrict file
    logVenues :: [Text] -> IO ()
    logVenues venues = forM_ venues $ \venue -> log ("\t" ++ toS venue)

buildGraph
    :: (Real numType)
    => (forall m. Monad m => String -> m ())
    -> [OrderBook numType]
    -> IO (GraphInfo numType, SellOrderGraph RealWorld "arb")
buildGraph log sellOrders = stToIO $ do
    log "Building graph..."
    graph <- build sellOrders -- buildFromOrders testOrders
    currencies <- DG.vertexLabels graph
    edgeCount <- DG.edgeCount graph
    let gi = GraphInfo { giVertices = currencies, giEdgeCount = edgeCount }
    log $ "Vertex count: " ++ show (length currencies)
    log $ "Edge count:   " ++ show edgeCount
    return (gi, graph)

-- NB: Phantom 'numType' is number type of input order book
data GraphInfo numType = GraphInfo
    { giVertices    :: [Currency]
    , giEdgeCount   :: Word
    }

-- | Find and remove all arbitrages from the input graph.
findArbitrages
    :: (forall m. Monad m => String -> m ())
    -> GraphInfo numType
    -> SellOrderGraph RealWorld "arb"
    -> IO (SellOrderGraph RealWorld "buy")
findArbitrages log gi graph = do
    stToIO $ runArb graph $ do
        log "Finding arbitrages..."
        let findArbs (_, arbsAccum) src = do
                log $ "\t" ++ toS src
                (buyGraph, arbs) <- arbitrages src
                return (buyGraph, arbs : arbsAccum)
        (buyGraph, arbs) <- foldM findArbs (error "Empty graph", []) (giVertices gi)
        log (arbLogStr $ concat arbs)
        return buyGraph
  where
    arbLogStr paths = toS $ T.unlines $ map (T.unlines . map ("\t" <>) . printGroup) $ groupOn (pStart . pathDescr) paths
    printGroup paths =
        T.unwords [toS (pStart . pathDescr $ head paths), toS . show @Double . realToFrac $ pathsQty paths]
        : map (("\t" <>) . showPath . head) (groupOn pathDescr paths)
    pathsQty :: [Path] -> NumType
    pathsQty = sum . map pQty
    groupOn f = groupBy (\a1 a2 -> f a1 == f a2) . sortOn f

type IBuyGraph = (DG.IDigraph Currency (Tagged "buy" CompactOrderList))

buildBuyGraph
    :: Real numType
    => (forall m. Monad m => String -> m ())
    -> [OrderBook numType]
    -> IO (GraphInfo numType, IBuyGraph)
buildBuyGraph log sellOrders = do
    (gi, mGraph) <- buildGraph log sellOrders
    newGraph <- findArbitrages log gi mGraph `catch` printGraph gi mGraph
    iGraph <- stToIO $ DG.freeze newGraph
    return (gi, iGraph)
  where
    -- printGraph :: SellOrderGraph RealWorld "arb" -> SomeException -> IO a
    printGraph gi graph se@(SomeException _) = do
        stToIO (DG.lookupVertexR graph (unsafeCoerce (67 :: Int))) >>= \lbl -> putStrLn $ show 67 ++ "  " ++ show lbl
        vsyVtx <- stToIO $ DG.lookupVertex graph "VSY"
        putStrLn $ "VSY: " ++ show vsyVtx
        putStrLn ""
        edges <- stToIO $ mapM (DG.outgoingEdges' graph) (giVertices gi)
        let edges' = fmap (compactOrderListHead . unTagged) <$> concat (catMaybes edges)
        putStrLn $ unlines (sedgewickWayneFormat (length $ giVertices gi)  edges')
        -- mapM_ (putStrLn . show . SO.fromCompactOrder) edges'
        -- print $ sort vertexList
        throwIO se

-- |
matchOrders
    :: (forall m. Monad m => String -> m ()) -- ^ logger
    -> Currency -- ^ numeraire
    -> Currency -- ^ cryptocurrency
    -> IBuyGraph
    -> ST s ([SellPath], [BuyPath])
matchOrders log numeraire crypto buyGraph = withBidsAsksOrder numeraire crypto $ \buyOrder sellOrder -> do
    buyGraph' <- DG.thaw buyGraph
    runMatch buyGraph' $ do
        log "Matching sell order..."
        sellPath <- map toSellPath <$> match sellOrder
        log "Matching buy order..."
        buyPath <- map toBuyPath <$> match buyOrder
        return (sellPath, buyPath)

-- | Liquidity info in both buy and sell direction
data LiquidityInfo = LiquidityInfo
    { liBuyLiquidity    :: Maybe SideLiquidity
    , liSellLiquidity   :: Maybe SideLiquidity
    } deriving (Eq, Show)

-- | Liquidity info in a single direction (either buy or sell)
data SideLiquidity = SideLiquidity
    { liLiquidity    :: NumType             -- ^ Non-zero liquidity
    , liPriceRange   :: PriceRange NumType
    , liPaths        :: NonEmpty (NumType, PriceRange NumType, PathDescr)  -- ^ (quantity, price_range, path)
    } deriving (Eq, Show)

data PriceRange numType =
    PriceRange
        { lowestPrice :: numType
        , highestPrice :: numType
        } deriving (Eq, Show)

toLiquidityInfo
    :: ([SellPath], [BuyPath])
    -> Maybe LiquidityInfo
toLiquidityInfo (sellPath, buyPath) = do
    Just $ LiquidityInfo
        { liBuyLiquidity    = toSideLiquidity <$> NE.nonEmpty buyPath
        , liSellLiquidity   = toSideLiquidity <$> NE.nonEmpty sellPath
        }

toSideLiquidity
    :: forall path.
       HasPathQuantity path NumType
    => NE.NonEmpty path
    -> SideLiquidity
toSideLiquidity nonEmptyOrders =
    let paths = NE.fromList $ sortByQuantity $ map quoteSumVenue (groupByPath $ NE.toList nonEmptyOrders)
    in SideLiquidity
        { liLiquidity    = quoteSum nonEmptyOrders
        , liPriceRange   = firstLastPrice nonEmptyOrders
        , liPaths        = paths
        }
  where
    firstLastPrice lst =
        let priceSorted = NE.sortBy (comparing pPrice) lst
        in PriceRange (pPrice $ NE.head priceSorted) (pPrice $ NE.last priceSorted)
    quoteSumVenue paths =
        (quoteSum paths, priceRange paths, pathDescr $ NE.head paths)
    groupByPath = NE.groupBy (\a b -> pathDescr a == pathDescr b) . sortOn pathDescr
    sortByQuantity = sortBy (flip $ comparing $ \(quoteQty, _, _) -> quoteQty)
    quoteSum orderList = sum $ NE.map quoteQuantity orderList
    quoteQuantity path = pQty path * pPrice path
    priceRange
        :: NE.NonEmpty path
        -> PriceRange NumType
    priceRange soList =
        let priceList = NE.map pPrice soList
        in PriceRange (minimum priceList) (maximum priceList)

vertexList :: [Integer]
vertexList =
    [ 401
    , 382
    , 58
    , 250
    , 247
    , 246
    , 243
    , 238
    , 231
    , 230
    , 229
    , 227
    , 226
    , 215
    , 213
    , 210
    , 206
    , 204
    , 199
    , 198
    , 195
    , 192
    , 189
    , 187
    , 184
    , 439
    , 438
    , 181
    , 437
    , 436
    , 435
    , 434
    , 433
    , 431
    , 174
    , 429
    , 428
    , 169
    , 165
    , 424
    , 423
    , 163
    , 421
    , 420
    , 160
    , 158
    , 157
    , 155
    , 415
    , 414
    , 413
    , 412
    , 411
    , 408
    , 406
    , 148
    , 146
    , 143
    , 142
    , 399
    , 384
    , 133
    , 132
    , 394
    , 393
    , 131
    , 130
    , 390
    , 389
    , 128
    , 387
    , 386
    , 127
    , 126
    , 379
    , 124
    , 123
    , 377
    , 376
    , 120
    , 374
    , 369
    , 116
    , 115
    , 114
    , 113
    , 112
    , 111
    , 367
    , 365
    , 361
    , 360
    , 103
    , 101
    , 98
    , 354
    , 96
    , 94
    , 93
    , 348
    , 89
    , 344
    , 87
    , 339
    , 80
    , 336
    , 77
    , 330
    , 74
    , 73
    , 329
    , 326
    , 69
    , 323
    , 322
    , 321
    , 320
    , 319
    , 63
    , 317
    , 316
    , 314
    , 57
    , 312
    , 311
    , 53
    , 308
    , 51
    , 306
    , 42
    , 295
    , 38
    , 290
    , 36
    , 289
    , 34
    , 33
    , 32
    , 31
    , 30
    , 29
    , 28
    , 279
    , 278
    , 275
    , 19
    , 274
    , 17
    , 272
    , 15
    , 14
    , 269
    , 266
    , 11
    , 10
    , 9
    , 265
    , 264
    , 261
    , 3
    , 1
    , 432
    , 430
    , 427
    , 426
    , 425
    , 422
    , 419
    , 418
    , 417
    , 416
    , 409
    , 407
    , 405
    , 404
    , 403
    , 402
    , 400
    , 398
    , 397
    , 396
    , 395
    , 392
    , 391
    , 385
    , 383
    , 381
    , 380
    , 378
    , 375
    , 373
    , 372
    , 371
    , 370
    , 368
    , 366
    , 364
    , 363
    , 362
    , 359
    , 358
    , 357
    , 356
    , 355
    , 353
    , 352
    , 351
    , 350
    , 349
    , 347
    , 346
    , 345
    , 343
    , 342
    , 341
    , 340
    , 338
    , 337
    , 335
    , 334
    , 333
    , 332
    , 331
    , 328
    , 327
    , 325
    , 324
    , 318
    , 315
    , 313
    , 310
    , 309
    , 307
    , 305
    , 304
    , 303
    , 302
    , 301
    , 300
    , 299
    , 298
    , 297
    , 296
    , 294
    , 293
    , 292
    , 291
    , 288
    , 287
    , 286
    , 285
    , 284
    , 283
    , 282
    , 281
    , 280
    , 277
    , 276
    , 273
    , 271
    , 270
    , 268
    , 267
    , 263
    , 262
    , 260
    , 259
    , 258
    , 257
    , 256
    , 255
    , 254
    , 253
    , 252
    , 251
    , 249
    , 248
    , 245
    , 244
    , 242
    , 241
    , 240
    , 239
    , 237
    , 236
    , 235
    , 234
    , 233
    , 232
    , 228
    , 225
    , 224
    , 223
    , 222
    , 221
    , 220
    , 219
    , 218
    , 217
    , 216
    , 214
    , 212
    , 211
    , 209
    , 208
    , 207
    , 205
    , 203
    , 202
    , 201
    , 200
    , 197
    , 196
    , 194
    , 193
    , 191
    , 190
    , 188
    , 186
    , 185
    , 183
    , 182
    , 180
    , 179
    , 178
    , 177
    , 176
    , 175
    , 173
    , 172
    , 171
    , 170
    , 168
    , 167
    , 166
    , 164
    , 162
    , 161
    , 159
    , 156
    , 154
    , 153
    , 152
    , 151
    , 150
    , 149
    , 147
    , 145
    , 144
    , 141
    , 140
    , 139
    , 138
    , 137
    , 136
    , 135
    , 134
    , 125
    , 122
    , 121
    , 119
    , 118
    , 117
    , 110
    , 109
    , 108
    , 107
    , 106
    , 105
    , 104
    , 102
    , 100
    , 99
    , 97
    , 95
    , 92
    , 91
    , 90
    , 88
    , 86
    , 85
    , 84
    , 83
    , 82
    , 81
    , 79
    , 76
    , 75
    , 72
    , 71
    , 70
    , 68
    , 67
    , 66
    , 64
    , 62
    , 61
    , 60
    , 56
    , 55
    , 54
    , 52
    , 50
    , 49
    , 48
    , 47
    , 46
    , 45
    , 44
    , 40
    , 39
    , 37
    , 35
    , 27
    , 26
    , 25
    , 24
    , 23
    , 22
    , 21
    , 20
    , 18
    , 16
    , 13
    , 12
    , 8
    , 7
    , 6
    , 5
    , 4
    , 2
    , 0
    , 58
    , 133
    , 189
    ]

testOrders :: [SomeSellOrder]
testOrders =
    [ SomeSellOrder' {soPrice = 3667212721853459 % 147573952589676412928, soQty = 904365732858723 % 274877906944, soBase = "ZRX", soQuote = "BTC", soVenue = "coinbase"}
    , SomeSellOrder' {soPrice = 6139076427730539 % 9444732965739290427392, soQty = 829128050378292987 % 137438953472, soBase = "ZIL", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5324191508274419 % 4611686018427387904, soQty = 8309141312498565 % 2251799813685248, soBase = "ZEN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8092356040835459 % 1152921504606846976, soQty = 1818891299504259 % 562949953421312, soBase = "ZEC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6351582919459673 % 2361183241434822606848, soQty = 3265439215567293 % 8796093022208, soBase = "ZCN", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1369486280032197 % 1180591620717411303424, soQty = 2259028582420441 % 137438953472, soBase = "YYW", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5761287109100967 % 4722366482869645213696, soQty = 177532 % 1, soBase = "YOYO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7762389906216979 % 9223372036854775808, soQty = 1 % 4, soBase = "XZC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 519460313115661 % 1180591620717411303424, soQty = 3565186 % 1, soBase = "XVG", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6841897376938873 % 36893488147419103232, soQty = 4013437343707955 % 4398046511104, soBase = "XTZ", soQuote = "BTC", soVenue = "coinbase"}
    , SomeSellOrder' {soPrice = 6038726139969559 % 295147905179352825856, soQty = 274 % 1, soBase = "XTP", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6776595902917941 % 1180591620717411303424, soQty = 5140986767922231 % 1099511627776, soBase = "XST", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 519460313115661 % 1180591620717411303424, soQty = 8170063434950543 % 137438953472, soBase = "XSR", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1948714043946677 % 73786976294838206464, soQty = 406 % 1, soBase = "XRP", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2833419889721787 % 18889465931478580854784, soQty = 2023872537461365 % 8589934592, soBase = "XMY", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8996246500447227 % 1152921504606846976, soQty = 3 % 2, soBase = "XMR", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8063440769499919 % 1180591620717411303424, soQty = 4787711232964559 % 137438953472, soBase = "XLM", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5752432671945587 % 147573952589676412928, soQty = 2133427865360387 % 8796093022208, soBase = "XHV", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5997405433244449 % 1180591620717411303424, soQty = 2341 % 1, soBase = "XEM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 4722366482869645213696, soQty = 8924041831665775 % 2199023255552, soBase = "XEL", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 75557863725914323419136, soQty = 346251866786265 % 16777216, soBase = "XDN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1 % 8979, soQty = 58023420674192644083 % 72057594037927936, soBase = "XCH", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 6186144448156113 % 36028797018963968, soQty = 5515695823429281 % 1125899906842624, soBase = "XAUT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 6611313076017503 % 147573952589676412928, soQty = 1805354112343081 % 4398046511104, soBase = "WTC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6611313076017503 % 9444732965739290427392, soQty = 1025450 % 1, soBase = "WPR", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1176164402139721 % 147573952589676412928, soQty = 5391187482413929 % 35184372088832, soBase = "WGP", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5737675276686619 % 2361183241434822606848, soQty = 4125389509173039 % 137438953472, soBase = "WAXP", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4946678890805953 % 1180591620717411303424, soQty = 1649615088549005 % 1099511627776, soBase = "WAX", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7194230188746725 % 73786976294838206464, soQty = 1201535311717335 % 1099511627776, soBase = "WAVES", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3786747623451097 % 147573952589676412928, soQty = 3447 % 1, soBase = "WAN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7679748492766761 % 590295810358705651712, soQty = 7603 % 1, soBase = "WABI", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6139076427730539 % 295147905179352825856, soQty = 50 % 1, soBase = "VTC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5312662293228351 % 1180591620717411303424, soQty = 5019578444053217 % 8796093022208, soBase = "VSY", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7366891713276647 % 1180591620717411303424, soQty = 3548187456165845 % 4398046511104, soBase = "VRC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6327971087045325 % 4722366482869645213696, soQty = 196068 % 1, soBase = "VITE", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3470939364909189 % 2361183241434822606848, soQty = 48176 % 1, soBase = "VIBE", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5548780617371833 % 2361183241434822606848, soQty = 17673 % 1, soBase = "VIB", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3184645896885217 % 147573952589676412928, soQty = 449 % 1, soBase = "VIA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6139076427730539 % 9444732965739290427392, soQty = 16445505 % 1, soBase = "VET", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2833419889721787 % 18889465931478580854784, soQty = 37203068574071 % 4294967296, soBase = "VEE", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 519460313115661 % 4722366482869645213696, soQty = 5747234945897983 % 17179869184, soBase = "VDX", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6327971087045325 % 9444732965739290427392, soQty = 3519851798123651 % 68719476736, soBase = "VBK", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7036326059475771 % 4722366482869645213696, soQty = 2345374632627677 % 68719476736, soBase = "UTK", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1 % 9420, soQty = 12494262743905346295 % 1125899906842624, soBase = "UST", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 8588804040719167 % 73786976294838206464, soQty = 1145245959948487 % 70368744177664, soBase = "USK", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 549755813888 % 5180884676837219, soQty = 13999577713379560972872696001795 % 4951760157141521099596496896, soBase = "USDT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 274877906944 % 2587499955098747, soQty = 9893264745708366292661633778211 % 9903520314283042199192993792, soBase = "USDS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 137438953472 % 1294837119671337, soQty = 1836976350755187038510130561881409315308430065 % 5577416374152508602090779345769908672659456, soBase = "USDC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1 % 9447, soQty = 242248031693481143804300551747156183 % 633825300114114700748351602688, soBase = "USD", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 37778931862957161709568, soQty = 2314393814358397 % 549755813888, soBase = "URAC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2597301565578305 % 2361183241434822606848, soQty = 2509292700205143 % 17179869184, soBase = "UPP", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6139076427730539 % 1180591620717411303424, soQty = 5558098833817025 % 68719476736, soBase = "UOS", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 156428389745057 % 147573952589676412928, soQty = 4995856497044683 % 4398046511104, soBase = "UKG", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5371691874264221 % 590295810358705651712, soQty = 300 % 1, soBase = "UBQ", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 137438953472 % 1294611719787643, soQty = 3962535152069024520407962403795 % 9903520314283042199192993792, soBase = "TUSD", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 519460313115661 % 4722366482869645213696, soQty = 6738213233831455 % 17179869184, soBase = "TUDA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4911261142184431 % 4722366482869645213696, soQty = 2251380952113809 % 549755813888, soBase = "TUBE", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2833419889721787 % 1180591620717411303424, soQty = 8313531337940403 % 8796093022208, soBase = "TTC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8878048987794933 % 9444732965739290427392, soQty = 2480865086164443 % 2199023255552, soBase = "TSHP", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1 % 56388, soQty = 63946383695266882983 % 72057594037927936, soBase = "TRY", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4911261142184431 % 2361183241434822606848, soQty = 4530704 % 1, soBase = "TRX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4722366482869645 % 9444732965739290427392, soQty = 2648850 % 1, soBase = "TROY", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 519460313115661 % 295147905179352825856, soQty = 7429244685547351 % 1099511627776, soBase = "TRAC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7703360325181109 % 147573952589676412928, soQty = 1913 % 1, soBase = "TOMO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3335171328526687 % 590295810358705651712, soQty = 28123 % 1, soBase = "TNT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1983393922805251 % 9444732965739290427392, soQty = 7782786483858706326825 % 1125899906842624, soBase = "TNB", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6676245615156961 % 590295810358705651712, soQty = 6565 % 1, soBase = "THETA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 18889465931478580854784, soQty = 7701900 % 1, soBase = "TFUEL", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 75557863725914323419136, soQty = 164290 % 1, soBase = "TEMCO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8878048987794933 % 9444732965739290427392, soQty = 403466 % 1, soBase = "TCT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7036326059475771 % 2361183241434822606848, soQty = 1634 % 1, soBase = "SYS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6097755721005429 % 590295810358705651712, soQty = 19 % 1, soBase = "STX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5981172298459585 % 147573952589676412928, soQty = 33 % 1, soBase = "STRAT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 90757980842651 % 73786976294838206464, soQty = 4926992220374315 % 68719476736, soBase = "STPT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4533471823554859 % 37778931862957161709568, soQty = 185903705 % 1, soBase = "STORM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1029328319312993 % 73786976294838206464, soQty = 700 % 1, soBase = "STORJ", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8329073884161337 % 590295810358705651712, soQty = 5193587154265047 % 2199023255552, soBase = "STJ", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5598955761252323 % 295147905179352825856, soQty = 240 % 1, soBase = "STEEM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 419110025354681 % 590295810358705651712, soQty = 3498 % 1, soBase = "SRN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 18889465931478580854784, soQty = 2666449743483451 % 536870912, soBase = "SPND", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7366891713276647 % 18889465931478580854784, soQty = 7148413275334509 % 1099511627776, soBase = "SPK", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 4722366482869645 % 9444732965739290427392, soQty = 1220425760904679 % 549755813888, soBase = "SPIN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1164358485932547 % 147573952589676412928, soQty = 859738771007401 % 2199023255552, soBase = "SPHR", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6422418416702717 % 18889465931478580854784, soQty = 9700 % 1, soBase = "SPC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7036326059475771 % 590295810358705651712, soQty = 5070954567838121 % 2199023255552, soBase = "SOLVE", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3848728683538761 % 2361183241434822606848, soQty = 58114 % 1, soBase = "SNT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1570186855554157 % 1180591620717411303424, soQty = 39558 % 1, soBase = "SNM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3872340515953109 % 4722366482869645213696, soQty = 581577 % 1, soBase = "SNGLS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5950181768415753 % 4722366482869645213696, soQty = 3530437607084365 % 34359738368, soBase = "SNG", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 2431280868914919 % 36893488147419103232, soQty = 3365906310253913 % 281474976710656, soBase = "SLT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 582110067675997 % 1152921504606846976, soQty = 3317963986329429 % 1125899906842624, soBase = "SLS", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8971020577926429 % 147573952589676412928, soQty = 64 % 1, soBase = "SKY", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7839128361563611 % 9444732965739290427392, soQty = 8106945927685963 % 1099511627776, soBase = "SIX", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2308056618502539 % 590295810358705651712, soQty = 4256370124876771 % 8796093022208, soBase = "SIB", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 944473296573929 % 9444732965739290427392, soQty = 47412 % 1, soBase = "SEN", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 519460313115661 % 4722366482869645213696, soQty = 1178939194941911 % 8589934592, soBase = "SEE", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1983393922805251 % 9444732965739290427392, soQty = 23568243 % 1, soBase = "SC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7371318931854337 % 73786976294838206464, soQty = 20 % 1, soBase = "SBD", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1485331832815093 % 73786976294838206464, soQty = 8294884099328743 % 35184372088832, soBase = "SAN", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7036326059475771 % 2361183241434822606848, soQty = 156430 % 1, soBase = "RVN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1 % 601067, soQty = 672540069444242748315 % 144115188075855872, soBase = "RUB", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1044823584334909 % 295147905179352825856, soQty = 2714372887695345 % 549755813888, soBase = "RRT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5293477679391693 % 73786976294838206464, soQty = 6 % 1, soBase = "RLC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 681791660964305 % 73786976294838206464, soQty = 3097700614053055 % 17592186044416, soBase = "RIF", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 75557863725914323419136, soQty = 677254796747847 % 268435456, soBase = "RFR", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3234821040765707 % 2361183241434822606848, soQty = 140409 % 1, soBase = "REQ", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7074326352267613 % 4611686018427387904, soQty = 2512164167142605 % 35184372088832, soBase = "REP", soQuote = "BTC", soVenue = "coinbase"}
    , SomeSellOrder' {soPrice = 2798002141100265 % 590295810358705651712, soQty = 115149 % 1, soBase = "REN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8512065585372535 % 590295810358705651712, soQty = 7 % 1, soBase = "RDN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 37778931862957161709568, soQty = 61161087616411 % 134217728, soBase = "RDD", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3358783160941035 % 590295810358705651712, soQty = 1649608794207775 % 274877906944, soBase = "RCN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8993688455858881 % 9007199254740992, soQty = 6471913336895489 % 72057594037927936, soBase = "RBT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 8749659649041915 % 147573952589676412928, soQty = 7179102644393603 % 35184372088832, soBase = "RADS", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4567413832650485 % 18446744073709551616, soQty = 6400037282958541 % 35184372088832, soBase = "QTUM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2296619637176839 % 9223372036854775808, soQty = 6547897513895405 % 8796093022208, soBase = "QTM", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 4958484807013127 % 4722366482869645213696, soQty = 33563 % 1, soBase = "QSP", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8205111763986009 % 1180591620717411303424, soQty = 2429160952399747 % 137438953472, soBase = "QSH", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3246626956972881 % 295147905179352825856, soQty = 2907179516852357 % 35184372088832, soBase = "QRL", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4815245839280773 % 9223372036854775808, soQty = 1597308126422383 % 281474976710656, soBase = "QNT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 4722366482869645213696, soQty = 159383 % 1, soBase = "QLC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 731966804844795 % 2361183241434822606848, soQty = 1911603 % 1, soBase = "QKC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1044823584334909 % 590295810358705651712, soQty = 2690464746267899 % 137438953472, soBase = "PXL", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 9444732965739290427392, soQty = 5459094873726495 % 274877906944, soBase = "PTOY", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 302231454903657293676544, soQty = 7148006933800711 % 8388608, soBase = "PTON", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4343470359595651 % 36893488147419103232, soQty = 4877776687551947 % 562949953421312, soBase = "PROM", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5780471722937625 % 147573952589676412928, soQty = 802 % 1, soBase = "PPT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7352134318017679 % 295147905179352825856, soQty = 8080803181891347 % 1125899906842624, soBase = "PPC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8830825322966237 % 2361183241434822606848, soQty = 3996 % 1, soBase = "POY", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 2833419889721787 % 590295810358705651712, soQty = 13784 % 1, soBase = "POWR", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1770887431076117 % 1180591620717411303424, soQty = 6470105946894431 % 1099511627776, soBase = "POT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2632719314199827 % 1180591620717411303424, soQty = 539628 % 1, soBase = "POLY", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 37778931862957161709568, soQty = 27060479 % 1, soBase = "POE", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 832317092605775 % 590295810358705651712, soQty = 32941 % 1, soBase = "POA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 151115727451828646838272, soQty = 8961612113616657 % 134217728, soBase = "PMA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 944473296573929 % 18889465931478580854784, soQty = 3667730320982911 % 1073741824, soBase = "PLG", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4533471823554859 % 75557863725914323419136, soQty = 958787422276427 % 2147483648, soBase = "PLA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4844852863519077 % 147573952589676412928, soQty = 4 % 1, soBase = "PIVX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6422418416702717 % 37778931862957161709568, soQty = 6805550630360185 % 17592186044416, soBase = "PINK", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8500259669165361 % 18889465931478580854784, soQty = 2127095203928965 % 2199023255552, soBase = "PI", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7177997053961861 % 18889465931478580854784, soQty = 10355744 % 1, soBase = "PHB", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8878048987794933 % 2361183241434822606848, soQty = 103758 % 1, soBase = "PERL", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 590295810358705651712, soQty = 4362236894163393 % 4398046511104, soBase = "PAY", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 549755813888 % 5174571100662661, soQty = 17711189325367338251456149816397 % 39614081257132168796771975168, soBase = "PAX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5413012580989331 % 73786976294838206464, soQty = 8545958630840431 % 17592186044416, soBase = "PART", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4533471823554859 % 75557863725914323419136, soQty = 6518504309133175 % 4294967296, soBase = "PAL", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3636222191809627 % 2361183241434822606848, soQty = 7330077518499337 % 2199023255552, soBase = "PAI", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1357680363825023 % 1180591620717411303424, soQty = 32477 % 1, soBase = "OST", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4722366482869645 % 590295810358705651712, soQty = 5290355172625613 % 274877906944, soBase = "ORS", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3258432873180055 % 4722366482869645213696, soQty = 8285692462946801 % 1099511627776, soBase = "ORBS", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3158082585419075 % 36893488147419103232, soQty = 4805569500822897 % 1099511627776, soBase = "ONT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2253454256044359 % 147573952589676412928, soQty = 70 % 1, soBase = "ONG", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 9444732965739290427392, soQty = 92014 % 1, soBase = "ONE", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8854437155380585 % 73786976294838206464, soQty = 6628691971583567 % 562949953421312, soBase = "OMN", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7607437255997819 % 73786976294838206464, soQty = 4718778453083833 % 4398046511104, soBase = "OMG", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5902958103587057 % 4611686018427387904, soQty = 99 % 1, soBase = "OKB", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 6375194751874021 % 2361183241434822606848, soQty = 8565535501959407 % 8796093022208, soBase = "OK", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 461537536724213 % 36893488147419103232, soQty = 300 % 1, soBase = "OGN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 813132478769117 % 147573952589676412928, soQty = 1725001802585211 % 8796093022208, soBase = "ODE", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3146276669211901 % 590295810358705651712, soQty = 2459950535785073 % 1099511627776, soBase = "OCEAN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 782141948725285 % 147573952589676412928, soQty = 26647 % 1, soBase = "OAX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6564089411188807 % 4722366482869645213696, soQty = 5153143235979107 % 4398046511104, soBase = "NXT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5534023222112865 % 295147905179352825856, soQty = 1747255917931397 % 70368744177664, soBase = "NXS", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 153061858951605 % 4611686018427387904, soQty = 136 % 1, soBase = "NULS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 151115727451828646838272, soQty = 2180781376086263 % 4194304, soBase = "NPXS", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2787625847558803 % 4611686018427387904, soQty = 6056869206320395 % 2251799813685248, soBase = "NMR", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1156979788303063 % 590295810358705651712, soQty = 3496791473991481 % 2199023255552, soBase = "NLG", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 37778931862957161709568, soQty = 4434810448110205 % 1073741824, soBase = "NLC2", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4911261142184431 % 2361183241434822606848, soQty = 3225 % 1, soBase = "NKN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1 % 3338092, soQty = 281185272612591000573 % 4503599627370496, soBase = "NGN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8216917680193183 % 2361183241434822606848, soQty = 416430035424285 % 274877906944, soBase = "NGC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6082813858305725 % 4611686018427387904, soQty = 8504012675657149 % 17592186044416, soBase = "NEO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4775493105801929 % 590295810358705651712, soQty = 8372109561272705 % 17592186044416, soBase = "NEC", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 4123216235355559 % 73786976294838206464, soQty = 268 % 1, soBase = "NEBL", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 37778931862957161709568, soQty = 90225899 % 1, soBase = "NCASH", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 944473296573929 % 9444732965739290427392, soQty = 2715330258685357 % 1073741824, soBase = "NCA", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 6263038547905867 % 590295810358705651712, soQty = 4591 % 1, soBase = "NAV", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8278898740280847 % 147573952589676412928, soQty = 4729945091064463 % 2199023255552, soBase = "NAS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6537526099722665 % 73786976294838206464, soQty = 7552941195379343 % 4398046511104, soBase = "NANO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1369486280032197 % 2361183241434822606848, soQty = 86628112750247 % 17179869184, soBase = "MUE", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6611313076017503 % 18889465931478580854784, soQty = 1466219297332627 % 68719476736, soBase = "MTN", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 4266362969367545 % 147573952589676412928, soQty = 3362 % 1, soBase = "MTL", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 156428389745057 % 147573952589676412928, soQty = 360559 % 1, soBase = "MTH", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2083744210566231 % 147573952589676412928, soQty = 206010274402653 % 4398046511104, soBase = "MRPH", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8476647836751013 % 2361183241434822606848, soQty = 6796187803129415 % 8796093022208, soBase = "MORE", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 470299740159225 % 2305843009213693952, soQty = 10 % 1, soBase = "MONA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 2361183241434822606848, soQty = 7345827230808905 % 17592186044416, soBase = "MOC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 148680757234099 % 36893488147419103232, soQty = 6249417658970669 % 137438953472, soBase = "MNA", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 8570530234871149 % 144115188075855872, soQty = 2418834901128703 % 562949953421312, soBase = "MKR", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 8216917680193183 % 9444732965739290427392, soQty = 1001317 % 1, soBase = "MITH", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7791904696734915 % 4722366482869645213696, soQty = 129332245855257 % 34359738368, soBase = "MIT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 75557863725914323419136, soQty = 202270845 % 1, soBase = "MFT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4061235175267895 % 9444732965739290427392, soQty = 5737783566750523 % 68719476736, soBase = "META", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3721077214548691 % 73786976294838206464, soQty = 8890788641997881 % 17592186044416, soBase = "MET", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5100155801499217 % 18889465931478580854784, soQty = 7254409296421357 % 274877906944, soBase = "MER", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8216917680193183 % 4722366482869645213696, soQty = 639250946381715 % 137438953472, soBase = "MEME", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4061235175267895 % 9444732965739290427392, soQty = 2675967470387075 % 68719476736, soBase = "MED", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7414115378105343 % 73786976294838206464, soQty = 6 % 1, soBase = "MDA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 313364064952141 % 576460752303423488, soQty = 2859785763380265 % 35184372088832, soBase = "MCO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4651530985626601 % 2361183241434822606848, soQty = 2512175 % 1, soBase = "MATIC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4651530985626601 % 1180591620717411303424, soQty = 28 % 1, soBase = "MANA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4011060031387405 % 295147905179352825856, soQty = 4558411202713181 % 1099511627776, soBase = "MAID", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2408406906263519 % 2361183241434822606848, soQty = 1793464859320033 % 68719476736, soBase = "LYM", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3146276669211901 % 147573952589676412928, soQty = 1000 % 1, soBase = "LUNA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8013265625619429 % 73786976294838206464, soQty = 1126031848237957 % 4398046511104, soBase = "LUN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2184786251229975 % 288230376151711744, soQty = 4717520609670595 % 1125899906842624, soBase = "LTC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8379249028041827 % 73786976294838206464, soQty = 4720335359437701 % 281474976710656, soBase = "LSK", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7579398205005781 % 2361183241434822606848, soQty = 27851 % 1, soBase = "LRC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4864037477355735 % 2361183241434822606848, soQty = 12362 % 1, soBase = "LOOM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2812851770079601 % 9223372036854775808, soQty = 40 % 1, soBase = "LINK", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 439309210115393 % 4611686018427387904, soQty = 774232160974761 % 137438953472, soBase = "LEO", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3648028108016801 % 1180591620717411303424, soQty = 199960 % 1, soBase = "LEND", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5548780617371833 % 2361183241434822606848, soQty = 645885628365829 % 68719476736, soBase = "LBC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 681791660964305 % 295147905179352825856, soQty = 20000 % 1, soBase = "LBA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1971588006598077 % 590295810358705651712, soQty = 3102681665136881 % 1099511627776, soBase = "LAMB", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 148680757234099 % 4611686018427387904, soQty = 449 % 1, soBase = "KNC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1311563503640749 % 18446744073709551616, soQty = 8196551321814303 % 9007199254740992, soBase = "KMD", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 37778931862957161709568, soQty = 23717846 % 1, soBase = "KEY", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4154206765399391 % 36893488147419103232, soQty = 801 % 1, soBase = "KAVA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1 % 1025000, soQty = 347683311077659315625 % 8796093022208, soBase = "JPY", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7579398205005781 % 2361183241434822606848, soQty = 2627702144477887 % 8796093022208, soBase = "JNT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4533471823554859 % 37778931862957161709568, soQty = 6002682662223237 % 137438953472, soBase = "IQX", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 8500259669165361 % 18889465931478580854784, soQty = 50985 % 1, soBase = "IOTX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 303264472571785 % 9223372036854775808, soQty = 59493 % 1, soBase = "IOTA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4796153459164483 % 147573952589676412928, soQty = 11400 % 1, soBase = "IOT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 6139076427730539 % 9444732965739290427392, soQty = 3580211 % 1, soBase = "IOST", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4675142818040949 % 4722366482869645213696, soQty = 4775705250061399 % 1099511627776, soBase = "IOS", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1345874447617849 % 295147905179352825856, soQty = 2908322591482669 % 17592186044416, soBase = "ION", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4409509703379531 % 590295810358705651712, soQty = 305824426264161 % 1099511627776, soBase = "IOC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5238875316933513 % 295147905179352825856, soQty = 4494 % 1, soBase = "INS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8754086867619605 % 590295810358705651712, soQty = 3984510478418125 % 17592186044416, soBase = "INCNT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7177997053961861 % 37778931862957161709568, soQty = 1092487521344927 % 34359738368, soBase = "IHT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7177997053961861 % 2361183241434822606848, soQty = 4298378011503843 % 17592186044416, soBase = "IGNIS", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8990205191763087 % 295147905179352825856, soQty = 471 % 1, soBase = "ICX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 75557863725914323419136, soQty = 8771362689241585 % 1073741824, soBase = "HYDRO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 944473296573929 % 4722366482869645213696, soQty = 8569436553799603 % 1099511627776, soBase = "HYC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2434970217729661 % 295147905179352825856, soQty = 3198579280834895 % 17592186044416, soBase = "HXRO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 37778931862957161709568, soQty = 1048166456866524291553 % 1099511627776, soBase = "HOT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6327971087045325 % 9444732965739290427392, soQty = 1090 % 1, soBase = "HMQ", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5025815422882167 % 18446744073709551616, soQty = 3056804785536963 % 140737488355328, soBase = "HEDG", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5784898941515315 % 2361183241434822606848, soQty = 993 % 1, soBase = "HDAC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3504881374004815 % 18446744073709551616, soQty = 1626221677945815 % 8796093022208, soBase = "HC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 832317092605775 % 590295810358705651712, soQty = 273037 % 1, soBase = "HBAR", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3983758850158315 % 73786976294838206464, soQty = 151 % 1, soBase = "GXS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4165274811843617 % 36893488147419103232, soQty = 1261007895663739 % 18014398509481984, soBase = "GVT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 18889465931478580854784, soQty = 4803599855148757 % 274877906944, soBase = "GUP", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7650233702248825 % 9444732965739290427392, soQty = 258969 % 1, soBase = "GTO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5814413732033251 % 295147905179352825856, soQty = 364 % 1, soBase = "GRS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4879901677259125 % 36893488147419103232, soQty = 4379797731196161 % 281474976710656, soBase = "GRIN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5761287109100967 % 2361183241434822606848, soQty = 5902 % 1, soBase = "GO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5300856377021177 % 1180591620717411303424, soQty = 3887 % 1, soBase = "GNT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7104579012548497 % 4611686018427387904, soQty = 6138619408069853 % 281474976710656, soBase = "GNO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3289423403223887 % 147573952589676412928, soQty = 2382733960665833 % 35184372088832, soBase = "GEO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1698841366253235 % 576460752303423488, soQty = 2514079480510131 % 1125899906842624, soBase = "GBYTE", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1 % 7169, soQty = 11608306266179786909 % 70368744177664, soBase = "GBP", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5216739224045061 % 36893488147419103232, soQty = 4046070848820347 % 2199023255552, soBase = "GAS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6375194751874021 % 1180591620717411303424, soQty = 8798861814456147 % 549755813888, soBase = "GAME", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 18889465931478580854784, soQty = 3473999271476553 % 17179869184, soBase = "FXC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2346425846175855 % 295147905179352825856, soQty = 6427658667834153 % 17592186044416, soBase = "FX", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 9444732965739290427392, soQty = 2292889 % 1, soBase = "FUN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2833419889721787 % 9444732965739290427392, soQty = 7773997 % 1, soBase = "FUEL", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2307687683621065 % 9223372036854775808, soQty = 2423726323751977 % 137438953472, soBase = "FTT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4722366482869645 % 4722366482869645213696, soQty = 1099824 % 1, soBase = "FTM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4911261142184431 % 4722366482869645213696, soQty = 2707929360101313 % 137438953472, soBase = "FTC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5660936821339987 % 295147905179352825856, soQty = 8936376451843475 % 17592186044416, soBase = "FSN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 18889465931478580854784, soQty = 1282100934053741 % 1099511627776, soBase = "FNB", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5501556952543137 % 1180591620717411303424, soQty = 318365777101901 % 137438953472, soBase = "FLO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 631616517083815 % 590295810358705651712, soQty = 8297358268222321 % 1099511627776, soBase = "FLETA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4923067058391605 % 1180591620717411303424, soQty = 119347 % 1, soBase = "FET", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2060870247914831 % 9223372036854775808, soQty = 3789329541339593 % 140737488355328, soBase = "FCT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8689154328480147 % 2361183241434822606848, soQty = 174264119451897 % 34359738368, soBase = "EXP", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3234821040765707 % 590295810358705651712, soQty = 142338662014901 % 2199023255552, soBase = "EXCL", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8568143687356613 % 295147905179352825856, soQty = 978 % 1, soBase = "EVX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1 % 8529, soQty = 524670054340283638436727170044397759 % 10141204801825835211973625643008, soBase = "EUR", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 2585495649371131 % 73786976294838206464, soQty = 3749189306537967 % 17592186044416, soBase = "ETP", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 364971713802105 % 18014398509481984, soQty = 99742664916299752461598417370115615977014438961272553427 % 934010386538622122012955713900182371475661886605754368, soBase = "ETH", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5709082823372369 % 4611686018427387904, soQty = 378148437071233 % 4398046511104, soBase = "ETC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 75557863725914323419136, soQty = 4037890878992873 % 34359738368, soBase = "ESS", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 37778931862957161709568, soQty = 83537446 % 1, soBase = "ERD", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8321326251650379 % 18446744073709551616, soQty = 6747984815960233 % 562949953421312, soBase = "EOS", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 403614760332765 % 36893488147419103232, soQty = 51624 % 1, soBase = "ENJ", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5933948633630889 % 147573952589676412928, soQty = 74 % 1, soBase = "ENG", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8476647836751013 % 2361183241434822606848, soQty = 1213076625547463 % 2199023255552, soBase = "EMC2", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 4562986614072795 % 590295810358705651712, soQty = 2416 % 1, soBase = "ELF", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 9444732965739290427392, soQty = 5632235619977617 % 17179869184, soBase = "EDR", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6239426715491519 % 295147905179352825856, soQty = 2408 % 1, soBase = "EDO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 90757980842651 % 73786976294838206464, soQty = 2278848821658625 % 4398046511104, soBase = "EDG", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1156979788303063 % 295147905179352825856, soQty = 783 % 1, soBase = "DUSK", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5100155801499217 % 18889465931478580854784, soQty = 825719112894291 % 274877906944, soBase = "DTH", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 75557863725914323419136, soQty = 398454382920133 % 4294967296, soBase = "DTA", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1792792939663647 % 144115188075855872, soQty = 2378195634063211 % 281474976710656, soBase = "DSH", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5324468209435525 % 1180591620717411303424, soQty = 2924146601784153 % 549755813888, soBase = "DRGN", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1983393922805251 % 9444732965739290427392, soQty = 16392810 % 1, soBase = "DREP", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5100155801499217 % 18889465931478580854784, soQty = 87466495 % 1, soBase = "DOGE", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 944473296573929 % 1180591620717411303424, soQty = 1655683 % 1, soBase = "DOCK", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6422418416702717 % 9444732965739290427392, soQty = 942009 % 1, soBase = "DNT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2532369026438847 % 147573952589676412928, soQty = 4244241891058039 % 70368744177664, soBase = "DMT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2721263685753633 % 590295810358705651712, soQty = 4621 % 1, soBase = "DLT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8460138000805043 % 2305843009213693952, soQty = 1188950301625811 % 36028797018963968, soBase = "DGD", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 419110025354681 % 590295810358705651712, soQty = 6549201211068451 % 4294967296, soBase = "DGB", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 151115727451828646838272, soQty = 5404062875598323 % 33554432, soBase = "DENT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 287769207549869 % 147573952589676412928, soQty = 4191250777304379 % 4398046511104, soBase = "DCT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2376378746865541 % 1152921504606846976, soQty = 10 % 1, soBase = "DCR", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 469285169235171 % 295147905179352825856, soQty = 143841 % 1, soBase = "DATA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7603010037420129 % 4722366482869645213696, soQty = 16000 % 1, soBase = "DAT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7161210516854785 % 576460752303423488, soQty = 3494793310839505 % 72057594037927936, soBase = "DASH", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7893730724021791 % 73786976294838206464, soQty = 6391496217876075 % 17592186044416, soBase = "DAI", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 164176022256015 % 36893488147419103232, soQty = 1803851629703725 % 274877906944, soBase = "DAD", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7036326059475771 % 2361183241434822606848, soQty = 25031246794575620647 % 1125899906842624, soBase = "CVC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 550450843159493 % 147573952589676412928, soQty = 1297884851004567 % 2199023255552, soBase = "CURE", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 590295810358705651712, soQty = 38 % 1, soBase = "CTXC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4096652923889417 % 147573952589676412928, soQty = 4399386409178859 % 2199023255552, soBase = "CTX", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 2353804543805339 % 73786976294838206464, soQty = 5029193402318453 % 8796093022208, soBase = "CTC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8205111763986009 % 1180591620717411303424, soQty = 200 % 1, soBase = "CRW", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6776595902917941 % 1180591620717411303424, soQty = 7702358163081403 % 8796093022208, soBase = "CRO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6422418416702717 % 37778931862957161709568, soQty = 3640512306401437 % 137438953472, soBase = "CPT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3872340515953109 % 4722366482869645213696, soQty = 6547878826947623 % 4398046511104, soBase = "COSM", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7461339042934039 % 9444732965739290427392, soQty = 139617 % 1, soBase = "COS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4533471823554859 % 75557863725914323419136, soQty = 379606959 % 1, soBase = "COCOS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1 % 65870, soQty = 139693477400509849355 % 4503599627370496, soBase = "CNHT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 731966804844795 % 1180591620717411303424, soQty = 695776 % 1, soBase = "CND", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 419110025354681 % 295147905179352825856, soQty = 182701 % 1, soBase = "CMT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5289050460814003 % 75557863725914323419136, soQty = 186642267940301 % 67108864, soBase = "CMCT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 75557863725914323419136, soQty = 1508135753206399 % 134217728, soBase = "CLO", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 7839128361563611 % 9444732965739290427392, soQty = 1807065 % 1, soBase = "CHZ", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7177997053961861 % 4722366482869645213696, soQty = 2210134109942137 % 8796093022208, soBase = "CHR", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 18889465931478580854784, soQty = 10629926 % 1, soBase = "CELR", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6800207735332289 % 9444732965739290427392, soQty = 331027 % 1, soBase = "CDT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1195349015976379 % 295147905179352825856, soQty = 721997213023837 % 2199023255552, soBase = "CBT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1520011711673667 % 295147905179352825856, soQty = 81 % 1, soBase = "BWX", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 274877906944 % 2592879315737641, soQty = 15837768733766399237182871210075 % 158456325028528675187087900672, soBase = "BUSD", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8500259669165361 % 18889465931478580854784, soQty = 8938301996923643 % 34359738368, soBase = "BURST", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5088349885292043 % 295147905179352825856, soQty = 5330914984483879 % 549755813888, soBase = "BTU", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 944473296573929 % 18889465931478580854784, soQty = 3208352337482079 % 8388608, soBase = "BTT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 4285547583204203 % 1180591620717411303424, soQty = 823 % 1, soBase = "BTS", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 6440127291013479 % 590295810358705651712, soQty = 4771908812909793 % 549755813888, soBase = "BTM", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5451012873781173 % 4611686018427387904, soQty = 3611851716779049 % 17592186044416, soBase = "BTG", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4346514072367813 % 144115188075855872, soQty = 40010565162817577687413806969167217554846133271749 % 726923855749505639835923980864581385494395355136, soBase = "BSV", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 5417439799567021 % 147573952589676412928, soQty = 4490765277596665 % 281474976710656, soBase = "BRZ", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1855742453815181 % 73786976294838206464, soQty = 116 % 1, soBase = "BRD", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 419110025354681 % 147573952589676412928, soQty = 158396 % 1, soBase = "BQX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4722366482869645 % 4722366482869645213696, soQty = 71653 % 32, soBase = "BORA", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8090004080966061 % 295147905179352825856, soQty = 173 % 1, soBase = "BNT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 282970171786943 % 144115188075855872, soQty = 6124895493223875 % 2251799813685248, soBase = "BNB", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5100155801499217 % 2361183241434822606848, soQty = 70855 % 1, soBase = "BLZ", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 944473296573929 % 590295810358705651712, soQty = 5847209069600205 % 17592186044416, soBase = "BLTV", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5852782959706567 % 36893488147419103232, soQty = 7125391098163841 % 17592186044416, soBase = "BLOCK", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 75557863725914323419136, soQty = 2420417072964115 % 1073741824, soBase = "BLOC", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2709457769546459 % 590295810358705651712, soQty = 4481650767326467 % 8796093022208, soBase = "BLK", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2833419889721787 % 9444732965739290427392, soQty = 2810264258370723 % 1099511627776, soBase = "BKX", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 469285169235171 % 295147905179352825856, soQty = 285379886777355 % 8589934592, soBase = "BFT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 6116940334842087 % 73786976294838206464, soQty = 5502989726321213 % 2199023255552, soBase = "BEAM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2798002141100265 % 1180591620717411303424, soQty = 47206 % 1, soBase = "BCPT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1464930886791075 % 36028797018963968, soQty = 2 % 1, soBase = "BCH", soQuote = "BTC", soVenue = "coinbase"}
    , SomeSellOrder' {soPrice = 2458213115262535 % 36893488147419103232, soQty = 2303 % 1, soBase = "BCD", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7127821910081371 % 295147905179352825856, soQty = 5150 % 1, soBase = "BAT", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7800759133890295 % 295147905179352825856, soQty = 14 % 1, soBase = "BAND", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5852517787760507 % 144115188075855872, soQty = 4644554774695815 % 281474976710656, soBase = "BAB", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 469285169235171 % 36893488147419103232, soQty = 4646110929797369 % 549755813888, soBase = "AVT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1156979788303063 % 1180591620717411303424, soQty = 2420530104824771 % 137438953472, soBase = "AUC", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 8610940133607619 % 18446744073709551616, soQty = 764204561769431 % 8796093022208, soBase = "ATOM", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 8649678296162409 % 18446744073709551616, soQty = 6686420232229453 % 17592186044416, soBase = "ATO", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 944473296573929 % 18889465931478580854784, soQty = 906091039775785 % 34359738368, soBase = "ATM", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 4722366482869645 % 2361183241434822606848, soQty = 373921 % 1, soBase = "AST", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1770887431076117 % 590295810358705651712, soQty = 1963727819676631 % 1099511627776, soBase = "ART", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2809808057307439 % 2361183241434822606848, soQty = 83547 % 1, soBase = "ARPA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 527207945626619 % 36893488147419103232, soQty = 631 % 1, soBase = "ARN", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5474993641076995 % 295147905179352825856, soQty = 837 % 1, soBase = "ARK", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3547677820255821 % 590295810358705651712, soQty = 39688 % 1, soBase = "ARDR", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 994648440454419 % 295147905179352825856, soQty = 8068 % 1, soBase = "APPC", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5814413732033251 % 147573952589676412928, soQty = 5557658888420867 % 140737488355328, soBase = "APM", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7366891713276647 % 2361183241434822606848, soQty = 2250 % 1, soBase = "APIX", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 5823268169188631 % 73786976294838206464, soQty = 6474200776015195 % 8796093022208, soBase = "ANT", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 3022314549036573 % 18889465931478580854784, soQty = 8451418 % 1, soBase = "ANKR", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 4796153459164483 % 36893488147419103232, soQty = 1349789268851067 % 1099511627776, soBase = "AMP", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 519460313115661 % 295147905179352825856, soQty = 8620 % 1, soBase = "AMB", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 7842079840615405 % 295147905179352825856, soQty = 402 % 1, soBase = "ALGO", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 2014384452849083 % 73786976294838206464, soQty = 7661957751283101 % 2199023255552, soBase = "ALG", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 2833419889721787 % 9444732965739290427392, soQty = 2638584344427163 % 8589934592, soBase = "AKRO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 828627743791033 % 73786976294838206464, soQty = 3617 % 1, soBase = "AION", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 419110025354681 % 36893488147419103232, soQty = 1958168526774601 % 4398046511104, soBase = "AIO", soQuote = "BTC", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 2597301565578305 % 2361183241434822606848, soQty = 3672383687567533 % 2199023255552, soBase = "AID", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 8925272652623629 % 4722366482869645213696, soQty = 122917 % 1, soBase = "AGI", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 3949078971299741 % 590295810358705651712, soQty = 3273918951210575 % 35184372088832, soBase = "AERGO", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2845225805928961 % 147573952589676412928, soQty = 2762332174187033 % 8796093022208, soBase = "AEON", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 1535506976695583 % 73786976294838206464, soQty = 6 % 1, soBase = "AE", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 5714063444272271 % 590295810358705651712, soQty = 11889 % 1, soBase = "ADX", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1983393922805251 % 9444732965739290427392, soQty = 216509859472109 % 8589934592, soBase = "ADT", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 7248832551204905 % 1180591620717411303424, soQty = 152064 % 1, soBase = "ADA", soQuote = "BTC", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1156979788303063 % 1180591620717411303424, soQty = 3197272978953549 % 2199023255552, soBase = "ABYSS", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 2446776133936835 % 295147905179352825856, soQty = 367 % 2, soBase = "1ST", soQuote = "BTC", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 147573952589676412928 % 4896503746925463, soQty = 19953252768721261725 % 36893488147419103232, soBase = "ETH", soQuote = "DNT", soVenue = "binance"}
    , SomeSellOrder' {soPrice = 1152921504606846976 % 7204606482288187, soQty = 460496832528414048479 % 1152921504606846976, soBase = "USDC", soQuote = "DNT", soVenue = "coinbase"}
    , SomeSellOrder' {soPrice = 9444732965739290427392 % 6422418416702717, soQty = 15241291518017319310847772558363 % 324518553658426726783156020576256, soBase = "BTC", soQuote = "DNT", soVenue = "bittrex"}
    , SomeSellOrder' {soPrice = 144115188075855872 % 6124895493223875, soQty = 16167932667960897230489732526375 % 1267650600228229401496703205376, soBase = "USD", soQuote = "VSY", soVenue = "bitfinex"}
    , SomeSellOrder' {soPrice = 1180591620717411303424 % 5277244544606829, soQty = 17444302218202046026683655033011 % 5192296858534827628530496329220096, soBase = "BTC", soQuote = "VSY", soVenue = "bitfinex"}
    ]
