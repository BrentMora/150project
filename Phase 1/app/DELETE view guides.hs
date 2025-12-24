-- | View Guides

-- | Main view function - renders entire UI
viewModel :: Model -> View Action
viewModel Model{..} =
  div_ [] $
    if not modelConnected
      then [ viewConnect modelPlayerName ]  -- Show connection screen
      else case modelGameState of
        Just gs -> [ viewGame gs modelPlayerId ]  -- Show game
        Nothing -> [ text "Loading..." ]           -- Loading state

-- | Connection screen view - name input and connect button
viewConnect :: Text -> View Action
viewConnect name =
  div_ [ style_ $ Map.fromList [("padding", "20px")] ]
    [ h1_ [] [ text "Game Client" ]
    , input_
        [ type_ "text"
        , placeholder_ "Enter your name"
        , value_ (ms name)
        , onInput $ UpdateName . fromMisoString  -- Update model on input
        ]
    , button_ [ onClick Connect ] [ text "Connect" ]
    ]

-- | Game view - canvas and controls
viewGame :: GameState -> Maybe Text -> View Action
viewGame gs myId =
  div_ []
    [ canvas_
        [ id_ "gameCanvas"
        , width_ "640"       -- Canvas width
        , height_ "480"      -- Canvas height
        , style_ $ Map.fromList [("border", "2px solid black")]
        ]
        []
    , script_ [] [ text $ renderScript gs myId ]  -- JavaScript to render game
    , div_ [ style_ $ Map.fromList [("margin-top", "10px")] ]
        [ text "Use arrow keys to move" ]
    ]

-- | Generate JavaScript code to render the game on canvas
renderScript :: GameState -> Maybe Text -> MisoString
renderScript gs myId = ms $ T.unpack $ T.unlines
  [ "const canvas = document.getElementById('gameCanvas');"
  , "const ctx = canvas.getContext('2d');"
  , "ctx.clearRect(0, 0, 640, 480);"  -- Clear canvas
  , T.concat $ map (renderPlayer myId) $ Map.elems (players gs)  -- Render each player
  ]

-- | Generate JavaScript to render a single player as a rectangle
renderPlayer :: Maybe Text -> Player -> Text
renderPlayer myId p =
  let isMe = Just (playerId p) == myId  -- Check if this is our player
      size = if isMe then "15" else "10"  -- Our player is larger
      x = T.pack $ show $ posX $ playerPos p
      y = T.pack $ show $ posY $ playerPos p
  in T.unlines
    [ "ctx.fillStyle = '" <> playerColor p <> "';"  -- Set fill color
    , "ctx.fillRect(" <> x <> ", " <> y <> ", " <> size <> ", " <> size <> ");"  -- Draw rectangle
    ]

-- | Subscription to keyboard events
-- Listens for keydown events and triggers KeyDown action
keyboardSub :: (Int -> action) -> Sub action model
keyboardSub f = \sink -> do
  cb <- asyncCallback1 $ \e -> do
    key <- getKeyCode e  -- Get key code from event
    sink (f key)         -- Trigger action with key code
  addEventListener "keydown" cb  -- Register listener
  return $ return ()

-- | FFI: Get key code from JavaScript keyboard event
foreign import javascript unsafe "$1.keyCode"
  getKeyCode :: JSVal -> IO Int

-- | FFI: Add event listener to window
foreign import javascript unsafe "window.addEventListener($1, $2)"
  addEventListener :: MisoString -> Callback (JSVal -> IO ()) -> IO ()