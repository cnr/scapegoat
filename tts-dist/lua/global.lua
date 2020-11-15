-- set the random seed
math.randomseed(os.time())

-- cards
function baseObj()
    return {
        Name = 'Base',
        Transform = {
            posX = 0, posY = 5, posZ = 0,
            rotX = 0, rotY = 0, rotZ = 0,
            scaleX = 1, scaleY = 1, scaleZ = 1
        },
        Nickname = '',
        Description = '',
        ColorDiffuse = { r = 1, g = 1, b = 1 },
        Locked = false,
        Grid = true,
        Snap = true,
        Autoraise = true,
        Sticky = true,
        Tooltip = true,
        GridProjection = false,
        Hands = true,
        XmlUI = '',
        LuaScript = '',
        LuaScriptState = '',
        GUID = 'abcdef'
    }
end

-- provide unique ID starting from 20 for present decks
local nextID
do
    local _nextID = 20
    nextID = function()
        _nextID = _nextID + 1
        return tostring(_nextID)
    end
end

-- Asset ID storage to avoid new ones for identical assets
local idLookup = {}
-- URL -> Int
local function assetID(front)
    local key = idLookup[front]
    if not key then
        key = nextID()
        idLookup[front] = key
    end
    return key
end

function shuffleTable(t)
    for i = #t, 2, -1 do
        local n = math.random(i)
        t[i], t[n] = t[n], t[i]
    end
    return t
end

-- URL -> URL -> Int -> Int -> Asset
function mkAsset(front, back, width, height, extra)
    local res = {}
    res.data = {
        FaceURL = front,
        BackURL = back,
        NumWidth = width,
        NumHeight = height,
        BackIsHidden = true,
        Type = 0
    }
    for k,v in pairs(extra or {}) do
        res.data[k] = v
    end
    res.id = assetID(front)
    return res
end

-- Asset -> Int -> Int -> Card
function mkCard(asset, row, column, extra)
    local card = baseObj()
    card.Name = 'Card'

    local ix = (row-1)*asset.data.NumWidth + column - 1
    ix = string.format('%02d', ix)
    card.CardID = asset.id .. ix
    card.CustomDeck = {[asset.id] = asset.data}
    card.BackIsHidden = true

    for k,v in pairs(extra or {}) do
        card[k] = v
    end
    return card
end

-- [Card] -> Deck
function mkDeck(cards)
    local deck = baseObj()
    deck.Name = 'Deck'
    deck.Hands = false
    deck.DeckIDs = {}
    deck.CustomDeck = {}
    deck.ContainedObjects = {}
    deck.BackIsHidden = true
    for _,card in ipairs(cards) do
        deck.DeckIDs[#deck.DeckIDs+1] = card.CardID
        local id = next(card.CustomDeck)
        if not deck.CustomDeck[id] then
            deck.CustomDeck[id] = card.CustomDeck[id]
        end
        deck.ContainedObjects[#deck.ContainedObjects+1] = card
    end
    return deck
end

-- URL -> Placemat
function mkPlacemat(image)
    local token = baseObj()
    token.Name = 'Custom_Token'
    token.Hands = false
    token.CustomImage = {
        ImageURL = image,
        ImageScalar = 1.0,
        CustomToken = {
            Thickness = 0.2,
            MergeDistancePixels = 15.0,
            StandUp = false,
            Stackable = false
        }
    }
    token.Transform.scaleX = 2
    token.Transform.scaleZ = 2

    return token
end

function mkPlayerToken(image)
    local tile = baseObj()
    tile.Name = 'Custom_Tile'
    tile.Hands = false
    tile.CustomImage = {
        ImageURL = image,
        ImageScalar = 1.0,
        CustomTile = {
            Type = 2,
            Thickness = 0.2,
            Stackable = false,
            Stretch = false
        }
    }
    tile.Transform.scaleX = 0.6
    tile.Transform.scaleZ = 0.6

    return tile
end

function mkPrepareToken()
    local piece = baseObj()
    piece.Name = 'go_game_piece_white'
    piece.Hands = false
    return piece
end

-- Spawnable a => a -> IO ()
function spawn(obj, pos, rotation, cb)
    local table = {json = JSON.encode(obj)}
    table.position = pos or {0,5,0}
    table.rotation = rotation or {0,0,0}
    table.sound = true
    table.callback_function = cb
    spawnObjectJSON(table)
end

-- Object -> Pos -> Rotation -> Bool -> IO ()
function dealTo(deckObj, pos, rotation, flipped)
    deckObj.takeObject({position = pos, rotation = rotation, flip = flipped or false})
end

----- goats

-- data Goat = Goat
--   { name :: String
--   , ix :: Int
--   , placemat :: URL
--   , token :: URL
--   }

--[[
local goatRed = {
    name = "RED",
    ix = 1,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_red.png",
    token = "file:////Users/connor/Desktop/scapegoat/hs/token_red.png"
}

local goatBlue = {
    name = "BLUE",
    ix = 2,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_blue.png",
    token = "file:////Users/connor/Desktop/scapegoat/hs/token_blue.png"
}

local goatYellow = {
    name = "YELLOW",
    ix = 3,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_yellow.png",
    token = "file:////Users/connor/Desktop/scapegoat/hs/token_yellow.png"
}

local goatGreen = {
    name = "GREEN",
    ix = 4,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_green.png",
    token = "file:////Users/connor/Desktop/scapegoat/hs/token_green.png"
}

local goatBrown = {
    name = "BROWN",
    ix = 5,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_brown.png",
    token = "file:////Users/connor/Desktop/scapegoat/hs/token_brown.png"
}

local goatPurple = {
    name = "PURPLE",
    ix = 6,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_purple.png",
    token = "file:////Users/connor/Desktop/scapegoat/hs/token_purple.png"
}
--]]

local goatRed = {
    name = "RED",
    ix = 1,
    placemat = "http://cloud-3.steamusercontent.com/ugc/1681493590731581874/E4353D609F38C7651B617D7AFE98B60726967175/",
    token = "http://cloud-3.steamusercontent.com/ugc/1681493590731583875/2ECBDF400EF13EABA1E81A02E28B43CF8580BB03/"
}

local goatBlue = {
    name = "BLUE",
    ix = 2,
    placemat = "http://cloud-3.steamusercontent.com/ugc/1681493590731580258/F552C2012C77446E874CDFF01F92637549B5F2BA/",
    token = "http://cloud-3.steamusercontent.com/ugc/1681493590731582573/72F2CE387DF71F2473260BEB0C6358C506F0A0D4/"
}

local goatYellow = {
    name = "YELLOW",
    ix = 3,
    placemat = "http://cloud-3.steamusercontent.com/ugc/1681493590731582224/15C1998101F12BAE43CA35E3207390380BB5FB69/",
    token = "http://cloud-3.steamusercontent.com/ugc/1681493590731584189/267BAAADF3750B7BDE0AB6C2CC14A58E243B8808/"
}

local goatGreen = {
    name = "GREEN",
    ix = 4,
    placemat = "http://cloud-3.steamusercontent.com/ugc/1681493590731581143/F95DA8C0C39B684389197DD43B6751341CD6FD5F/",
    token = "http://cloud-3.steamusercontent.com/ugc/1681493590731583218/9344C893F33B8BFC617D70F21B98EC2EEDFC67D6/"
}

local goatBrown = {
    name = "BROWN",
    ix = 5,
    placemat = "http://cloud-3.steamusercontent.com/ugc/1681493590731580809/0CBE7A2C36F14B71C3E38B1A113049D8855435CD/",
    token = "http://cloud-3.steamusercontent.com/ugc/1681493590731582897/7E85987E4CD27D264EFA7209A848BF2CEBE75DBA/"
}

local goatPurple = {
    name = "PURPLE",
    ix = 6,
    placemat = "http://cloud-3.steamusercontent.com/ugc/1681493590731581497/2B85128720CC3BFE71C647A37FAB4516D49C6489/",
    token = "http://cloud-3.steamusercontent.com/ugc/1681493590731583520/886CB9BA414ABCB12B6F7F9E768E9066ED4B3110/"
}

-- () -> [Goat]
function allGoats()
    return {
        goatRed,
        goatBlue,
        goatYellow,
        goatGreen,
        goatBrown,
        goatPurple
    }
end

---------- card sets

--local matrix = mkAsset("file:////Users/connor/Desktop/scapegoat/hs/goat_deck.png", "file:////Users/connor/Desktop/scapegoat/hs/goat_back.png", 7, 6)
local matrix = mkAsset("http://cloud-3.steamusercontent.com/ugc/1681493590731577840/FC98BA38BDD0C9D480CA20A56B529D8A2E724A78/", "http://cloud-3.steamusercontent.com/ugc/1681493590731578640/4BF4132E93F5B5D6513EC7B7CD84C8C69D31F44E/", 7, 6)

local function half(top, bottom)
    return mkCard(matrix, top, bottom)
end

-- [Goat] -> Deck
function deckForGoats(goats)
    if #goats < 3 then error('At least three goats are required') end
    if #goats > 6 then error('At most six goats are required') end

    -- confusingly, these aren't the "real" colors. the names here make it easier to map against the in-real-life game
    local red = goats[1].ix
    local blue = goats[2].ix
    local yellow = goats[3].ix
    local green, orange, purple
    if #goats >= 4 then green = goats[4].ix else green = 1 end
    if #goats >= 5 then orange = goats[5].ix else orange = 1 end
    if #goats >= 6 then purple = goats[6].ix else purple = 1 end

    local fullred = mkCard(matrix, red, red)
    local fullblue = mkCard(matrix, blue, blue)
    local fullyellow = mkCard(matrix, yellow, yellow)
    local fullgreen = mkCard(matrix, green, green)
    local fullorange = mkCard(matrix, orange, orange)
    local fullpurple = mkCard(matrix, purple, purple)

    local sixgoats = mkCard(matrix, 1, 7)
    local innocent = mkCard(matrix, 2, 7)

    local threePlayers = mkDeck({
        innocent,
        innocent,
        innocent,
        innocent,
        fullblue,
        fullblue,
        fullblue,
        fullred,
        fullred,
        fullred,
        fullyellow,
        fullyellow,
        fullyellow
    })

    local fourPlayers = mkDeck({
        innocent,
        fullblue,
        fullblue,
        fullblue,
        fullred,
        fullred,
        fullred,
        fullyellow,
        fullyellow,
        fullyellow,
        fullgreen,
        fullgreen,
        fullgreen,
        half(yellow,green),
        half(red,blue)
    })

    local fivePlayers = mkDeck({
        innocent,
        innocent,
        fullblue,
        fullblue,
        fullblue,
        fullred,
        fullred,
        fullred,
        fullyellow,
        fullyellow,
        fullyellow,
        fullgreen,
        fullgreen,
        fullgreen,
        fullorange,
        fullorange,
        fullorange,
        half(yellow,green),
        half(red,blue),
        half(red,orange),
        half(blue,yellow),
        half(green,orange)
    })

    local sixPlayers = mkDeck({
        fullblue,
        fullblue,
        fullblue,
        fullred,
        fullred,
        fullred,
        fullyellow,
        fullyellow,
        fullyellow,
        fullgreen,
        fullgreen,
        fullgreen,
        fullorange,
        fullorange,
        fullorange,
        fullpurple,
        fullpurple,
        fullpurple,
        half(yellow,green),
        half(red,blue),
        half(blue,yellow),
        half(green,orange),
        half(red,purple),
        half(orange,purple),
        sixgoats
    })

    if #goats == 3 then return threePlayers
    elseif #goats == 4 then return fourPlayers
    elseif #goats == 5 then return fivePlayers
    elseif #goats == 6 then return sixPlayers
    end
end

---------- locations

--local locationsAsset = mkAsset("file:////Users/connor/Desktop/scapegoat/hs/locations_deck.png", "file:////Users/connor/Desktop/scapegoat/hs/locations_back.png", 5, 1, {UniqueBack = true})
local locationsAsset = mkAsset("http://cloud-3.steamusercontent.com/ugc/1681493590731579409/E96EDD0A2ABE27787F8D1367FAC9E68078583CB7/", "http://cloud-3.steamusercontent.com/ugc/1681493590731578993/B0090EC9B72FC7E9684B6E525514C19F8ABBB26F/", 5, 1, {UniqueBack = true})

-- order: prepare, stash, spy, trade, cops
local prepareCard = mkCard(locationsAsset, 1, 1, {SidewaysCard = true})
local stashCard = mkCard(locationsAsset, 1, 2, {SidewaysCard = true})
local spyCard = mkCard(locationsAsset, 1, 3, {SidewaysCard = true})
local tradeCard = mkCard(locationsAsset, 1, 4, {SidewaysCard = true})
local copsCard = mkCard(locationsAsset, 1, 5, {SidewaysCard = true})

---------- placemats

-- Player -> Goat -> IO ()
function spawnPlacemat(player, goat)
    local placemat = mkPlacemat(goat.placemat)
    local hand = player.getHandTransform()

    local pos = hand.position + hand.forward * 5
    local rotation = hand.rotation + Vector(0,180,0)

    spawn(placemat, pos, rotation)
end


---------- game preparation

function getSeatedPlayers()
    local players = {}
    for _, player in ipairs(Player.getPlayers()) do
        if player.seated and (player.getHandTransform() ~= null) then
            players[#players+1] = player
        end
    end

    return players
end

-- Int -> [Goat]
function getRandomGoats(num)
    local goats = {}
    local goatPool = allGoats()
    for i=1,num do
        goats[#goats+1] = table.remove(goatPool, math.random(#goatPool))
    end
    return goats
end

-- attached to the "click to start" button
function clickToStart(obj, playerColor)
    if not(Player[playerColor].admin) then
        return
    end

    local players = getSeatedPlayers()

    if #players < 3 then
        broadcastToAll('Not enough players (need 3-6)')
        return
    end

    if #players > 6 then
        broadcastToAll('Too many players (need 3-6)')
        return
    end

    startGame()
    destroyObject(obj)
end

function startGame(players)
    local players = getSeatedPlayers()

    if #players < 3 then
        broadcastToAll('Not enough players (need 3-6)')
        return
    end

    if #players > 6 then
        broadcastToAll('Too many players (need 3-6)')
        return
    end

    -- get random goats (indices will map to `players` indices)
    local goats = getRandomGoats(#players)

    local scapegoat = goats[math.random(#goats)]

    for i, player in ipairs(players) do
        local goat = goats[i]
        spawnPlacemat(player, goat)

        local goatToBlame = scapegoat
        if goat == scapegoat then
            -- pick a new goat to blame lol
            local otherGoats = {table.unpack(goats)}
            table.remove(otherGoats, i)

            goatToBlame = otherGoats[math.random(#otherGoats)]
        end
        broadcastToColor(goatToBlame.name .. ' GOAT is the scapegoat', player.color)
    end

    local deck = deckForGoats(goats)

    -- spawn locations
    spawn(prepareCard, {0,0,-7})
    spawn(spyCard, {0,0,-3.5})
    spawn(stashCard, {0,0,0})
    spawn(tradeCard, {0,0,3.5})
    spawn(copsCard, {0,0,7})

    -- spawn prepare tokens
    spawn(mkPrepareToken(), {0.4,1.5,-7.6})
    spawn(mkPrepareToken(), {0.4,1.5,-6.2})

    -- spawn deck; deal cards to locations and players
    spawn(deck, {-10,0,0}, {180,180,0}, function(obj)
        obj.randomize()

        dealTo(obj, {-3.5,0,-7}, {0,90,0}, true)
        dealTo(obj, {-3.5,0,-3.5}, {0,90,0}, true)
        dealTo(obj, {-3.5,0,-0}, {0,90,0}, true)
        dealTo(obj, {-3.5,0,3.5}, {0,90,0}, true)

        dealTo(obj, {2.5,0,0})
        dealTo(obj, {4.7,0,0})
        dealTo(obj, {6.9,0,0})

        local remainingNum = #obj.getObjects()
        for i=1,remainingNum/#players do
            obj.deal(1)
        end

    end)

    -- spawn player tokens
    Wait.frames(function()
        shuffleTable(goats)

        local tokenSpawnLocs = {
            {1.75,5,-7.8}, -- first location, left
            {1.75,5,-4.3}, -- second location, left
            {1,5,0}, -- third location
            {1.75,5,3.5}, -- fourth location
            {1.75,5,-6}, -- first location, right
            {1.75,5,-2.5}, -- second location, right
        }

        for i, goat in ipairs(goats) do
            spawn(mkPlayerToken(goat.token), tokenSpawnLocs[i], {0,90,0})
        end
    end, 60)

    -- TODO: spawn prep tokens
end

--getObjectFromGUID('abcdef').createButton({click_function='clickToStart', label='Click to start\n(3-6 Players)', position={0,0,0}, width=1000, height=500})
