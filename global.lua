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

-- Spawnable a => a -> IO ()
function spawn(obj, pos, rotation)
    local table = {json = JSON.encode(obj)}
    table.position = pos or {0,5,0}
    table.rotation = rotation or {0,0,0}
    --table.scale = {1,1,1}
    table.sound = true
    spawnObjectJSON(table)
end

----- goats

-- data Goat = Goat
--   { name :: String
--   , ix :: Int
--   , placemat :: URL
--   }

local goatRed = {
    name = "RED",
    ix = 1,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_red.png"
}

local goatBlue = {
    name = "BLUE",
    ix = 2,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_blue.png"
}

local goatYellow = {
    name = "YELLOW",
    ix = 3,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_yellow.png"
}

local goatGreen = {
    name = "GREEN",
    ix = 4,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_green.png"
}

local goatBrown = {
    name = "BROWN",
    ix = 5,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_brown.png"
}

local goatPurple = {
    name = "PURPLE",
    ix = 6,
    placemat = "file:////Users/connor/Desktop/scapegoat/hs/placemat_purple.png"
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
local matrix = mkAsset("https://cdn.discordapp.com/attachments/199605031199178752/776150866024857610/deck.png", "https://cdn.discordapp.com/attachments/199605031199178752/776159476171931668/goat_back.png", 7, 6)

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

local locationsAsset = mkAsset("file:////Users/connor/Desktop/scapegoat/hs/locations_deck.png", "file:////Users/connor/Desktop/scapegoat/hs/locations_back.png", 5, 1, {UniqueBack = true})

-- order: prepare, stash, spy, trade, cops
local prepareCard = mkCard(locationsAsset, 1, 1, {SidewaysCard = true})
local stashCard = mkCard(locationsAsset, 1, 2, {SidewaysCard = true})
local spyCard = mkCard(locationsAsset, 1, 3, {SidewaysCard = true})
local tradeCard = mkCard(locationsAsset, 1, 4, {SidewaysCard = true})
local copsCard = mkCard(locationsAsset, 1, 5, {SidewaysCard = true})

local locationsDeck = mkDeck({prepareCard, stashCard, spyCard, tradeCard, copsCard})

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

function startGame()
    local players = {}
    for _, player in ipairs(Player.getPlayers()) do
        if player.seated then
            players[#players+1] = player
        end
    end

    if #players < 3 then
        broadcastToAll('Not enough players (need 3-6)')
        return
    end

    if #players > 6 then
        broadcastToAll('Too many players (need 3-6)')
        return
    end

    -- get random goats (indices will map to `players` indices)
    local goats = {}
    local goatPool = allGoats()
    for i=1,#players do
        goats[#goats+1] = table.remove(goatPool, math.random(#goatPool))
    end

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
        broadcastToColor(goatToBlame.name .. ' GOAT is the scape goat', player.color)
    end

    local deck = deckForGoats(goats)

    -- TODO: remove
    spawn(deck)
    -- TODO: spawn cards + deal to players and locations
    -- TODO: spawn locations
    -- TODO: spawn prep tokens
end

--local deck = deckForGoats({goatYellow,goatBlue,goatRed})
--spawn(deck)

spawn(locationsDeck)

--startGame()
