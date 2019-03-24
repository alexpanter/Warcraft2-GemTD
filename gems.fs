module Gems

type GemType =
    | Amethyst
    | Diamond
    | Opal
    | Emerald
    | Topaz
    | Ruby
    | Sapphire

type Quality =
    | Chipped
    | Flawed
    | Regular
    | Flawless
    | Perfect

let getGemTypeString = function
    | Amethyst -> "Amethyst"
    | Diamond  -> "Diamond"
    | Opal     -> "Opal"
    | Emerald  -> "Emerald"
    | Topaz    -> "Topaz"
    | Ruby     -> "Ruby"
    | Sapphire -> "Sapphire"

let getGemQualityString = function
    | Chipped  -> "Chipped"
    | Flawed   -> "Flawed"
    | Regular  -> ""
    | Flawless -> "Flawless"
    | Perfect  -> "Perfect"

let getGemTypeFromNumber = function
    | 1 -> Some(Amethyst)
    | 2 -> Some(Diamond)
    | 3 -> Some(Opal)
    | 4 -> Some(Emerald)
    | 5 -> Some(Topaz)
    | 6 -> Some(Ruby)
    | 7 -> Some(Sapphire)
    | _ -> None

let getGemQualityFromNumber = function
    | 1 -> Some(Chipped)
    | 2 -> Some(Flawed)
    | 3 -> Some(Regular)
    | 4 -> Some(Flawless)
    | 5 -> Some(Perfect)
    | _ -> None

type Gem = {quality: Quality; gemType: GemType; name: string}
type SpecialGem =
    | Special3 of Gem * Gem * Gem * string
    | Special4 of Gem * Gem * Gem * Gem * string
with
    member special.GetList() =
        match special with
        | Special3(g1, g2, g3, _)     -> [g1; g2; g3]
        | Special4(g1, g2, g3, g4, _) -> [g1; g2; g3; g4]
    member special.GetName() =
        match special with
        | Special3(_, _, _, s)    -> s
        | Special4(_, _, _, _, s) -> s
end

let createGem(q, t, n) = {quality = q; gemType = t; name = n}

let chippedAmethyst  = createGem(Chipped,  Amethyst, "Chipped Amethyst")
let flawedAmethyst   = createGem(Flawed,   Amethyst, "Flawed Amethyst")
let regularAmethyst  = createGem(Regular,  Amethyst, "Amethyst")
let flawlessAmethyst = createGem(Flawless, Amethyst, "Flawless Amethyst")
let perfectAmethyst  = createGem(Perfect,  Amethyst, "Perfect Amethyst")

let chippedDiamond  = createGem(Chipped,  Diamond, "Chipped Diamond")
let flawedDiamond   = createGem(Flawed,   Diamond, "Flawed Diamond")
let regularDiamond  = createGem(Regular,  Diamond, "Diamond")
let flawlessDiamond = createGem(Flawless, Diamond, "Flawless Diamond")
let perfectDiamond  = createGem(Perfect,  Diamond, "Perfect Diamond")

let chippedOpal  = createGem(Chipped,  Opal, "Chipped Opal")
let flawedOpal   = createGem(Flawed,   Opal, "Flawed Opal")
let regularOpal  = createGem(Regular,  Opal, "Opal")
let flawlessOpal = createGem(Flawless, Opal, "Flawless Opal")
let perfectOpal  = createGem(Perfect,  Opal, "Perfect Opal")

let chippedEmerald  = createGem(Chipped,  Emerald, "Chipped Emerald")
let flawedEmerald   = createGem(Flawed,   Emerald, "Flawed Emerald")
let regularEmerald  = createGem(Regular,  Emerald, "Emerald")
let flawlessEmerald = createGem(Flawless, Emerald, "Flawless Emerald")
let perfectEmerald  = createGem(Perfect,  Emerald, "Perfect Emerald")

let chippedTopaz  = createGem(Chipped,  Topaz, "Chipped Topaz")
let flawedTopaz   = createGem(Flawed,   Topaz, "Flawed Topaz")
let regularTopaz  = createGem(Regular,  Topaz, "Topaz")
let flawlessTopaz = createGem(Flawless, Topaz, "Flawless Topaz")
let perfectTopaz  = createGem(Perfect,  Topaz, "Perfect Topaz")

let chippedRuby  = createGem(Chipped,  Ruby, "Chipped Ruby")
let flawedRuby   = createGem(Flawed,   Ruby, "Flawed Ruby")
let regularRuby  = createGem(Regular,  Ruby, "Ruby")
let flawlessRuby = createGem(Flawless, Ruby, "Flawless Ruby")
let perfectRuby  = createGem(Perfect,  Ruby, "Perfect Ruby")

let chippedSapphire  = createGem(Chipped,  Sapphire, "Chipped Sapphire")
let flawedSapphire   = createGem(Flawed,   Sapphire, "Flawed Sapphire")
let regularSapphire  = createGem(Regular,  Sapphire, "Sapphire")
let flawlessSapphire = createGem(Flawless, Sapphire, "Flawless Sapphire")
let perfectSapphire  = createGem(Perfect,  Sapphire, "Perfect Sapphire")


// e.g. ...
let starSapphire = Special3(chippedAmethyst, regularDiamond, perfectOpal, "Star Sapphire")
let uranium = Special3(chippedAmethyst, regularDiamond, perfectOpal, "Uranium238")

let getAllSpecialGems() =
    [ starSapphire;
      uranium;
    ]
