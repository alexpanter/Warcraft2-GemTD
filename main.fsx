#load "gems.fs"
open Gems
open System.Collections.Generic
open System


let gems = new Dictionary<Gem, int>()
let potentialSpecialGems = new HashSet<SpecialGem>()
let allSpecialGems = getAllSpecialGems()

let addToCollection(gem: Gem) = ()

let hasGem(gem: Gem) = gems.ContainsKey(gem) && gems.[gem] > 0

let addGem(gem: Gem) =
    // add gem
    if not(gems.ContainsKey(gem)) then gems.Add(gem, 1) else gems.[gem] <- gems.[gem] + 1
    // update possibilities for creating special gems
    for specialGem in allSpecialGems do
        match specialGem with
        | s -> for g in s.GetList() do
                   if hasGem(g) then potentialSpecialGems.Add(s) |> ignore

let removeGem(gem: Gem) =
    if gems.ContainsKey(gem) && gems.[gem] > 0 then gems.[gem] <- gems.[gem] - 1

let gemsOwned() =
    printfn "Gems owned:"
    for gem in gems do
        if gem.Value > 0 then
            printfn "%s (%i)" gem.Key.name gem.Value

let gemsNeededToCreateSpecial() =
    printfn "Gems missing:"
    for special in potentialSpecialGems do
        for gem in special.GetList() do
            if not(gems.ContainsKey(gem)) || (gems.ContainsKey(gem) && gems.[gem] = 0) then
                printfn "%18s (%s)" <|| (gem.name, special.GetName())

let addSpecialGem = function
    | (s: SpecialGem) ->
        let lst = s.GetList() in
            if List.forall (fun gem -> hasGem(gem)) lst then
                List.iter (fun gem -> removeGem(gem)) lst
                printfn "Special gem added (%s)" (s.GetName())
            else printfn "Can't add special gem, don't have the requirements!"

let rec main() =
    printf "\nContinue (press enter): "
    Console.ReadLine() |> ignore
    Console.Clear()
    printfn "1: add gem, 2: check specials, 3: add special, 4: gems owned, 5: quit"
    let input = ref 0
    let mutable readInput = true
    while readInput do
        printf "Choice: "
        if Int32.TryParse(Console.ReadLine(), input) then readInput <- false

    match !input with
        | 1 ->
            printfn "GemQuality: 1=Chipped, 2=Flawed, 3=, 4=Flawless, 5=Perfect"
            printf "GemType: 1=Amethyst, 2=Diamond, 3=Opal, "
            printfn "4=Emerald, 5=Topaz, 6=Ruby, 7=Sapphire"
            let (gemQuality, gemType) = (ref 0, ref 0)
            let mutable (readQualityInput, readTypeInput) = (true, true)
            while readQualityInput do
                printf "Choose Quality: "
                if Int32.TryParse(Console.ReadLine(), gemQuality) then
                    readQualityInput <- false
            while readTypeInput do
                printf "Choose Type: "
                if Int32.TryParse(Console.ReadLine(), gemType) then
                    readTypeInput <- false
            match (getGemQualityFromNumber(!gemQuality),
                   getGemTypeFromNumber(!gemType)) with
                | (Some(q), Some(t)) ->
                    let qualityS = getGemQualityString(q)
                    let typeS = getGemTypeString(t)
                    let name = (if q = Regular then "" else qualityS + " ") + typeS
                    let gem = createGem(q, t, name)
                    printf "Add %s (y/n): " gem.name
                    match Console.ReadLine() with
                        | "y" ->
                            addGem(gem)
                            printfn "Added %s" gem.name
                        | _ ->
                            printfn "Gem will not be added."
                | _ -> printfn "Invalid quality or type input."
            main()
        | 2 ->
            gemsNeededToCreateSpecial()
            main()
        | 3 ->
            ignore <| List.fold(fun i (s: SpecialGem) ->
                          printfn "%i: %s" <|| (i, s.GetName()); i+1) 1 allSpecialGems
            let specialInput = ref 0
            let mutable readSpecialInput = true
            while readSpecialInput do
                printf "Special choice: "
                if Int32.TryParse(Console.ReadLine(), specialInput) then
                    readSpecialInput <- false
            specialInput := !specialInput - 1
            try
                addSpecialGem(allSpecialGems.[!specialInput])
            with
                | _ -> printfn "Invalid special choice."
            main()
        | 4 ->
            gemsOwned()
            main()
        | 5 ->
            printfn "Terminating program."
        | _ ->
            printfn "Invalid choice."
            main()

main()
