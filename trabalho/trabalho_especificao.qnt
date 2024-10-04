module rpgBattle {

    import basicSpells.* from "./spells/basicSpells"

    // RPG TYPES    
    type CreaturesType =  Wizard 
                        | Hunter 
                        | Druid | Bear 
                        | Sheep 
                        | Monster 
                        | Nobody

    type ActionType[damage] =  Attack(int) 
                              | RemoveParalysis
                              | CreatesIlusion
                              | Blind
                              | WildShape

    type Creature = {
        id: int,
        name: str,
        hp: int,
        paralysis: bool,
        creatureType: CreaturesType
    }

    type Status = {
        agent: Creature,
        receiver: Creature,
        creatureAction: ActionType,
    }

    type Iniciative = {id:int, rollResult: int, creatureType: CreaturesType}

    type CombatStatus[s] = Combat(Status) | Start


    // CREATURES VAL

    // Creatures in Start Battle
    pure val CREATURES = Set(

        {id: 1, name: "Presto", hp: 20, paralysis: false, creatureType: Wizard},
        {id: 2, name: "Hank", hp: 20, paralysis: false, creatureType: Hunter},
        {id: 3, name: "Bobby", hp: 20, paralysis: false, creatureType: Druid},

        // Monsters
        {id: 4, name: "Gelatinous Cube", hp: 100, paralysis: false, creatureType: Monster},
        // {id: 5, name: "Venger", hp: 100, paralysis: false, wild_shape: false, creatureType: Monster}

        // Animals
        {id: -1, name: "Ilusion Sheep", hp: 1, paralysis: false, creatureType: Sheep},
        {id: -1, name: "Dark Bear", hp: 60, paralysis: false, creatureType: Bear}
    )

    // STATUS VAR ROUND
    var druidTurnedBear: bool
    var wizardMakeIlusion: bool

    // TURN VAR
    var status: CombatStatus

    var turn: int

    var round: int
    var listInitatives: List[Iniciative]

    // USEFUL DEFINITIONS 

    pure def damage(c: Creature, d: int): Creature = {
        {...c, hp: c.hp - d}
    }

    pure def alterStatusIlusion(ilusion: bool): bool = {
        not(ilusion)
    }

    pure def alterStatusBear(bear: bool): bool = {
        not(bear)
    }

    pure def restartAnimalsAtributs(c: Creature): Creature = {
        match c.creatureType {
            | Bear => {...c, hp: 60, paralysis: false}
            | Sheep => {...c, hp: 1, paralysis: false}
            | _ => c
        }
    }

    // pure def monsterReciverChoice(): List[Creature] = {
    //     // List with Bear and Sheep 
    //     if ()

    //     // List with Bear
    //     else if ()

    //     // List with Sheep 
    //     else if ()

    //     else
    // }


    // ACTIONS 

    action diceRolls = {
        // 
        nondet wizardD20 = 1.to(20).oneOf()
        nondet hunterD20 = 1.to(20).oneOf()
        nondet druidD20 = 1.to(20).oneOf()

        //  Monsters 
        nondet monster1D20 = 1.to(20).oneOf()
        // nondet monster2 = 1.to(20).oneOf()

        val initatives = List(
                         {id: 1, rollResult: wizardD20, creatureType: Wizard}, 
                         {id: 2, rollResult: hunterD20, creatureType: Hunter},
                         {id: 3, rollResult: druidD20, creatureType: Druid},

                         {id: 4, rollResult: monster1D20, creatureType: Monster} //,
                         // {id: 5, rollResult: monster2, cretureType: Monster}
                    )
        
        listInitatives' = sortList(initatives, (x, y) => x.rollResult > y.rollResult)                                        
    }

    // action choiseReciver(idAgent: Iniciative,  idReciver: Iniciative): int = {
        
    // }

    action init = {
         all {
            status' = Start,
            round' = 0,
            turn' = 0,

            // Dice Rolls 
            diceRolls
         }
    }

    action step = {

        // val agent = listInitatives[round % listInitatives.length()]
        // val reciver = 

        turn' = turn + 1
    }
        
    val inv_start_test = not(turn > 0)

}