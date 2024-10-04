module rpgBattle {
    // RPG type
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
        name: str,
        hp: int,
        paralysis: bool,
        wild_shape: bool, 
        creature_type: CreaturesType
    }

    type Status = {
        acted: Creature,
        received: Creature,
        creature_action: ActionType,
    }

    type BattleStatus[s] = StatusBattle(Status) | StartBattle

    // Creatures val

    // Creatures in Start Battle
    pure val CREATURES = Set(
        {name: "Presto", hp: 20, paralysis: false, wild_shape: false, creature_type: Wizard},
        {name: "Hank", hp: 20, paralysis: false, wild_shape: false, creature_type: Hunter},
        {name: "Bobby", hp: 20, paralysis: false, wild_shape: false, creature_type: Druid},
        {name: "Gelatinous Cube", hp: 100, paralysis: false, wild_shape: false, creature_type: Monster} //,
        // {name: "Venger", hp: 100, paralysis: false, wild_shape: false, creature_type: Monster}
    )

    pure val SHEEP = {name: "Ilusion Sheep", hp: 1, }

    // Round var
    var status: BattleStatus

    var round: int

    pure def damage(c: Creature, d: int): Creature = {
        {...c, hp: c.hp - d}
    }




}