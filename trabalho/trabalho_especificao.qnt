module rpgBattle {

    import basicSpells.* from "./spells/basicSpells"

    // RPG TYPES    
    type CreaturesType =  Wizard 
                        | Hunter 
                        | Druid | Bear 
                        | Sheep 
                        | Monster 
                        | Null 

    type ActionType[damage] =  Attack(int) 
                              | Paralysis
                              | RemoveParalysis
                              | CreatesIlusion
                              | Blind
                              | WildShape
                              | Skip

    type Creature = {
        id: int,
        name: str,
        hp: int,
        paralysis: bool,
        creatureType: CreaturesType
    }

    // 
    type Status = {
        agent: Creature,
        receiver: Creature,
        creatureAction: ActionType,
    }

    // data about turn
    type Turn = {
        num: int,
        round: int,
        bear: bool,
        sheep: bool
    }

    type Iniciative = {id:int, rollResult: int, creatureType: CreaturesType}

    type CombatStatus[s] = Action(Status) | Start


    // CREATURES VAL

    // Creatures in Start Battle
    pure val CREATURES = Map(

        1 -> {id: 1, name: "Presto", hp: 20, paralysis: false, creatureType: Wizard},
        2 -> {id: 2, name: "Hank", hp: 20, paralysis: false, creatureType: Hunter},
        3 -> {id: 3, name: "Bobby", hp: 20, paralysis: false, creatureType: Druid},

        // Monsters
        4 -> {id: 4, name: "Gelatinous Cube", hp: 100, paralysis: false, creatureType: Monster},
        // 5 -> {id: 5, name: "Venger", hp: 100, paralysis: false, wild_shape: false, creatureType: Monster},

        // Animals
        6 -> {id: 6, name: "Ilusion Sheep", hp: 1, paralysis: false, creatureType: Sheep},
        7 -> {id: 7, name: "Dark Bear", hp: 60, paralysis: false, creatureType: Bear}
    )

    // STATUS VAR ROUND
    var creatures: int -> Creature

    // TURN VAR
    var status: CombatStatus

    var turn: Turn

    var listInitatives: List[Iniciative]

    // USEFUL DEFINITIONS 

    pure def newTurn(turn: Turn): Turn = {
        {...turn, num: turn.num + 1, round: 0}
    }

    pure def newRound(turn: Turn): Turn = {
        {...turn, round: turn.round + 1}
    }

    pure def damage(c: Creature, d: int): Creature = {
        {...c, hp: c.hp - d}
    }

    pure def paralysis(c: Creature): Creature = {
        {...c, paralysis: true}
    }

    pure def alterStatusSheep(ilusion: bool): bool = {
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

    pure def canChooseAnimal(animalType: CreaturesType, druidTurnedBear: bool, wizardMakeIlusion: bool): bool = {
        match animalType {
            | Bear => druidTurnedBear and CREATURES.values().exists(c => c.creatureType == Bear and c.hp > 0)
            | Sheep => wizardMakeIlusion and CREATURES.values().exists(c => c.creatureType == Sheep and c.hp > 0)
            | _ => false
        }
    }

    pure def monsterReciverChoice(druidTurnedBear: bool, wizardMakeIlusion: bool): Set[Creature] = {
        // List with Bear and Sheep 
        if (
            canChooseAnimal(Bear, druidTurnedBear, wizardMakeIlusion)
            and
            canChooseAnimal(Sheep, druidTurnedBear, wizardMakeIlusion)
        )
            CREATURES.values().filter(c => c.creatureType == Bear and c.creatureType == Sheep )

        // List with Bear
        else if (canChooseAnimal(Bear, druidTurnedBear, wizardMakeIlusion))
            CREATURES.values().filter(c => c.creatureType == Bear)

        // List with Sheep 
        else if (canChooseAnimal(Sheep, druidTurnedBear, wizardMakeIlusion))
            CREATURES.values().filter(c => c.creatureType != Sheep)

        else
            CREATURES.values().filter(c => c.creatureType != Monster and c.creatureType != Bear and c.creatureType != Sheep and c.hp > 0)

    }

    pure def choiseReciver(agent: Creature, druidTurnedBear: bool, wizardMakeIlusion: bool): Set[Creature] = {
        if(agent.creatureType == Monster)
            monsterReciverChoice(druidTurnedBear, wizardMakeIlusion)
        else
            CREATURES.values().filter(c => c.creatureType == Monster and c.hp > 0)
    }

    // ACTIONS 

    action updateTurn: bool = {
        if (turn.round == listInitatives.length())
            turn' = newTurn(turn)
        else
            turn' = newRound(turn)
    }

    action diceRolls = {
        // 
        nondet wizardD20 = 1.to(20).oneOf()
        nondet hunterD20 = 1.to(20).oneOf()
        nondet druidD20 = 1.to(20).oneOf()

        //  Monsters 
        nondet monster1D20 = 1.to(20).oneOf()
        // nondet monster2D20 = 1.to(20).oneOf()

        val initatives = List(
                         {id: 1, rollResult: wizardD20, creatureType: Wizard}, 
                         {id: 2, rollResult: hunterD20, creatureType: Hunter},
                         {id: 3, rollResult: druidD20, creatureType: Druid},

                         {id: 4, rollResult: monster1D20, creatureType: Monster} //,
                         // {id: 5, rollResult: monster2D20, cretureType: Monster}
                    )
        
        listInitatives' = sortList(initatives, (x, y) => x.rollResult > y.rollResult)                                        
    }


    // ATTACK
    action attack(attacker: Creature, receiver: Creature, damg: int): bool = {
        all {
            attacker != receiver,
            attacker.hp > 0,
            receiver.hp > 0,

            creatures' = creatures.setBy(receiver.id, c => damage(c, damg)),
            status' = Action({
                agent: attacker,
                receiver: receiver,
                creatureAction: Attack(damg)
            })
        }
    }

    action monsterAttack(attacker: Creature, receiver: Creature): bool = {
        if(turn.num == 0)
            attack(attacker, receiver, 10)
        else
            attack(attacker, receiver, 20)
    }

    // PARALYSIS
    action paralysisCreature(monster: Creature, receiver: Creature): bool = {
        all {
            monster != receiver,
            monster.hp > 0,
            receiver.hp > 0, 

            creatures' = creatures.setBy(receiver.id, c => paralysis(c)),
            status' = Action({
                agent: monster,
                receiver: receiver,
                creatureAction: Paralysis
            })
        }
    }

    action heroAction(agent: Creature, receiver: Creature): bool = {
        any {
            attack(agent, receiver, 10)
        }
    }

    action monsterAction(agent: Creature, receiver: Creature): bool = {
        any {
            monsterAttack(agent, receiver),
            paralysisCreature(agent, receiver)
        }
    }

    action creaturAction(agent: Creature, receiver: Creature): bool = {
       if (agent.creatureType == Monster)
            monsterAction(agent, receiver)
        else
            heroAction(agent, receiver)
    }

    action init = {
         all {
            creatures' = CREATURES,

            status' = Start,
            turn' = {num: 0, round: 0, bear: false, sheep: false},

            // Dice Rolls 
            diceRolls
         }
    }

    action step = {
        val agent = creatures.get(listInitatives[turn.round % listInitatives.length()].id)

        nondet receiver = choiseReciver(agent, turn.bear, turn.sheep).oneOf()
      
        all {
            listInitatives' = listInitatives,

            creaturAction(agent, receiver),

            updateTurn
        }
    }
        
    val inv_start_test = not(turn.num > 1)

    val inv_creature_is_paralysi = creatures.values().exists(c => c.creatureType != Monster implies c.paralysis == true)
}