module rpgBattle {

    import basicSpells.* from "./spells/basicSpells"

    // RPG TYPES    
    type CreaturesType =  Wizard 
                        | Hunter 
                        | Druid | Bear 
                        | Sheep 
                        | Monster 

    type ActionType[damage] =  Attack(int) // v
                              | Paralysis // V
                              | RemoveParalysis
                              | CreatesIlusion // V
                              | Blind
                              | WildShape
                              | Skip // V
                              | Dead

    type Ability = Active | Deactivated | Empty

    type Creature = {
        id: int,
        name: str,
        hp: int,
        paralysis: bool,
        blind: Ability,
        wildShape: Ability,
        ilusion: Ability,
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
    }

    type Iniciative = {id:int, rollResult: int, creatureType: CreaturesType}

    type CombatStatus[s] = Action(Status) | Start


    // CREATURES VAL

    // Creatures in Start Battle
    pure val CREATURES_ENUM = Map(
        "WIZARD" -> 1,
        "HUNTER" -> 2,
        "DRUID" -> 3,
        "MONSTER1" -> 4,
        "MONSTER2" -> 4,
        "SHEEP" -> 6,
        "BEAR" -> 7
    )

    pure val CREATURES = Map(

        1 -> {id: 1, name: "Presto", hp: 20, paralysis: false, blind: Empty, wildShape: Empty, ilusion: Deactivated, creatureType: Wizard},
        2 -> {id: 2, name: "Hank", hp: 20, paralysis: false, blind: Deactivated, wildShape: Empty, ilusion: Empty, creatureType: Hunter},
        3 -> {id: 3, name: "Bobby", hp: 20, paralysis: false, blind: Empty, wildShape: Deactivated, ilusion: Empty,  creatureType: Druid},

        // Monsters
        4 -> {id: 4, name: "Gelatinous Cube", hp: 100, paralysis: false, blind: Empty, wildShape: Empty, ilusion: Empty, creatureType: Monster},
        // 5 -> {id: 5, name: "Venger", hp: 100, paralysis: false, blind: Empty, wildShape: Empty, ilusion: Empty, creatureType: Monster},

        // Animals
        6 -> {id: 6, name: "Ilusion Sheep", hp: 1, paralysis: false, blind: Empty, wildShape: Empty, ilusion: Empty, creatureType: Sheep},
        7 -> {id: 7, name: "Dark Bear", hp: 60, paralysis: false, blind: Empty, wildShape: Empty, ilusion: Empty, creatureType: Bear}
    )

    // STATUS VAR ROUND
    var creatures: int -> Creature

    // TURN VAR
    var status: CombatStatus

    var turn: Turn

    var listInitatives: List[Iniciative]

    // USEFUL DEFINITIONS 

    pure def newTurn(turn: Turn): Turn = {
        {num: turn.num + 1, round: 0}
    }

    pure def newRound(turn: Turn): Turn = {
        {...turn, round: turn.round + 1}
    }

    pure def damage(c: Creature, d: int): Creature = {
        {...c, hp: c.hp - d}
    }

    pure def paralysis(c: Creature, paralysis: bool): Creature = {
        {...c, paralysis: not(paralysis)}
    }

    pure def nothing(c: Creature, worthless: a): Creature = {
        c
    }

    pure def createIlusion(wizard: Creature, ability: Ability): Creature = {
        match ability {
            | Active => {...wizard, ilusion: Deactivated}
            | Deactivated => {...wizard, ilusion: Active}
            | Empty => {...wizard, ilusion: Empty}
        }
    }

    pure def getIlusionStatus(wizard: Creature): bool = {
        wizard.ilusion == Active
    }

    pure def activeWildShape(druid: Creature, ability: Ability): Creature = {
        match ability {
            | Active => {...druid, wildShape: Deactivated}
            | Deactivated =>  {...druid, wildShape: Active}
            | Empty =>  {...druid, wildShape: Empty}
        }
    }

    pure def getWildShapeStatus(druid: Creature): bool = {
        druid.wildShape == Active
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
            CREATURES.values().filter(c => c.creatureType == Sheep)

        else
            CREATURES.values().filter(c => c.creatureType != Monster and c.creatureType != Bear and c.creatureType != Sheep and c.hp > 0)

    }

    pure def choiseReciver(creatures: int -> Creature ,agent: Creature, druidTurnedBear: bool, wizardMakeIlusion: bool): Set[Creature] = {
        if(agent.creatureType == Monster)
            monsterReciverChoice(druidTurnedBear, wizardMakeIlusion)
        else
            creatures.values().filter(c => c.creatureType == Monster and c.hp > 0)
    }

    pure def choiseRemoveParalyisisCreature(creatures: int -> Creature): Set[Creature] = {
        creatures.values().filter(c => c.creatureType != Monster 
                                                    and c.creatureType != Sheep 
                                                    and c.creatureType != Bear 
                                                    and c.hp > 0 and c.paralysis == true)
    }

    // ACTIONS 
    action upadateCreaturesEndTurn(creatureUpdate: Creature, creatureAction: (Creature, a) => Creature, op: a): bool = {
            creatures' = creatures.setBy(creatureUpdate.id, c => creatureAction(c, op))
                        .setBy(CREATURES_ENUM.get("SHEEP"), s => restartAnimalsAtributs(s))
                        .setBy(CREATURES_ENUM.get("BEAR"), b => restartAnimalsAtributs(b))
                        .setBy(CREATURES_ENUM.get("WIZARD"), w => createIlusion(w, Active))
                        .setBy(CREATURES_ENUM.get("DRUID"), d => activeWildShape(d, Active))
    }

    action updateCreaturesTurn(  agentType: CreaturesType,
                                creatureUpdate: Creature, 
                                creatureAction: (Creature, a) => Creature, creatureOp: a): bool = {

        match agentType {
            | Wizard => creatures' = creatures
                                    .setBy(CREATURES_ENUM.get("WIZARD"), w => createIlusion(w, Active))
                                    .setBy(CREATURES_ENUM.get("SHEEP"), s => restartAnimalsAtributs(s))
                                    .setBy(creatureUpdate.id, c => creatureAction(c, creatureOp))
            | _ =>  creatures' = creatures.setBy(creatureUpdate.id, c => creatureAction(c, creatureOp))
        }
    }

    action updateCreatures(agent: Creature, receiver: Creature, creatureAction: ActionType): bool = {
        match creatureAction {
            | Attack(damg) => updateCreaturesTurn(agent.creatureType, receiver, damage, damg)
            | Paralysis => updateCreaturesTurn(agent.creatureType, receiver, paralysis, receiver.paralysis)
            | RemoveParalysis => updateCreaturesTurn(agent.creatureType, receiver, paralysis, receiver.paralysis)
            | CreatesIlusion => updateCreaturesTurn(agent.creatureType, agent, createIlusion, Deactivated)
            | WildShape => updateCreaturesTurn(agent.creatureType, agent, activeWildShape, Deactivated)
            | _ => updateCreaturesTurn(agent.creatureType, agent, nothing, Empty)
        }
    }

    action updateTurn: bool = {
        // End turn
        if (turn.round == listInitatives.length() - 1) 
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

            updateCreatures(attacker, receiver, Attack(damg)), //creatures.setBy(receiver.id, c => damage(c, damg)),
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

            updateCreatures(monster, receiver, Paralysis), //creatures.setBy(receiver.id, c => c),
            status' = Action({
                agent: monster,
                receiver: receiver,
                creatureAction: Paralysis
            })
        }
    }

    // REMOVE PARALYSIS     
    action removeParalysisCreature(agent: Creature): bool = {
        nondet receiver = choiseRemoveParalyisisCreature(creatures).oneOf()

        all {
            agent != receiver,
            agent.hp > 0,
            receiver.hp > 0, 
            agent.paralysis == false,

            updateCreatures(agent, receiver, RemoveParalysis), 
            status' = Action({
                agent: agent,
                receiver: receiver,
                creatureAction: Paralysis
            })
        }
    }

    // CREATURE ILUSION
    action createIlusionSheep(agent: Creature): bool = {
        val sheep= creatures.get(CREATURES_ENUM.get("SHEEP"))
        all {
            agent.hp > 0,
            updateCreatures(agent, sheep, CreatesIlusion),
            status' = Action({
                agent: agent,
                receiver: creatures.get(6),
                creatureAction: CreatesIlusion
            })
        }
    }

    action wizardAction(agent: Creature, receiver: Creature): bool = {
        if (choiseRemoveParalyisisCreature(creatures).size() > 0)
            any {
                attack(agent, receiver, 10),
                createIlusionSheep(agent),
                removeParalysisCreature(agent)
            }
        else 
            any {
                attack(agent, receiver, 10),
                createIlusionSheep(agent),
            }
    }

    action hunterAction(agent: Creature, receiver: Creature): bool = {
        if (choiseRemoveParalyisisCreature(creatures).size() > 0)
            any {
                attack(agent, receiver, 10),
                removeParalysisCreature(agent)
            }
        else 
            any {
                attack(agent, receiver, 10),
            }
    }

    action druidAction(agent: Creature, receiver: Creature): bool = {
        if (choiseRemoveParalyisisCreature(creatures).size() > 0)
            any {
                attack(agent, receiver, 10),
                removeParalysisCreature(agent)
            }
        else 
            any {
                attack(agent, receiver, 10),
            }
    }

    // SKIP
    action skipTurn(agent: Creature, receiver: Creature): bool = {
            all {
                updateCreatures(agent, receiver, Skip), 
                status' = Action({
                    agent: agent,
                    receiver: receiver,
                    creatureAction: Skip
                })
            }
    }

    // DEAD
    action dead(agent: Creature, receiver: Creature): bool = {
        all {
            updateCreatures(agent, receiver, Dead),
            status' = Action({
                    agent: agent,
                    receiver: receiver,
                    creatureAction: Dead
                })
        }
    }


    action heroAction(agent: Creature, receiver: Creature): bool = {
        if (agent.hp > 0)
            if (agent.paralysis == false)
                match agent.creatureType {
                    | Wizard =>  wizardAction(agent, receiver)  
                    | Hunter => hunterAction(agent, receiver)
                    | Druid => druidAction(agent, receiver)
                    | _ => attack(agent, receiver, 10)            
                }
            else 
                skipTurn(agent, receiver)
        else
                dead(agent, receiver)
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
            turn' = {num: 0, round: 0},

            // Dice Rolls 
            diceRolls
         }
    }

    action step = {
        val agent = creatures.get(listInitatives[turn.round % listInitatives.length()].id)

        val bearStatus = getWildShapeStatus(creatures.get(CREATURES_ENUM.get("DRUID")))

        val sheepStatus = getIlusionStatus(creatures.get(CREATURES_ENUM.get("WIZARD")))

        nondet receiver = choiseReciver(creatures ,agent, bearStatus, sheepStatus).oneOf()
      
        all {
            listInitatives' = listInitatives,

            updateTurn,
            
            creaturAction(agent, receiver)
        }
    }

    // INV
    val inv_start_test = not(turn.num > 3)


    // INV PARALYSIS
    val inv_creature_is_paralysi = not(creatures.values().exists(c => c.paralysis == true))

    // INV SHEEP
    val inv_create_ilusion = not(creatures.values().exists(c => c.ilusion == Active))

    val inv_monster_attack_sheep = not(creatures.values().exists(c => c.creatureType == Sheep and c.hp < 0))
}