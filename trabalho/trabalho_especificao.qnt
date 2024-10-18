module rpgBattle {

    import basicSpells.* from "./spells/basicSpells"

    // RPG TYPES    
    type CreaturesType =  Wizard 
                        | Hunter 
                        | Druid | Bear 
                        | Sheep 
                        | Monster 

    type ActionType[damage] =  Attack(int) 
                              | Paralysis 
                              | RemoveParalysis
                              | CreatesIlusion 
                              | Blind 
                              | WildShape 
                              | Skip 
                              | Dead 

    type Creature = {
        id: int,
        name: str,
        hp: int,
        paralysis: bool,
        skill: bool,
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
        "MONSTER2" -> 5,
        "SHEEP" -> 6,
        "BEAR" -> 7
    )

    pure val CREATURES = Map(

        1 -> {id: 1, name: "Presto", hp: 20, paralysis: false, skill: false ,creatureType: Wizard},
        2 -> {id: 2, name: "Hank", hp: 20, paralysis: false, skill: false ,creatureType: Hunter},
        3 -> {id: 3, name: "Bobby", hp: 20, paralysis: false, skill:false, creatureType: Druid},

        // Monsters
        4 -> {id: 4, name: "Gelatinous Cube", hp: 100, paralysis: false, skill: false, creatureType: Monster},
        // 5 -> {id: 5, name: "Venger", hp: 100, paralysis: false, skill: false, creatureType: Monster},

        // Animals
        6 -> {id: 6, name: "Ilusion Sheep", hp: 1, paralysis: false, skill: false, creatureType: Sheep},
        7 -> {id: 7, name:"Spectacled bear", hp: 60, paralysis: false, skill: false, creatureType: Bear}
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

    pure def resetSkill(c: Creature, skill: bool): Creature = {
        if(skill) 
            {...c, skill: false}
        else
            c
    }

    pure def activatedSkill(c: Creature, skill: bool): Creature = {
        {...c, skill: true}
    }

    pure def restartAnimalsAtributs(c: Creature): Creature = {
        match c.creatureType {
            | Bear => {...c, hp: 60, paralysis: false}
            | Sheep => {...c, hp: 1, paralysis: false}
            | _ => c
        }
    }

    pure def canChooseAnimal(animalType: CreaturesType, isActived: bool): bool = {
        match animalType {
            | Bear => isActived and CREATURES.values().exists(c => c.creatureType == Bear)
            | Sheep => isActived and CREATURES.values().exists(c => c.creatureType == Sheep)
            | _ => false
        }
    }

    pure def monsterReciverChoice(creatures: int -> Creature, druidTurnedBear: bool, wizardMakeIlusion: bool): Set[Creature] = {
        // List with Bear and Sheep 
        if (
            canChooseAnimal(Bear, druidTurnedBear)
            and
            canChooseAnimal(Sheep, wizardMakeIlusion)
        )
            creatures.values().filter(c => c.creatureType == Bear or c.creatureType == Sheep )

        // List with Bear
        else if (canChooseAnimal(Bear, druidTurnedBear))
            creatures.values().filter(c => c.creatureType == Bear)

        // List with Sheep 
        else if (canChooseAnimal(Sheep, wizardMakeIlusion))
           creatures.values().filter(c => c.creatureType == Sheep)

        else
            creatures.values().filter(c => c.creatureType != Monster and c.creatureType != Bear and c.creatureType != Sheep)

    }

    pure def choiseReciver(creatures: int -> Creature, agent: Creature, druidTurnedBear: bool, wizardMakeIlusion: bool): Set[Creature] = {

        if(agent.creatureType == Monster)
           monsterReciverChoice(creatures, druidTurnedBear, wizardMakeIlusion)
        else
           creatures.values().filter(c => c.creatureType == Monster)
    }

    pure def choiseRemoveParalyisisCreature(agent: Creature, creatures: int -> Creature): Set[Creature] = {
        creatures.values().filter(c => c.creatureType != Monster 
                                                    and c.creatureType != Sheep 
                                                    and c.creatureType != Bear 
                                                    and c.id != agent.id
                                                    and c.hp > 0 and c.paralysis == true)
    }

    // ACTIONS 
    action updateCreaturesTurn(  agentType: CreaturesType,
                                creatureUpdate: Creature, 
                                creatureAction: (Creature, a) => Creature, creatureOp: a): bool = {

        match agentType {
            | Monster => creatures' = creatures
                                      .setBy(creatureUpdate.id, c => creatureAction(c, creatureOp))
                                      .setBy(CREATURES_ENUM.get("HUNTER"), h => resetSkill(h, h.skill))
            | Wizard => creatures' = creatures
                                    .setBy(CREATURES_ENUM.get("WIZARD"), w => resetSkill(w, w.skill))
                                    .setBy(CREATURES_ENUM.get("SHEEP"), s => restartAnimalsAtributs(s))
                                    .setBy(creatureUpdate.id, c => creatureAction(c, creatureOp))
            | Druid => creatures' = creatures
                                    .setBy(CREATURES_ENUM.get("DRUID"), d => resetSkill(d, d.skill))
                                    .setBy(CREATURES_ENUM.get("BEAR"), b => restartAnimalsAtributs(b))
                                    .setBy(creatureUpdate.id, c => creatureAction(c, creatureOp))
            | Hunter => creatures' = creatures
                                     .setBy(creatureUpdate.id, c => creatureAction(c, creatureOp))
            // _ contain {Sheep, Bear}
            | _ =>  creatures' = creatures
        }
    }

    action updateCreatures(agent: Creature, receiver: Creature, creatureAction: ActionType): bool = {
        match creatureAction {
            | Attack(damg) => updateCreaturesTurn(agent.creatureType, receiver, damage, damg)
            | Paralysis => updateCreaturesTurn(agent.creatureType, receiver, paralysis, receiver.paralysis)
            | RemoveParalysis => updateCreaturesTurn(agent.creatureType, receiver, paralysis, receiver.paralysis)
            | CreatesIlusion => updateCreaturesTurn(agent.creatureType, agent, activatedSkill, agent.skill)
            | WildShape => updateCreaturesTurn(agent.creatureType, agent, activatedSkill, agent.skill)
            | Blind => updateCreaturesTurn(agent.creatureType, agent, activatedSkill, agent.skill)
            | _ => creatures' = creatures
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
        // Hero
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

            updateCreatures(attacker, receiver, Attack(damg)), 
            status' = Action({
                agent: attacker,
                receiver: receiver,
                creatureAction: Attack(damg)
            })
        }
    }


    action monsterAttack(attacker: Creature, receiver: Creature): bool = {
        val hunter = creatures.get(CREATURES_ENUM.get("HUNTER"))

        if(hunter.skill)
            attack(attacker, receiver, 0)
        else
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

            updateCreatures(monster, receiver, Paralysis), 
            status' = Action({
                agent: monster,
                receiver: receiver,
                creatureAction: Paralysis
            })
        }
    }

    // REMOVE PARALYSIS     
    action removeParalysisCreature(agent: Creature): bool = {
        nondet receiver = choiseRemoveParalyisisCreature(agent, creatures).oneOf()

        all {
            agent != receiver,
            agent.hp > 0,
            receiver.hp > 0, 

            updateCreatures(agent, receiver, RemoveParalysis), 
            status' = Action({
                agent: agent,
                receiver: receiver,
                creatureAction: RemoveParalysis
            })
        }
    }

    // CREATURE ILUSION
    action createIlusionSheep(agent: Creature): bool = {
        val sheep = creatures.get(CREATURES_ENUM.get("SHEEP"))
        all {
            agent.hp > 0,
            updateCreatures(agent, sheep, CreatesIlusion),
            status' = Action({
                agent: agent,
                receiver: sheep,
                creatureAction: CreatesIlusion
            })
        }
    }

    // WILD SHAPE 
    action activateWildShape(agent: Creature): bool = {
        val bear = creatures.get(CREATURES_ENUM.get("BEAR"))
        all {
            agent.hp > 0,
            updateCreatures(agent, bear, WildShape),
            status' = Action({
                agent: agent,
                receiver: bear,
                creatureAction: WildShape
            })
        }
    }

    // BLIND 
    action useBlindMonster(agent: Creature, receiver: Creature): bool = {
        all {
            agent != receiver,
            agent.hp > 0,
            receiver.hp > 0, 
            updateCreatures(agent, receiver, Blind),
            status' = Action({
                agent: agent,
                receiver: receiver,
                creatureAction: Blind
            })
        }
    }

    action wizardAction(agent: Creature, receiver: Creature): bool = {
        if (choiseRemoveParalyisisCreature(agent, creatures).size() > 0)
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
        if (choiseRemoveParalyisisCreature(agent, creatures).size() > 0)
            any {
                attack(agent, receiver, 10),
                useBlindMonster(agent, receiver),
                removeParalysisCreature(agent)
            }
        else 
            any {
                useBlindMonster(agent, receiver),
                attack(agent, receiver, 10)
            }
    }

    action druidAction(agent: Creature, receiver: Creature): bool = {
        if (choiseRemoveParalyisisCreature(agent, creatures).size() > 0)
            any {
                attack(agent, receiver, 10),
                activateWildShape(agent),
                removeParalysisCreature(agent)
            }
        else 
            any {
                activateWildShape(agent),
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

        val bearStatus = creatures.get(CREATURES_ENUM.get("DRUID")).skill

        val sheepStatus = creatures.get(CREATURES_ENUM.get("WIZARD")).skill

        nondet receiver = choiseReciver(creatures, agent, bearStatus, sheepStatus).oneOf()
      
        all {
            listInitatives' = listInitatives,

            updateTurn,
            
            creaturAction(agent, receiver)
        }
    }

    // INV
    val inv_start_test = not(turn.num > 2)

    // INV ATTACK 
    val inv_first_attack_monster_deals_10_damage = not(match status {
                                                | Action(a) => a.creatureAction == Attack(10) 
                                                  and a.agent.creatureType == Monster 
                                                  and turn.num == 0
                                                | Start => false
                                           })

    // INV PARALYSIS
    val inv_creature_is_paralysi = not(creatures.values().exists(c => c.paralysis == true))

    val inv_creature_is_remove_paralaysi = not(match status {
                                                | Action(a) => a.creatureAction == RemoveParalysis
                                                | Start => false
                                           })

    val inv_creature_is_skip = not(match status {
                                                | Action(a) => a.creatureAction == Skip
                                                | Start => false
                                           })

    //  INV DEAD
    val inv_creature_is_dead = not(match status {
                                    | Action(a) => a.creatureAction == Dead
                                    | Start => false
                                })

    // INV SHEEP
    val inv_create_ilusion = not(creatures.values().exists(c => c.skill == true and c.creatureType == Wizard))

    val inv_monster_attack_sheep = not(creatures.values().exists(c => c.creatureType == Sheep and c.hp < 0))

    // INV BEAR
    val inv_create_bear = not(creatures.values().exists(c => c.skill and c.creatureType == Druid))

    val inv_monster_attack_bear = not(creatures.values().exists(c => c.creatureType == Bear and c.hp < 60))

    // BLIND 
    val inv_use_blind = not(creatures.values().exists(c => c.skill == true and c.creatureType == Hunter))
}