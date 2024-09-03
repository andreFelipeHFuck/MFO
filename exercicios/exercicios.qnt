module exercicios {
    // Exercício 1
    /*
        Escreva um operador que recebe um conjunto de inteiros e retorna o
        maior valor
    */
    pure def maior_valor(c: Set[int]) : int = c.fold(0, (acc, i) => if (i > acc) i else acc)

    // Exercício 2
    /*
        Dado um conjunto de records do tipo { nome: str, idade:
        int }, escreva um operador que recebe esse conjunto e retorna a
        média de idade.
    */
    val pessoas = Set(
        {nome: "João", idade: 25},
        {nome: "Maria", idade: 30},
        {nome: "Carlos", idade: 22},
        {nome: "Ana", idade: 28},
        {nome: "Pedro", idade: 35},
        {nome: "Luiza", idade: 27},
        {nome: "Marcos", idade: 40},
        {nome: "Fernanda", idade: 32},
        {nome: "Lucas", idade: 23},
        {nome: "Beatriz", idade: 29}
    )

    pure def media_idade(p: Set[{nome: str, idade: int}]):int =
        pessoas.fold(0, (acc, i) => acc + i.idade)/pessoas.fold(0, (acc, _) => acc + 1)

    // Exercício 3
    /*
        Defina um valor que contenha todos os conjuntos possíveis com val-
        ores inteiros de 1 a 10, que contenham o número 5 ou o 6
    */

    type Valor = Some(Set[Set[int]]) | None

    val escolha = Map(5 -> 6, 6 -> 5)

    pure def definir_valor(v: int): Valor =
        if (v.in(Set(5, 6)))
            Some(1.to(10)
            .powerset()
            .filter(x => v.in(x) and not(escolha.get(v).in(x))))
        else 
            None

    // Exercício 4
    /*
        Escreva um operador que recebe uma lista e retorna o reverso dela.
    */
    pure def lista_reversa(l: List[int]): List[int] = l.foldr([], (i, acc) => acc.append(i))

    // Exercício 5
    /*
        Dado um conjunto de records do tipo { nome: str, idade:
        int }, escreva um operador que recebe esse conjunto e retorna um
        mapa de nome pra idade.
    */
    pure def map_nome_para_idade(p: Set[{nome: str, idade: int}]): str -> int = 
        p.map(p => (p.nome, p.idade)).setToMap()

    val ex1 = maior_valor(Set(86, 65, 83, 65, 54, 68, 57, 98, 53, 81, 18, 100, 81, 1, 64))

    val ex2 = media_idade(pessoas)

    val ex3_1 = definir_valor(5)
    val ex3_2 = definir_valor(6)

    val ex4 = lista_reversa([86, 65, 83, 65, 54, 68, 57, 98, 53, 81, 18, 100, 81, 1, 64])

    val ex5 = map_nome_para_idade(pessoas)

}