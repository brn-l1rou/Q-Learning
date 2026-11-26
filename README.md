# Q-Learning em Haskell

Implementação didática do **Q-Learning tabular** em Haskell, feita para o projeto em
grupo (tema 3.2: *Q-Learning*). Um agente aprende, por tentativa e erro, a sair do
início de um labirinto e chegar ao objetivo desviando de armadilhas.

O enunciado pede estados, ações, recompensas, políticas, função de valor, a
atualização da tabela Q e exploração x aproveitamento. Cada item está em:

|Conceito pedido no enunciado|Onde está no código|
|-|-|
|Estados, ações e recompensas|`src/Environment.hs` (`State`, `Action`, `step`)|
|Tabela Q|`src/QTable.hs`|
|Função de valor V(s) = max Q(s,a)|`maxQ` em `src/QTable.hs`, exibida por `src/Render.hs`|
|Políticas (gulosa e epsilon-greedy)|`src/Policy.hs`|
|Atualização da tabela Q (Bellman)|`qUpdate` em `src/QLearning.hs`|
|Exploração x aproveitamento|`epsilonGreedy`, `epsilonAt` e a seção 7 da demo|
|Aleatoriedade reproduzível|`src/Rng.hs`|
|Avaliação do aprendizado|`src/Evaluation.hs`|
|Testes automatizados|`test/Spec.hs`|

## O ambiente

```
 S  .  .  .  .      S = início        G = objetivo (+10, termina)
 .  #  #  .  X      # = parede        X = armadilha (-10, termina)
 .  .  .  #  .      . = livre         cada passo = -1
 X  #  .  .  .
 .  .  .  #  G      ações: North, South, West, East
```

Parâmetros: `alpha = 0.1`, `gamma = 0.9`, epsilon de 1.0 a 0.05 (×0.99 por
episódio), 500 episódios de até 100 passos.

## Pré-requisitos

* **GHC** 9.x e **cabal** 3.x. A forma mais fácil de instalar é pelo
[GHCup](https://www.haskell.org/ghcup/).
* Só usa pacotes que já vêm com o GHC (`base` e `containers`).

## Como compilar e executar

```bash
cabal update                     # só na primeira vez
cabal build                      # compila
cabal run qlearning-demo         # roda a demonstração (semente 42)
cabal run qlearning-demo -- 7    # roda com outra semente aleatória
cabal test                       # roda os 28 testes automatizados
```

## O que a demonstração mostra

1. O ambiente: labirinto, ações, recompensas e hiperparâmetros.
2. **Passo a passo** de três atualizações da tabela Q com os números reais,
inclusive o valor do objetivo "voltando" para o estado anterior.
3. O **treinamento** em blocos de 50 episódios: epsilon, recompensa média, passos
médios e taxa de sucesso (de \~10% no início a \~100% no fim).
4. A **política aprendida** (setas), a **função de valor** V(s) e os valores Q
do estado inicial.
5. O **caminho** seguindo a política gulosa: 8 passos, o mais curto possível.
6. **Exploração x aproveitamento**: comparação entre epsilon = 0, 0.1, 0.5,
epsilon decrescente e um agente totalmente aleatório.

## Estrutura do projeto

```
qlearning-hs/
├── qlearning-hs.cabal   configuração do projeto (módulos e dependências)
├── app/Main.hs          programa de demonstração
├── src/
│   ├── Rng.hs           gerador de números pseudoaleatórios (LCG)
│   ├── Environment.hs   labirinto, estados, ações, recompensas e dinâmica
│   ├── QTable.hs        tabela Q, maxQ (função de valor) e bestAction
│   ├── Policy.hs        políticas gulosa, aleatória e epsilon-greedy
│   ├── QLearning.hs     regra de atualização, episódio e treinamento
│   ├── Evaluation.hs    caminho guloso, taxa de sucesso e médias
│   └── Render.hs        desenho da grade, política, valores e caminho
├── test/Spec.hs         testes automatizados
└── docs/                slides e documento explicativo
```

## Limitações (simplificações intencionais)

* **Q-Learning tabular**: uma entrada por par (estado, ação). Não escala para espaços
de estados enormes ou contínuos (para isso existem Linear Q-Learning e Deep Q-Learning).
* O ambiente é **determinístico** (a ação sempre faz o que diz).
* O agente vê o estado exato (ambiente **totalmente observável**).
* Os hiperparâmetros são fixos e escolhidos manualmente.

## Integrantes

|Integrante|Commits|
|-|-|
|Integrante 1 - Bruno|01, 05|
|Integrante 2 - Guilherme R|02, 06|
|Integrante 3 - Lucas|03, 08|
|Integrante 4 - Alexandre|04, 07|
|Integrante 5 - Artur|09, 15|
|Integrante 6 - Henrique|10, 16|
|Integrante 7 - Thiago|11, 14|
|Integrante 8 - Murilo|12, 13|
|Integrante 9 - Guilherme T|17, 18|



