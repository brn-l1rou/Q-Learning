# Breve descrição do exemplo prático

**Tema:** 3.2 — Q-Learning

O exemplo prático é uma implementação do **Q-Learning tabular** em Haskell. Um agente
aprende, por tentativa e erro, a sair do início de um labirinto 5×5 e chegar ao objetivo
desviando de paredes e armadilhas, sem conhecer o mapa de antemão.

## O problema resolvido: o labirinto

Imagine um robô colocado em um labirinto que ele nunca viu. Ele não tem o mapa, não sabe
onde ficam as paredes nem onde está a saída. A única coisa que recebe é um retorno numérico
depois de cada movimento: perde 1 ponto a cada passo, ganha 10 ao chegar à saída e perde 10
se cair em uma armadilha. A pergunta é: só com esse retorno, o robô consegue descobrir
sozinho o melhor caminho?

```
 S  .  .  .  .      S = início      G = saída (+10)
 .  #  #  .  X      # = parede      X = armadilha (−10)
 .  .  .  #  .      cada passo custa −1
 X  #  .  .  .
 .  .  .  #  G
```

O labirinto foi montado para não ter uma solução óbvia. O caminho pela linha de cima
parece curto, mas termina em uma armadilha ou em uma parede. A rota correta desce pela
esquerda, atravessa o meio e chega à saída em **8 passos**, passando ao lado de outra
armadilha. Encontrar esse caminho exige três coisas, que são justamente as dificuldades
centrais do aprendizado por reforço:

- **Recompensa atrasada:** o prêmio só aparece no fim. O agente precisa descobrir quais
  dos passos anteriores foram responsáveis por ele.
- **Exploração x aproveitamento:** é preciso testar caminhos desconhecidos sem deixar de
  usar o que já se aprendeu.
- **Caminho mais curto:** como cada passo custa −1, chegar não basta; é preciso chegar
  gastando o mínimo.

O Q-Learning resolve isso montando uma tabela com o valor de cada ação em cada uma das
20 posições, ajustada a cada movimento. No começo o agente anda praticamente ao acaso e
chega à saída em só 10% das tentativas. Ao longo de 500 tentativas, o valor da saída vai
"voltando" para as casas anteriores, até formar o caminho. No fim, o agente acerta perto
de 100% das vezes e segue exatamente a rota ótima de 8 passos. Os valores aprendidos
coincidem com os calculados à mão. Para comparação, um agente que só escolhe direções ao
acaso chega à saída em apenas 4% das vezes.

O labirinto é pequeno de propósito, para que todos os resultados possam ser conferidos à
mão. É a mesma estrutura de problemas reais de decisão em sequência: robôs de armazém
que aprendem rotas, personagens de jogos, controle de semáforos e roteamento em redes.
Nesses casos o agente também aprende com recompensas, sem receber um mapa pronto.

## Elementos do problema

- **Estados:** as 20 posições livres do labirinto (linha, coluna).
- **Ações:** North, South, West, East.
- **Recompensas:** +10 ao chegar no objetivo, −10 ao cair em uma armadilha e −1 por passo.
- **Política:** ε-greedy durante o treino (ε de 1.0 até 0.05) e gulosa depois do treino.
- **Função de valor:** V(s) = max_a Q(s,a), exibida para cada célula.
- **Atualização:** Q(s,a) ← Q(s,a) + α [r + γ max Q(s',·) − Q(s,a)], com α = 0.1 e γ = 0.9.

## O que a demonstração mostra

1. O ambiente, as recompensas e os hiperparâmetros.
2. Três atualizações da tabela Q calculadas passo a passo, com os números reais.
3. A evolução do treino (500 episódios): o sucesso sobe de ~10% para ~100%.
4. A política aprendida (setas) e a função de valor, que coincide com o valor ótimo
   calculado à mão ao longo do caminho.
5. O caminho final: 8 passos, o menor possível.
6. Uma comparação de estratégias de exploração (ε = 0, 0.1, 0.5, decrescente) e de
   um agente aleatório (~4% de sucesso).

## Como reproduzir

```bash
cabal update
cabal run qlearning-demo         # demonstração (semente 42)
cabal test                       # 28 testes automatizados
```

## Materiais nesta pasta

- `Apresentacao_QLearning_Haskell.pptx`: slides da apresentação.
- `Guia_do_Codigo_QLearning_Haskell.pdf`: explicação de cada trecho do código e
  perguntas prováveis da arguição.
