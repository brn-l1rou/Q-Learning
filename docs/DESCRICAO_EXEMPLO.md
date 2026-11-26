# Breve descrição do exemplo prático

**Tema:** 3.2 — Q-Learning

O exemplo prático é uma implementação do **Q-Learning tabular** em Haskell. Um agente
aprende, por tentativa e erro, a sair do início de um labirinto 5×5 e chegar ao objetivo
desviando de paredes e armadilhas, sem conhecer o mapa de antemão.

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

