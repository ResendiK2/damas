# Jogo de Damas em Prolog

Este é um projeto de um jogo de damas desenvolvido em Prolog. Ele pode ser jogado no modo texto via terminal.

## Requisitos

- SWI-Prolog instalado ([Download SWI-Prolog](https://www.swi-prolog.org/download/stable))

## Como Jogar (Modo Texto)

1. Iniciar o Prolog:
   ```bash
   swipl jogo-damas.pl
    
2. Iniciar o jogo:
   ```prolog
   ?- play.

3. Movimentos
  
   - Movimentar uma peça:
      ```prolog
      mv(a3, b4).
   - Capturar uma peça:
      ```prolog
      cap(f4, [d6,b4]).

4. Para encerrar o jogo

   ```prolog
   halt.



