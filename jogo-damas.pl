:- use_module(library(random)).

% Mapa de coordenadas alfanumericas para indices de tabuleiro
coord_map([
    [a1, (8, 1)], [a2, (7, 1)], [a3, (6, 1)], [a4, (5, 1)], [a5, (4, 1)], [a6, (3, 1)], [a7, (2, 1)], [a8, (1, 1)],
    [b1, (8, 2)], [b2, (7, 2)], [b3, (6, 2)], [b4, (5, 2)], [b5, (4, 2)], [b6, (3, 2)], [b7, (2, 2)], [b8, (1, 2)],
    [c1, (8, 3)], [c2, (7, 3)], [c3, (6, 3)], [c4, (5, 3)], [c5, (4, 3)], [c6, (3, 3)], [c7, (2, 3)], [c8, (1, 3)],
    [d1, (8, 4)], [d2, (7, 4)], [d3, (6, 4)], [d4, (5, 4)], [d5, (4, 4)], [d6, (3, 4)], [d7, (2, 4)], [d8, (1, 4)],
    [e1, (8, 5)], [e2, (7, 5)], [e3, (6, 5)], [e4, (5, 5)], [e5, (4, 5)], [e6, (3, 5)], [e7, (2, 5)], [e8, (1, 5)],
    [f1, (8, 6)], [f2, (7, 6)], [f3, (6, 6)], [f4, (5, 6)], [f5, (4, 6)], [f6, (3, 6)], [f7, (2, 6)], [f8, (1, 6)],
    [g1, (8, 7)], [g2, (7, 7)], [g3, (6, 7)], [g4, (5, 7)], [g5, (4, 7)], [g6, (3, 7)], [g7, (2, 7)], [g8, (1, 7)],
    [h1, (8, 8)], [h2, (7, 8)], [h3, (6, 8)], [h4, (5, 8)], [h5, (4, 8)], [h6, (3, 8)], [h7, (2, 8)], [h8, (1, 8)]
]).

% Funcao para converter coordenadas alfanumericas para indices
coord_to_index(AlphaCoord, RowIdx, ColIdx) :-
    coord_map(Map),
    member([AlphaCoord, (RowIdx, ColIdx)], Map).  % Busca a coordenada no mapa

% Representacao do tabuleiro inicial 8x8 com pecas de A e B
initial_board([
    [b, 0, b, 0, b, 0, b, 0],  % b representa uma peca do jogador B
    [0, b, 0, b, 0, b, 0, b],
    [b, 0, b, 0, b, 0, b, 0],
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linhas vazias no meio
    [0, 0, 0, 0, 0, 0, 0, 0],
    [0, a, 0, a, 0, a, 0, a],  % a representa uma peca do jogador A
    [a, 0, a, 0, a, 0, a, 0],
    [0, a, 0, a, 0, a, 0, a]
]).

% Funcao principal que inicia o jogo
play :- 
    write('Escolha o modo de jogo:'), nl,
    write('1. Jogar contra o computador'), nl,
    write('2. Ver dois computadores jogando entre si'), nl,
    read(Escolha),
    (Escolha = 1 -> 
        % Modo Jogador vs Computador
        inicializar_jogo(jogador, computador)
    ; Escolha = 2 ->
        % Modo Computador vs Computador
        inicializar_jogo(computador, computador)
    ; 
        write('Escolha invalida! Tente novamente.'), nl,
        play
    ).

% Inicializa o jogo com os tipos de jogadores
inicializar_jogo(TipoJogadorA, TipoJogadorB) :-
    initial_board(Board),
    game_loop(Board, a, TipoJogadorA, TipoJogadorB).  % Jogador A comeca

% Funcao de loop do jogo: alterna entre os jogadores
game_loop(Board, Player, TipoJogadorA, TipoJogadorB) :-
    print_board(Board),  % Imprime o tabuleiro atual
    format('Vez do jogador: ~w~n', [Player]),  % Exibe de quem é a vez
    obter_jogada(Player, TipoJogadorA, TipoJogadorB, Move, Board),
    (execute_move(Move, Player, Board, NewBoard) ->  % Tenta executar o movimento
        next_player(Player, NextPlayer),
        game_loop(NewBoard, NextPlayer, TipoJogadorA, TipoJogadorB)   % Continua o jogo
    ;   write('Movimento invalido! Tente novamente.'), nl,  % Movimento inválido
        game_loop(Board, Player, TipoJogadorA, TipoJogadorB)).  % Continua com o mesmo jogador

% Alterna jogadores
next_player(a, b).
next_player(b, a).

% Decide como obter a jogada com base no tipo de jogador
obter_jogada(a, TipoJogadorA, _, Move, Board) :-
    (TipoJogadorA = jogador -> 
        % Jogador humano insere a jogada
        write('Insira sua jogada (formato: mv(a3, b4)): '),
        read(Move)
    ; 
        % Jogador computador gera a jogada
        escrever('Computador A está jogando...'),
        gerar_jogada_computador(a, Board, Move),
        format('Jogador A jogou: ~w~n', [Move])
    ).

obter_jogada(b, _, TipoJogadorB, Move, Board) :-
    (TipoJogadorB = jogador -> 
        % Jogador humano insere a jogada
        write('Insira sua jogada (formato: mv(a3, b4)): '),
        read(Move)
    ; 
        % Jogador computador gera a jogada
        escrever('Computador B está jogando...'),
        gerar_jogada_computador(b, Board, Move),
        format('Jogador B jogou: ~w~n', [Move])
    ).

% Função auxiliar para escrever mensagens sem quebrar a linha
escrever(Mensagem) :-
    write(Mensagem), nl.

% Gera uma jogada válida aleatória para o computador
gerar_jogada_computador(Player, Board, Move) :-
    encontrar_todas_jogadas_validas(Player, Board, TodasJogadas),
    length(TodasJogadas, N),
    N > 0,  % Assegura que há jogadas válidas
    random_between(1, N, Indice),
    nth1(Indice, TodasJogadas, Move).

% Encontra todas as jogadas válidas para um jogador
encontrar_todas_jogadas_validas(Player, Board, TodasJogadas) :-
    findall(
        mv(Coord1, Coord2),
        (
            coord_map(Map),
            member([Coord1, (Ci, Lj)], Map),
            nth1(Ci, Board, Row),
            nth1(Lj, Row, Piece),
            (Piece = Player ; Piece = 'A' ; Piece = 'B'),  % Peca do jogador ou dama
            member([Coord2, (Cf, Lf)], Map),
            valid_move(Ci, Lj, Cf, Lf, Player, Board)
        ),
        TodasJogadas
    ).

% Executa o movimento: mv(coord1, coord2)
execute_move(Move, Player, Board, NewBoard) :-
    (   Move = mv(Coord1, Coord2)  % Verifica se e um movimento normal
    ->  coord_to_index(Coord1, Ci, Lj),  % Converte coordenadas de origem usando o mapa
        coord_to_index(Coord2, Cf, Lf),  % Converte coordenadas de destino usando o mapa
        nth1(Ci, Board, OrigRow), nth1(Lj, OrigRow, Piece),  % Verifica peca na origem
        write('Peca na origem: '), write(Piece), nl,
        (valid_move(Ci, Lj, Cf, Lf, Player, Board) ->  % Verifica se o movimento e valido
            move(Piece, [Ci, Lj], [Cf, Lf], Board, TempBoard),
            promote([Cf, Lf], Piece, TempBoard, NewBoard)  % Verifica se deve promover
        ; write('Movimento invalido! Deve ser na diagonal permitida.'), nl, fail)
    ;   Move = cap(Coord1, Captures)  % Verifica se e um movimento de captura
    ->  execute_capture(cap(Coord1, Captures), Player, Board, NewBoard)  % Executa a captura
    ;   write('Movimento invalido! Tente novamente.'), nl, fail).

% Verifica se o movimento e valido para pecas normais e damas
valid_move(Ci, Lj, Cf, Lf, Player, Board) :-
    nth1(Ci, Board, Row), nth1(Lj, Row, Piece),
    (
        (Piece = Player; Piece = 'A'; Piece = 'B'), % Verifica se a peca e do jogador ou uma dama
        (
            (Piece = Player, valid_move_simple(Ci, Lj, Cf, Lf, Player, Board)) ;  % Movimento de peca simples
            (Piece = 'A', Player = a, valid_move_dama(Ci, Lj, Cf, Lf, Player, Board)) ;  % Movimento de dama A
            (Piece = 'B', Player = b, valid_move_dama(Ci, Lj, Cf, Lf, Player, Board))  % Movimento de dama B
        )
    ).

% Verifica se o movimento e valido para pecas simples
valid_move_simple(Ci, Lj, Cf, Lf, Player, Board) :-
    DifX is abs(Ci - Cf),
    DifY is abs(Lj - Lf),
    (
        % Movimento simples
        DifX =:= 1, DifY =:= 1,
        (Player = a -> Cf < Ci ; Cf > Ci),
        nth1(Cf, Board, NewRow), nth1(Lf, NewRow, Dest),
        Dest = 0
    ;
        % Movimento de captura
        DifX =:= 2, DifY =:= 2,
        MidRow is (Ci + Cf) // 2,
        MidCol is (Lj + Lf) // 2,
        nth1(MidRow, Board, MidRowList), nth1(MidCol, MidRowList, MidPiece),
        opponent(Player, MidPiece),
        nth1(Cf, Board, NewRow), nth1(Lf, NewRow, Dest),
        Dest = 0
    ).

% Valida se o movimento da dama e possivel
valid_move_dama(Ci, Lj, Cf, Lf, Player, Board) :-
    DifX is abs(Ci - Cf),
    DifY is abs(Lj - Lf),
    DifX =:= DifY,  % Movimento na diagonal
    path_clear(Ci, Lj, Cf, Lf, Board).  % Verifica se o caminho esta livre

% Verifica se o caminho esta livre para a dama
path_clear(Ci, Lj, Cf, Lf, Board) :-
    Di is sign(Cf - Ci),  % Direcao na linha
    Dj is sign(Lf - Lj),  % Direcao na coluna
    path_clear_step(Ci, Lj, Cf, Lf, Board, Di, Dj).  % Checa cada passo no caminho

% Checa se o caminho esta livre entre as posicoes
path_clear_step(Ci, Lj, Cf, Lf, _Board, _Di, _Dj) :-
    Ci = Cf, Lj = Lf, !.  % Chegou ao destino

path_clear_step(Ci, Lj, Cf, Lf, Board, Di, Dj) :-
    NewCi is Ci + Di,
    NewLj is Lj + Dj,
    nth1(NewCi, Board, Row),
    nth1(NewLj, Row, 0),  % Verifica se a casa esta vazia
    path_clear_step(NewCi, NewLj, Cf, Lf, Board, Di, Dj).  % Continua verificando

% Funcao para promover a peca se atingir a linha de promocao
promote([Cf, Lf], Piece, Board, NewBoard) :-
    (Piece = a, Cf =:= 1 -> replace_element(Cf, Lf, 'A', Board, NewBoard) ;  % 'a' vira 'A'
     Piece = b, Cf =:= 8 -> replace_element(Cf, Lf, 'B', Board, NewBoard) ;  % 'b' vira 'B'
     NewBoard = Board).  % Se nao for linha de promocao, mantem o tabuleiro inalterado.

% Movimento de uma peca simples (nao-dama)
move(Piece, [Ci, Lj], [Cf, Lf], Board, NewBoard) :-
    replace_element(Ci, Lj, 0, Board, TempBoard),  % Remove peca da posicao inicial
    replace_element(Cf, Lf, Piece, TempBoard, TempBoard2),  % Coloca a peca na posicao final
    promote([Cf, Lf], Piece, TempBoard2, NewBoard).  % Verifica se a peca deve ser promovida

% Substitui um elemento no tabuleiro
replace_element(RowIdx, ColIdx, Elem, Board, NewBoard) :-
    nth1(RowIdx, Board, OldRow),  % Seleciona a linha correta
    replace_in_list(ColIdx, Elem, OldRow, NewRow),  % Modifica a linha
    replace_in_list(RowIdx, NewRow, Board, NewBoard).  % Substitui a linha no tabuleiro

% Substitui um elemento em uma lista
replace_in_list(1, Elem, [_|T], [Elem|T]).
replace_in_list(N, Elem, [H|T], [H|R]) :- 
    N > 1, N1 is N - 1, 
    replace_in_list(N1, Elem, T, R).

% Funcao para imprimir o tabuleiro
print_board(Board) :- 
    print_board(Board, 8).

print_board([], 0) :- 
    write('  A B C D E F G H'), nl.  % Imprime as colunas no final
print_board([Row | Rest], N) :- 
    write(N), write(' '),  % Imprime o numero da linha
    print_row(Row), nl,
    N1 is N - 1,
    print_board(Rest, N1).

print_row([]).
print_row([Cell | Rest]) :- 
    (Cell = 0 -> write('. ') ; write(Cell), write(' ')), 
    print_row(Rest).

execute_capture(cap(Coord1, Captures), Player, Board, NewBoard) :-
    coord_to_index(Coord1, Ci, Lj),  % Converte coordenadas de origem usando o mapa
    coord_to_index(Coord2, Cf, Lf),  % Converte coordenadas da primeira captura
    capture(Player, [Ci, Lj], [Cf, Lf], Board, TempBoard),  % Realiza a primeira captura
    (Rest = Captures -> 
        (Rest = [] -> NewBoard = TempBoard 
        ; execute_capture(cap(Coord2, Rest), Player, TempBoard, NewBoard))
    ; 
        NewBoard = TempBoard).  % Continua capturando ou finaliza.
    
% Realiza a captura de uma peca simples ou dama
capture(Player, [Ci, Lj], [Cf, Lf], Board, NewBoard) :-
    MidRow is (Ci + Cf) // 2,  % Calcula a posicao intermediaria (onde a peca adversaria esta)
    MidCol is (Lj + Lf) // 2,  % Calcula a coluna intermediaria
    nth1(MidRow, Board, MidRowList), nth1(MidCol, MidRowList, OpponentPiece),  % Verifica se ha uma peca adversaria no meio
    opponent(Player, OpponentPiece),  % Verifica se a peca do meio pertence ao adversario
    path_clear_capture(Ci, Lj, Cf, Lf, Board, [MidRow, MidCol]), % Verifica se o caminho esta livre ate a captura
    replace_element(MidRow, MidCol, 0, Board, TempBoard1),  % Remove a peca adversaria
    replace_element(Ci, Lj, 0, TempBoard1, TempBoard2),  % Remove a peca da posicao original
    replace_element(Cf, Lf, Player, TempBoard2, NewBoard),  % Move a peca para a nova posicao
    write('Captura realizada!'), nl.

% Verifica se o caminho esta livre para a captura da dama
path_clear_capture(Ci, Lj, Cf, Lf, Board, [MidRow, MidCol]) :-
    Di is sign(Cf - Ci),  % Direcao na linha
    Dj is sign(Lf - Lj),  % Direcao na coluna
    path_clear_capture_step(Ci, Lj, MidRow, MidCol, Board, Di, Dj).

% Checa se o caminho esta livre entre as posicoes ate a captura
path_clear_capture_step(Ci, Lj, MidRow, MidCol, _Board, _Di, _Dj) :-
    Ci = MidRow, Lj = MidCol, !.  % Chegou a peca a ser capturada

path_clear_capture_step(Ci, Lj, MidRow, MidCol, Board, Di, Dj) :-
    NewCi is Ci + Di,
    NewLj is Lj + Dj,
    nth1(NewCi, Board, Row),
    nth1(NewLj, Row, 0),  % Verifica se a casa esta vazia
    path_clear_capture_step(NewCi, NewLj, MidRow, MidCol, Board, Di, Dj).

% Verifica o oponente
opponent(a, b).
opponent(b, a).

% Funcao para imprimir as colunas do tabuleiro
print_board_final :-
    write('  A B C D E F G H'), nl.

% Tabuleiro de teste com pecas prestes a virar damas
test_board([
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 1
    [0, 0, 0, 0, 0, a, 0, 0],  % Linha 2
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 3
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 4
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 5
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 6
    [0, 0, 0, 0, b, 0, 0, 0],  % Linha 7: Jogador A prestes a virar dama
    [0, 0, 0, 0, 0, 0, 0, 0]   % Linha 8
]).

% Funcao principal para iniciar com o tabuleiro de teste
play_test :- 
    test_board(Board), 
    game_loop(Board, a, a, b).

% Funcao para substituir o elemento no final
replace_last_element(List, Elem, NewList) :-
    append(Prefix, [_], List),
    append(Prefix, [Elem], NewList).
