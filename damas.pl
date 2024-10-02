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
    print_board(Board),
    (player_has_moves(Player, Board) ->  % Verifica se o jogador atual tem jogadas válidas
        format('Vez do jogador: ~w~n', [Player]),  % Exibe de quem é a vez
        obter_jogada(Player, TipoJogadorA, TipoJogadorB, Move, Board),
        (execute_move(Move, Player, Board, NewBoard) ->  % Tenta executar o movimento
            next_player(Player, NextPlayer),
            game_loop(NewBoard, NextPlayer, TipoJogadorA, TipoJogadorB)  % Continua o jogo
        ;   write('Movimento inválido! Tente novamente.'), nl,  % Movimento inválido
            game_loop(Board, Player, TipoJogadorA, TipoJogadorB))  % Continua com o mesmo jogador
    ;   % Caso não haja movimentos, o outro jogador vence
        write('O jogador '), (Player = a -> write('b'); write('a')), write(' venceu!'), nl
    ).

% Alterna jogadores
next_player(a, b).
next_player(b, a).

% Decide como obter a jogada com base no tipo de jogador
obter_jogada(a, TipoJogadorA, _, Move, Board) :-
    (TipoJogadorA = jogador -> 
        % Jogador humano insere a jogada
        write('Insira sua jogada (formato: mv(a3, b4)) ou cap(a3, [b5, ..., b4]): '),
        read(Move)
    ; 
        % Jogador computador gera a jogada
        write('Computador A esta jogando...'), nl,
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
        write('Computador B esta jogando...'), nl,
        gerar_jogada_computador(b, Board, Move),
        format('Jogador B jogou: ~w~n', [Move])
    ).

% Gera uma jogada valida aleatória para o computador
% Gera uma jogada valida aleatória para o computador
gerar_jogada_computador(Player, Board, Move) :-
    encontrar_todas_capturas_validas(Player, Board, TodasCapturas),
    (
        TodasCapturas \= [] ->  % Se existirem capturas válidas, escolher uma captura
        random_member(Move, TodasCapturas)
    ;
        encontrar_todas_jogadas_validas(Player, Board, TodasJogadas),
        % Filtra jogadas onde a origem e destino são diferentes
        exclude(mesma_casa, TodasJogadas, JogadasValidas),
        random_member(Move, JogadasValidas)
    ).

% Verifica se a jogada move a peça para a mesma casa
mesma_casa(mv(Coord1, Coord2)) :-
    Coord1 = Coord2.


% Encontra todas as jogadas validas para um jogador
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

encontrar_todas_capturas_validas(Player, Board, TodasCapturas) :-
    findall(
        cap(Coord1, Captures),
        (
            coord_map(Map),
            member([Coord1, (Ci, Lj)], Map),
            nth1(Ci, Board, Row),
            nth1(Lj, Row, Piece),
            (Piece = Player ; Piece = 'A' ; Piece = 'B'),  % Peça do jogador ou dama
            findall(Capture,
                (
                    member([_Coord2, (Cf, Lf)], Map),
                    valid_capture(Ci, Lj, Cf, Lf, Player, Board, Capture)
                ),
                CapturesList),
            CapturesList \= [],
            flatten(CapturesList, Captures)
        ),
        TodasCapturas
    ).

% Executa o movimento: mv(coord1, coord2)
execute_move(Move, Player, Board, NewBoard) :-
    (   Move = mv(Coord1, Coord2)
    ->  coord_to_index(Coord1, Ci, Lj),
        coord_to_index(Coord2, Cf, Lf),
        nth1(Ci, Board, OrigRow), nth1(Lj, OrigRow, Piece),
        (valid_move(Ci, Lj, Cf, Lf, Player, Board) ->
            move(Piece, [Ci, Lj], [Cf, Lf], Board, TempBoard),
            promote([Cf, Lf], Piece, TempBoard, NewBoard)
        ;   write('Movimento inválido! Deve ser na diagonal permitida.'), nl, fail)
    ;   Move = cap(Coord1, Captures)
    ->  execute_capture(cap(Coord1, Captures), Player, Board, NewBoard)
    ;   write('Captura inválida! Tente novamente.'), nl, fail).

% Verifica se o movimento e valido para pecas normais e damas
% Verifica se o movimento é válido para peças normais e damas
valid_move(Ci, Lj, Cf, Lf, Player, Board) :-
    nth1(Ci, Board, Row), nth1(Lj, Row, Piece),
    % Verifica se a peça pertence ao jogador atual
    ((Player = a, (Piece = a ; Piece = 'A')) ;  % Jogador A pode mover 'a' e 'A'
     (Player = b, (Piece = b ; Piece = 'B'))),  % Jogador B pode mover 'b' e 'B'
    (
        (Piece = Player, valid_move_simple(Ci, Lj, Cf, Lf, Player, Board)) ;  % Movimento de peça simples
        (Piece = 'A', Player = a, valid_move_dama(Ci, Lj, Cf, Lf, Board)) ;   % Movimento de dama A
        (Piece = 'B', Player = b, valid_move_dama(Ci, Lj, Cf, Lf, Board))     % Movimento de dama B
    ).


% Verifica se o movimento e valido para pecas simples
% Verifica se o movimento é válido para peças simples (com captura)
valid_move_simple(Ci, Lj, Cf, Lf, Player, Board) :-
    DifX is abs(Ci - Cf),
    DifY is abs(Lj - Lf),
    (
        % Movimento simples (não capturando)
        DifX =:= 1, DifY =:= 1,
        (Player = a -> Cf < Ci ; Cf > Ci),
        nth1(Cf, Board, NewRow), nth1(Lf, NewRow, Dest),
        Dest = 0  % O destino precisa estar vazio
    ;
        % Movimento de captura
        DifX =:= 2, DifY =:= 2,
        MidRow is (Ci + Cf) // 2,
        MidCol is (Lj + Lf) // 2,
        nth1(MidRow, Board, MidRowList), nth1(MidCol, MidRowList, MidPiece),
        opponent(Player, MidPiece),  % Verifica se a peça do meio pertence ao oponente
        nth1(Cf, Board, NewRow), nth1(Lf, NewRow, Dest),
        Dest = 0  % O destino deve estar vazio para capturar
    ).

% Valida se o movimento da dama e possivel
valid_move_dama(Ci, Lj, Cf, Lf, Board) :-
    DifX is abs(Ci - Cf),
    DifY is abs(Lj - Lf),
    DifX =:= DifY,  % Movimento na diagonal
    path_clear(Ci, Lj, Cf, Lf, Board).  % Verifica se o caminho está livre

% Verifica se o caminho esta livre para a dama
path_clear(Ci, Lj, Cf, Lf, Board) :-
    Di is sign(Cf - Ci),  % Direção na linha
    Dj is sign(Lf - Lj),  % Direção na coluna
    path_clear_step(Ci, Lj, Cf, Lf, Board, Di, Dj).  % Checa cada passo no caminho

% Checa se o caminho esta livre entre as posicoes
path_clear_step(Ci, Lj, Cf, Lf, _Board, _Di, _Dj) :-
    Ci = Cf, Lj = Lf, !.  % Chegou ao destino

path_clear_step(Ci, Lj, Cf, Lf, Board, Di, Dj) :-
    NewCi is Ci + Di,
    NewLj is Lj + Dj,
    nth1(NewCi, Board, Row),
    nth1(NewLj, Row, 0),  % Verifica se a casa está vazia
    path_clear_step(NewCi, NewLj, Cf, Lf, Board, Di, Dj).  % Continua verificando

% Funcao para promover a peca se atingir a linha de promocao
promote([Cf, Lf], Piece, Board, NewBoard) :-
    (Piece = a, Cf =:= 1 -> 
        replace_element(Cf, Lf, 'A', Board, NewBoard),  % 'a' vira 'A'
        write('Peça A promovida a dama!'), nl
    ; Piece = b, Cf =:= 8 -> 
        replace_element(Cf, Lf, 'B', Board, NewBoard),  % 'b' vira 'B'
        write('Peça B promovida a dama!'), nl
    ; NewBoard = Board).  % Se nao for linha de promocao, mantem o tabuleiro inalterado.


% Movimento de uma peca simples (nao-dama)
move(Piece, [Ci, Lj], [Cf, Lf], Board, NewBoard) :-
    replace_element(Ci, Lj, 0, Board, TempBoard),  % Remove peça da posição inicial
    replace_element(Cf, Lf, Piece, TempBoard, TempBoard2),  % Coloca a peça na posição final
    promote([Cf, Lf], Piece, TempBoard2, NewBoard).  % Verifica se a peça deve ser promovida

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

% Função para executar a captura
execute_capture(cap(Coord1, [Coord2 | Rest]), Player, Board, NewBoard) :-
    write('Executando captura...'), nl,
    coord_to_index(Coord1, Ci, Lj),
    coord_to_index(Coord2, Cf, Lf),
    capture(Player, [Ci, Lj], [Cf, Lf], Board, TempBoard),  % Executa a captura inicial
    (Rest \= [] ->  % Se houver mais capturas na lista
        validate_and_execute_next_capture(cap(Coord2, Rest), Player, TempBoard, NewBoard)
    ;
        % Verifica se há mais capturas disponíveis após a primeira
        encontrar_todas_capturas_validas(Player, TempBoard, MaisCapturas),
        (MaisCapturas \= [] ->
            format('Capturas adicionais disponíveis. Jogador ~w deve continuar capturando.', [Player]), nl,
            random_member(ProximaCaptura, MaisCapturas),
            execute_capture(ProximaCaptura, Player, TempBoard, NewBoard)
        ;
            NewBoard = TempBoard)
    ).

validate_and_execute_next_capture(cap(Coord2, Rest), Player, TempBoard, NewBoard) :-
    coord_to_index(Coord2, Ci, Lj),
    (   Rest = [NextCoord | _],  % Há mais capturas
        coord_to_index(NextCoord, Cf, Lf),
        valid_capture(Ci, Lj, Cf, Lf, Player, TempBoard, _) ->
        execute_capture(cap(Coord2, Rest), Player, TempBoard, NewBoard)
    ;   NewBoard = TempBoard).

% Função para capturar peças
% Função para capturar peças, garantindo que a peça capturadora mantenha seu tipo (dama ou peça simples)
capture(Player, [Ci, Lj], [Cf, Lf], Board, NewBoard) :-
    MidRow is (Ci + Cf) // 2,
    MidCol is (Lj + Lf) // 2,
    nth1(MidRow, Board, MidRowList), nth1(MidCol, MidRowList, OpponentPiece),
    opponent(Player, OpponentPiece),  % Verifica se a peça do meio pertence ao oponente
    replace_element(MidRow, MidCol, 0, Board, TempBoard1),  % Remove a peça capturada
    nth1(Ci, Board, OrigRow), nth1(Lj, OrigRow, Piece),  % Identifica a peça original (dama ou peça simples)
    replace_element(Ci, Lj, 0, TempBoard1, TempBoard2),  % Remove a peça da posição original
    replace_element(Cf, Lf, Piece, TempBoard2, NewBoard),  % Move a peça para a posição de captura, mantendo seu tipo
    write('Captura realizada!'), nl.



% Verifica o oponente
opponent(a, b).
opponent(b, a).

% Verifica o caminho para capturas de damas
path_clear_capture(Ci, Lj, Cf, Lf, Board, [MidRow, MidCol]) :-
    Di is sign(Cf - Ci),  % Direção na linha
    Dj is sign(Lf - Lj),  % Direção na coluna
    path_clear_capture_step(Ci, Lj, MidRow, MidCol, Board, Di, Dj).

% Verifica se o caminho está livre até a captura
path_clear_capture_step(Ci, Lj, MidRow, MidCol, _Board, _Di, _Dj) :-
    Ci = MidRow, Lj = MidCol, !.

path_clear_capture_step(Ci, Lj, MidRow, MidCol, Board, Di, Dj) :-
    NewCi is Ci + Di,
    NewLj is Lj + Dj,
    nth1(NewCi, Board, Row),
    nth1(NewLj, Row, 0),  % Verifica se a casa está vazia
    path_clear_capture_step(NewCi, NewLj, MidRow, MidCol, Board, Di, Dj).

player_has_moves(Player, Board) :-
    encontrar_todas_jogadas_validas(Player, Board, TodasJogadas),
    TodasJogadas \= [].  % Verifica se há jogadas disponíveis

% Verifica se a captura de uma dama é possível
valid_capture_dama(Ci, Lj, Cf, Lf, Board, [Coord2]) :-
    DifX is abs(Ci - Cf),
    DifY is abs(Lj - Lf),
    DifX =:= DifY,  % Movimento na diagonal
    path_clear_capture(Ci, Lj, Cf, Lf, Board, [MidRow, MidCol]),  % Verifica o caminho
    nth1(MidRow, Board, MidRowList), nth1(MidCol, MidRowList, MidPiece),
    opponent_dama(MidPiece),  % Verifica se a peça do meio é uma dama adversária
    coord_map(Map),
    member([Coord2, (Cf, Lf)], Map),
    nth1(Cf, Board, NewRow), nth1(Lf, NewRow, 0).  % O destino deve estar vazio

% Verifica se a peça é uma dama adversária
opponent_dama('A') :- write('Capturando dama A'), nl.
opponent_dama('B') :- write('Capturando dama B'), nl.


valid_capture(Ci, Lj, Cf, Lf, Player, Board, [Coord2]) :-
    DifX is abs(Ci - Cf),
    DifY is abs(Lj - Lf),
    DifX =:= 2,
    DifY =:= 2,
    MidRow is (Ci + Cf) // 2,
    MidCol is (Lj + Lf) // 2,
    nth1(MidRow, Board, MidRowList), nth1(MidCol, MidRowList, MidPiece),
    opponent(Player, MidPiece),
    coord_map(Map),
    member([Coord2, (Cf, Lf)], Map),
    nth1(Cf, Board, NewRow), nth1(Lf, NewRow, 0).  % Destino deve estar vazio

test_promotion_board([
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 8
    [0, a, 0, 0, 0, 0, 0, 0],  % Linha 7 (a prestes a virar dama)
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 6
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 5
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 4
    [0, 0, 0, 0, 0, 0, 0, 0],  % Linha 3
    [0, 0, 0, 0, b, 0, 0, 0],  % Linha 2 (b prestes a virar dama)
    [0, 0, 0, 0, 0, 0, 0, 0]   % Linha 1
]).


% Inicializa o jogo com um tabuleiro de teste
inicializar_jogo_test(TipoJogadorA, TipoJogadorB) :-
    test_promotion_board(Board),  % Usa o tabuleiro de teste
    game_loop(Board, a, TipoJogadorA, TipoJogadorB).  % Jogador A começa
