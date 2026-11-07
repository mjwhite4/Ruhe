#!/usr/bin/env python3

import chess
import chess.polyglot

# use python-chess to generate POLYGLOT hash keys for reference comparison

test_fens = [

    # STARTPOS
    "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",

    # white king on e1, black king at e8 white to move, then black to move
    "4k3/8/8/8/8/8/8/4K3 w - - 0 1", 
    "4k3/8/8/8/8/8/8/4K3 b - - 0 1", 

    # Position 3
    "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", 

    # En Passant square with valid capture available
    "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3",

    # En passant square with no opposing pawns to perform ep
    "8/8/8/8/3p4/8/8/8 w - a3 0 1",

    # Fine 70
    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P3R/R3K2R b KQkq - 0 1", 

    "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", 
    "r3k2r/p1pp1pb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1", 

    # kiwipete
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", 

    # Additional FENs harvested from PERFT
    "8/8/8/k7/3pP2R/8/8/3K4 b - e3 0 1", 
    "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1", 
    "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", 
    "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 1", 
    "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", 
]

# write FEN and POLYGLOT hash pairs as a Fortran include .INC file
with open("test/zobrist_vectors.inc", "w") as f:

    for fen in test_fens:
        board = chess.Board(fen)
        polyglot_hash = chess.polyglot.zobrist_hash(board)
        hash_str = f"{polyglot_hash:016X}"

        # output line below broken to neatly fit under 80 characters
        # Fortran accepts BOZ literal hex digits supplied to int(), along
        # with a data type, e.g. int(z"463B96181691FC9C", int64)

        #   use, intrinsic :: iso_fortran_env, only: int64
        #   integer, parameter :: ZHK = int64

        f.write(
            f"  call run_test( &\n"
            f'    "{fen}", &\n'
            f'    int(z"{hash_str}", ZHK))\n'
        )

