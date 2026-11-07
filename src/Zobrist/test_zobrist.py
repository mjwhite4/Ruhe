#!/usr/bin/env python3

import chess
import chess.polyglot

if __name__ == "__main__":

  board = chess.Board()
  print(board.legal_moves)

  board.set_fen('8/8/8/8/8/8/8/r7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/n7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/b7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/q7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/k7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/p7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))


  board.set_fen('8/8/8/8/8/8/8/R7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/N7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/B7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/Q7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/K7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/P7 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))
  

  board.set_fen('8/8/8/8/8/8/8/8 b K - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/8 b Q - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/8 b k - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('r3k3/8/8/8/8/8/8/8 b q - 0 1')
  print(board.fen())
  h1 = chess.polyglot.zobrist_hash(board)
  print(hex(h1))

  board.set_fen('r3k3/8/8/8/8/8/8/8 b - - 0 1')
  print(board.fen())
  h2 = chess.polyglot.zobrist_hash(board)
  print(hex(h2))
  print(hex(h1^h2))

  board.set_fen('8/8/8/8/8/8/8/8 b - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))

  board.set_fen('8/8/8/8/8/8/8/8 w - - 0 1')
  print(board.fen())
  h = chess.polyglot.zobrist_hash(board)
  print(hex(h))
  
