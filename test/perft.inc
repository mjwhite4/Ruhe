! Perft 1: Standard Starting Position
call assert_perft( &
  "test_perft_startpos:", &
  "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1", &
  4, 197281_int64, breakdown_opt=.false.)

! Perft 2: Kiwipete Position
call assert_perft( &
  "test_perft_kiwipete:", &
  "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1", &
  4, 4085603_int64, breakdown_opt=.false.)

! Perft 3: Position 3 (Rook Attacks Castling Transit)
call assert_perft( &
  "test_perft_rook_attack:", &
  "r3k2r/p1pp1pb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R b KQkq - 0 1", &
  4, 3517436_int64, breakdown_opt=.false.)

! Perft 4: Position 4 (Promotions and Castling)
call assert_perft( &
  "test_perft_pos4:", &
  "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1", &
  5, 15833292_int64, breakdown_opt=.false.)

! Perft 5: Position 5
call assert_perft( &
  "test_perft_pos5:", &
  "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8", &
  4, 2103487_int64, breakdown_opt=.false.)

! Perft 6: Position 6
call assert_perft( &
  "test_perft_pos6:", &
  "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 1", &
  4, 3894594_int64, breakdown_opt=.false.)

! Perft 7: Simultaneous Check & Promotion
call assert_perft( &
  "test_perft_simul_check:", &
  "n1n5/PPPk4/8/8/8/8/4Kppp/5N1N b - - 0 1", &
  5, 3605103_int64, breakdown_opt=.false.)

! Perft 8: Rook Captures / Castling Rights
call assert_perft( &
  "test_perft_rook_cap:", &
  "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1", &
  5, 7594526_int64, breakdown_opt=.false.)

! Perft 9: En Passant Discovered Check
call assert_perft( &
  "test_perft_ep_disc:", &
  "8/8/8/k7/3pP2R/8/8/3K4 b - e3 0 1", &
  6, 1321438_int64, breakdown_opt=.false.)

! Perft 10: En Passant and Check
call assert_perft( &
  "test_perft_ep_check:", &
  "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1", &
  5, 674624_int64, breakdown_opt=.false.)
