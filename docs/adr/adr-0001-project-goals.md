# ADR-0001: Long-Term Project Goals and Scope for Ruhe

* **Status:** Proposed
* **Context:** Following the establishment of the 0.1 milestone (basic UCI interaction, legal move generation, and a minimal alpha-beta search), the project requires a formal long-term vision. The existing `TODO.md` focuses on immediate correctness, robustness, and regression testing. This ADR defines the strategic horizon, boundaries, and architectural principles for future evolution.

---

## 1. Core Mission Statement

**Ruhe** is a simple, highly verifiable platform designed for the testing and continuous improvement of advanced search strategies in computer chess. The project marries clean, modern Fortran architecture (such as Fortran 2018 debug and release patterns, explicit invariants, and rigorous test suites) with classic algorithmic literature and an empirical, metrics-driven development loop.

---

## 2. Primary Goals & Capabilities (The Ruhe Horizon)

* **Advanced Search Strategy Exploration:**
* Focus on implementing and benchmarking sophisticated search enhancements, drawing heavily on foundational literature such as Erik Heinz's *Scalable Search in Computer Chess* (Vieweg, 2000).
* Mature the search stack from the current baseline to include Quiescence Search, Principal Variation Search (PVS), transposition tables with proper bound/mate handling, and move ordering heuristics (history heuristic, hash-move priority).


* **Empirical Rigor & Iterative Testing Workflow:**
* Adopt a systematic approach to engineering inspired by iterative testing and detailed results logging (akin to the coding challenges documented by Sebastian Lague).
* Prioritize automated process-level integration testing, structured perft/search log tracking, and reproducible strength evaluations over guesswork.


* **External Integrations & Auxiliary Features:**
* **Polyglot Hashing & Opening Books:** Implement Polyglot-compatible Zobrist hashing and Zobrist provenance strictly to support a read-only opening book feature. (Opening book *development* and self-adaptation are out of scope).
* **Syzygy Tablebases:** Integrate support for Syzygy endgame tablebases (consuming pre-generated tablebases rather than generating them).
* **Basic NNUE Integration:** Explore basic Efficiently Updatable Neural Network (NNUE) evaluation. Modern Fortran array architectures offer clean syntax and performance characteristics comparable to Python/C++ implementations, though hardware deployment remains strictly CPU-bound (no GPU integration planned).



---

## 3. Secondary Projects & Boundaries

To maintain focus, certain ideas are explicitly decoupled or deferred:

* **Opening Book Authoring & Adaptation:** Building custom opening book compilation tools or dynamic, self-adapting book learning belongs to a separate project domain. Ruhe will consume standard, read-only formats.
* **Multi-Threaded Parallel Search ("Ruhe-SMP"):** True multi-threading (e.g., OpenMP-based search parallelization) introduces race conditions and synchronization complexities that alter the deterministic, single-threaded debugging paradigm. If pursued, it will live in a separate architectural branch or project.

---
