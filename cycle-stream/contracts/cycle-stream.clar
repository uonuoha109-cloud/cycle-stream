;; CycleStream Protocol - Temporal Liquidity Mining via Dynamic Cycle-Based AMM

;; Core Features:
;;   - Adaptive Cycle Pools (high-yield farming and stability phases)
;;   - Cross-Cycle Yield Amplification (compounding rewards)
;;   - CYCS governance token integration
;;   - Impermanent loss mitigation via cycle-based rebalancing
;;   - Fee collection on cycle transitions

;; ============================================================
;; CONSTANTS
;; ============================================================

(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POOL-NOT-FOUND (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-INVALID-AMOUNT (err u103))
(define-constant ERR-CYCLE-NOT-ENDED (err u104))
(define-constant ERR-ALREADY-INITIALIZED (err u105))
(define-constant ERR-POOL-PAUSED (err u106))
(define-constant ERR-ZERO-SUPPLY (err u107))

;; Cycle duration bounds in blocks (roughly 7-14 days at ~10 min/block)
(define-constant MIN-CYCLE-BLOCKS u1008)   ;; ~7 days
(define-constant MAX-CYCLE-BLOCKS u2016)   ;; ~14 days
(define-constant DEFAULT-CYCLE-BLOCKS u1008)

;; Fee rates in basis points (1 bp = 0.01%)
(define-constant TRANSITION-FEE-BPS u30)   ;; 0.30% on cycle transitions
(define-constant YIELD-FEE-BPS u10)        ;; 0.10% protocol yield fee
(define-constant BPS-DENOM u10000)

;; Pool phases
(define-constant PHASE-FARMING u0)
(define-constant PHASE-STABILITY u1)

;; ============================================================
;; FUNGIBLE TOKEN - CYCS Governance Token
;; ============================================================

(define-fungible-token cycs-token)

;; ============================================================
;; DATA MAPS AND VARS
;; ============================================================

;; Global protocol state
(define-data-var protocol-paused bool false)
(define-data-var total-pools uint u0)
(define-data-var treasury-fees uint u0)   ;; accumulated protocol fees in microSTX

;; Pool registry
;; pool-id -> pool metadata
(define-map pools
  { pool-id: uint }
  {
    token-x-reserve: uint,
    token-y-reserve: uint,
    total-lp-shares: uint,
    current-phase: uint,       ;; 0 = farming, 1 = stability
    cycle-start-block: uint,
    cycle-duration: uint,      ;; in blocks
    accumulated-fees: uint,
    is-active: bool,
    cycle-count: uint
  }
)

;; LP positions per user per pool
(define-map lp-positions
  { pool-id: uint, user: principal }
  {
    lp-shares: uint,
    entry-cycle: uint,
    pending-rewards: uint
  }
)

;; Per-cycle yield snapshots for cross-cycle amplification
(define-map cycle-snapshots
  { pool-id: uint, cycle-number: uint }
  {
    end-block: uint,
    total-fees-collected: uint,
    total-lp-shares: uint,
    phase: uint,
    yield-per-share: uint   ;; scaled by 1e6
  }
)

;; CYCS governance votes on pool parameters
(define-map governance-proposals
  { proposal-id: uint }
  {
    pool-id: uint,
    proposed-cycle-duration: uint,
    votes-for: uint,
    votes-against: uint,
    end-block: uint,
    executed: bool
  }
)

(define-data-var proposal-nonce uint u0)

;; ============================================================
;; READ-ONLY FUNCTIONS
;; ============================================================

(define-read-only (get-pool (pool-id uint))
  (map-get? pools { pool-id: pool-id })
)

(define-read-only (get-lp-position (pool-id uint) (user principal))
  (map-get? lp-positions { pool-id: pool-id, user: user })
)

(define-read-only (get-cycle-snapshot (pool-id uint) (cycle-number uint))
  (map-get? cycle-snapshots { pool-id: pool-id, cycle-number: cycle-number })
)

(define-read-only (get-cycs-balance (user principal))
  (ft-get-balance cycs-token user)
)

(define-read-only (get-total-cycs-supply)
  (ft-get-supply cycs-token)
)

(define-read-only (get-protocol-stats)
  {
    total-pools: (var-get total-pools),
    treasury-fees: (var-get treasury-fees),
    paused: (var-get protocol-paused)
  }
)

;; Check whether the current cycle has ended for a given pool
(define-read-only (is-cycle-ended (pool-id uint))
  (match (map-get? pools { pool-id: pool-id })
    pool (>= block-height
               (+ (get cycle-start-block pool)
                  (get cycle-duration pool)))
    false
  )
)

;; Compute current spot price of token-x in terms of token-y
(define-read-only (get-spot-price (pool-id uint))
  (match (map-get? pools { pool-id: pool-id })
    pool (if (> (get token-x-reserve pool) u0)
            (ok (/ (* (get token-y-reserve pool) u1000000)
                   (get token-x-reserve pool)))
            ERR-ZERO-SUPPLY)
    ERR-POOL-NOT-FOUND
  )
)

;; Estimate output amount for a given input using constant-product formula
(define-read-only (get-swap-output (pool-id uint) (amount-in uint) (swap-x-to-y bool))
  (match (map-get? pools { pool-id: pool-id })
    pool
      (let (
        (reserve-in  (if swap-x-to-y
                       (get token-x-reserve pool)
                       (get token-y-reserve pool)))
        (reserve-out (if swap-x-to-y
                       (get token-y-reserve pool)
                       (get token-x-reserve pool)))
        ;; Apply 0.30% swap fee
        (amount-in-with-fee (/ (* amount-in (- BPS-DENOM TRANSITION-FEE-BPS))
                               BPS-DENOM))
      )
      (if (> reserve-in u0)
        (ok (/ (* amount-in-with-fee reserve-out)
               (+ reserve-in amount-in-with-fee)))
        ERR-ZERO-SUPPLY))
    ERR-POOL-NOT-FOUND
  )
)

;; Calculate pending cross-cycle amplified rewards for a user
(define-read-only (get-pending-rewards (pool-id uint) (user principal))
  (match (map-get? lp-positions { pool-id: pool-id, user: user })
    position
      (match (map-get? pools { pool-id: pool-id })
        pool
          (let (
            (base-rewards (get pending-rewards position))
            ;; Amplification: 10% bonus per additional cycle held beyond entry
            (cycles-held (- (get cycle-count pool) (get entry-cycle position)))
            (amplification (+ u1000000 (* cycles-held u100000)))  ;; scaled 1e6
            (amplified (/ (* base-rewards amplification) u1000000))
          )
          (ok amplified))
        ERR-POOL-NOT-FOUND)
    (ok u0)
  )
)

;; ============================================================
;; INTERNAL HELPERS
;; ============================================================

(define-private (assert-owner)
  (if (is-eq tx-sender CONTRACT-OWNER)
    (ok true)
    ERR-NOT-AUTHORIZED)
)

(define-private (assert-not-paused)
  (if (var-get protocol-paused)
    ERR-POOL-PAUSED
    (ok true))
)

;; Compute LP shares to mint for a deposit of token-x and token-y
(define-private (compute-lp-shares
    (token-x-amount uint)
    (pool-x-reserve uint)
    (total-lp-shares uint))
  (if (is-eq total-lp-shares u0)
    ;; Initial liquidity - use geometric mean approximation
    token-x-amount
    ;; Proportional shares
    (/ (* token-x-amount total-lp-shares) pool-x-reserve)
  )
)

;; ============================================================
;; POOL MANAGEMENT
;; ============================================================

;; Initialize a new liquidity pool
(define-public (create-pool
    (initial-x uint)
    (initial-y uint)
    (cycle-duration uint))
  (begin
    (try! (assert-not-paused))
    (asserts! (>= initial-x u0) ERR-INVALID-AMOUNT)
    (asserts! (>= initial-y u0) ERR-INVALID-AMOUNT)
    (asserts! (and (>= cycle-duration MIN-CYCLE-BLOCKS)
                   (<= cycle-duration MAX-CYCLE-BLOCKS))
              ERR-INVALID-AMOUNT)
    (let (
      (pool-id (+ (var-get total-pools) u1))
      (initial-shares (compute-lp-shares initial-x u0 u0))
    )
    (map-set pools { pool-id: pool-id }
      {
        token-x-reserve: initial-x,
        token-y-reserve: initial-y,
        total-lp-shares: initial-shares,
        current-phase: PHASE-FARMING,
        cycle-start-block: block-height,
        cycle-duration: cycle-duration,
        accumulated-fees: u0,
        is-active: true,
        cycle-count: u0
      })
    (map-set lp-positions { pool-id: pool-id, user: tx-sender }
      {
        lp-shares: initial-shares,
        entry-cycle: u0,
        pending-rewards: u0
      })
    (var-set total-pools pool-id)
    (ok pool-id))
  )
)

;; ============================================================
;; LIQUIDITY PROVISION
;; ============================================================

;; Add liquidity to an existing pool
(define-public (add-liquidity
    (pool-id uint)
    (token-x-amount uint)
    (token-y-amount uint))
  (begin
    (try! (assert-not-paused))
    (asserts! (> token-x-amount u0) ERR-INVALID-AMOUNT)
    (match (map-get? pools { pool-id: pool-id })
      pool
        (begin
          (asserts! (get is-active pool) ERR-POOL-PAUSED)
          (let (
            (new-shares (compute-lp-shares
                          token-x-amount
                          (get token-x-reserve pool)
                          (get total-lp-shares pool)))
            (existing-position
              (default-to
                { lp-shares: u0, entry-cycle: (get cycle-count pool), pending-rewards: u0 }
                (map-get? lp-positions { pool-id: pool-id, user: tx-sender })))
          )
          ;; Update pool reserves
          (map-set pools { pool-id: pool-id }
            (merge pool {
              token-x-reserve: (+ (get token-x-reserve pool) token-x-amount),
              token-y-reserve: (+ (get token-y-reserve pool) token-y-amount),
              total-lp-shares: (+ (get total-lp-shares pool) new-shares)
            }))
          ;; Update LP position
          (map-set lp-positions { pool-id: pool-id, user: tx-sender }
            (merge existing-position {
              lp-shares: (+ (get lp-shares existing-position) new-shares)
            }))
          (ok new-shares)))
      ERR-POOL-NOT-FOUND
    )
  )
)

;; Remove liquidity from a pool
(define-public (remove-liquidity
    (pool-id uint)
    (lp-shares-to-burn uint))
  (begin
    (try! (assert-not-paused))
    (asserts! (> lp-shares-to-burn u0) ERR-INVALID-AMOUNT)
    (match (map-get? pools { pool-id: pool-id })
      pool
        (match (map-get? lp-positions { pool-id: pool-id, user: tx-sender })
          position
            (begin
              (asserts! (>= (get lp-shares position) lp-shares-to-burn)
                        ERR-INSUFFICIENT-BALANCE)
              (asserts! (> (get total-lp-shares pool) u0) ERR-ZERO-SUPPLY)
              (let (
                (total-shares (get total-lp-shares pool))
                (token-x-out  (/ (* lp-shares-to-burn (get token-x-reserve pool))
                                 total-shares))
                (token-y-out  (/ (* lp-shares-to-burn (get token-y-reserve pool))
                                 total-shares))
              )
              (map-set pools { pool-id: pool-id }
                (merge pool {
                  token-x-reserve: (- (get token-x-reserve pool) token-x-out),
                  token-y-reserve: (- (get token-y-reserve pool) token-y-out),
                  total-lp-shares: (- total-shares lp-shares-to-burn)
                }))
              (map-set lp-positions { pool-id: pool-id, user: tx-sender }
                (merge position {
                  lp-shares: (- (get lp-shares position) lp-shares-to-burn),
                  pending-rewards: u0
                }))
              (ok { token-x-out: token-x-out, token-y-out: token-y-out })))
          ERR-INSUFFICIENT-BALANCE)
      ERR-POOL-NOT-FOUND
    )
  )
)

;; ============================================================
;; SWAP
;; ============================================================

;; Execute a swap within a pool
(define-public (swap
    (pool-id uint)
    (amount-in uint)
    (min-amount-out uint)
    (swap-x-to-y bool))
  (begin
    (try! (assert-not-paused))
    (asserts! (> amount-in u0) ERR-INVALID-AMOUNT)
    (match (map-get? pools { pool-id: pool-id })
      pool
        (begin
          (asserts! (get is-active pool) ERR-POOL-PAUSED)
          (let (
            (reserve-in  (if swap-x-to-y
                           (get token-x-reserve pool)
                           (get token-y-reserve pool)))
            (reserve-out (if swap-x-to-y
                           (get token-y-reserve pool)
                           (get token-x-reserve pool)))
            (fee-amount  (/ (* amount-in TRANSITION-FEE-BPS) BPS-DENOM))
            (amount-in-net (- amount-in fee-amount))
            (amount-out  (/ (* amount-in-net reserve-out)
                            (+ reserve-in amount-in-net)))
          )
          (asserts! (>= amount-out min-amount-out) ERR-INVALID-AMOUNT)
          ;; Update pool with new reserves and accumulated fees
          (map-set pools { pool-id: pool-id }
            (merge pool {
              token-x-reserve: (if swap-x-to-y
                                 (+ (get token-x-reserve pool) amount-in)
                                 (- (get token-x-reserve pool) amount-out)),
              token-y-reserve: (if swap-x-to-y
                                 (- (get token-y-reserve pool) amount-out)
                                 (+ (get token-y-reserve pool) amount-in)),
              accumulated-fees: (+ (get accumulated-fees pool) fee-amount)
            }))
          (ok { amount-out: amount-out, fee-paid: fee-amount })))
      ERR-POOL-NOT-FOUND
    )
  )
)

;; ============================================================
;; CYCLE TRANSITIONS
;; ============================================================

;; Advance pool to next cycle - callable by anyone when cycle has ended
(define-public (advance-cycle (pool-id uint))
  (begin
    (try! (assert-not-paused))
    (asserts! (is-cycle-ended pool-id) ERR-CYCLE-NOT-ENDED)
    (match (map-get? pools { pool-id: pool-id })
      pool
        (let (
          (current-cycle  (get cycle-count pool))
          (total-shares   (get total-lp-shares pool))
          (fees-collected (get accumulated-fees pool))
          ;; Protocol takes a cut of transition fees
          (protocol-cut   (/ (* fees-collected YIELD-FEE-BPS) BPS-DENOM))
          (lp-fees        (- fees-collected protocol-cut))
          ;; Yield per share scaled by 1e6 for precision
          (yield-per-share (if (> total-shares u0)
                             (/ (* lp-fees u1000000) total-shares)
                             u0))
          ;; Alternate between farming and stability phases
          (next-phase (if (is-eq (get current-phase pool) PHASE-FARMING)
                        PHASE-STABILITY
                        PHASE-FARMING))
        )
        ;; Snapshot the ending cycle
        (map-set cycle-snapshots { pool-id: pool-id, cycle-number: current-cycle }
          {
            end-block: block-height,
            total-fees-collected: fees-collected,
            total-lp-shares: total-shares,
            phase: (get current-phase pool),
            yield-per-share: yield-per-share
          })
        ;; Advance pool state to next cycle
        (map-set pools { pool-id: pool-id }
          (merge pool {
            current-phase: next-phase,
            cycle-start-block: block-height,
            accumulated-fees: u0,
            cycle-count: (+ current-cycle u1)
          }))
        ;; Accrue protocol treasury fees
        (var-set treasury-fees (+ (var-get treasury-fees) protocol-cut))
        ;; Mint CYCS rewards to the caller for triggering the transition
        (try! (ft-mint? cycs-token u1000000 tx-sender))
        (ok {
          cycle-ended: current-cycle,
          next-phase: next-phase,
          yield-per-share: yield-per-share,
          protocol-fees: protocol-cut
        }))
      ERR-POOL-NOT-FOUND
    )
  )
)

;; ============================================================
;; REWARD CLAIMS
;; ============================================================

;; Claim accrued cross-cycle amplified rewards for a pool position
(define-public (claim-rewards (pool-id uint))
  (begin
    (try! (assert-not-paused))
    (match (map-get? lp-positions { pool-id: pool-id, user: tx-sender })
      position
        (match (get-pending-rewards pool-id tx-sender)
          rewards-ok
            (if (> rewards-ok u0)
              (begin
                ;; Reset pending rewards
                (map-set lp-positions { pool-id: pool-id, user: tx-sender }
                  (merge position { pending-rewards: u0 }))
                ;; Mint CYCS as yield reward
                (try! (ft-mint? cycs-token rewards-ok tx-sender))
                (ok rewards-ok))
              (ok u0))
          err-val (err err-val))
      ERR-INSUFFICIENT-BALANCE
    )
  )
)

;; Accrue pending rewards into LP position from a completed cycle snapshot
(define-public (accrue-cycle-rewards (pool-id uint) (cycle-number uint))
  (match (map-get? lp-positions { pool-id: pool-id, user: tx-sender })
    position
      (match (map-get? cycle-snapshots { pool-id: pool-id, cycle-number: cycle-number })
        snapshot
          (let (
            (user-shares  (get lp-shares position))
            (yps          (get yield-per-share snapshot))
            ;; Reward proportional to share of pool at cycle end
            (cycle-reward (/ (* user-shares yps) u1000000))
          )
          (map-set lp-positions { pool-id: pool-id, user: tx-sender }
            (merge position {
              pending-rewards: (+ (get pending-rewards position) cycle-reward)
            }))
          (ok cycle-reward))
        ERR-POOL-NOT-FOUND)
    ERR-INSUFFICIENT-BALANCE
  )
)

;; ============================================================
;; GOVERNANCE - CYCS Token Voting
;; ============================================================

;; Submit a governance proposal to change a pool's cycle duration
(define-public (submit-proposal
    (pool-id uint)
    (proposed-cycle-duration uint)
    (voting-period-blocks uint))
  (begin
    (asserts! (>= (ft-get-balance cycs-token tx-sender) u10000000) ERR-NOT-AUTHORIZED)
    (asserts! (and (>= proposed-cycle-duration MIN-CYCLE-BLOCKS)
                   (<= proposed-cycle-duration MAX-CYCLE-BLOCKS))
              ERR-INVALID-AMOUNT)
    (let ((proposal-id (+ (var-get proposal-nonce) u1)))
    (map-set governance-proposals { proposal-id: proposal-id }
      {
        pool-id: pool-id,
        proposed-cycle-duration: proposed-cycle-duration,
        votes-for: u0,
        votes-against: u0,
        end-block: (+ block-height voting-period-blocks),
        executed: false
      })
    (var-set proposal-nonce proposal-id)
    (ok proposal-id))
  )
)

;; Vote on a governance proposal
(define-public (vote-on-proposal
    (proposal-id uint)
    (vote-for bool))
  (match (map-get? governance-proposals { proposal-id: proposal-id })
    proposal
      (begin
        (asserts! (< block-height (get end-block proposal)) ERR-CYCLE-NOT-ENDED)
        (let ((voter-power (ft-get-balance cycs-token tx-sender)))
        (asserts! (> voter-power u0) ERR-NOT-AUTHORIZED)
        (map-set governance-proposals { proposal-id: proposal-id }
          (if vote-for
            (merge proposal { votes-for: (+ (get votes-for proposal) voter-power) })
            (merge proposal { votes-against: (+ (get votes-against proposal) voter-power) })))
        (ok voter-power)))
    ERR-POOL-NOT-FOUND
  )
)

;; Execute a passed governance proposal
(define-public (execute-proposal (proposal-id uint))
  (match (map-get? governance-proposals { proposal-id: proposal-id })
    proposal
      (begin
        (asserts! (>= block-height (get end-block proposal)) ERR-CYCLE-NOT-ENDED)
        (asserts! (not (get executed proposal)) ERR-ALREADY-INITIALIZED)
        (asserts! (> (get votes-for proposal) (get votes-against proposal))
                  ERR-NOT-AUTHORIZED)
        (match (map-get? pools { pool-id: (get pool-id proposal) })
          pool
            (begin
              (map-set pools { pool-id: (get pool-id proposal) }
                (merge pool {
                  cycle-duration: (get proposed-cycle-duration proposal)
                }))
              (map-set governance-proposals { proposal-id: proposal-id }
                (merge proposal { executed: true }))
              (ok true))
          ERR-POOL-NOT-FOUND))
    ERR-POOL-NOT-FOUND
  )
)

;; ============================================================
;; ADMIN FUNCTIONS
;; ============================================================

;; Pause or unpause the entire protocol
(define-public (set-protocol-paused (paused bool))
  (begin
    (try! (assert-owner))
    (var-set protocol-paused paused)
    (ok paused)
  )
)

;; Pause or unpause a specific pool
(define-public (set-pool-active (pool-id uint) (active bool))
  (begin
    (try! (assert-owner))
    (match (map-get? pools { pool-id: pool-id })
      pool
        (begin
          (map-set pools { pool-id: pool-id }
            (merge pool { is-active: active }))
          (ok active))
      ERR-POOL-NOT-FOUND
    )
  )
)

;; Mint initial CYCS supply to the contract owner (one-time bootstrap)
(define-public (bootstrap-cycs (amount uint))
  (begin
    (try! (assert-owner))
    (asserts! (is-eq (ft-get-supply cycs-token) u0) ERR-ALREADY-INITIALIZED)
    (ft-mint? cycs-token amount tx-sender)
  )
)

;; Transfer accumulated treasury fees (denominated as CYCS tokens) to a recipient
(define-public (withdraw-treasury (recipient principal) (amount uint))
  (begin
    (try! (assert-owner))
    (asserts! (<= amount (var-get treasury-fees)) ERR-INSUFFICIENT-BALANCE)
    (var-set treasury-fees (- (var-get treasury-fees) amount))
    (ft-mint? cycs-token amount recipient)
  )
)

;; ============================================================
;; CYCS TOKEN TRANSFERS (SIP-010 subset)
;; ============================================================

(define-public (transfer-cycs (amount uint) (sender principal) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender sender) ERR-NOT-AUTHORIZED)
    (ft-transfer? cycs-token amount sender recipient)
  )
)
