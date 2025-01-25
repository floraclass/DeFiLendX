;; DeFiLendX: Community Reputation and Governance System

;; Constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_INPUT (err u101))
(define-constant ERR_NOT_FOUND (err u102))

;; Data Variables
(define-data-var governance-token-name (string-ascii 32) "DEFIX")
(define-data-var min-participation-threshold uint u100000) ;; 100,000 tokens
(define-data-var last-proposal-id uint u0)

;; Maps
(define-map user-reputation
  { user: principal }
  { 
    score: uint, 
    lending-count: uint, 
    borrowing-count: uint, 
    on-time-payments: uint, 
    late-payments: uint 
  }
)

(define-map proposals
  { id: uint }
  { 
    creator: principal, 
    title: (string-ascii 64), 
    description: (string-utf8 256), 
    votes-for: uint, 
    votes-against: uint, 
    status: (string-ascii 16) 
  }
)

(define-map user-identity
  { user: principal }
  { 
    kyc-status: (string-ascii 16), 
    reputation-score: uint 
  }
)

;; Private Functions
(define-private (is-valid-action (action (string-ascii 16)))
  (or 
    (is-eq action "lend")
    (is-eq action "borrow")
    (is-eq action "on-time-payment")
    (is-eq action "late-payment")
  )
)

;; Public Functions

;; Reputation Scoring
(define-public (update-reputation (user principal) (action (string-ascii 16)) (amount uint))
  (begin
    (asserts! (is-valid-action action) ERR_INVALID_INPUT)
    (let (
      (current-rep (default-to 
        { 
          score: u0, 
          lending-count: u0, 
          borrowing-count: u0, 
          on-time-payments: u0, 
          late-payments: u0 
        } 
        (map-get? user-reputation { user: user })))
    )
      (if (is-eq action "lend")
        (map-set user-reputation 
          { user: user } 
          (merge current-rep { 
            score: (+ (get score current-rep) amount), 
            lending-count: (+ (get lending-count current-rep) u1) 
          }))
        (if (is-eq action "borrow")
          (map-set user-reputation 
            { user: user } 
            (merge current-rep { 
              score: (+ (get score current-rep) amount), 
              borrowing-count: (+ (get borrowing-count current-rep) u1) 
            }))
          (if (is-eq action "on-time-payment")
            (map-set user-reputation 
              { user: user } 
              (merge current-rep { 
                score: (+ (get score current-rep) amount), 
                on-time-payments: (+ (get on-time-payments current-rep) u1) 
              }))
            (map-set user-reputation 
              { user: user } 
              (merge current-rep { 
                score: (- (get score current-rep) u10), 
                late-payments: (+ (get late-payments current-rep) u1) 
              }))
          )
        )
      )
      (ok true)
    )
  )
)

;; Governance
(define-public (create-proposal (title (string-ascii 64)) (description (string-utf8 256)))
  (let (
    (proposal-id (+ (var-get last-proposal-id) u1))
    (current-rep (unwrap! (map-get? user-reputation { user: tx-sender }) ERR_UNAUTHORIZED))
  )
    (asserts! (>= (get score current-rep) (var-get min-participation-threshold)) ERR_UNAUTHORIZED)
    (map-set proposals { id: proposal-id }
      { 
        creator: tx-sender, 
        title: title, 
        description: description, 
        votes-for: u0, 
        votes-against: u0, 
        status: "active" 
      }
    )
    (var-set last-proposal-id proposal-id)
    (ok proposal-id)
  )
)

(define-public (vote-on-proposal (proposal-id uint) (vote bool))
  (let (
    (proposal (unwrap! (map-get? proposals { id: proposal-id }) ERR_NOT_FOUND))
    (user-rep (unwrap! (map-get? user-reputation { user: tx-sender }) ERR_UNAUTHORIZED))
  )
    (asserts! (>= (get score user-rep) (var-get min-participation-threshold)) ERR_UNAUTHORIZED)
    (if vote
      (map-set proposals { id: proposal-id } 
        (merge proposal { votes-for: (+ (get votes-for proposal) (get score user-rep)) }))
      (map-set proposals { id: proposal-id } 
        (merge proposal { votes-against: (+ (get votes-against proposal) (get score user-rep)) }))
    )
    (ok true)
  )
)

;; Identity Verification
(define-public (verify-identity (kyc-status (string-ascii 16)))
  (let (
    (current-rep (default-to { score: u0 } (map-get? user-reputation { user: tx-sender })))
  )
    (map-set user-identity 
      { user: tx-sender } 
      { 
        kyc-status: kyc-status, 
        reputation-score: (get score current-rep) 
      }
    )
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-user-reputation (user principal))
  (map-get? user-reputation { user: user })
)

(define-read-only (get-proposal (proposal-id uint))
  (map-get? proposals { id: proposal-id })
)

(define-read-only (get-user-identity (user principal))
  (map-get? user-identity { user: user })
)

;; Initialize contract
(define-private (init-contract)
  (begin
    (map-set user-reputation 
      { user: tx-sender } 
      { 
        score: u100, 
        lending-count: u0, 
        borrowing-count: u0, 
        on-time-payments: u0, 
        late-payments: u0 
      }
    )
    (map-set user-identity 
      { user: tx-sender } 
      { 
        kyc-status: "verified", 
        reputation-score: u100 
      }
    )
    true
  )
)

;; Execute initialization
(init-contract)

