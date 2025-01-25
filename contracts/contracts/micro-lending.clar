;; DeFiLendX: Micro-Lending and Loan Management System

;; Constants
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_INVALID_INPUT (err u101))
(define-constant ERR_INSUFFICIENT_FUNDS (err u102))
(define-constant ERR_LOAN_NOT_FOUND (err u103))
(define-constant MAX_LOAN_TERM u360) ;; Maximum loan term in days
(define-constant MIN_LOAN_AMOUNT u100) ;; Minimum loan amount
(define-constant MAX_LOAN_AMOUNT u100000) ;; Maximum loan amount

;; Data Variables
(define-data-var last-loan-id uint u0)
(define-data-var min-risk-score uint u30) ;; Minimum risk score for loan eligibility

;; Maps
(define-map loans
  { id: uint }
  { 
    borrower: principal, 
    amount: uint, 
    interest-rate: uint, 
    term: uint, 
    status: (string-ascii 16), 
    repaid-amount: uint,
    start-date: uint
  }
)

(define-map loan-applications
  { id: uint }
  { 
    applicant: principal, 
    amount: uint, 
    purpose: (string-ascii 32), 
    status: (string-ascii 16),
    applied-date: uint
  }
)

(define-map user-risk-score
  { user: principal }
  { 
    score: uint, 
    last-updated: uint 
  }
)

;; Private Functions
(define-private (is-valid-purpose (purpose (string-ascii 32)))
  (or
    (is-eq purpose "personal")
    (is-eq purpose "business")
    (is-eq purpose "education")
    (is-eq purpose "emergency")
  )
)

;; Loan Application
(define-public (apply-for-loan (amount uint) (purpose (string-ascii 32)))
  (begin
    ;; Validate input parameters
    (asserts! (and 
      (>= amount MIN_LOAN_AMOUNT) 
      (<= amount MAX_LOAN_AMOUNT)
    ) ERR_INVALID_INPUT)
    
    ;; Validate loan purpose
    (asserts! (is-valid-purpose purpose) ERR_INVALID_INPUT)
    
    ;; Check user risk score
    (let (
      (risk-score (default-to { score: u0, last-updated: u0 } 
                    (map-get? user-risk-score { user: tx-sender })))
    )
      (asserts! (>= (get score risk-score) (var-get min-risk-score)) ERR_UNAUTHORIZED)
      
      ;; Create loan application
      (let ((application-id (+ (var-get last-loan-id) u1)))
        (map-set loan-applications { id: application-id }
          { 
            applicant: tx-sender, 
            amount: amount, 
            purpose: purpose, 
            status: "pending",
            applied-date: stacks-block-height
          }
        )
        (var-set last-loan-id application-id)
        (ok application-id)
      )
    )
  )
)

;; Risk Assessment
(define-public (update-risk-score (user principal) (score uint))
  (begin
    ;; Ensure only the contract itself can call this function
    (asserts! (is-eq tx-sender (as-contract tx-sender)) ERR_UNAUTHORIZED)
    ;; Update the user risk score
    (map-set user-risk-score 
      { user: user } 
      { 
        score: score, 
        last-updated: stacks-block-height 
      }
    )
    (ok true)
  )
)


;; Loan Approval
(define-public (approve-loan (application-id uint) (interest-rate uint) (term uint))
  (let (
    (application (unwrap! (map-get? loan-applications { id: application-id }) ERR_LOAN_NOT_FOUND))
    (loan-id (+ (var-get last-loan-id) u1))
  )
    (asserts! (is-eq (get status application) "pending") ERR_INVALID_INPUT)
    (asserts! (<= term MAX_LOAN_TERM) ERR_INVALID_INPUT)
    
    (map-set loans { id: loan-id }
      { 
        borrower: (get applicant application), 
        amount: (get amount application), 
        interest-rate: interest-rate, 
        term: term, 
        status: "active", 
        repaid-amount: u0,
        start-date: stacks-block-height
      }
    )
    (map-set loan-applications { id: application-id } 
      (merge application { status: "approved" })
    )
    (var-set last-loan-id loan-id)
    (ok loan-id)
  )
)

;; Loan Repayment
(define-public (repay-loan (loan-id uint) (amount uint))
  (let (
    (loan (unwrap! (map-get? loans { id: loan-id }) ERR_LOAN_NOT_FOUND))
    (new-repaid-amount (+ (get repaid-amount loan) amount))
  )
    (asserts! (is-eq (get borrower loan) tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get status loan) "active") ERR_INVALID_INPUT)
    
    (map-set loans { id: loan-id } 
      (merge loan { repaid-amount: new-repaid-amount })
    )
    
    ;; Mark loan as repaid if full amount is paid
    (if (>= new-repaid-amount (get amount loan))
      (map-set loans { id: loan-id } 
        (merge loan { 
          status: "repaid", 
          repaid-amount: (get amount loan) 
        })
      )
      true
    )
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-loan (loan-id uint))
  (map-get? loans { id: loan-id })
)

(define-read-only (get-loan-application (application-id uint))
  (map-get? loan-applications { id: application-id })
)

(define-read-only (get-user-risk-score (user principal))
  (map-get? user-risk-score { user: user })
)

;; Initialize contract
(define-private (init-contract)
  (begin
    (map-set user-risk-score 
      { user: tx-sender } 
      { 
        score: u50, 
        last-updated: stacks-block-height 
      }
    )
    true
  )
)

;; Execute initialization
(init-contract)