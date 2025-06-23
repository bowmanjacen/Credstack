
;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u100))
(define-constant ERR_USER_NOT_FOUND (err u101))
(define-constant ERR_INVALID_AMOUNT (err u102))
(define-constant ERR_LOAN_NOT_FOUND (err u103))
(define-constant ERR_LOAN_ALREADY_REPAID (err u104))
(define-constant ERR_INSUFFICIENT_CREDIT (err u105))
(define-constant ERR_INVALID_SCORE (err u106))
(define-constant MIN_CREDIT_SCORE u300)
(define-constant MAX_CREDIT_SCORE u850)
(define-constant DEFAULT_CREDIT_SCORE u500)
(define-constant SCORE_ADJUSTMENT_PAYMENT u10)
(define-constant SCORE_ADJUSTMENT_DEFAULT u50)

;; data vars
(define-data-var next-loan-id uint u1)
(define-data-var total-users uint u0)
(define-data-var total-loans uint u0)

;; data maps
(define-map credit-profiles principal {
    credit-score: uint,
    total-loans: uint,
    repaid-loans: uint,
    defaulted-loans: uint,
    total-borrowed: uint,
    total-repaid: uint,
    last-updated: uint,
    is-active: bool
})

(define-map loans uint {
    borrower: principal,
    amount: uint,
    repaid-amount: uint,
    due-date: uint,
    is-repaid: bool,
    is-defaulted: bool,
    created-at: uint
})

(define-map user-loans principal (list 50 uint))

(define-map credit-history principal (list 100 {
    action: (string-ascii 20),
    score-change: int,
    timestamp: uint,
    loan-id: (optional uint)
}))

;; public functions
(define-public (create-profile)
    (let ((caller tx-sender))
        (if (is-none (map-get? credit-profiles caller))
            (begin
                (map-set credit-profiles caller {
                    credit-score: DEFAULT_CREDIT_SCORE,
                    total-loans: u0,
                    repaid-loans: u0,
                    defaulted-loans: u0,
                    total-borrowed: u0,
                    total-repaid: u0,
                    last-updated: stacks-block-height,
                    is-active: true
                })
                (var-set total-users (+ (var-get total-users) u1))
                (add-credit-history caller "profile_created" 0 none)
                (ok true))
            (ok false))))

(define-public (request-loan (amount uint) (duration-blocks uint))
    (let ((caller tx-sender)
          (loan-id (var-get next-loan-id))
          (profile (unwrap! (map-get? credit-profiles caller) ERR_USER_NOT_FOUND)))
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (>= (get credit-score profile) MIN_CREDIT_SCORE) ERR_INSUFFICIENT_CREDIT)
        (map-set loans loan-id {
            borrower: caller,
            amount: amount,
            repaid-amount: u0,
            due-date: (+ stacks-block-height duration-blocks),
            is-repaid: false,
            is-defaulted: false,
            created-at: stacks-block-height
        })
        (map-set credit-profiles caller (merge profile {
            total-loans: (+ (get total-loans profile) u1),
            total-borrowed: (+ (get total-borrowed profile) amount),
            last-updated: stacks-block-height
        }))
        (map-set user-loans caller 
            (unwrap! (as-max-len? (append (default-to (list) (map-get? user-loans caller)) loan-id) u50) ERR_INVALID_AMOUNT))
        (var-set next-loan-id (+ loan-id u1))
        (var-set total-loans (+ (var-get total-loans) u1))
        (add-credit-history caller "loan_requested" 0 (some loan-id))
        (ok loan-id)))

(define-public (repay-loan (loan-id uint) (amount uint))
    (let ((loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
          (borrower (get borrower loan))
          (profile (unwrap! (map-get? credit-profiles borrower) ERR_USER_NOT_FOUND)))
        (asserts! (is-eq tx-sender borrower) ERR_UNAUTHORIZED)
        (asserts! (not (get is-repaid loan)) ERR_LOAN_ALREADY_REPAID)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (let ((new-repaid-amount (+ (get repaid-amount loan) amount))
              (is-fully-repaid (>= new-repaid-amount (get amount loan))))
            (map-set loans loan-id (merge loan {
                repaid-amount: new-repaid-amount,
                is-repaid: is-fully-repaid
            }))
            (if is-fully-repaid
                (begin
                    (map-set credit-profiles borrower (merge profile {
                        credit-score: (if (<= (+ (get credit-score profile) SCORE_ADJUSTMENT_PAYMENT) MAX_CREDIT_SCORE)
                                          (+ (get credit-score profile) SCORE_ADJUSTMENT_PAYMENT)
                                          MAX_CREDIT_SCORE),
                        repaid-loans: (+ (get repaid-loans profile) u1),
                        total-repaid: (+ (get total-repaid profile) (get amount loan)),
                        last-updated: stacks-block-height
                    }))
                    (add-credit-history borrower "loan_repaid" (to-int SCORE_ADJUSTMENT_PAYMENT) (some loan-id)))
                (begin
                    (map-set credit-profiles borrower (merge profile {
                        total-repaid: (+ (get total-repaid profile) amount),
                        last-updated: stacks-block-height
                    }))
                    (add-credit-history borrower "partial_payment" 0 (some loan-id))))
            (ok is-fully-repaid))))

(define-public (mark-loan-default (loan-id uint))
    (let ((loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
          (borrower (get borrower loan))
          (profile (unwrap! (map-get? credit-profiles borrower) ERR_USER_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> stacks-block-height (get due-date loan)) ERR_UNAUTHORIZED)
        (asserts! (not (get is-repaid loan)) ERR_LOAN_ALREADY_REPAID)
        (map-set loans loan-id (merge loan {
            is-defaulted: true
        }))
        (map-set credit-profiles borrower (merge profile {
            credit-score: (max MIN_CREDIT_SCORE (- (get credit-score profile) SCORE_ADJUSTMENT_DEFAULT)),
            defaulted-loans: (+ (get defaulted-loans profile) u1),
            last-updated: stacks-block-height
        }))
        (add-credit-history borrower "loan_defaulted" (- 0 (to-int SCORE_ADJUSTMENT_DEFAULT)) (some loan-id))
        (ok true)))

(define-public (update-credit-score (user principal) (new-score uint))
    (let ((profile (unwrap! (map-get? credit-profiles user) ERR_USER_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (and (>= new-score MIN_CREDIT_SCORE) (<= new-score MAX_CREDIT_SCORE)) ERR_INVALID_SCORE)
        (let ((score-change (- (to-int new-score) (to-int (get credit-score profile)))))
            (map-set credit-profiles user (merge profile {
                credit-score: new-score,
                last-updated: stacks-block-height
            }))
            (add-credit-history user "manual_adjustment" score-change none)
            (ok true))))

;; read only functions
(define-read-only (get-credit-profile (user principal))
    (map-get? credit-profiles user))

(define-read-only (get-credit-score (user principal))
    (match (map-get? credit-profiles user)
        profile (some (get credit-score profile))
        none))

(define-read-only (get-loan (loan-id uint))
    (map-get? loans loan-id))

(define-read-only (get-user-loans (user principal))
    (default-to (list) (map-get? user-loans user)))

(define-read-only (get-credit-history (user principal))
    (default-to (list) (map-get? credit-history user)))

(define-read-only (calculate-credit-utilization (user principal))
    (match (map-get? credit-profiles user)
        profile (let ((total-borrowed (get total-borrowed profile))
                     (total-repaid (get total-repaid profile)))
                    (if (> total-borrowed u0)
                        (some (/ (* (- total-borrowed total-repaid) u100) total-borrowed))
                        (some u0)))
        none))

(define-read-only (get-loan-status (loan-id uint))
    (match (map-get? loans loan-id)
        loan (if (get is-repaid loan)
                (some "repaid")
                (if (get is-defaulted loan)
                    (some "defaulted")
                    (if (> stacks-block-height (get due-date loan))
                        (some "overdue")
                        (some "active"))))
        none))

(define-read-only (get-contract-stats)
    {
        total-users: (var-get total-users),
        total-loans: (var-get total-loans),
        next-loan-id: (var-get next-loan-id)
    })

(define-read-only (is-loan-overdue (loan-id uint))
    (match (map-get? loans loan-id)
        loan (and (not (get is-repaid loan))
                 (not (get is-defaulted loan))
                 (> stacks-block-height (get due-date loan)))
        false))

;; utility functions
(define-read-only (max (a uint) (b uint))
    (if (>= a b) a b)
)

;; private functions
(define-private (add-credit-history (user principal) (action (string-ascii 20)) (score-change int) (loan-id (optional uint)))
    (let ((current-history (default-to (list) (map-get? credit-history user)))
          (new-entry {
              action: action,
              score-change: score-change,
              timestamp: stacks-block-height,
              loan-id: loan-id
          }))
        (map-set credit-history user 
            (unwrap! (as-max-len? (append current-history new-entry) u100) false))
        true))