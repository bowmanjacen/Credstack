
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
(define-constant ERR_INSURANCE_NOT_FOUND (err u107))
(define-constant ERR_INSURANCE_EXPIRED (err u108))
(define-constant ERR_INSUFFICIENT_POOL_FUNDS (err u109))
(define-constant ERR_CLAIM_ALREADY_PROCESSED (err u110))
(define-constant ERR_INVALID_PREMIUM (err u111))
(define-constant INSURANCE_DURATION_BLOCKS u4320)
(define-constant BASE_PREMIUM_RATE u100)
(define-constant POOL_FEE_PERCENTAGE u5)
(define-constant ERR_STAKE_NOT_FOUND (err u112))
(define-constant ERR_STAKE_LOCKED (err u113))
(define-constant ERR_INSUFFICIENT_STAKE (err u114))
(define-constant ERR_STAKE_ALREADY_EXISTS (err u115))
(define-constant ERR_EARLY_WITHDRAWAL_PENALTY (err u116))
(define-constant MIN_STAKE_AMOUNT u1000000)
(define-constant MAX_STAKE_BOOST u100)
(define-constant STAKE_BOOST_RATE u50)
(define-constant MIN_STAKE_DURATION u1440)
(define-constant EARLY_WITHDRAWAL_PENALTY u20)

;; data vars
(define-data-var next-loan-id uint u1)
(define-data-var total-users uint u0)
(define-data-var total-loans uint u0)
(define-data-var insurance-pool-balance uint u0)
(define-data-var next-insurance-id uint u1)
(define-data-var total-insurance-policies uint u0)
(define-data-var total-staked-amount uint u0)
(define-data-var next-stake-id uint u1)
(define-data-var total-stakes uint u0)

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

(define-map insurance-policies uint {
    policyholder: principal,
    coverage-amount: uint,
    premium-paid: uint,
    expiry-block: uint,
    is-active: bool,
    created-at: uint
})

(define-map user-insurance-policies principal (list 20 uint))

(define-map insurance-claims uint {
    policy-id: uint,
    claimant: principal,
    loan-id: uint,
    claim-amount: uint,
    score-loss: uint,
    is-processed: bool,
    is-approved: bool,
    processed-at: (optional uint),
    created-at: uint
})

(define-map policy-claims uint (list 10 uint))

(define-map credit-stakes principal {
    stake-id: uint,
    staked-amount: uint,
    score-boost: uint,
    stake-start: uint,
    stake-duration: uint,
    is-active: bool,
    is-liquidated: bool
})

(define-map stake-details uint {
    staker: principal,
    amount: uint,
    boost-applied: uint,
    start-block: uint,
    end-block: uint,
    is-active: bool,
    liquidation-loan-id: (optional uint),
    created-at: uint
})

(define-map user-stakes principal (list 10 uint))

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

(define-public (purchase-insurance (coverage-amount uint))
    (let ((caller tx-sender)
          (profile (unwrap! (map-get? credit-profiles caller) ERR_USER_NOT_FOUND))
          (insurance-id (var-get next-insurance-id))
          (premium (calculate-insurance-premium caller coverage-amount)))
        (asserts! (> coverage-amount u0) ERR_INVALID_AMOUNT)
        (asserts! (> premium u0) ERR_INVALID_PREMIUM)
        (try! (stx-transfer? premium caller (as-contract tx-sender)))
        (let ((pool-contribution (- premium (/ (* premium POOL_FEE_PERCENTAGE) u100))))
            (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) pool-contribution))
            (map-set insurance-policies insurance-id {
                policyholder: caller,
                coverage-amount: coverage-amount,
                premium-paid: premium,
                expiry-block: (+ stacks-block-height INSURANCE_DURATION_BLOCKS),
                is-active: true,
                created-at: stacks-block-height
            })
            (map-set user-insurance-policies caller
                (unwrap! (as-max-len? (append (default-to (list) (map-get? user-insurance-policies caller)) insurance-id) u20) ERR_INVALID_AMOUNT))
            (var-set next-insurance-id (+ insurance-id u1))
            (var-set total-insurance-policies (+ (var-get total-insurance-policies) u1))
            (add-credit-history caller "insurance_purchased" 0 none)
            (ok insurance-id))))

(define-public (file-insurance-claim (policy-id uint) (loan-id uint))
    (let ((policy (unwrap! (map-get? insurance-policies policy-id) ERR_INSURANCE_NOT_FOUND))
          (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
          (caller tx-sender))
        (asserts! (is-eq caller (get policyholder policy)) ERR_UNAUTHORIZED)
        (asserts! (is-eq caller (get borrower loan)) ERR_UNAUTHORIZED)
        (asserts! (get is-active policy) ERR_INSURANCE_EXPIRED)
        (asserts! (> (get expiry-block policy) stacks-block-height) ERR_INSURANCE_EXPIRED)
        (asserts! (get is-defaulted loan) ERR_LOAN_NOT_FOUND)
        (let ((claim-id (var-get next-insurance-id))
              (score-loss SCORE_ADJUSTMENT_DEFAULT)
              (claim-amount (min (get coverage-amount policy) (* score-loss u10))))
            (map-set insurance-claims claim-id {
                policy-id: policy-id,
                claimant: caller,
                loan-id: loan-id,
                claim-amount: claim-amount,
                score-loss: score-loss,
                is-processed: false,
                is-approved: false,
                processed-at: none,
                created-at: stacks-block-height
            })
            (map-set policy-claims policy-id
                (unwrap! (as-max-len? (append (default-to (list) (map-get? policy-claims policy-id)) claim-id) u10) ERR_INVALID_AMOUNT))
            (var-set next-insurance-id (+ claim-id u1))
            (ok claim-id))))

(define-public (process-insurance-claim (claim-id uint) (approve bool))
    (let ((claim (unwrap! (map-get? insurance-claims claim-id) ERR_INSURANCE_NOT_FOUND))
          (policy (unwrap! (map-get? insurance-policies (get policy-id claim)) ERR_INSURANCE_NOT_FOUND))
          (claimant (get claimant claim)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (not (get is-processed claim)) ERR_CLAIM_ALREADY_PROCESSED)
        (if approve
            (begin
                (asserts! (>= (var-get insurance-pool-balance) (get claim-amount claim)) ERR_INSUFFICIENT_POOL_FUNDS)
                (try! (as-contract (stx-transfer? (get claim-amount claim) tx-sender claimant)))
                (var-set insurance-pool-balance (- (var-get insurance-pool-balance) (get claim-amount claim)))
                (let ((profile (unwrap! (map-get? credit-profiles claimant) ERR_USER_NOT_FOUND)))
                    (map-set credit-profiles claimant (merge profile {
                        credit-score: (min MAX_CREDIT_SCORE (+ (get credit-score profile) (get score-loss claim))),
                        last-updated: stacks-block-height
                    }))
                    (add-credit-history claimant "ins_claim_approved" (to-int (get score-loss claim)) (some (get loan-id claim)))))
            (add-credit-history claimant "ins_claim_denied" 0 (some (get loan-id claim))))
        (map-set insurance-claims claim-id (merge claim {
            is-processed: true,
            is-approved: approve,
            processed-at: (some stacks-block-height)
        }))
        (ok approve)))

(define-public (contribute-to-insurance-pool (amount uint))
    (let ((caller tx-sender))
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (try! (stx-transfer? amount caller (as-contract tx-sender)))
        (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) amount))
        (add-credit-history caller "pool_contribution" 0 none)
        (ok true)))

(define-public (withdraw-from-insurance-pool (amount uint))
    (begin
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (> amount u0) ERR_INVALID_AMOUNT)
        (asserts! (>= (var-get insurance-pool-balance) amount) ERR_INSUFFICIENT_POOL_FUNDS)
        (try! (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER)))
        (var-set insurance-pool-balance (- (var-get insurance-pool-balance) amount))
        (ok true)))

(define-public (stake-for-credit-boost (amount uint) (duration-blocks uint))
    (let ((caller tx-sender)
          (profile (unwrap! (map-get? credit-profiles caller) ERR_USER_NOT_FOUND))
          (stake-id (var-get next-stake-id))
          (boost-amount (calculate-score-boost amount duration-blocks)))
        (asserts! (>= amount MIN_STAKE_AMOUNT) ERR_INVALID_AMOUNT)
        (asserts! (>= duration-blocks MIN_STAKE_DURATION) ERR_INVALID_AMOUNT)
        (asserts! (is-none (map-get? credit-stakes caller)) ERR_STAKE_ALREADY_EXISTS)
        (try! (stx-transfer? amount caller (as-contract tx-sender)))
        (let ((new-score (min MAX_CREDIT_SCORE (+ (get credit-score profile) boost-amount))))
            (map-set credit-stakes caller {
                stake-id: stake-id,
                staked-amount: amount,
                score-boost: boost-amount,
                stake-start: stacks-block-height,
                stake-duration: duration-blocks,
                is-active: true,
                is-liquidated: false
            })
            (map-set stake-details stake-id {
                staker: caller,
                amount: amount,
                boost-applied: boost-amount,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height duration-blocks),
                is-active: true,
                liquidation-loan-id: none,
                created-at: stacks-block-height
            })
            (map-set credit-profiles caller (merge profile {
                credit-score: new-score,
                last-updated: stacks-block-height
            }))
            (map-set user-stakes caller
                (unwrap! (as-max-len? (append (default-to (list) (map-get? user-stakes caller)) stake-id) u10) ERR_INVALID_AMOUNT))
            (var-set next-stake-id (+ stake-id u1))
            (var-set total-stakes (+ (var-get total-stakes) u1))
            (var-set total-staked-amount (+ (var-get total-staked-amount) amount))
            (add-credit-history caller "stake_created" (to-int boost-amount) none)
            (ok stake-id))))

(define-public (unstake-credit-boost)
    (let ((caller tx-sender)
          (stake (unwrap! (map-get? credit-stakes caller) ERR_STAKE_NOT_FOUND))
          (profile (unwrap! (map-get? credit-profiles caller) ERR_USER_NOT_FOUND))
          (current-block stacks-block-height)
          (stake-end (+ (get stake-start stake) (get stake-duration stake))))
        (asserts! (get is-active stake) ERR_STAKE_NOT_FOUND)
        (asserts! (not (get is-liquidated stake)) ERR_STAKE_LOCKED)
        (let ((is-early-withdrawal (< current-block stake-end))
              (penalty-amount (if is-early-withdrawal
                                 (/ (* (get staked-amount stake) EARLY_WITHDRAWAL_PENALTY) u100)
                                 u0))
              (return-amount (- (get staked-amount stake) penalty-amount))
              (new-score (max MIN_CREDIT_SCORE (- (get credit-score profile) (get score-boost stake)))))
            (try! (as-contract (stx-transfer? return-amount tx-sender caller)))
            (if (> penalty-amount u0)
                (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) penalty-amount))
                true)
            (map-set credit-stakes caller (merge stake {
                is-active: false
            }))
            (map-set stake-details (get stake-id stake) (merge (unwrap! (map-get? stake-details (get stake-id stake)) ERR_STAKE_NOT_FOUND) {
                is-active: false
            }))
            (map-set credit-profiles caller (merge profile {
                credit-score: new-score,
                last-updated: stacks-block-height
            }))
            (var-set total-staked-amount (- (var-get total-staked-amount) (get staked-amount stake)))
            (add-credit-history caller "stake_removed" (- 0 (to-int (get score-boost stake))) none)
            (ok return-amount))))

(define-public (liquidate-stake (staker principal) (loan-id uint))
    (let ((stake (unwrap! (map-get? credit-stakes staker) ERR_STAKE_NOT_FOUND))
          (loan (unwrap! (map-get? loans loan-id) ERR_LOAN_NOT_FOUND))
          (profile (unwrap! (map-get? credit-profiles staker) ERR_USER_NOT_FOUND)))
        (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
        (asserts! (is-eq staker (get borrower loan)) ERR_UNAUTHORIZED)
        (asserts! (get is-defaulted loan) ERR_LOAN_NOT_FOUND)
        (asserts! (get is-active stake) ERR_STAKE_NOT_FOUND)
        (asserts! (not (get is-liquidated stake)) ERR_STAKE_LOCKED)
        (let ((liquidation-amount (min (get staked-amount stake) (get amount loan)))
              (remaining-stake (- (get staked-amount stake) liquidation-amount))
              (remaining-boost (if (> remaining-stake u0)
                                  (/ (* (get score-boost stake) remaining-stake) (get staked-amount stake))
                                  u0))
              (new-score (max MIN_CREDIT_SCORE (- (get credit-score profile) (- (get score-boost stake) remaining-boost)))))
            (var-set insurance-pool-balance (+ (var-get insurance-pool-balance) liquidation-amount))
            (if (is-eq remaining-stake u0)
                (map-set credit-stakes staker (merge stake {
                    is-active: false,
                    is-liquidated: true
                }))
                (map-set credit-stakes staker (merge stake {
                    staked-amount: remaining-stake,
                    score-boost: remaining-boost
                })))
            (map-set stake-details (get stake-id stake) (merge (unwrap! (map-get? stake-details (get stake-id stake)) ERR_STAKE_NOT_FOUND) {
                liquidation-loan-id: (some loan-id)
            }))
            (map-set credit-profiles staker (merge profile {
                credit-score: new-score,
                last-updated: stacks-block-height
            }))
            (var-set total-staked-amount (- (var-get total-staked-amount) liquidation-amount))
            (add-credit-history staker "stake_liquidated" (- 0 (to-int (- (get score-boost stake) remaining-boost))) (some loan-id))
            (ok liquidation-amount))))

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
        next-loan-id: (var-get next-loan-id),
        total-insurance-policies: (var-get total-insurance-policies),
        insurance-pool-balance: (var-get insurance-pool-balance)
    })

(define-read-only (get-insurance-policy (policy-id uint))
    (map-get? insurance-policies policy-id))

(define-read-only (get-user-insurance-policies (user principal))
    (default-to (list) (map-get? user-insurance-policies user)))

(define-read-only (get-insurance-claim (claim-id uint))
    (map-get? insurance-claims claim-id))

(define-read-only (get-policy-claims (policy-id uint))
    (default-to (list) (map-get? policy-claims policy-id)))

(define-read-only (is-insurance-policy-active (policy-id uint))
    (match (map-get? insurance-policies policy-id)
        policy (and (get is-active policy)
                   (> (get expiry-block policy) stacks-block-height))
        false))

(define-read-only (get-insurance-pool-stats)
    {
        pool-balance: (var-get insurance-pool-balance),
        total-policies: (var-get total-insurance-policies),
        next-insurance-id: (var-get next-insurance-id)
    })

(define-read-only (calculate-insurance-premium (user principal) (coverage-amount uint))
    (match (map-get? credit-profiles user)
        profile (let ((credit-score (get credit-score profile))
                     (risk-multiplier (if (< credit-score u600) u200
                                     (if (< credit-score u700) u150
                                     (if (< credit-score u750) u120
                                     u100)))))
                    (/ (* coverage-amount risk-multiplier) u10000))
        u0))

(define-read-only (get-credit-stake (user principal))
    (map-get? credit-stakes user))

(define-read-only (get-stake-details (stake-id uint))
    (map-get? stake-details stake-id))

(define-read-only (get-user-stakes (user principal))
    (default-to (list) (map-get? user-stakes user)))

(define-read-only (is-stake-active (user principal))
    (match (map-get? credit-stakes user)
        stake (and (get is-active stake)
                  (not (get is-liquidated stake))
                  (> (+ (get stake-start stake) (get stake-duration stake)) stacks-block-height))
        false))

(define-read-only (get-stake-end-block (user principal))
    (match (map-get? credit-stakes user)
        stake (some (+ (get stake-start stake) (get stake-duration stake)))
        none))

(define-read-only (calculate-early-withdrawal-penalty (user principal))
    (match (map-get? credit-stakes user)
        stake (if (and (get is-active stake)
                      (< stacks-block-height (+ (get stake-start stake) (get stake-duration stake))))
                 (some (/ (* (get staked-amount stake) EARLY_WITHDRAWAL_PENALTY) u100))
                 (some u0))
        none))

(define-read-only (get-staking-stats)
    {
        total-staked-amount: (var-get total-staked-amount),
        total-stakes: (var-get total-stakes),
        next-stake-id: (var-get next-stake-id)
    })

(define-read-only (calculate-score-boost (amount uint) (duration-blocks uint))
    (let ((base-boost (/ (* amount STAKE_BOOST_RATE) u10000000))
          (duration-multiplier (if (>= duration-blocks u8640) u150
                               (if (>= duration-blocks u4320) u125
                               (if (>= duration-blocks u2160) u110
                               u100)))))
        (min MAX_STAKE_BOOST (/ (* base-boost duration-multiplier) u100))))

(define-read-only (get-effective-credit-score (user principal))
    (match (map-get? credit-profiles user)
        profile (let ((base-score (get credit-score profile)))
                    (match (map-get? credit-stakes user)
                        stake (if (and (get is-active stake)
                                     (not (get is-liquidated stake))
                                     (> (+ (get stake-start stake) (get stake-duration stake)) stacks-block-height))
                                 (some base-score)
                                 (some (max MIN_CREDIT_SCORE (- base-score (get score-boost stake)))))
                        (some base-score)))
        none))

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

(define-read-only (min (a uint) (b uint))
    (if (<= a b) a b)
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





