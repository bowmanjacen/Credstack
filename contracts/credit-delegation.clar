;; Credit Score Delegation Contract
;; Allows users to temporarily delegate portions of their credit score to help others

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_UNAUTHORIZED (err u200))
(define-constant ERR_USER_NOT_FOUND (err u201))
(define-constant ERR_INSUFFICIENT_CREDIT_SCORE (err u202))
(define-constant ERR_DELEGATION_NOT_FOUND (err u203))
(define-constant ERR_DELEGATION_EXPIRED (err u204))
(define-constant ERR_INVALID_AMOUNT (err u205))
(define-constant ERR_DELEGATION_EXISTS (err u206))
(define-constant ERR_SELF_DELEGATION (err u207))
(define-constant ERR_INVALID_DURATION (err u208))

;; Delegation limits
(define-constant MAX_DELEGATION_BOOST u100)
(define-constant MIN_DELEGATION_BOOST u10)
(define-constant MAX_DELEGATION_DURATION u4320) ;; ~30 days
(define-constant MIN_DELEGATION_DURATION u144)  ;; ~1 day
(define-constant MIN_DELEGATOR_SCORE u600)     ;; Minimum score to delegate

;; Data variables
(define-data-var next-delegation-id uint u1)
(define-data-var total-active-delegations uint u0)

;; Data maps
(define-map score-delegations 
    { from: principal, to: principal }
    {
        delegation-id: uint,
        boost-amount: uint,
        start-block: uint,
        end-block: uint,
        is-active: bool,
        created-at: uint
    }
)

(define-map delegation-details uint
    {
        from: principal,
        to: principal,
        boost-amount: uint,
        start-block: uint,
        end-block: uint,
        is-active: bool,
        revoked-at: (optional uint)
    }
)

(define-map user-outbound-delegations principal (list 10 uint))
(define-map user-inbound-delegations principal (list 10 uint))

;; Public functions
(define-public (delegate-credit-score (to principal) (boost-amount uint) (duration-blocks uint))
    (let (
        (delegation-id (var-get next-delegation-id))
        (delegator tx-sender)
        (existing-delegation (map-get? score-delegations { from: delegator, to: to }))
    )
        ;; Input validation
        (asserts! (not (is-eq delegator to)) ERR_SELF_DELEGATION)
        (asserts! (and (>= boost-amount MIN_DELEGATION_BOOST) (<= boost-amount MAX_DELEGATION_BOOST)) ERR_INVALID_AMOUNT)
        (asserts! (and (>= duration-blocks MIN_DELEGATION_DURATION) (<= duration-blocks MAX_DELEGATION_DURATION)) ERR_INVALID_DURATION)
        (asserts! (is-none existing-delegation) ERR_DELEGATION_EXISTS)
        
        ;; Check if delegator has sufficient credit score
        (let (
            (delegator-profile (unwrap! (contract-call? .Credstack get-credit-profile delegator) ERR_USER_NOT_FOUND))
            (current-score (get credit-score delegator-profile))
        )
            (asserts! (>= current-score MIN_DELEGATOR_SCORE) ERR_INSUFFICIENT_CREDIT_SCORE)
            (asserts! (> current-score boost-amount) ERR_INSUFFICIENT_CREDIT_SCORE)
            
            ;; Create delegation
            (map-set score-delegations
                { from: delegator, to: to }
                {
                    delegation-id: delegation-id,
                    boost-amount: boost-amount,
                    start-block: stacks-block-height,
                    end-block: (+ stacks-block-height duration-blocks),
                    is-active: true,
                    created-at: stacks-block-height
                }
            )
            
            (map-set delegation-details delegation-id
                {
                    from: delegator,
                    to: to,
                    boost-amount: boost-amount,
                    start-block: stacks-block-height,
                    end-block: (+ stacks-block-height duration-blocks),
                    is-active: true,
                    revoked-at: none
                }
            )
            
            ;; Update user delegation lists
            (let (
                (delegator-outbound (default-to (list) (map-get? user-outbound-delegations delegator)))
                (recipient-inbound (default-to (list) (map-get? user-inbound-delegations to)))
            )
                (map-set user-outbound-delegations delegator
                    (unwrap! (as-max-len? (append delegator-outbound delegation-id) u10) ERR_INVALID_AMOUNT)
                )
                (map-set user-inbound-delegations to
                    (unwrap! (as-max-len? (append recipient-inbound delegation-id) u10) ERR_INVALID_AMOUNT)
                )
            )
            
            ;; Update counters
            (var-set next-delegation-id (+ delegation-id u1))
            (var-set total-active-delegations (+ (var-get total-active-delegations) u1))
            
            (ok delegation-id)
        )
    )
)

(define-public (revoke-delegation (to principal))
    (let (
        (delegator tx-sender)
        (delegation (unwrap! (map-get? score-delegations { from: delegator, to: to }) ERR_DELEGATION_NOT_FOUND))
    )
        (asserts! (get is-active delegation) ERR_DELEGATION_NOT_FOUND)
        
        ;; Update delegation status
        (map-set score-delegations
            { from: delegator, to: to }
            (merge delegation { is-active: false })
        )
        
        (map-set delegation-details (get delegation-id delegation)
            (merge (unwrap! (map-get? delegation-details (get delegation-id delegation)) ERR_DELEGATION_NOT_FOUND)
                { is-active: false, revoked-at: (some stacks-block-height) }
            )
        )
        
        ;; Update counter
        (var-set total-active-delegations (- (var-get total-active-delegations) u1))
        
        (ok true)
    )
)

(define-public (extend-delegation (to principal) (additional-blocks uint))
    (let (
        (delegator tx-sender)
        (delegation (unwrap! (map-get? score-delegations { from: delegator, to: to }) ERR_DELEGATION_NOT_FOUND))
        (new-end-block (+ (get end-block delegation) additional-blocks))
    )
        (asserts! (get is-active delegation) ERR_DELEGATION_EXPIRED)
        (asserts! (>= (get end-block delegation) stacks-block-height) ERR_DELEGATION_EXPIRED)
        (asserts! (<= new-end-block (+ stacks-block-height MAX_DELEGATION_DURATION)) ERR_INVALID_DURATION)
        
        ;; Update delegation
        (map-set score-delegations
            { from: delegator, to: to }
            (merge delegation { end-block: new-end-block })
        )
        
        (map-set delegation-details (get delegation-id delegation)
            (merge (unwrap! (map-get? delegation-details (get delegation-id delegation)) ERR_DELEGATION_NOT_FOUND)
                { end-block: new-end-block }
            )
        )
        
        (ok new-end-block)
    )
)

;; Read-only functions
(define-read-only (get-delegation (from principal) (to principal))
    (map-get? score-delegations { from: from, to: to })
)

(define-read-only (get-delegation-by-id (delegation-id uint))
    (map-get? delegation-details delegation-id)
)

(define-read-only (is-delegation-active (from principal) (to principal))
    (match (map-get? score-delegations { from: from, to: to })
        delegation (and 
            (get is-active delegation)
            (>= (get end-block delegation) stacks-block-height)
        )
        false
    )
)

(define-read-only (get-active-boost-for-user (user principal))
    (let (
        (inbound-delegations (default-to (list) (map-get? user-inbound-delegations user)))
    )
        (fold calculate-total-boost inbound-delegations u0)
    )
)

(define-read-only (get-user-outbound-delegations (user principal))
    (default-to (list) (map-get? user-outbound-delegations user))
)

(define-read-only (get-user-inbound-delegations (user principal))
    (default-to (list) (map-get? user-inbound-delegations user))
)

(define-read-only (get-delegation-stats)
    {
        next-delegation-id: (var-get next-delegation-id),
        total-active-delegations: (var-get total-active-delegations),
        max-delegation-boost: MAX_DELEGATION_BOOST,
        min-delegator-score: MIN_DELEGATOR_SCORE
    }
)

(define-read-only (get-effective-credit-score-with-delegation (user principal))
    (match (contract-call? .Credstack get-credit-profile user)
        profile
            (let (
                (base-score (get credit-score profile))
                (delegation-boost (get-active-boost-for-user user))
            )
                (some (+ base-score delegation-boost))
            )
        none
    )
)

;; Private functions
(define-private (calculate-total-boost (delegation-id uint) (current-total uint))
    (match (map-get? delegation-details delegation-id)
        delegation 
            (if (and 
                (get is-active delegation)
                (>= (get end-block delegation) stacks-block-height)
            )
                (+ current-total (get boost-amount delegation))
                current-total
            )
        current-total
    )
)
