;; Error constants
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-LOAN-EXISTS (err u103))
(define-constant ERR-NO-LOAN-EXISTS (err u104))
(define-constant ERR-LOAN-NOT-DUE (err u105))
(define-constant ERR-INSUFFICIENT-VOTES (err u106))
(define-constant ERR-INSUFFICIENT-COLLATERAL (err u107))
(define-constant ERR-COLLATERAL-LOCKED (err u108))

;; Data variables
(define-data-var pool-balance uint u0)
(define-data-var total-loans uint u0)
(define-data-var min-reputation uint u100)
(define-data-var collateral-ratio uint u15000)
(define-data-var total-collateral uint u0)

;; Data maps
(define-map loans 
    { borrower: principal }
    { amount: uint, due-height: uint, status: (string-ascii 20), collateral-amount: uint })

(define-map reputation 
    { user: principal }
    { score: uint })

(define-map votes
    { proposal-id: uint }
    { votes-for: uint, votes-against: uint, status: (string-ascii 20) })

(define-map collateral-balances
    { user: principal }
    { locked: uint, available: uint })

;; Private functions
(define-private (calculate-required-collateral (loan-amount uint))
    (/ (* loan-amount (var-get collateral-ratio)) u10000))

;; Public functions
(define-public (deposit-collateral (amount uint))
    (let ((current-balance (default-to { locked: u0, available: u0 } (map-get? collateral-balances { user: tx-sender }))))
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (map-set collateral-balances
            { user: tx-sender }
            { locked: (get locked current-balance),
              available: (+ (get available current-balance) amount) })
        (var-set total-collateral (+ (var-get total-collateral) amount))
        (ok true)))

(define-public (withdraw-collateral (amount uint))
    (let ((current-balance (unwrap! (map-get? collateral-balances { user: tx-sender }) ERR-INSUFFICIENT-BALANCE)))
        (asserts! (<= amount (get available current-balance)) ERR-INSUFFICIENT-BALANCE)
        (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
        (map-set collateral-balances
            { user: tx-sender }
            { locked: (get locked current-balance),
              available: (- (get available current-balance) amount) })
        (var-set total-collateral (- (var-get total-collateral) amount))
        (ok true)))

(define-public (deposit-funds (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set pool-balance (+ (var-get pool-balance) amount))
        (ok true)))

(define-public (request-loan (amount uint) (duration uint))
    (let ((borrower-rep (default-to { score: u0 } (map-get? reputation { user: tx-sender })))
          (required-collateral (calculate-required-collateral amount))
          (current-collateral (default-to { locked: u0, available: u0 } (map-get? collateral-balances { user: tx-sender }))))
        (asserts! (>= (get score borrower-rep) (var-get min-reputation)) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (var-get pool-balance)) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-none (map-get? loans { borrower: tx-sender })) ERR-LOAN-EXISTS)
        (asserts! (>= (get available current-collateral) required-collateral) ERR-INSUFFICIENT-COLLATERAL)
        (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
        (map-set loans 
            { borrower: tx-sender }
            { amount: amount, 
              due-height: (+ stacks-block-height duration), 
              status: "active",
              collateral-amount: required-collateral })
        (map-set collateral-balances
            { user: tx-sender }
            { locked: (+ (get locked current-collateral) required-collateral),
              available: (- (get available current-collateral) required-collateral) })
        (var-set pool-balance (- (var-get pool-balance) amount))
        (var-set total-loans (+ (var-get total-loans) u1))
        (ok true)))

(define-public (repay-loan)
    (let ((loan (unwrap! (map-get? loans { borrower: tx-sender }) ERR-NO-LOAN-EXISTS))
          (current-collateral (unwrap! (map-get? collateral-balances { user: tx-sender }) ERR-INSUFFICIENT-BALANCE)))
        (try! (stx-transfer? (get amount loan) tx-sender (as-contract tx-sender)))
        (map-delete loans { borrower: tx-sender })
        (map-set collateral-balances
            { user: tx-sender }
            { locked: (- (get locked current-collateral) (get collateral-amount loan)),
              available: (+ (get available current-collateral) (get collateral-amount loan)) })
        (var-set pool-balance (+ (var-get pool-balance) (get amount loan)))
        (map-set reputation 
            { user: tx-sender }
            { score: (+ (get score (default-to { score: u0 } (map-get? reputation { user: tx-sender }))) u10) })
        (ok true)))

(define-public (liquidate-collateral (borrower principal))
    (let ((loan (unwrap! (map-get? loans { borrower: borrower }) ERR-NO-LOAN-EXISTS))
          (borrower-collateral (unwrap! (map-get? collateral-balances { user: borrower }) ERR-INSUFFICIENT-BALANCE)))
        (asserts! (>= stacks-block-height (get due-height loan)) ERR-LOAN-NOT-DUE)
        (map-delete loans { borrower: borrower })
        (map-set collateral-balances
            { user: borrower }
            { locked: (- (get locked borrower-collateral) (get collateral-amount loan)),
              available: (get available borrower-collateral) })
        (var-set pool-balance (+ (var-get pool-balance) (get collateral-amount loan)))
        (var-set total-collateral (- (var-get total-collateral) (get collateral-amount loan)))
        (ok true)))

(define-public (create-default-proposal (borrower principal))
    (let ((loan (unwrap! (map-get? loans { borrower: borrower }) ERR-NO-LOAN-EXISTS)))
        (asserts! (>= stacks-block-height (get due-height loan)) ERR-LOAN-NOT-DUE)
        (map-set votes
            { proposal-id: (var-get total-loans) }
            { votes-for: u0, votes-against: u0, status: "active" })
        (ok true)))

(define-public (vote-on-default (proposal-id uint) (vote bool))
    (let ((proposal (unwrap! (map-get? votes { proposal-id: proposal-id }) ERR-NO-LOAN-EXISTS))
          (voter-rep (default-to { score: u0 } (map-get? reputation { user: tx-sender }))))
        (if vote
            (map-set votes 
                { proposal-id: proposal-id }
                { votes-for: (+ (get votes-for proposal) u1),
                  votes-against: (get votes-against proposal),
                  status: (get status proposal) })
            (map-set votes
                { proposal-id: proposal-id }
                { votes-for: (get votes-for proposal),
                  votes-against: (+ (get votes-against proposal) u1),
                  status: (get status proposal) }))
        (ok true)))

;; Read-only functions
(define-read-only (get-loan-details (borrower principal))
    (map-get? loans { borrower: borrower }))

(define-read-only (get-user-reputation (user principal))
    (default-to { score: u0 } (map-get? reputation { user: user })))

(define-read-only (get-pool-balance)
    (ok (var-get pool-balance)))

(define-read-only (get-collateral-balance (user principal))
    (default-to { locked: u0, available: u0 } (map-get? collateral-balances { user: user })))

(define-read-only (get-total-collateral)
    (ok (var-get total-collateral)))
