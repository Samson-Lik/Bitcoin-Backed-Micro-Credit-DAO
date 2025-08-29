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
(define-constant ERR-INSUFFICIENT-INTEREST-PAYMENT (err u109))
(define-constant ERR-MAX-EXTENSIONS-REACHED (err u110))
(define-constant ERR-EXTENSION-NOT-ALLOWED (err u111))

;; Data variables
(define-data-var pool-balance uint u0)
(define-data-var total-loans uint u0)
(define-data-var min-reputation uint u100)
(define-data-var collateral-ratio uint u15000)
(define-data-var total-collateral uint u0)
(define-data-var base-interest-rate uint u500)
(define-data-var max-interest-rate uint u2000)
(define-data-var extension-fee-rate uint u300)
(define-data-var max-extensions-per-loan uint u3)

;; Data maps
(define-map loans 
    { borrower: principal }
    { amount: uint, due-height: uint, status: (string-ascii 20), collateral-amount: uint, interest-rate: uint, accrued-interest: uint, extensions-used: uint })

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

(define-private (calculate-interest-rate (borrower-reputation uint) (pool-utilization uint))
    (let ((base-rate (var-get base-interest-rate))
          (max-rate (var-get max-interest-rate))
          (reputation-factor (if (> borrower-reputation u200) u9000 (if (> borrower-reputation u100) u9500 u10000)))
          (utilization-factor (+ u10000 (* pool-utilization u50)))
          (calculated-rate (/ (* base-rate reputation-factor utilization-factor) u100000000)))
        (if (> calculated-rate max-rate) max-rate calculated-rate)))

(define-private (calculate-pool-utilization)
    (if (> (var-get total-collateral) u0)
        (/ (* (- (var-get total-collateral) (var-get pool-balance)) u10000) (var-get total-collateral))
        u0))

(define-private (calculate-accrued-interest (principal-amount uint) (interest-rate uint) (blocks-elapsed uint))
    (/ (* (* principal-amount interest-rate) blocks-elapsed) u5256000))

(define-private (calculate-extension-fee (loan-amount uint))
    (/ (* loan-amount (var-get extension-fee-rate)) u10000))

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
          (current-collateral (default-to { locked: u0, available: u0 } (map-get? collateral-balances { user: tx-sender })))
          (pool-utilization (calculate-pool-utilization))
          (loan-interest-rate (calculate-interest-rate (get score borrower-rep) pool-utilization)))
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
              collateral-amount: required-collateral,
              interest-rate: loan-interest-rate,
              accrued-interest: u0,
              extensions-used: u0 })
        (map-set collateral-balances
            { user: tx-sender }
            { locked: (+ (get locked current-collateral) required-collateral),
              available: (- (get available current-collateral) required-collateral) })
        (var-set pool-balance (- (var-get pool-balance) amount))
        (var-set total-loans (+ (var-get total-loans) u1))
        (ok true)))

(define-public (repay-loan)
    (let ((loan (unwrap! (map-get? loans { borrower: tx-sender }) ERR-NO-LOAN-EXISTS))
          (current-collateral (unwrap! (map-get? collateral-balances { user: tx-sender }) ERR-INSUFFICIENT-BALANCE))
          (blocks-elapsed (- stacks-block-height (- (get due-height loan) u144)))
          (interest-owed (calculate-accrued-interest (get amount loan) (get interest-rate loan) blocks-elapsed))
          (total-repayment (+ (get amount loan) interest-owed)))
        (asserts! (>= (stx-get-balance tx-sender) total-repayment) ERR-INSUFFICIENT-BALANCE)
        (try! (stx-transfer? total-repayment tx-sender (as-contract tx-sender)))
        (map-delete loans { borrower: tx-sender })
        (map-set collateral-balances
            { user: tx-sender }
            { locked: (- (get locked current-collateral) (get collateral-amount loan)),
              available: (+ (get available current-collateral) (get collateral-amount loan)) })
        (var-set pool-balance (+ (var-get pool-balance) total-repayment))
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

(define-public (pay-interest)
    (let ((loan (unwrap! (map-get? loans { borrower: tx-sender }) ERR-NO-LOAN-EXISTS))
          (blocks-elapsed (- stacks-block-height (- (get due-height loan) u144)))
          (interest-owed (calculate-accrued-interest (get amount loan) (get interest-rate loan) blocks-elapsed)))
        (asserts! (>= (stx-get-balance tx-sender) interest-owed) ERR-INSUFFICIENT-INTEREST-PAYMENT)
        (try! (stx-transfer? interest-owed tx-sender (as-contract tx-sender)))
        (map-set loans
            { borrower: tx-sender }
            { amount: (get amount loan),
              due-height: (get due-height loan),
              status: (get status loan),
              collateral-amount: (get collateral-amount loan),
              interest-rate: (get interest-rate loan),
              accrued-interest: (+ (get accrued-interest loan) interest-owed),
              extensions-used: (get extensions-used loan) })
        (var-set pool-balance (+ (var-get pool-balance) interest-owed))
        (ok true)))

(define-read-only (get-current-interest-rate (borrower-reputation uint))
    (let ((pool-utilization (calculate-pool-utilization)))
        (ok (calculate-interest-rate borrower-reputation pool-utilization))))

(define-read-only (get-loan-interest-owed (borrower principal))
    (match (map-get? loans { borrower: borrower })
        loan (let ((blocks-elapsed (- stacks-block-height (- (get due-height loan) u144))))
                (ok (calculate-accrued-interest (get amount loan) (get interest-rate loan) blocks-elapsed)))
        ERR-NO-LOAN-EXISTS))

(define-public (extend-loan (extension-blocks uint))
    (let ((loan (unwrap! (map-get? loans { borrower: tx-sender }) ERR-NO-LOAN-EXISTS))
          (extension-fee (calculate-extension-fee (get amount loan)))
          (new-extensions-count (+ (get extensions-used loan) u1)))
        (asserts! (< (get extensions-used loan) (var-get max-extensions-per-loan)) ERR-MAX-EXTENSIONS-REACHED)
        (asserts! (< stacks-block-height (get due-height loan)) ERR-EXTENSION-NOT-ALLOWED)
        (asserts! (>= (stx-get-balance tx-sender) extension-fee) ERR-INSUFFICIENT-BALANCE)
        (try! (stx-transfer? extension-fee tx-sender (as-contract tx-sender)))
        (map-set loans
            { borrower: tx-sender }
            { amount: (get amount loan),
              due-height: (+ (get due-height loan) extension-blocks),
              status: (get status loan),
              collateral-amount: (get collateral-amount loan),
              interest-rate: (get interest-rate loan),
              accrued-interest: (get accrued-interest loan),
              extensions-used: new-extensions-count })
        (var-set pool-balance (+ (var-get pool-balance) extension-fee))
        (ok true)))

(define-read-only (get-extension-eligibility (borrower principal))
    (match (map-get? loans { borrower: borrower })
        loan (let ((current-extensions (get extensions-used loan))
                   (max-extensions (var-get max-extensions-per-loan))
                   (loan-due (get due-height loan)))
                (ok { 
                    eligible: (and 
                        (< current-extensions max-extensions)
                        (< stacks-block-height loan-due)),
                    extensions-used: current-extensions,
                    extensions-remaining: (- max-extensions current-extensions),
                    extension-fee: (calculate-extension-fee (get amount loan)) }))
        ERR-NO-LOAN-EXISTS))

(define-read-only (get-total-collateral)
    (ok (var-get total-collateral)))
