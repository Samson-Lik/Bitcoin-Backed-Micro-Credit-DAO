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
(define-constant ERR-INVALID-RISK-TIER (err u112))
(define-constant ERR-ADJUSTMENT-LIMIT-EXCEEDED (err u113))

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

;; ==========================================
;; LOAN ANALYTICS & PERFORMANCE TRACKING SYSTEM
;; ==========================================

;; Additional error constants for analytics
(define-constant ERR-ANALYTICS-NOT-INITIALIZED (err u200))
(define-constant ERR-INVALID-TIME-PERIOD (err u201))
(define-constant ERR-INSUFFICIENT-DATA (err u202))

;; Analytics data variables
(define-data-var total-loans-issued uint u0)
(define-data-var total-loans-repaid uint u0)
(define-data-var total-loans-defaulted uint u0)
(define-data-var total-interest-earned uint u0)
(define-data-var analytics-initialized bool false)
(define-data-var last-analytics-update uint u0)

;; Performance tracking maps
(define-map borrower-analytics
    { borrower: principal }
    { 
        total-borrowed: uint,
        total-repaid: uint,
        loans-count: uint,
        default-count: uint,
        avg-repayment-time: uint,
        performance-score: uint,
        last-loan-date: uint
    })

(define-map lending-pool-snapshots
    { snapshot-id: uint }
    {
        timestamp: uint,
        pool-size: uint,
        active-loans: uint,
        total-collateral: uint,
        utilization-rate: uint,
        avg-interest-rate: uint
    })

(define-map daily-analytics
    { date-block: uint }
    {
        loans-issued: uint,
        loans-repaid: uint,
        volume-issued: uint,
        volume-repaid: uint,
        interest-collected: uint,
        new-borrowers: uint
    })

;; Analytics initialization
(define-public (initialize-analytics)
    (begin
        (asserts! (not (var-get analytics-initialized)) ERR-ANALYTICS-NOT-INITIALIZED)
        (var-set analytics-initialized true)
        (var-set last-analytics-update stacks-block-height)
        (map-set lending-pool-snapshots
            { snapshot-id: u0 }
            {
                timestamp: stacks-block-height,
                pool-size: (var-get pool-balance),
                active-loans: (var-get total-loans),
                total-collateral: (var-get total-collateral),
                utilization-rate: (calculate-pool-utilization),
                avg-interest-rate: (var-get base-interest-rate)
            })
        (ok true)))

;; Update borrower analytics on loan request
(define-public (update-analytics-on-borrow (amount uint))
    (let ((current-analytics (default-to 
                                {
                                    total-borrowed: u0,
                                    total-repaid: u0,
                                    loans-count: u0,
                                    default-count: u0,
                                    avg-repayment-time: u0,
                                    performance-score: u100,
                                    last-loan-date: u0
                                }
                                (map-get? borrower-analytics { borrower: tx-sender }))))
        (map-set borrower-analytics
            { borrower: tx-sender }
            {
                total-borrowed: (+ (get total-borrowed current-analytics) amount),
                total-repaid: (get total-repaid current-analytics),
                loans-count: (+ (get loans-count current-analytics) u1),
                default-count: (get default-count current-analytics),
                avg-repayment-time: (get avg-repayment-time current-analytics),
                performance-score: (get performance-score current-analytics),
                last-loan-date: stacks-block-height
            })
        (var-set total-loans-issued (+ (var-get total-loans-issued) u1))
        (ok true)))

;; Update borrower analytics on loan repayment
(define-public (update-analytics-on-repay (amount uint))
    (let ((current-analytics (default-to 
                                {
                                    total-borrowed: u0,
                                    total-repaid: u0,
                                    loans-count: u0,
                                    default-count: u0,
                                    avg-repayment-time: u0,
                                    performance-score: u100,
                                    last-loan-date: u0
                                }
                                (map-get? borrower-analytics { borrower: tx-sender }))))
        (map-set borrower-analytics
            { borrower: tx-sender }
            {
                total-borrowed: (get total-borrowed current-analytics),
                total-repaid: (+ (get total-repaid current-analytics) amount),
                loans-count: (get loans-count current-analytics),
                default-count: (get default-count current-analytics),
                avg-repayment-time: (get avg-repayment-time current-analytics),
                performance-score: (if (> (+ (get performance-score current-analytics) u10) u1000) u1000 (+ (get performance-score current-analytics) u10)),
                last-loan-date: (get last-loan-date current-analytics)
            })
        (var-set total-loans-repaid (+ (var-get total-loans-repaid) u1))
        (ok true)))

;; Create daily analytics snapshot
(define-public (create-daily-snapshot)
    (let ((today-block (/ stacks-block-height u144))
          (current-snapshot (default-to
                                {
                                    loans-issued: u0,
                                    loans-repaid: u0,
                                    volume-issued: u0,
                                    volume-repaid: u0,
                                    interest-collected: u0,
                                    new-borrowers: u0
                                }
                                (map-get? daily-analytics { date-block: today-block }))))
        (map-set daily-analytics
            { date-block: today-block }
            current-snapshot)
        (var-set last-analytics-update stacks-block-height)
        (ok true)))

;; Analytics read-only functions
(define-read-only (get-borrower-performance (borrower principal))
    (let ((analytics (map-get? borrower-analytics { borrower: borrower })))
        (match analytics
            data (let ((repayment-rate (if (> (get loans-count data) u0) 
                                        (/ (* (- (get loans-count data) (get default-count data)) u10000) (get loans-count data))
                                        u0))
                       (avg-loan-size (if (> (get loans-count data) u0)
                                        (/ (get total-borrowed data) (get loans-count data))
                                        u0)))
                    (ok {
                        borrower: borrower,
                        total-borrowed: (get total-borrowed data),
                        total-repaid: (get total-repaid data),
                        loans-count: (get loans-count data),
                        default-count: (get default-count data),
                        repayment-rate: repayment-rate,
                        performance-score: (get performance-score data),
                        avg-loan-size: avg-loan-size,
                        last-activity: (get last-loan-date data)
                    }))
            ERR-INSUFFICIENT-DATA)))

(define-read-only (get-pool-analytics)
    (let ((utilization (calculate-pool-utilization))
          (default-rate (if (> (var-get total-loans-issued) u0)
                            (/ (* (var-get total-loans-defaulted) u10000) (var-get total-loans-issued))
                            u0))
          (avg-interest (if (> (var-get total-loans-issued) u0)
                            (/ (var-get total-interest-earned) (var-get total-loans-issued))
                            u0)))
        (ok {
            pool-balance: (var-get pool-balance),
            total-collateral: (var-get total-collateral),
            active-loans: (var-get total-loans),
            total-loans-issued: (var-get total-loans-issued),
            total-loans-repaid: (var-get total-loans-repaid),
            total-loans-defaulted: (var-get total-loans-defaulted),
            utilization-rate: utilization,
            default-rate: default-rate,
            total-interest-earned: (var-get total-interest-earned),
            avg-interest-rate: avg-interest
        })))

(define-read-only (get-daily-analytics (date-block uint))
    (match (map-get? daily-analytics { date-block: date-block })
        data (ok data)
        ERR-INSUFFICIENT-DATA))

(define-read-only (calculate-risk-score (borrower principal))
    (match (map-get? borrower-analytics { borrower: borrower })
        data (let ((default-rate (if (> (get loans-count data) u0)
                                    (/ (* (get default-count data) u10000) (get loans-count data))
                                    u0))
                   (activity-score (if (> stacks-block-height (get last-loan-date data))
                                     (let ((score-calc (- u1000 (/ (- stacks-block-height (get last-loan-date data)) u1440))))
                                         (if (< score-calc u0) u0 score-calc))
                                     u1000))
                   (volume-score (let ((vol-calc (/ (get total-borrowed data) u1000000)))
                                    (if (> vol-calc u1000) u1000 vol-calc)))
                   (composite-score (/ (+ (get performance-score data) activity-score volume-score) u3)))
                (ok {
                    borrower: borrower,
                    risk-score: (- u1000 (if (> default-rate u1000) u1000 default-rate)),
                    performance-score: (get performance-score data),
                    activity-score: activity-score,
                    volume-score: volume-score,
                    composite-score: composite-score,
                    recommendation: (if (> composite-score u750) "low-risk" 
                                       (if (> composite-score u500) "medium-risk" "high-risk"))
                }))
        ERR-INSUFFICIENT-DATA))

(define-read-only (get-lending-trends (days-back uint))
    (let ((start-block (let ((calc-start (- (/ stacks-block-height u144) days-back)))
                          (if (< calc-start u0) u0 calc-start)))
          (end-block (/ stacks-block-height u144)))
        (ok {
            period-start: start-block,
            period-end: end-block,
            days-analyzed: (- end-block start-block),
            current-utilization: (calculate-pool-utilization),
            avg-daily-volume: (if (> days-back u0) (/ (var-get total-interest-earned) days-back) u0)
        })))

(define-read-only (get-system-health)
    (let ((pool-health (if (> (var-get pool-balance) u0) u100 u0))
          (collateral-health (if (> (var-get total-collateral) (* (var-get pool-balance) u2)) u100 u50))
          (loan-health (if (> (var-get total-loans-issued) u0)
                          (- u100 (/ (* (var-get total-loans-defaulted) u100) (var-get total-loans-issued)))
                          u100)))
        (ok {
            overall-health: (/ (+ pool-health collateral-health loan-health) u3),
            pool-health: pool-health,
            collateral-health: collateral-health,
            loan-performance: loan-health,
            analytics-active: (var-get analytics-initialized),
            last-update: (var-get last-analytics-update)
        })))

;; ==========================================
;; DYNAMIC RISK-BASED COLLATERAL ADJUSTMENT SYSTEM
;; ==========================================

(define-data-var risk-adjustment-enabled bool true)
(define-data-var max-collateral-discount-rate uint u3000)
(define-data-var min-collateral-premium-rate uint u1000)

(define-map risk-tier-multipliers
    { tier: (string-ascii 10) }
    { multiplier: uint })

(define-private (get-borrower-risk-tier (composite-score uint))
    (if (>= composite-score u800) "tier-1" 
        (if (>= composite-score u600) "tier-2" 
            (if (>= composite-score u400) "tier-3" "tier-4"))))

(define-private (get-risk-multiplier (risk-tier (string-ascii 10)))
    (let ((tier-multiplier (default-to { multiplier: u10000 } (map-get? risk-tier-multipliers { tier: risk-tier }))))
        (get multiplier tier-multiplier)))

(define-private (calculate-adjusted-collateral (loan-amount uint) (borrower-reputation uint))
    (let ((base-collateral (calculate-required-collateral loan-amount))
          (utilization (calculate-pool-utilization))
          (reputation-bonus (if (> borrower-reputation u300) u2500 (if (> borrower-reputation u150) u5000 u7500)))
          (market-adjustment (if (> utilization u7000) u11000 u10000))
          (adjusted (/ (* base-collateral reputation-bonus market-adjustment) u100000000)))
        (if (< adjusted base-collateral) adjusted base-collateral)))

(define-public (initialize-risk-tiers)
    (begin
        (map-set risk-tier-multipliers { tier: "tier-1" } { multiplier: u7000 })
        (map-set risk-tier-multipliers { tier: "tier-2" } { multiplier: u8500 })
        (map-set risk-tier-multipliers { tier: "tier-3" } { multiplier: u9500 })
        (map-set risk-tier-multipliers { tier: "tier-4" } { multiplier: u11500 })
        (ok true)))

(define-public (request-loan-with-dynamic-collateral (amount uint) (duration uint))
    (let ((borrower-rep (default-to { score: u0 } (map-get? reputation { user: tx-sender })))
          (current-collateral (default-to { locked: u0, available: u0 } (map-get? collateral-balances { user: tx-sender })))
          (pool-utilization (calculate-pool-utilization))
          (loan-interest-rate (calculate-interest-rate (get score borrower-rep) pool-utilization))
          (dynamic-collateral-required (if (var-get risk-adjustment-enabled) 
                                          (calculate-adjusted-collateral amount (get score borrower-rep))
                                          (calculate-required-collateral amount))))
        (asserts! (>= (get score borrower-rep) (var-get min-reputation)) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (var-get pool-balance)) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-none (map-get? loans { borrower: tx-sender })) ERR-LOAN-EXISTS)
        (asserts! (>= (get available current-collateral) dynamic-collateral-required) ERR-INSUFFICIENT-COLLATERAL)
        (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
        (map-set loans 
            { borrower: tx-sender }
            { amount: amount, 
              due-height: (+ stacks-block-height duration), 
              status: "active",
              collateral-amount: dynamic-collateral-required,
              interest-rate: loan-interest-rate,
              accrued-interest: u0,
              extensions-used: u0 })
        (map-set collateral-balances
            { user: tx-sender }
            { locked: (+ (get locked current-collateral) dynamic-collateral-required),
              available: (- (get available current-collateral) dynamic-collateral-required) })
        (var-set pool-balance (- (var-get pool-balance) amount))
        (var-set total-loans (+ (var-get total-loans) u1))
        (ok true)))

(define-read-only (get-dynamic-collateral-requirement (borrower principal) (amount uint))
    (if (var-get risk-adjustment-enabled)
        (let ((borrower-rep (default-to { score: u0 } (map-get? reputation { user: borrower }))))
            (ok (calculate-adjusted-collateral amount (get score borrower-rep))))
        (ok (calculate-required-collateral amount))))

(define-read-only (get-borrower-risk-assessment (borrower principal))
    (match (calculate-risk-score borrower)
        risk-data (let ((composite (get composite-score risk-data))
                        (tier (get-borrower-risk-tier composite)))
                    (ok {
                        risk-tier: tier,
                        collateral-multiplier: (get-risk-multiplier tier),
                        composite-score: composite
                    }))
        err-val (err err-val)))

(define-public (set-risk-adjustment-status (enabled bool))
    (begin
        (var-set risk-adjustment-enabled enabled)
        (ok true)))

(define-public (set-max-collateral-discount (rate uint))
    (begin
        (asserts! (<= rate u5000) ERR-ADJUSTMENT-LIMIT-EXCEEDED)
        (var-set max-collateral-discount-rate rate)
        (ok true)))

(define-read-only (get-risk-adjustment-config)
    (ok {
        enabled: (var-get risk-adjustment-enabled),
        max-discount: (var-get max-collateral-discount-rate),
        min-premium: (var-get min-collateral-premium-rate)
    }))