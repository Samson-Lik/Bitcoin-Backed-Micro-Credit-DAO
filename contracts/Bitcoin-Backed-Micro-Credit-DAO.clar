(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-INSUFFICIENT-BALANCE (err u101))
(define-constant ERR-INVALID-AMOUNT (err u102))
(define-constant ERR-LOAN-EXISTS (err u103))
(define-constant ERR-NO-LOAN-EXISTS (err u104))
(define-constant ERR-LOAN-NOT-DUE (err u105))
(define-constant ERR-INSUFFICIENT-VOTES (err u106))

(define-data-var pool-balance uint u0)
(define-data-var total-loans uint u0)
(define-data-var min-reputation uint u100)

(define-map loans 
    { borrower: principal }
    { amount: uint, due-height: uint, status: (string-ascii 20) })

(define-map reputation 
    { user: principal }
    { score: uint })

(define-map votes
    { proposal-id: uint }
    { votes-for: uint, votes-against: uint, status: (string-ascii 20) })

(define-public (deposit-funds (amount uint))
    (begin
        (try! (stx-transfer? amount tx-sender (as-contract tx-sender)))
        (var-set pool-balance (+ (var-get pool-balance) amount))
        (ok true)))

(define-public (request-loan (amount uint) (duration uint))
    (let ((borrower-rep (default-to { score: u0 } (map-get? reputation { user: tx-sender }))))
        (asserts! (>= (get score borrower-rep) (var-get min-reputation)) ERR-NOT-AUTHORIZED)
        (asserts! (<= amount (var-get pool-balance)) ERR-INSUFFICIENT-BALANCE)
        (asserts! (is-none (map-get? loans { borrower: tx-sender })) ERR-LOAN-EXISTS)
        (try! (as-contract (stx-transfer? amount (as-contract tx-sender) tx-sender)))
        (map-set loans 
            { borrower: tx-sender }
            { amount: amount, 
              due-height: (+ stacks-block-height duration), 
              status: "active" })
        (var-set pool-balance (- (var-get pool-balance) amount))
        (var-set total-loans (+ (var-get total-loans) u1))
        (ok true)))
(define-public (repay-loan)
    (let ((loan (unwrap! (map-get? loans { borrower: tx-sender }) ERR-NO-LOAN-EXISTS)))
        (try! (stx-transfer? (get amount loan) tx-sender (as-contract tx-sender)))
        (map-delete loans { borrower: tx-sender })
        (var-set pool-balance (+ (var-get pool-balance) (get amount loan)))
        (map-set reputation 
            { user: tx-sender }
            { score: (+ (get score (default-to { score: u0 } (map-get? reputation { user: tx-sender }))) u10) })
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

(define-read-only (get-loan-details (borrower principal))
    (map-get? loans { borrower: borrower }))

(define-read-only (get-user-reputation (user principal))
    (default-to { score: u0 } (map-get? reputation { user: user })))

(define-read-only (get-pool-balance)
    (ok (var-get pool-balance)))
