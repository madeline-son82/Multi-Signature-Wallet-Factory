;; Multi-Signature Wallet Factory
;; Creates and manages customizable multi-signature wallets

;; Constants
(define-constant CONTRACT_OWNER (as-contract tx-sender))
(define-constant ERR_UNAUTHORIZED (err u1000))
(define-constant ERR_INVALID_PARAMS (err u1001))
(define-constant ERR_WALLET_EXISTS (err u1002))
(define-constant ERR_WALLET_NOT_FOUND (err u1003))
(define-constant ERR_INVALID_THRESHOLD (err u1004))
(define-constant ERR_DUPLICATE_OWNER (err u1005))
(define-constant ERR_MAX_OWNERS_EXCEEDED (err u1006))
(define-constant ERR_INVALID_ROLE (err u1007))

;; Maximum number of owners per wallet
(define-constant MAX_OWNERS u50)

;; Role definitions
(define-constant ROLE_ADMIN u1)
(define-constant ROLE_OPERATOR u2)
(define-constant ROLE_USER u3)

;; Data structures
(define-map wallets
  { wallet-id: uint }
  {
    name: (string-ascii 64),
    owners: (list 50 principal),
    threshold: uint,
    created-at: uint,
    creator: principal,
    daily-limit: uint,
    daily-spent: uint,
    last-reset-day: uint,
    is-active: bool
  }
)

(define-map wallet-owners
  { wallet-id: uint, owner: principal }
  { role: uint, added-at: uint }
)

(define-map wallet-counter principal uint)
(define-data-var next-wallet-id uint u1)

;; Factory fee (in microSTX)
(define-data-var factory-fee uint u1000000) ;; 1 STX

;; Events
(define-data-var wallet-created-event
  {
    wallet-id: uint,
    name: (string-ascii 64),
    creator: principal,
    owners: (list 50 principal),
    threshold: uint,
    daily-limit: uint
  }
  {
    wallet-id: u0,
    name: "",
    creator: CONTRACT_OWNER,
    owners: (list),
    threshold: u0,
    daily-limit: u0
  }
)

;; Private functions
(define-private (validate-owners (owners (list 50 principal)))
  (let ((unique-owners (fold check-duplicate owners (list))))
    (and
      (> (len owners) u0)
      (<= (len owners) MAX_OWNERS)
      (is-eq (len owners) (len unique-owners))
    )
  )
)

(define-private (check-duplicate (owner principal) (acc (list 50 principal)))
  (if (is-none (index-of acc owner))
    (unwrap-panic (as-max-len? (append acc owner) u50))
    acc
  )
)

(define-private (validate-threshold (threshold uint) (owners-count uint))
  (and (> threshold u0) (<= threshold owners-count))
)

(define-private (get-current-day)
  (/ stacks-block-height u144) ;; Approximately 144 blocks per day
)

(define-private (is-valid-role (role uint))
  (or (is-eq role ROLE_ADMIN)
      (or (is-eq role ROLE_OPERATOR) (is-eq role ROLE_USER)))
)

(define-private (add-wallet-owner (owner principal) (wallet-id uint))
  (begin
    (map-set wallet-owners
      { wallet-id: wallet-id, owner: owner }
      { role: ROLE_USER, added-at: stacks-block-height }
    )
    wallet-id
  )
)

;; Read-only functions
(define-read-only (get-wallet (wallet-id uint))
  (map-get? wallets { wallet-id: wallet-id })
)

(define-read-only (get-wallet-owner-role (wallet-id uint) (owner principal))
  (map-get? wallet-owners { wallet-id: wallet-id, owner: owner })
)

(define-read-only (get-user-wallets (user principal))
  (default-to u0 (map-get? wallet-counter user))
)

(define-read-only (get-factory-fee)
  (var-get factory-fee)
)

(define-read-only (get-next-wallet-id)
  (var-get next-wallet-id)
)

(define-read-only (is-wallet-owner (wallet-id uint) (user principal))
  (is-some (map-get? wallet-owners { wallet-id: wallet-id, owner: user }))
)

(define-read-only (can-execute-transaction (wallet-id uint) (user principal) (amount uint))
  (match (get-wallet wallet-id)
    wallet
    (let (
      (owner-info (map-get? wallet-owners { wallet-id: wallet-id, owner: user }))
      (current-day (get-current-day))
      (daily-spent (if (is-eq (get last-reset-day wallet) current-day)
                     (get daily-spent wallet)
                     u0))
    )
      (and
        (get is-active wallet)
        (is-some owner-info)
        (or
          (is-eq (get role (unwrap-panic owner-info)) ROLE_ADMIN)
          (and
            (>= (get role (unwrap-panic owner-info)) ROLE_OPERATOR)
            (<= (+ daily-spent amount) (get daily-limit wallet))
          )
        )
      )
    )
    false
  )
)

;; Public functions
(define-public (create-multisig-wallet
  (name (string-ascii 64))
  (owners (list 50 principal))
  (threshold uint)
  (daily-limit uint)
)
  (let (
    (wallet-id (var-get next-wallet-id))
    (fee (var-get factory-fee))
    (owners-count (len owners))
  )
    (asserts! (validate-owners owners) ERR_INVALID_PARAMS)
    (asserts! (validate-threshold threshold owners-count) ERR_INVALID_THRESHOLD)
    (asserts! (> daily-limit u0) ERR_INVALID_PARAMS)
    (asserts! (> (len name) u0) ERR_INVALID_PARAMS)

    ;; Transfer fee to contract
    (if (> fee u0)
      (try! (stx-transfer? fee tx-sender (as-contract tx-sender)))
      true
    )

    ;; Create wallet
    (map-set wallets
      { wallet-id: wallet-id }
      {
        name: name,
        owners: owners,
        threshold: threshold,
        created-at: stacks-block-height,
        creator: tx-sender,
        daily-limit: daily-limit,
        daily-spent: u0,
        last-reset-day: (get-current-day),
        is-active: true
      }
    )

    ;; Add wallet owners with roles
    (fold add-wallet-owner owners wallet-id)

    ;; Set creator as admin
    (map-set wallet-owners
      { wallet-id: wallet-id, owner: tx-sender }
      { role: ROLE_ADMIN, added-at: stacks-block-height }
    )

    ;; Update counters
    (var-set next-wallet-id (+ wallet-id u1))
    (map-set wallet-counter tx-sender (+ (get-user-wallets tx-sender) u1))

    ;; Emit event
    (var-set wallet-created-event {
      wallet-id: wallet-id,
      name: name,
      creator: tx-sender,
      owners: owners,
      threshold: threshold,
      daily-limit: daily-limit
    })

    (ok wallet-id)
  )
)

(define-public (update-wallet-role (wallet-id uint) (owner principal) (new-role uint))
  (let ((wallet (unwrap! (get-wallet wallet-id) ERR_WALLET_NOT_FOUND)))
    (asserts! (is-wallet-owner wallet-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get role (unwrap! (get-wallet-owner-role wallet-id tx-sender) ERR_UNAUTHORIZED)) ROLE_ADMIN) ERR_UNAUTHORIZED)
    (asserts! (is-wallet-owner wallet-id owner) ERR_WALLET_NOT_FOUND)
    (asserts! (is-valid-role new-role) ERR_INVALID_ROLE)
    (asserts! (get is-active wallet) ERR_WALLET_NOT_FOUND)

    (map-set wallet-owners
      { wallet-id: wallet-id, owner: owner }
      { role: new-role, added-at: stacks-block-height }
    )

    (ok true)
  )
)

(define-public (update-daily-limit (wallet-id uint) (new-limit uint))
  (let ((wallet (unwrap! (get-wallet wallet-id) ERR_WALLET_NOT_FOUND)))
    (asserts! (is-wallet-owner wallet-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get role (unwrap! (get-wallet-owner-role wallet-id tx-sender) ERR_UNAUTHORIZED)) ROLE_ADMIN) ERR_UNAUTHORIZED)
    (asserts! (> new-limit u0) ERR_INVALID_PARAMS)
    (asserts! (get is-active wallet) ERR_WALLET_NOT_FOUND)

    (map-set wallets
      { wallet-id: wallet-id }
      (merge wallet { daily-limit: new-limit })
    )

    (ok true)
  )
)

(define-public (deactivate-wallet (wallet-id uint))
  (let ((wallet (unwrap! (get-wallet wallet-id) ERR_WALLET_NOT_FOUND)))
    (asserts! (is-wallet-owner wallet-id tx-sender) ERR_UNAUTHORIZED)
    (asserts! (is-eq (get role (unwrap! (get-wallet-owner-role wallet-id tx-sender) ERR_UNAUTHORIZED)) ROLE_ADMIN) ERR_UNAUTHORIZED)
    (asserts! (get is-active wallet) ERR_WALLET_NOT_FOUND)

    (map-set wallets
      { wallet-id: wallet-id }
      (merge wallet { is-active: false })
    )

    (ok true)
  )
)

;; Admin functions
(define-public (set-factory-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (var-set factory-fee new-fee)
    (ok true)
  )
)

(define-public (withdraw-fees (amount uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_UNAUTHORIZED)
    (as-contract (stx-transfer? amount tx-sender CONTRACT_OWNER))
  )
)
