;; EcoSphere Guilds - Blockchain Gaming Ecosystem
;; A smart contract for managing ecological guilds and adaptive proof-of-contribution

;; Constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_GUILD_NOT_FOUND (err u101))
(define-constant ERR_INSUFFICIENT_BALANCE (err u102))
(define-constant ERR_INVALID_CONTRIBUTION (err u103))
(define-constant ERR_ECOSYSTEM_CRITICAL (err u104))
(define-constant MAX_ECOSYSTEM_HEALTH u100)
(define-constant MIN_ECOSYSTEM_HEALTH u10)
(define-constant BASE_REWARD u1000)

;; Data Variables
(define-data-var next-guild-id uint u1)
(define-data-var global-ecosystem-health uint u100)
(define-data-var total-contribution-pool uint u0)

;; Guild Structure
(define-map guilds
  { guild-id: uint }
  {
    name: (string-ascii 64),
    leader: principal,
    members: uint,
    ecosystem-score: uint,
    total-contributions: uint,
    created-at: uint
  }
)

;; Player Data
(define-map players
  { player: principal }
  {
    guild-id: uint,
    contribution-score: uint,
    total-rewards: uint,
    last-action: uint,
    reputation: uint
  }
)

;; Ecosystem Actions Tracking
(define-map ecosystem-actions
  { action-id: uint }
  {
    player: principal,
    guild-id: uint,
    action-type: (string-ascii 32),
    impact-score: int,
    timestamp: uint
  }
)

;; Guild Memberships
(define-map guild-members
  { guild-id: uint, player: principal }
  { joined-at: uint, active: bool }
)

;; Token balances for rewards
(define-map token-balances
  { owner: principal }
  { balance: uint }
)

;; Private Functions

;; Calculate adaptive reward based on ecosystem health and contribution
(define-private (calculate-reward (contribution uint) (ecosystem-health uint))
  (let ((health-multiplier (/ (* ecosystem-health u10) u100))
        (base-calc (* contribution BASE_REWARD)))
    (/ (* base-calc health-multiplier) u10)))

;; Update global ecosystem health based on all guild scores
(define-private (update-global-ecosystem-health)
  (let ((current-health (var-get global-ecosystem-health)))
    ;; Simplified calculation - in real implementation would aggregate all guild scores
    (if (> current-health MIN_ECOSYSTEM_HEALTH)
      (var-set global-ecosystem-health (- current-health u1))
      (var-set global-ecosystem-health MIN_ECOSYSTEM_HEALTH))))

;; Public Functions

;; Create a new guild
(define-public (create-guild (name (string-ascii 64)))
  (let ((guild-id (var-get next-guild-id)))
    (begin
      (map-set guilds
        { guild-id: guild-id }
        {
          name: name,
          leader: tx-sender,
          members: u1,
          ecosystem-score: u50,
          total-contributions: u0,
          created-at: block-height
        })
      (map-set guild-members
        { guild-id: guild-id, player: tx-sender }
        { joined-at: block-height, active: true })
      (map-set players
        { player: tx-sender }
        {
          guild-id: guild-id,
          contribution-score: u0,
          total-rewards: u0,
          last-action: block-height,
          reputation: u10
        })
      (var-set next-guild-id (+ guild-id u1))
      (ok guild-id))))

;; Join an existing guild
(define-public (join-guild (guild-id uint))
  (match (map-get? guilds { guild-id: guild-id })
    guild-data
    (begin
      (map-set guild-members
        { guild-id: guild-id, player: tx-sender }
        { joined-at: block-height, active: true })
      (map-set players
        { player: tx-sender }
        {
          guild-id: guild-id,
          contribution-score: u0,
          total-rewards: u0,
          last-action: block-height,
          reputation: u5
        })
      (map-set guilds
        { guild-id: guild-id }
        (merge guild-data { members: (+ (get members guild-data) u1) }))
      (ok true))
    ERR_GUILD_NOT_FOUND))

;; Submit an ecosystem action (positive or negative impact)
(define-public (submit-ecosystem-action (action-type (string-ascii 32)) (impact-score int))
  (match (map-get? players { player: tx-sender })
    player-data
    (let ((guild-id (get guild-id player-data))
          (current-ecosystem (var-get global-ecosystem-health)))
      (begin
        ;; Validate ecosystem isn't in critical state for negative actions
        (asserts! (or (> impact-score 0) (> current-ecosystem u20)) ERR_ECOSYSTEM_CRITICAL)
        
        ;; Update player contribution score
        (map-set players
          { player: tx-sender }
          (merge player-data {
            contribution-score: (if (> impact-score 0)
              (+ (get contribution-score player-data) (to-uint impact-score))
              (get contribution-score player-data)),
            last-action: block-height
          }))
        
        ;; Update guild ecosystem score
        (match (map-get? guilds { guild-id: guild-id })
          guild-data
          (let ((new-score (if (> impact-score 0)
                            (+ (get ecosystem-score guild-data) (to-uint (/ impact-score 10)))
                            (if (>= (get ecosystem-score guild-data) (to-uint (/ (- 0 impact-score) 10)))
                              (- (get ecosystem-score guild-data) (to-uint (/ (- 0 impact-score) 10)))
                              u0))))
            (map-set guilds
              { guild-id: guild-id }
              (merge guild-data { ecosystem-score: new-score })))
          true)
        
        ;; Update global ecosystem health
        (update-global-ecosystem-health)
        (ok true)))
    ERR_NOT_AUTHORIZED))

;; Claim rewards based on contribution
(define-public (claim-rewards)
  (match (map-get? players { player: tx-sender })
    player-data
    (let ((contribution (get contribution-score player-data))
          (ecosystem-health (var-get global-ecosystem-health))
          (reward-amount (calculate-reward contribution ecosystem-health)))
      (begin
        (asserts! (> contribution u0) ERR_INVALID_CONTRIBUTION)
        
        ;; Update player balance
        (map-set token-balances
          { owner: tx-sender }
          { balance: (+ (default-to u0 (get balance (map-get? token-balances { owner: tx-sender }))) reward-amount) })
        
        ;; Reset contribution score after claiming
        (map-set players
          { player: tx-sender }
          (merge player-data {
            contribution-score: u0,
            total-rewards: (+ (get total-rewards player-data) reward-amount)
          }))
        
        (ok reward-amount)))
    ERR_NOT_AUTHORIZED))

;; Transfer tokens between players
(define-public (transfer (amount uint) (recipient principal))
  (let ((sender-balance (default-to u0 (get balance (map-get? token-balances { owner: tx-sender }))))
        (recipient-balance (default-to u0 (get balance (map-get? token-balances { owner: recipient })))))
    (begin
      (asserts! (>= sender-balance amount) ERR_INSUFFICIENT_BALANCE)
      
      (map-set token-balances
        { owner: tx-sender }
        { balance: (- sender-balance amount) })
      
      (map-set token-balances
        { owner: recipient }
        { balance: (+ recipient-balance amount) })
      
      (ok true))))

;; Emergency ecosystem restoration (only contract owner)
(define-public (restore-ecosystem (new-health uint))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (<= new-health MAX_ECOSYSTEM_HEALTH) (err u105))
    (var-set global-ecosystem-health new-health)
    (ok true)))

;; Read-only functions

;; Get guild information
(define-read-only (get-guild (guild-id uint))
  (map-get? guilds { guild-id: guild-id }))

;; Get player information
(define-read-only (get-player (player principal))
  (map-get? players { player: player }))

;; Get current ecosystem health
(define-read-only (get-ecosystem-health)
  (var-get global-ecosystem-health))

;; Get player balance
(define-read-only (get-balance (owner principal))
  (default-to u0 (get balance (map-get? token-balances { owner: owner }))))

;; Check if player is guild member
(define-read-only (is-guild-member (guild-id uint) (player principal))
  (match (map-get? guild-members { guild-id: guild-id, player: player })
    membership (get active membership)
    false))

;; Get total number of guilds
(define-read-only (get-guild-count)
  (- (var-get next-guild-id) u1))

;; Calculate potential reward for a player
(define-read-only (calculate-potential-reward (player principal))
  (match (map-get? players { player: player })
    player-data
    (let ((contribution (get contribution-score player-data))
          (ecosystem-health (var-get global-ecosystem-health)))
      (some (calculate-reward contribution ecosystem-health)))
    none))