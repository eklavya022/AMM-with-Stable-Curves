;; AMM with Stable Curves Contract
;; Specialized Automated Market Maker for stablecoin trading with reduced slippage

;; Define fungible tokens for the trading pair
(define-fungible-token token-a)
(define-fungible-token token-b)

;; Error constants
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-INVALID-AMOUNT (err u101))
(define-constant ERR-INSUFFICIENT-BALANCE (err u102))
(define-constant ERR-SLIPPAGE-TOO-HIGH (err u103))
(define-constant ERR-INSUFFICIENT-LIQUIDITY (err u104))
(define-constant ERR-INVALID-TOKEN (err u105))

;; Contract owner
(define-constant CONTRACT-OWNER tx-sender)

;; Pool reserves
(define-data-var reserve-a uint u0)
(define-data-var reserve-b uint u0)
(define-data-var total-lp-tokens uint u0)

;; Amplification parameter for stable curve (default: 100)
(define-data-var amplification uint u100)

;; Trading fee (0.3% = 3000 basis points out of 1,000,000)
(define-data-var trading-fee uint u3000)

;; LP token balances
(define-map lp-token-balances principal uint)

;; Read-only functions
(define-read-only (get-reserves)
  (ok {
    reserve-a: (var-get reserve-a),
    reserve-b: (var-get reserve-b),
    total-lp: (var-get total-lp-tokens)
  }))

(define-read-only (get-lp-balance (user principal))
  (ok (default-to u0 (map-get? lp-token-balances user))))

;; Function 1: Add Liquidity
(define-public (add-liquidity (amount-a uint) (amount-b uint) (min-lp-out uint))
  (let (
    (current-reserve-a (var-get reserve-a))
    (current-reserve-b (var-get reserve-b))
    (current-lp-supply (var-get total-lp-tokens))
    (lp-tokens-out
      (if (is-eq current-lp-supply u0)
        ;; First liquidity provision - use sum for stable pairs
        (+ amount-a amount-b)
        ;; Subsequent additions - maintain proportion
        (let (
          (share-a (/ (* amount-a current-lp-supply) current-reserve-a))
          (share-b (/ (* amount-b current-lp-supply) current-reserve-b)))
          (if (<= share-a share-b) share-a share-b)))))

    ;; Input validation
    (asserts! (> amount-a u0) ERR-INVALID-AMOUNT)
    (asserts! (> amount-b u0) ERR-INVALID-AMOUNT)
    (asserts! (>= lp-tokens-out min-lp-out) ERR-SLIPPAGE-TOO-HIGH)

    ;; Transfer tokens to contract
    (try! (ft-transfer? token-a amount-a tx-sender (as-contract tx-sender)))
    (try! (ft-transfer? token-b amount-b tx-sender (as-contract tx-sender)))

    ;; Update reserves
    (var-set reserve-a (+ current-reserve-a amount-a))
    (var-set reserve-b (+ current-reserve-b amount-b))
    (var-set total-lp-tokens (+ current-lp-supply lp-tokens-out))

    ;; Update LP balance
    (map-set lp-token-balances tx-sender 
      (+ (default-to u0 (map-get? lp-token-balances tx-sender)) lp-tokens-out))

    (ok {
      lp-tokens: lp-tokens-out,
      new-reserve-a: (var-get reserve-a),
      new-reserve-b: (var-get reserve-b)
    })))

;; Function 2: Stable Swap
(define-public (swap (amount-in uint) (token-in (string-ascii 1)) (min-amount-out uint))
  (let (
    (res-a (var-get reserve-a))
    (res-b (var-get reserve-b))
    (amp (var-get amplification))
    (fee-amount (/ (* amount-in (var-get trading-fee)) u1000000))
    (amount-after-fee (- amount-in fee-amount))
    (is-a-to-b (is-eq token-in "a"))
    
    ;; Calculate output using stable curve formula
    (amount-out
      (if is-a-to-b
        ;; Swapping A for B: simplified stable curve calculation
        (let (
          (new-res-a (+ res-a amount-after-fee))
          (invariant (+ res-a res-b (* amp res-a res-b (/ u1 (+ res-a res-b))))))
          (- res-b (- invariant new-res-a (* amp new-res-a (- invariant new-res-a) (/ u1 invariant)))))
        ;; Swapping B for A
        (let (
          (new-res-b (+ res-b amount-after-fee))
          (invariant (+ res-a res-b (* amp res-a res-b (/ u1 (+ res-a res-b))))))
          (- res-a (- invariant new-res-b (* amp new-res-b (- invariant new-res-b) (/ u1 invariant))))))))

    ;; Validation
    (asserts! (> amount-in u0) ERR-INVALID-AMOUNT)
    (asserts! (or (is-eq token-in "a") (is-eq token-in "b")) ERR-INVALID-TOKEN)
    (asserts! (> amount-out u0) ERR-INSUFFICIENT-LIQUIDITY)
    (asserts! (>= amount-out min-amount-out) ERR-SLIPPAGE-TOO-HIGH)

    ;; Execute swap
    (if is-a-to-b
      (begin
        (try! (ft-transfer? token-a amount-in tx-sender (as-contract tx-sender)))
        (try! (as-contract (ft-transfer? token-b amount-out (as-contract tx-sender) tx-sender)))
        (var-set reserve-a (+ res-a amount-in))
        (var-set reserve-b (- res-b amount-out)))
      (begin
        (try! (ft-transfer? token-b amount-in tx-sender (as-contract tx-sender)))
        (try! (as-contract (ft-transfer? token-a amount-out (as-contract tx-sender) tx-sender)))
        (var-set reserve-b (+ res-b amount-in))
        (var-set reserve-a (- res-a amount-out))))

    (ok {
      amount-out: amount-out,
      fee-paid: fee-amount,
      price-impact: (/ (* (- amount-in amount-out) u10000) amount-in)
    })))