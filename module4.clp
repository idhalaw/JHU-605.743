(deffacts initial
    (stock "Tesla" TSLA)
    (stock-pe TSLA 75.01)
    (stock-de TSLA 0.08)
    (stock-pb TSLA 25.02)
    (stock-peg TSLA 2.4)
    (stock-beta TSLA 2.17)
    (stock-dividend-yield TSLA 0)
    (stock-fcf-current-yr TSLA 1557)
    (stock-fcf-prior-yr TSLA 619)
    (belongs-to-industry TSLA automotive)
    (industry-rank TSLA 65)

    (stock "General Motors" GM)
    (stock-pe GM 5.28)
    (stock-de GM 1.14)
    (stock-pb GM 0.77)
    (stock-peg GM 0.54)
    (stock-beta GM 1.27)
    (stock-dividend-yield GM 0)
    (stock-fcf-current-yr GM -2257)
    (stock-fcf-prior-yr GM 5212)
    (belongs-to-industry GM automotive)
    (industry-rank GM 64)

    (stock "Ford Motor" F)
    (stock-pe F 7.9)
    (stock-de F 1.92)
    (stock-pb F 1.39)
    (stock-peg F 0.87)
    (stock-beta F 1.33)
    (stock-dividend-yield F 2.61)
    (stock-fcf-current-yr F 1248)
    (stock-fcf-prior-yr F -757)
    (belongs-to-industry F automotive)
    (industry-rank F 22)

    (stock "United Airlines" UAL)
    (stock-pe UAL 93.23)
    (stock-de UAL 11.77)
    (stock-pb UAL 3.11)
    (stock-peg UAL 0)
    (stock-beta UAL 1.29)
    (stock-dividend-yield UAL 0)
    (stock-fcf-current-yr UAL 2213)
    (stock-fcf-prior-yr UAL 1814)
    (belongs-to-industry UAL airline)
    (industry-rank UAL 146)

    (stock "Delta Airlines" DAL)
    (stock-pe DAL 11.36)
    (stock-de DAL 18.94)
    (stock-pb DAL 5.35)
    (stock-peg DAL 0)
    (stock-beta DAL 1.20)
    (stock-dividend-yield DAL 0)
    (stock-fcf-current-yr DAL 1577)
    (stock-fcf-prior-yr DAL 1105)
    (belongs-to-industry DAL airline)
    (industry-rank DAL 113)

    (stock "Southwest Airlines" LUV)
    (stock-pe LUV 14.52)
    (stock-de LUV 1.03)
    (stock-pb LUV 2.04)
    (stock-peg LUV 2.42)
    (stock-beta LUV 1)
    (stock-dividend-yield LUV 0)
    (stock-fcf-current-yr LUV 837)
    (stock-fcf-prior-yr LUV 634)
    (belongs-to-industry LUV airline)
    (industry-rank LUV 234)

    (stock "Apple" AAPL)
    (stock-pe AAPL 26.45)
    (stock-de AAPL 1.63)
    (stock-pb AAPL 44.67)
    (stock-peg AAPL 2.09)
    (stock-beta AAPL 1.23)
    (stock-dividend-yield AAPL 0.57)
    (stock-fcf-current-yr AAPL 107582)
    (stock-fcf-prior-yr AAPL 94768)
    (belongs-to-industry AAPL technology)
    (industry-rank AAPL 3)

    (stock "Google" GOOGL)
    (stock-pe GOOGL 22.07)
    (stock-de GOOGL 0.06)
    (stock-pb GOOGL 5.94)
    (stock-peg GOOGL 1.86)
    (stock-beta GOOGL 1.08)
    (stock-dividend-yield GOOGL 0)
    (stock-fcf-current-yr GOOGL 12594)
    (stock-fcf-prior-yr GOOGL 16394)
    (belongs-to-industry GOOGL technology)
    (industry-rank GOOGL 8)

    (stock "IBM" IBM)
    (stock-pe IBM 13.92)
    (stock-de IBM 2.28)
    (stock-pb IBM 6.11)
    (stock-peg IBM 1.99)
    (stock-beta IBM 0.85)
    (stock-dividend-yield IBM 5.01)
    (stock-fcf-current-yr IBM 1000)
    (stock-fcf-prior-yr IBM 2141)
    (belongs-to-industry IBM technology)
    (industry-rank IBM 49)

    (industry-avg-de automotive 2.04)
    (industry-avg-de airline 5.5)
    (industry-avg-de technology 0.3)

    (industry-avg-pe automotive 21.14)
    (industry-avg-pe airline 16.23)
    (industry-avg-pe technology 24.31)

    (industry-avg-dividend automotive 0.84)
    (industry-avg-dividend airline 0.9)
    (industry-avg-dividend technology 0)
)

(defrule investment-goal-is
    (not (investment-goal ?))
    =>
    (printout t "Which investment strategy do you prefer: Longer horizon with lower risk (value), or High risk / high reward (growth)?")
    (assert (investment-goal (read))))

(defrule investment-goal-is-value
    (investment-goal value)
    (best-value-stock ?ticker ?)
    (stock ?name ?ticker)
    =>
    (printout t "Best value stock recommendation: " ?name "(" ?ticker ")" crlf))

(defrule investment-goal-is-growth
    (investment-goal growth)
    (best-growth-stock ?ticker ?)
    (stock ?name ?ticker)
    =>
    (printout t "Best growth stock recommendation: " ?name "(" ?ticker ")" crlf))

(defrule best-value-stock-initial
    (stock ?name ?ticker)
    (stock-category ?ticker "value")
    (stock-category-score ?ticker ?score)
    (not (best-value-stock ? ?))
    =>
    (assert (best-value-stock ?ticker ?score)))

(defrule best-value-stock-update
    (stock ?name ?ticker)
    (stock-category ?ticker "value")
    (stock-category-score ?ticker ?score)
    ?fact <- (best-value-stock ? ?bestscore)
    (test (> ?score ?bestscore))
    =>
    (assert (best-value-stock ?ticker ?score))
    (retract ?fact))

(defrule best-growth-stock-initial
    (stock ?name ?ticker)
    (stock-category ?ticker growth)
    (stock-category-score ?ticker ?score)
    (not (best-growth-stock ? ?))
    =>
    (assert (best-growth-stock ?ticker ?score)))

(defrule best-growth-stock-update
    (stock ?name ?ticker)
    (stock-category ?ticker growth)
    (stock-category-score ?ticker ?score)
    ?fact <- (best-growth-stock ? ?bestscore)
    (test (> ?score ?bestscore))
    =>
    (assert (best-growth-stock ?ticker ?score))
    (retract ?fact))

(defrule stock-category-is-value
    (stock ?name ?ticker)
    (stock-risk ?ticker ?risk)
    (stock-efficiency ?ticker ?efficiency)
    (not (stock-category ?ticker ?))
    (stock-value ?ticker undervalued)
    (test (> ?risk 0))
    (test (> ?efficiency 0))
    =>
    (assert (stock-category ?ticker "value"))
    (assert (stock-category-score ?ticker (+ ?risk ?efficiency))))

(defrule stock-category-is-growth
    (stock ?name ?ticker)
    (stock-risk ?ticker ?risk)
    (stock-efficiency ?ticker ?efficiency)
    (not (stock-category ?ticker ?))
    (stock-value ?ticker overvalued)
    (industry-rank ?ticker ?rank)
    (test (<= ?rank 100))
    (stock-fcf-current-yr ?ticker ?current)
    (stock-fcf-prior-yr ?ticker ?prior)
    (test (> ?current ?prior))
    =>
    (assert (stock-category ?ticker growth))
    (assert (stock-category-score ?ticker (+ ?risk ?efficiency))))

(defrule stock-value-is-undervalued
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (industry-avg-pe ?industry ?avgpe)
    (not (stock-value ?ticker ?))
    (stock-pe ?ticker ?pe)
    (stock-pb ?ticker ?pb)
    (stock-peg ?ticker ?peg)
    (test (<= ?peg 1))
    (test (<= ?pb 3))
    (test (<= ?pe ?avgpe))
    =>
    (assert (stock-value ?ticker undervalued)))

(defrule stock-value-is-overvalued
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (industry-avg-pe ?industry ?avgpe)
    (not (stock-value ?ticker ?))
    (stock-pe ?ticker ?pe)
    (stock-pb ?ticker ?pb)
    (stock-peg ?ticker ?peg)
    (or (test (> ?peg 1)) (or (test (> ?pb 3)) (test (> ?pe ?avgpe))))
    =>
    (assert (stock-value ?ticker overvalued)))

(defrule stock-risk-is-low
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (industry-avg-de ?industry ?avgde)
    (not (stock-risk ?ticker ?))
    (stock-beta ?ticker ?beta)
    (stock-de ?ticker ?de)
    (test (<= ?beta 1.33))
    (test (<= ?de ?avgde))
    => 
    (assert (stock-risk ?ticker 1)))

(defrule stock-risk-is-medium
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (industry-avg-de ?industry ?avgde)
    (not (stock-risk ?ticker ?))
    (stock-beta ?ticker ?beta)
    (stock-de ?ticker ?de)
    (test (> ?beta 1.33))
    (test (<= ?de ?avgde))
    => 
    (assert (stock-risk ?ticker 0.5)))

(defrule stock-risk-is-high
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (industry-avg-de ?industry ?avgde)
    (not (stock-risk ?ticker ?))
    (stock-beta ?ticker ?beta)
    (stock-de ?ticker ?de)
    (test (> ?de ?avgde))
    => 
    (assert (stock-risk ?ticker 0)))

(defrule stock-efficiency-is-high
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (stock-dividend-yield ?ticker ?dividend)
    (not (stock-efficiency ?ticker ?))
    (industry-avg-dividend ?industry ?inddividend)
    (stock-fcf-current-yr ?ticker ?current)
    (stock-fcf-prior-yr ?ticker ?prior)
    (test (>= ?current ?prior))
    (test (>= ?dividend ?inddividend))
    =>
    (assert (stock-efficiency ?ticker 1)))

(defrule stock-efficiency-is-medium
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (stock-dividend-yield ?ticker ?dividend)
    (not (stock-efficiency ?ticker ?))
    (industry-avg-dividend ?industry ?inddividend)
    (stock-fcf-current-yr ?ticker ?current)
    (stock-fcf-prior-yr ?ticker ?prior)
    (test (>= ?current ?prior))
    (test (< ?dividend ?inddividend))
    =>
    (assert (stock-efficiency ?ticker 0.5)))

(defrule stock-efficiency-is-low
    (stock ?name ?ticker)
    (not (stock-efficiency ?ticker ?))
    (stock-fcf-current-yr ?ticker ?current)
    (stock-fcf-prior-yr ?ticker ?prior)
    (test (< ?current ?prior))
    =>
    (assert (stock-efficiency ?ticker 0)))

(defrule stock-pe-is
    (stock ?name ?ticker)
    (not (stock-pe ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s Price to Earnings (P/E) Ratio?")
    (assert (stock-pe ?ticker (read))))

(defrule stock-de-is
    (stock ?name ?ticker)
    (not (stock-de ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s Debt to Equity (D/E) Ratio?")
	(assert (stock-de ?ticker (read))))

(defrule stock-pb-is
    (stock ?name ?ticker)
    (not (stock-pb ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s Price to Book (P/B) Ratio?")
    (assert (stock-pb ?ticker (read))))

(defrule stock-peg-is
    (stock ?name ?ticker)
    (not (stock-peg ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s Price/Earnings to Growth (PEG) Ratio?")
    (assert (stock-peg ?ticker (read))))

(defrule stock-beta-is
    (stock ?name ?ticker)
    (not (stock-beta ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s volatility in relation to the overall market (beta)?")
    (assert (stock-beta ?ticker (read))))

(defrule stock-dividend_yield-is
    (stock ?name ?ticker)
    (not (stock-dividend-yield ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s Divided yield (percent)?")
    (assert (stock-dividend-yield ?ticker (read))))

(defrule stock-fcf-current-yr-is
    (stock ?name ?ticker)
    (not (stock-fcf-current-yr ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s current year Free Cash Flow (millions)?")
    (assert (stock-fcf-current-yr ?ticker (read))))

(defrule stock-fcf-prior-yr-is
    (stock ?name ?ticker)
    (not (stock-fcf-prior-yr ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s prior year Free Cash Flow (millions)?")
    (assert (stock-fcf-prior-yr ?ticker (read))))

(defrule industry-is
	(stock ?name ?ticker)
    (not (belongs-to-industry ?ticker ?))
	=>
    (printout t "What industry does " ?name " belong to?")
	(assert (belongs-to-industry ?ticker (read))))

(defrule industry-rank-is
    (stock ?name ?ticker)
    (not (industry-rank ?ticker ?))
    =>
    (printout t "What is " ?ticker "'s industry ranking?")
    (assert (industry-rank ?ticker (read))))

(defrule industry-avg-de-is
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (not (industry-avg-de ?industry ?))
    =>
    (printout t "What is the " ?industry " industry average D/E?")
	(assert (industry-avg-de ?industry (read))))

(defrule industry-avg-pe-is
    (stock ?name ?ticker)
    (belongs-to-industry ?ticker ?industry)
    (not (industry-avg-pe ?industry ?))
    =>
    (printout t "What is the " ?industry " industry average P/E?")
	(assert (industry-avg-pe ?industry (read))))

(defrule no-recommendation-value
    ?fact <- (investment-goal value) 
    (not (best-value-stock ? ?))
    =>
    (printout t "No stock recommendations meet the criteria for a Value investment strategy" crlf))

(defrule no-recommendation-growth
    ?fact <- (investment-goal growth) 
    (not (best-growth-stock ? ?))
    =>
    (printout t "No stock recommendations meet the criteria for a Growth investment strategy" crlf))