(deftemplate fx-pe "price to equity ratio"
    0 100
    ((undervalued (z 15 16.5))
     (fair (pi 2 16.5))
     (overvalued (s 16.5 18))))

(deftemplate fx-pb "price to book ratio"
    0 100
    ((undervalued (z 1 3))
     (fair (pi 1 2))
     (overvalued (s 2 3))))

(deftemplate fx-peg "price/earnings to growth"
    0 5
    ((undervalued (z 0.75 1))
     (fair (pi 1 1))
     (overvalued (s 1 2))))

(deftemplate fx-de "debt to equity ratio"
    0 20
    ((low (z 1 1.5))
     (avg (pi 1 1.5))
     (high (s 1.5 2))))

(deftemplate fx-beta "volatility"
    0 5
    ((low (z 1 2))
     (avg (pi 1 1.5))
     (high (s 1.5 2))))

(deftemplate fx-current "liquidity ratio"
    0 5
    ((low (s 1.2 2))
     (avg (pi 0.5 1.2))
     (high (z 1 1.2))))

(deftemplate fx-dividend "Dividend yield"
    0 15
    ((low (z 1 2))
     (avg (pi 1 3))
     (high (s 3 6))))

(deftemplate fx-rank "Industry rank"
    0 500
    ((low (s 50 100))
     (avg (pi 25 50))
     (high (z 10 50))))

(deftemplate STOCK
    (slot stock-name (type STRING) (default ?NONE))
    (slot ticker (type SYMBOL) (default ?NONE))
    (slot pe (type FUZZY-VALUE fx-pe))
    (slot pb (type FUZZY-VALUE fx-pb))
    (slot peg (type FUZZY-VALUE fx-peg))
    (slot de (type FUZZY-VALUE fx-de))
    (slot beta (type FUZZY-VALUE fx-beta))
    (slot current (type FUZZY-VALUE fx-current))
    (slot dividend-yield (type FUZZY-VALUE fx-dividend))
    (slot industry-rank (type FUZZY-VALUE fx-rank))
)

(deffacts initialize
    (STOCK (stock-name "Tesla")
           (ticker TSLA)
           (pe extremely overvalued)  ;75.01
           (pb extremely overvalued)  ;25.02
           (peg overvalued) ;2.4
           (de extremely low) ;0.08
           (beta somewhat high) ;2.17
           (current low) ;1.43
           (dividend-yield extremely low) ;0.0
           (industry-rank more-or-less avg) ;65
    ) 
    (STOCK (stock-name "General Motors")
           (ticker GM)
           (pe very undervalued) ;5.28
           (pb very undervalued) ;0.77
           (peg very undervalued) ;0.54
           (de low) ;1.14
           (beta more-or-less low) ;1.27
           (current avg) ;1.15
           (dividend-yield low) ;0.0
           (industry-rank more-or-less avg) ;64
    )
    (STOCK (stock-name "Ford Motor")
           (ticker F)
           (pe very undervalued) ;7.9
           (pb somewhat undervalued) ;1.39
           (peg undervalued) ;0.87
           (de somewhat high) ;1.92
           (beta more-or-less avg) ;1.33
           (current avg) ;1.16
           (dividend-yield more-or-less avg) ;2.61
           (industry-rank somewhat somewhat high) ;22
    )
    (STOCK (stock-name "United Airlines")
           (ticker UAL)
           (pe overvalued) ;93.23
           (pb overvalued) ;3.11
           (peg undervalued) ;0.0
           (de high) ;11.77
           (beta low) ;1.29
           (current high) ;1.04
           (dividend-yield low) ;0.0
           (industry-rank low) ;146
    )
    (STOCK (stock-name "Delta Airlines")
           (ticker DAL)
           (pe undervalued) ;11.36
           (pb overvalued) ;5.35
           (peg extremely undervalued) ;0.0
           (de extremely high) ;18.94
           (beta low) ;1.2
           (current high) ;0.66
           (dividend-yield low) ;0.0
           (industry-rank low) ;113
    )
    (STOCK (stock-name "Southwest Airlines")
           (ticker LUV)
           (pe undervalued) ;14.52
           (pb fair) ;2.04
           (peg overvalued) ;2.42
           (de low) ;1.03
           (beta low) ;1.0
           (current low) ;1.66
           (dividend-yield low) ;0.0
           (industry-rank low) ;234
    )
    (STOCK (stock-name "Apple")
           (ticker AAPL)
           (pe overvalued) ;26.45
           (pb overvalued) ;44.67
           (peg overvalued) ;2.09
           (de avg) ;1.63
           (beta avg) ;1.23
           (current high) ;0.86
           (dividend-yield low) ;0.57
           (industry-rank high) ;3
    )
    (STOCK (stock-name "Google")
           (ticker GOOGL)
           (pe overvalued) ;22.07
           (pb overvalued) ;5.94
           (peg overvalued) ;1.86
           (de low) ;0.06
           (beta low) ;1.08
           (current low) ;2.81
           (dividend-yield low) ;0.0
           (industry-rank high) ;8
    )
    (STOCK (stock-name "IBM")
           (ticker IBM)
           (pe undervalued) ;13.92
           (pb overvalued) ;6.11
           (peg overvalued) ;1.99
           (de high) ;2.28
           (beta low) ;0.85
           (current high) ;0.88
           (dividend-yield high) ;5.01
           (industry-rank avg) ;49
    )
    (STOCK (stock-name "Microsoft")
           (ticker MSFT)
           (pe overvalued) ;27.85
           (pb very overvalued) ;12.65
           (peg overvalued) ;2.38
           (de very low) ;0.28
           (beta low) ;0.93
           (current low) ;1.78
           (dividend-yield low) ;0.88
           (industry-rank high) ;21
    )
)

(defrule value-stock
    ?s <- (STOCK (ticker ?ticker)
           (pe undervalued)
           (pb undervalued)
           (peg undervalued)
           (de not high)
           (beta not high)
           (current not high)
           (dividend-yield not very low)
           (industry-rank not extremely low)
    )
    =>
    (assert (category ?ticker value))
)
(defrule growth-stock
    ?s <- (STOCK (ticker ?ticker)
           (pe overvalued)
           (pb overvalued)
           (peg overvalued)
           (de not very high)
           (beta not very low)
           (current low)
           (dividend-yield not very high)
           (industry-rank more-or-less high))
    =>
    (assert (category ?ticker growth))
)

(defrule best-value-stock-initial
    ?x <- (category ?ticker value)
    (not (best-value-stock ? ?))
    =>
    (assert (best-value-stock ?ticker (get-cf ?x)))
)
(defrule best-value-stock-update
    ?x <- (category ?ticker value)
    ?fact <- (best-value-stock ? ?bestscore)
    (test (> (get-cf ?x) ?bestscore))
    =>
    (assert (best-value-stock ?ticker (get-cf ?x)))
    (retract ?fact)
)
(defrule best-growth-stock-initial
    ?x <- (category ?ticker growth)
    (not (best-growth-stock ? ?))
    =>
    (assert (best-growth-stock ?ticker (get-cf ?x)))
)
(defrule best-growth-stock-update
    ?x <- (category ?ticker growth)
    ?fact <- (best-growth-stock ? ?bestscore)
    (test (> (get-cf ?x) ?bestscore))
    =>
    (assert (best-growth-stock ?ticker (get-cf ?x)))
    (retract ?fact)
)
(defrule investment-goal-is
    (not (investment-goal ?))
    =>
    (printout t crlf "Select an investment strategy (value or growth), or (quit)?" crlf)
    (printout t "-->")
    (assert (investment-goal (read)))
)
(defrule investment-goal-is-value
    ?goal <- (investment-goal value)
    (best-value-stock ?ticker ?)
    =>
    (printout t "Best value stock recommendation: " ?ticker crlf)
    (retract ?goal)
)
(defrule investment-goal-is-growth
    ?goal <- (investment-goal growth)
    (best-growth-stock ?ticker ?)
    =>
    (printout t "Best growth stock recommendation: " ?ticker crlf)
    (retract ?goal)
)