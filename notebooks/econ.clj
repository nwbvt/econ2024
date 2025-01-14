;; # Economic Perception
^{:nextjournal.clerk/visibility {:result :hide}}
(ns econ 
  (:require [tablecloth.api :as tc]
            [tablecloth.column.api :as tcc]
            [fastmath.ml.regression :as reg]
            [nextjournal.clerk :as clerk]
            [tech.v3.dataset.math :as math]
            [clojure.instant :as instant]))

;; ## Consumer Sentiment Index

;; Consumer sentiment data taken from https://data.sca.isr.umich.edu/data-archive/mine.php on December 20th, 2024

^{:nextjournal.clerk/visibility {:result :hide}}
(def full-data (-> "data/sca-tableall-on-2024-Dec-20.csv"
                       (tc/dataset {:key-fn keyword})
                       (tc/map-columns :date [:Month :yyyy] (fn [month, year] (instant/read-instant-date (format "%d-%02d-15" year month))))))

;; Looking at the data since the 2008 financial crash
^{:nextjournal.clerk/visibility {:result :hide}}
(def start-date (instant/read-instant-date "2008-01-01"))

^{:nextjournal.clerk/visibility {:result :hide}}
(def consumer-sentiment (-> full-data
                            (tc/drop-rows #(.before (:date %) start-date))
                            (tc/map-columns :pre-2020 [:date] #(.before % (instant/read-instant-date "2020-01-01")))))

(-> consumer-sentiment
    (tc/select-columns [:date :ics_all :ice_all :icc_all])
    (tc/order-by [:date] :desc)
    (clerk/table))

(clerk/code
  (tc/info (tc/select-columns consumer-sentiment [:ics_all :ice_all :icc_all])))

(clerk/plotly
  {:data [{:y (seq (:ics_all consumer-sentiment))
           :x (seq (:date consumer-sentiment))
           :name "Consumer Sentiment"}
          {:y (seq (:ice_all consumer-sentiment))
           :x (seq (:date consumer-sentiment))
           :name "Consumer Expectations"}
          {:y (seq (:icc_all consumer-sentiment))
           :x (seq (:date consumer-sentiment))
           :name "Current Economic Conditions"}]
   :layout {:title {:text "Consumer Sentiment"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Index Value"}}
            } })

;; Lets pull out the questions that contribute to the indexes
^{:nextjournal.clerk/visibility {:result :hide}}
(def index-sources [:pago_r_all :pexp_r_all :bus12_r_all :bus5_r_all :dur_r_all])

(clerk/code
  (tc/info (-> consumer-sentiment
               (tc/select-columns index-sources))))

(clerk/plotly
  {:data (for [source index-sources]
           {:y (seq (source consumer-sentiment))
            :x (seq (:date consumer-sentiment))
            :name (symbol source)})
   :layout {:title {:text "Sources for Consumer sentiment"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Positive Rate"}}}})

(clerk/table 
  (conj 
    (for [question index-sources]
      (let [means (-> consumer-sentiment
                      (tc/group-by [:pre-2020])
                      (tc/mean question)
                      (get "summary"))]
        (concat [(symbol question)] means [(apply - means)])))
    ["Question" "Pre Pandemic" "Post Pandemic" "Difference"]))

;; ## Current Economic Conditions
;; ### Durable Goods

;; It looks like the question that really went off the deep end (including relative to 2008) and didn't recover after the pandemic was the DUR question.
;; From the codebook that translates to 
;; > "About the big things people buy for their homes ‐‐ such as furniture, a refrigerator, stove, television, and things like that.  Generally speaking, do you think now is a good or bad time for people to buy major household items?

;; Why are people saying that?
(let [answers [:durrn_np_all :durrn_nr_all :durrn_nt_all]]
  (clerk/plotly
    {:data (for [answer answers]
             {:y (seq (answer consumer-sentiment))
              :x (seq (:date consumer-sentiment))
              :name ({:durrn_np_all "Prices" :durrn_nr_all "Interest Rates" :durrn_nt_all "Times"} answer)})
     :layout {:title {:text "Reasons for Durable Goods Answer"}
              :xaxis {:title {:text "Month"}}
              :yaxis {:title {:text "Positive Rate"}}}}))

;;Let break it down by individual answers
(let [answers [:durrn_lp_all :durrn_biap_all :durrn_lr_all :durrn_biar_all :durrn_gt_all :durrn_hp_all :durrn_hr_all :durrn_tb_all :durrn_fb_all]]
  (clerk/plotly
    {:data (for [answer answers]
             {:y (seq (tcc/* (answer consumer-sentiment) (if (answer #{:durrn_lp_all :durrn_biap_all :durrn_lr_all :durrn_biar_all :durrn_gt_all}) 1 -1)))
              :x (seq (:date consumer-sentiment))
              :name ({:durrn_lp_all "Good: Prices Are Low" :durrn_biap_all "Good: Prices Won't Come Down Again" :durrn_lr_all "Good: Interest Rates Low"
                      :durrn_biar_all "Good: Interest Rates Will Rise" :durrn_gt_all "Good: Times Are Prosperous" :durrn_hp_all "Bad: Prices Are High"
                      :durrn_hr_all "Bad: Interest Rates Are High" :durrn_tb_all "Bad: Times Are Bad" :durrn_fb_all "Bad: Times Are Uncertain"} answer)
              :type "bar"})
     :layout {:title {:text "Reasons for Durable Goods Answer"}
              :barmode "relative"
              :bargap 0
              :xaxis {:title {:text "Month"}}
              :yaxis {:title {:text "Answer Rate"}}}}))

;; People are down on buying durable goods, but the reason changes. At the start of the pandemic, the reason was dominated by people answering "Times are bad; can't afford to buy" "Bad times ahead; uncertain future"
;; instead of "Times are good; prosperity". Which makes sense. 2020 was a bad time what with the pandemic and all.

;; But that changes in 2021. Those answers end up going back to close to normal, while in 2021 and 2022 it becomes dominated by "Prices are high" instead of "Prices are low; good buys available"

;; But that improves in 2023, getting back to normal by the start of 2024. Unfortunately then the third reason has ticked down, representing "Interest rates are high; credit is tight" instead of "Interest rates are low"

;; Are these answers connected to reality? Let's pull down inflation date from https://fred.stlouisfed.org/series/CORESTICKM159SFRBATL
^{:nextjournal.clerk/visibility {:result :hide}}
(def first-date
  (-> full-data
      :date
      seq
      first))

^{:nextjournal.clerk/visibility {:result :hide}}
(def inflation-data
  (-> "data/CORESTICKM159SFRBATL.csv"
      (tc/dataset {:key-fn keyword})
      (tc/map-columns :date [:observation_date] #(-> % 
                                                     (.plusDays 14)
                                                     (.atStartOfDay (java.time.ZoneId/of "UTC"))
                                                     (.toInstant)
                                                     (java.util.Date/from)))
      (tc/rename-columns {:CORESTICKM159SFRBATL :inflation})
      (tc/drop-rows #(.before (:date %) first-date))))

(clerk/plotly 
  {:data [{:y (seq (:durrn_lp_all consumer-sentiment))
           :x (seq (:date consumer-sentiment))
           :name "Prices Are Low"}
          {:y (seq (:durrn_biap_all consumer-sentiment))
           :x (seq (:date consumer-sentiment))
           :name "Prices Won't Come Back Down"}
          {:y (seq (:durrn_hp_all consumer-sentiment))
           :x (seq (:date consumer-sentiment))
           :name "Prices Are High"}
          {:y (seq (:inflation inflation-data))
           :x (seq (:date inflation-data))
           :name "Inflation Rate"
           :line {:width 5}
           :yaxis "y2"}]
   :layout {:title {:text "Inflation vs Price Concerns"}
            :xaxis {:title {:text "Month"}
                    :range [start-date (instant/read-instant-date "2025-01-01")]}
            :yaxis {:title "Response Rate"
                    :showgrid false
                    :range [0 50]}
            :yaxis2 {:title "Inflation Rate"
                     :showgrid false
                     :range [0 7]
                     :overlaying "y"
                     :side "right"}}})

;; Concerns about prices largely tracks the inflation rate, at least until we get to 2024. Then while the inflation rate continues its downward trend, the tradeoff between prices being low and too high reverse course. 
;; Since 2024 was an election year, this could be a result of campaigns reminding Americans about the inflation they had faced over the past administration.

;; ### Personal Finances

;; The PAGO question also contributes to the current conditions score. It also went down in the 20's, though it doesn't get as far low as it was during 2008. It translates to:
;; > "We are interested in how people are getting along financially these days.  Would you say that you (and your family living there) are better or worse off financially than you were a year ago?"

;; What where their reasons?
(let [answers [:pagorn_ny_all :pagorn_nad_all]]
  (clerk/plotly
    {:data (for [answer answers]
             {:y (seq (answer consumer-sentiment))
              :x (seq (:date consumer-sentiment))
              :name ({:pagorn_ny_all "Income" :pagorn_nad_all "Assets and Debts"} answer)})
     :layout {:title {:text "Reasons for Personal Finances Answer"}
              :xaxis {:title {:text "Month"}}
              :yaxis {:title {:text "Positive Rate"}}}}))

;; It looks like when the pandemic hit, people's incomes took a hit. That partially improved, but starting in late 2021, people's assets vs debts started getting worse

;; ## Consumer Expectations

;; The consumer expectations questions should be expected to be more opinionated and subject to "vibes" as they are prognasticating the future. Let's look at how they did.

;; ### Business Conditions in the next 12 months. It reads as follows:

;; > "Now turning to business conditions in the country as a whole ‐‐ do you think that during the next 12 months we'll have good times financially, or bad times, or what?"

;; Here are how people responded to it

(let [answers [:bus12_f_all :bus12_s_all :bus12_u_all :bus12_dk_all :bus12_na_all]]
  (clerk/plotly
    {:data (for [answer answers]
             {:y (seq (answer consumer-sentiment))
              :x (seq (:date consumer-sentiment))
              :name ({:bus12_f_all "Good Times" :bus12_s_all "Uncertain" :bus12_u_all "Bad Times" :bus12_dk_all "Don't Know" :bus12_na_all "N/A" } answer)})
     :layout {:title {:text "Business Conditions in the Next 12 Months"}
              :xaxis {:title {:text "Month"}}
              :yaxis {:title {:text "Response Percentage"}}}}))

;; Here we see the "good times" of the late teens quickly reversing come the pandemic. Things improve in early 2021, and by the summer things once again look good. 
;; But that drops away again come the summer, presumably when inflation becomes apparent. But the next year that slowly improves.
;; What's interesting here is that in early 2024 the "Uncertain" numbers shoot up. This may also be the result of the coming election. We see spikes in that response 
;; in other election years, though not as prominentThese answeres do not factor into the index but will make the BUS12 result more neutral.

;; ### Conditions in 5 Years
;; The BUS5 question has a similar trajectory as the BUS12 question, though not as pronounced. It reads as follows
;; > "Looking ahead, which would you say is more likely ‐‐ that in the country as a whole we'll have continuous good times during the next 5 years or so, or that we'll have periods of widespread unemployment or depression, or what?"

;; Here are the responses

(let [answers [:bus5_f_all :bus5_s_all :bus5_u_all :bus5_na_all]]
  (clerk/plotly
    {:data (for [answer answers]
             {:y (seq (answer consumer-sentiment))
              :x (seq (:date consumer-sentiment))
              :name ({:bus5_f_all "Good Times" :bus5_s_all "Uncertain" :bus5_u_all "Bad Times" :bus5_na_all "N/A" } answer)})
     :layout {:title {:text "Conditions in the Next 5 Years"}
              :xaxis {:title {:text "Month"}}
              :yaxis {:title {:text "Response Percentage"}}}}))

;; Unsurprisingly this looks similar to the BUS12 response only the Uncertain rate is higher.

;; ### Personal Expectations

;; The last component to the Expectations Index is the PEXP question. It reads:
;; >"Now looking ahead ‐‐ do you think that a year from now you (and your family living there) will be better off financially, worse off, or just about the same as now?"

;; And had the answers
(let [answers [:pexp_f_all :pexp_s_all :pexp_u_all :pexp_dk_all]]
  (clerk/plotly
    {:data (for [answer answers]
             {:y (seq (answer consumer-sentiment))
              :x (seq (:date consumer-sentiment))
              :name ({:pexp_f_all "Better" :pexp_s_all "Same" :pexp_u_all "Worse" :pexp_dk_all "N/A,DK" } answer)})
     :layout {:title {:text "Personal Expectations In a Year"}
              :xaxis {:title {:text "Month"}}
              :yaxis {:title {:text "Response Percentage"}}}}))

;; Interestingly, we don't have the sudden drop here when the pandemic started that we saw in other questions. This makes sense, people were expecting the pandemic to end and
;; life to get back to normal. We do see the rise in 2021 of negative responses we saw in other questions, which also started disapating in late 2022. However the fact that almost 
;; throughout the time period most people answered "The Same" limits the impact of this question.

;; ## Looking further

;; There are some other valuable measurements in here. PAGO5 is siilar to PAGO looks back a bit further
;; > "Now thinking back 5 years, would you say you (and your family living there) are better off or worse off financially now than you were 5 years ago?"

;; And PEXP5 looks forward a bit futher than PEXP5
;; > "And 5 years from now, do you expect that you (and your family living there) will be better off financially, worse off, or just about the same as now?"

(let [questions [:pago5_r_all :pexp5_r_all]]
  (clerk/plotly
    {:data (for [q questions]
             {:y (seq (q consumer-sentiment))
              :x (seq (:date consumer-sentiment))
              :name ({:pago5_r_all "5 Years Ago" :pexp5_r_all "5 Years From Now"} q)})
     :layout {:title {:text "5 Year Sentiments"}
              :xaxis {:title {:text "Month"}}
              :yaxis {:title {:text "Positive Rate"}}}}))

;; We have less historical context for these answers, but the 5 years ago answer is pretty damning. 
