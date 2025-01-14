(ns modeling
  (:use [econ])
  (:require [tablecloth.api :as tc]
            [clojure.math :as cljmath]
            [nextjournal.clerk :as clerk]
            [tech.v3.dataset.math :as math]
            [fastmath.ml.regression :as reg]
            [clojure.instant :as instant]))


;; # Modeling
;; Given this, can we model the answers to these questions? We have the inflation data from before
(clerk/plotly
  {:data [{:x (seq (:date inflation-data))
           :y (seq (:inflation inflation-data))
           :name "Inflation"}]
   :layout {:title {:text "Inflation"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Inflation"}}}})
;; Let's pull in a few other data sources.
;; From https://fred.stlouisfed.org/series/UNRATE we can get the unemployment rate
^{:nextjournal.clerk/visibility {:result :hide}}
(def unemployment
  (-> "data/UNRATE.csv"
      (tc/dataset {:key-fn keyword})
      (tc/map-columns :date [:observation_date] #(-> % 
                                                     (.plusDays 14)
                                                     (.atStartOfDay (java.time.ZoneId/of "UTC"))
                                                     (.toInstant)
                                                     (java.util.Date/from)))
      (tc/rename-columns {:UNRATE :unemployment})
      (tc/drop-rows #(.before (:date %) first-date)))); 

(clerk/plotly
  {:data [{:x (seq (:date unemployment))
           :y (seq (:unemployment unemployment))
           :name "Unemployment"}]
   :layout {:title {:text "Unemployment"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Unemployment"}}}})

;; From https://fred.stlouisfed.org/series/DFF we can get interest rates
;; It's a daily statistic so we can average them
^{:nextjournal.clerk/visibility {:result :hide}}
(def interest-rates
  (-> "data/DFF.csv"
      (tc/dataset {:key-fn keyword})
      (tc/map-columns :year [:observation_date] #(.getYear %))
      (tc/map-columns :month [:observation_date] #(.getMonthValue %))
      (tc/select-columns [:year :month :DFF])
      (tc/group-by [:year :month])
      (tc/mean [:DFF])
      (tc/rename-columns {"summary" :interest-rate})
      (tc/map-columns :date [:year :month] (fn [year, month] (instant/read-instant-date (format "%d-%02d-15" year month))))
      (tc/select-columns [:date :interest-rate])
      (tc/drop-rows #(.before (:date %) first-date))))

(clerk/plotly
  {:data [{:x (seq (:date interest-rates))
           :y (seq (:interest-rate interest-rates))
           :name "Interest Rates"}]
   :layout {:title {:text "Interest Rates"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Interest Rates"}}}})

(def joined-data
  (-> full-data
      (tc/inner-join inflation-data :date)
      (tc/inner-join unemployment :date)
      (tc/inner-join interest-rates :date)))

;; We can look at the correlations

(let [columns [:unemployment :inflation :interest-rate :ics_all :ice_all :icc_all :pago_r_all :pexp_r_all :bus12_r_all :bus5_r_all :dur_r_all]
      correlations (-> joined-data
                       (tc/select-columns columns)
                       (math/correlation-table))
      measures [:unemployment :inflation :interest-rate]
      headers (concat [:measure] columns)]
  (clerk/table {:head headers
                :rows (for [measure measures]
                  (map (assoc (apply hash-map (apply concat (measure correlations))) :measure measure) headers))}))

;; Unemployment and Inflation appear to correlate best with the indexes. Interest rates have a correlation with inflation 
;; (which is to be expected as raising interest rates is a common response to inflation) and aren't really correlated to any index or question except for
;; the 5 year business expectations, which has a much strong correlation to inflation. So we will use unemployment and inflation in our model.
;; We can also add an interaction between the two.

(def data
  (-> joined-data
      (tc/map-columns :unemployment*inflation [:unemployment :inflation] *)))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn model
  [ds target columns]
  (reg/lm (ds target) (-> ds (tc/select-columns columns) tc/rows) {:names columns}))

^{:nextjournal.clerk/visibility {:result :hide}}
(defn predict
  [m ds]
  (let [xs (-> ds (tc/select-columns (rest (:names m))) tc/rows)]
    (map #(reg/predict m %) xs)))

;; ## Modeling indexes
;; ### Model for consumer sentiment
^{:nextjournal.clerk/visibility {:result :hide}}
(def ics-model (model data :ics_all [:unemployment :inflation :unemployment*inflation]))
(-> ics-model
    println
    with-out-str
    clerk/code)
(clerk/plotly
  {:data [{:x (seq (:date data))
           :y (seq (:ics_all data))
           :name "Actual"}
          {:x (seq (:date data))
           :y (predict ics-model data)
           :name "Predicted"}]
   :layout {:title {:text "Modeled Consumer Sentiment"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Index"}}}})

;; ### Model for consumer expectations
^{:nextjournal.clerk/visibility {:result :hide}}
(def ice-model (model data :ice_all [:unemployment :inflation :unemployment*inflation]))
(-> ice-model
    println
    with-out-str
    clerk/code)
(clerk/plotly
  {:data [{:x (seq (:date data))
           :y (seq (:ice_all data))
           :name "Actual"}
          {:x (seq (:date data))
           :y (predict ice-model data)
           :name "Predicted"}]
   :layout {:title {:text "Modeled Consumer Expectations"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Index"}}}})

;; ### Model for current conditions
^{:nextjournal.clerk/visibility {:result :hide}}
(def icc-model (model data :icc_all [:unemployment :inflation :unemployment*inflation]))
(-> icc-model
    println
    with-out-str
    clerk/code)
(clerk/plotly
  {:data [{:x (seq (:date data))
           :y (seq (:icc_all data))
           :name "Actual"}
          {:x (seq (:date data))
           :y (predict icc-model data)
           :name "Predicted"}]
   :layout {:title {:text "Modeled Current Conditions"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Index"}}}})

;; All three models predict pretty well, though the current conditions model appears to be estimating the economy is doing better than people report it is in the post pandemic era.
;; Let's analyze that model a big more. First the fitted vs residuals

(defn fitted-vs-residuals
  [model names]
  (clerk/plotly
    {:data [{:x (:fitted model)
             :y (:raw (:residuals model))
             :type "scatter"
             :mode "markers"
             :text (seq names)}]
     :layout {:title {:text "Fitted vs Residuals"}
              :xaxis {:title {:text "Fitted"}}
              :yaxis {:title {:text "Residuals"}} }}))

(fitted-vs-residuals icc-model (:date data))

;; This challenges both the linear and equal variance assumptions of the linear model.

;; Can we get a better model if we include quadratic terms?

(def quad-data
  (-> data
      (tc/map-columns :unemployment-squared [:unemployment] #(* % %))
      (tc/map-columns :inflation-squared [:inflation] #(* % %))))

(def icc-quad-model
  (model quad-data :icc_all [:unemployment :inflation :unemployment*inflation :unemployment-squared :inflation-squared]))
(-> icc-quad-model
    println
    with-out-str
    clerk/code)
(clerk/plotly
  {:data [{:x (seq (:date data))
           :y (seq (:icc_all data))
           :name "Actual"}
          {:x (seq (:date data))
           :y (predict icc-quad-model quad-data)
           :name "Predicted"}]
   :layout {:title {:text "Modeled Current Conditions"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Index"}}}})
(fitted-vs-residuals icc-quad-model (:date data))
;; It helps a little but not much. And the inflation^2 component isn't even significant.
;; We also see that it's blowing up on a few outliers, especially during the pandemic itself. Let's look at the x data plotted against each other

(clerk/plotly
  {:data [{:x (seq (:inflation data))
           :y (seq (:unemployment data))
           :text (seq (:date data))
           :type "scatter"
           :mode "lines+markers"
           :marker {:size 4
                    :colorscale "Hot"
                    :color (seq (:icc_all data))}
           :line {:width 1 :color "grey"}
           }]
   :layout {:title {:text "Inflation vs Unemployment"}
            :xaxis {:title {:text "Inflation"}}
            :yaxis {:title {:text "Unemployment"}}}})

;; This does show us something interesting. The pandemic era is outside the norms of the pre-pandemic era. We have both a period of time where the unemployment rate was unusually high,
;; and another where the rate was unusually low. Previous times when inflation was high also had high unemployment, and we never had unemployment as high as early in the pandemic.

;; Another thing that becomes apparent from this plot is that people's perceptions appear to be relative, not absolute. If unemployment is high and goes down, people are more positive
;; than if it is low and rises to the same level.

;; Since extremes seem to be blowing up, lets try square roots instead of quadratic terms

(def root-data
  (-> data
      (tc/map-columns :unemployment-root [:unemployment] #(cljmath/sqrt %))
      (tc/map-columns :inflation-root [:inflation] #(cljmath/sqrt %))))
(def icc-root-model
  (model root-data :icc_all [:unemployment :inflation :unemployment*inflation :unemployment-root :inflation-root]))
(-> icc-root-model
    println
    with-out-str
    clerk/code)
(clerk/plotly
  {:data [{:x (seq (:date data))
           :y (seq (:icc_all data))
           :name "Actual"}
          {:x (seq (:date data))
           :y (predict icc-root-model root-data)
           :name "Predicted"}]
   :layout {:title {:text "Modeled Current Conditions"}
            :xaxis {:title {:text "Month"}}
            :yaxis {:title {:text "Index"}}}})
;; That improves things a bit more
