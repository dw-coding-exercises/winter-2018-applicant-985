(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]))


(defn search-params
  "Select only the valid search params.
  For this exercise, it'll be city and state from the form."
  [m]
  (select-keys m [:city :state]))

(defmulti ocd-ids (fn [m] (set (keys m))))

(defmethod ocd-ids #{:city :state}
  [{:keys [city state] :as params}]
  [(format "ocd-division/country:us/state:%s/place:%s" state city)
   (format "ocd-division/country:us/state:%s" state)
   "ocd-division/country:us"])

(defmethod ocd-ids #{:state}
  [{:keys [state] :as params}]
  [(format "ocd-division/country:us/state:%s" state)
   "ocd-division/country:us"])

(defmethod ocd-ids :default [params] ["ocd-division/country:us"])

(defn build-ocd-ids
  [params]
  (->> params
       search-params
       (into {} (filter (comp some? seq val)))
       ocd-ids))

(defn fetch-upcoming-elections
  [{:keys [params] :as request}]
  (str (build-ocd-ids (search-params params))))

(defn upcoming-elections
  [request]
  [:div (fetch-upcoming-elections request)])

(defn page
  "Create the search page view from the request"
  [request]
  (html5
    (upcoming-elections request)))

