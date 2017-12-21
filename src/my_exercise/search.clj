(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]
            [clj-http.client :as http]))

;; Declarations
(declare ocd-ids)

;; Utils
(defn update-vals
  [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn sanitize-param
  [s]
  (clojure.string/lower-case
    (clojure.string/replace s " " "_")))

(defn search-params
  "Select only the valid search params.
  For this exercise, it'll be city and state from the form."
  [m]
  (update-vals (select-keys m [:city :state]) sanitize-param))

(defn turbovote-url
  "Builds a turbovote url to make a request for election data
  based off of a collection of OCD-IDs.

  Lacks validation so be careful."
  [ocd-ids]
  (apply str (apply conj ["https://api.turbovote.org/elections/upcoming?district-divisions="] (interpose "," ocd-ids))))

(defn build-ocd-ids
  "Builds a vector of 1 or more ocd-ids to query the
  Turbovote API. Used with `fetch-upcoming-elections`."
  [params]
  (->> params
       search-params
       (into {} (filter (comp some? seq val)))
       ocd-ids))

(defn fetch-upcoming-elections
  "Makes an HTTP request to the Turbovote API to get upcoming election data
  base on the ocd-ids from `build-ocd-ids`."
  [{:keys [params] :as request}]
  (read-string (:body (http/get (turbovote-url (build-ocd-ids (search-params params)))))))

;; OCD IDs
(defmulti ocd-ids (fn [m] (set (keys m))))
(defmethod ocd-ids :default [params] ["ocd-division/country:us"])

(defmethod ocd-ids #{:city :state}
  [{:keys [city state] :as params}]
  [(format "ocd-division/country:us/state:%s/place:%s" state city)
   (format "ocd-division/country:us/state:%s" state)
   "ocd-division/country:us"])

(defmethod ocd-ids #{:state}
  [{:keys [state] :as params}]
  [(format "ocd-division/country:us/state:%s" state)
   "ocd-division/country:us"])

;; Views
(defn election-row
  "Build a table row of election data"
  [election-data]
  nil)

(defn election-table
  "Builds a table of election data"
  [elections]
  [:ul
   (for [e elections]
     [:li e])])

(defn upcoming-elections
  "Creates the view for upcoming elections."
  [request]
  (let [election-data (fetch-upcoming-elections request)]
    [:div
     [:div
      [:h4 "Upcoming elections in your area"]]
     [:div
      [:p "Here is your request"]
      [:p (str request)]]
     [:div
      (for [election election-data]
        [:ul
         [:li election]])]]))

(defn page
  "Create the search page view from the request"
  [request]
  (html5
    (upcoming-elections request)))

