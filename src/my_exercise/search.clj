(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]
            [clj-http.client :as http]))

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
  "Builds a vector of 1 or more ocd-ids to query the
  Turbovote API. Used with `fetch-upcoming-elections`."
  [params]
  (->> params
       search-params
       (into {} (filter (comp some? seq val)))
       ocd-ids))

(defn turbovote-url
  [ocd-ids]
  (apply str (apply conj ["https://api.turbovote.org/elections/upcoming?district-divisions="] (interpose "," ocd-ids))))

(defn fetch-upcoming-elections
  "Makes an HTTP request to the Turbovote API to get upcoming election data
  base on the ocd-ids from `build-ocd-ids`."
  [{:keys [params] :as request}]
  (str (http/get (turbovote-url (build-ocd-ids (search-params params))))))

(defn upcoming-elections
  "Creates the view for upcoming elections."
  [request]
  [:div (fetch-upcoming-elections request)])

(defn page
  "Create the search page view from the request"
  [request]
  (html5
    (upcoming-elections request)))

