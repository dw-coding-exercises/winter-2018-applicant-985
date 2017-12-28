(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]
            [clj-http.client :as http]))

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

;; OCD IDs
(def place-id "ocd-division/country:us/state:%s/place:%s")
(def state-id "ocd-division/country:us/state:%s")
(def national-id "ocd-division/country:us")

(defmulti ocd-ids (fn [m] (set (keys m))))
(defmethod ocd-ids :default [params] ["ocd-division/country:us"])

(defmethod ocd-ids #{:city :state}
  [{:keys [city state] :as params}]
  [(format place-id state city)
   (format state-id state)
   national-id])

(defmethod ocd-ids #{:state}
  [{:keys [state] :as params}]
  [(format state-id state)
   national-id])

(defmethod ocd-ids :default
  [_]
  [national-id])

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

;; Views
(defn voting-methods
  "Builds the section for voting methods"
  [{:keys [district-divisions] :as election}]
  (let [voting-list (map :voting-methods district-divisions)]
    (for [voting-set voting-list
          voting-map voting-set]
      (reduce-kv
        (fn [acc k v]
          (conj acc [:li (str (name k) " - " (cond (keyword? v) (name v)
                                                   (map? v) (get-in v [:voting-id])
                                                   :default v))]))
        [:div "<br />"]
        voting-map))))

(defn voting-registration
  "Builds the section for registration"
  [{:keys [district-divisions] :as election}]
  (let [reg-auth (:voter-registration-authority-level election)
        reg-methods-list (map :voter-registration-methods district-divisions)]
    (for [reg-methods-set reg-methods-list
          reg-method reg-methods-set]
      [:div "<br />"
       [:div (str y)]])))

(defn desc-div
  "Builds the divs for the election description
  information #{description, website, and date}"
  [{:keys [description website date]}]
  [:div
   [:div description]
   [:div [:a {:href website} "More info"]]
   [:div date]])

(defn upcoming-elections
  "Creates the view for upcoming elections."
  [request]
  (let [election-data (fetch-upcoming-elections request)]
     [:div
      (for [election election-data]
        [:div
          (desc-div election)
          [:ul "Voting Methods"
           (voting-methods election)]
          [:ul "Registration methods"
           (voting-registration election)]])]))

(defn page
  "Create the search page view from the request"
  [request]
  (html5
    (upcoming-elections request)))

