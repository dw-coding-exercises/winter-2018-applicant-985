(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]
            [clj-http.client :as http]))

;; Utils
(defn update-vals
  "The flavor of `map-vals` that I decided to go with.

  ;;

  `(update-vals {:a 1 :b 2} inc) => {:a 2 :b 3}`

  ;;

  Casts everything to a `clojure.lang.PersistentMap` so be careful"
  [m f & args]
  (reduce (fn [r [k v]] (assoc r k (apply f v args))) {} m))

(defn sanitize-param
  "Replaces spaces with underscores '_' and
  converts all letters to lowercase."
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

;; String formats to be used by the `ocd-ids` multimethod
(def place-id "ocd-division/country:us/state:%s/place:%s")
(def state-id "ocd-division/country:us/state:%s")
(def national-id "ocd-division/country:us")


(defmulti ocd-ids
  "Creates a vector of `ocd-id`s based on
  the parameters available in the input map `m`.

  Defaults to the national `ocd-id` only."
  (fn [m] (set (keys m))))

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
;; Ideally, stuff like voting-methods and voting-registration would be spec'd
;; and I would have a multimethod built off of those specs to make the code look
;; much less ugly and work more flexibly, but it be what it be!
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
       [:div (str reg-method)]])))

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

;  Brownie points time!
;
;  Some of this code is good, I think.
;  Some of it is really bad.
;  That's okay though because the bad code isn't unfixable!
;
;  If I was looking at this for the first time, my biggest gripe would
;  be that there is a lot going on in this ns.
;
;  I think my first step would be to a file structure like,
;  src/my_exercise/home.clj
;  src/my_exercise/home/view.clj
;  src/my_exercise/search.clj
;  src/my_exercise/search/view.clj
;
;  My second biggest gripe is the lack of specs.
;  I really enjoy the flexibility of multimethods and leveraging spec
;  for things like the `ocd-id` multimethod.
;  Given the flexible nature of the data coming back from Turbovote,
;  I think the views would appreciate some multimethods like I described
;  above the `voting-methods` function.
;
;  My third move would be to get organized with the building of the
;  Turbovote URL. Right now it's pretty choppy which I'm not big on.
;  A little TLC and some smarter threading macro action and you've got
;  a very nice little data pipeline for easier testing and simpler code.
;
;  My last and final move would be toward serious validation/execution scheme
;  for the code a la http://cjohansen.no/referentially-transparent-crud/
;
;  At the end of the day it looks like exactly what it is, I think, which is
;  nervous code written under a time crunch. Which is cool, because I think
;  my best projects have all started there at some point trying to get a
;  quick and dirty POC done to impress some peeps! Just as long as those bad
;  practices are cleaned up in small bits, you'll wind up in a good spot.
