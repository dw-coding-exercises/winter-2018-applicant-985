(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]))

(defn search-params
  "Select only the valid search params.
  For this exercise, it'll be city and state from the form."
  [m]
  (select-keys m [:city :state]))

(defn fetch-upcoming-elections
  [{:keys [params] :as request}]
  (str (search-params params)))

(defn upcoming-elections
  [request]
  [:div (fetch-upcoming-elections request)])

(defn page
  "Create the search page view from the request"
  [request]
  (html5
    (upcoming-elections request)))

