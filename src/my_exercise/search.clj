(ns my-exercise.search
  (:require [hiccup.page :refer [html5]]))

;; Views
(defn fetch-upcoming-elections
  [{:keys [params] :as request}]
  (str params))

(defn upcoming-elections
  [request]
  [:div (fetch-upcoming-elections request)])

(defn page
  "Create the search page view from the request"
  [request]
  (html5
    (upcoming-elections request)))

