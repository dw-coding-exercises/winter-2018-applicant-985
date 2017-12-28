(ns my-exercise.search-test
  (:require [clojure.test :refer :all]
            [my-exercise.search :refer :all]))

(deftest update-vals-test
  (testing "Update vals should operate on evey value in a map"
    (is (= {:a 2 :b 3}
           (update-vals {:a 1 :b 2} inc)))))

(deftest sanitize-param-test
  (testing "Should replace spaces with underscores and make lowercase"
    (is (= "foo_bar"
           (sanitize-param "Foo BAR")))))

(deftest search-params-test
  (testing "Sould only get :city and :state and then sanitize them"
    (is (= {:city "foo_bar"
            :state "baz_bang_bing"}
           (search-params {:city "Foo Bar" :state "Baz Bang Bing" :a 1})))))

(deftest turbovote-url-test
  (testing "Should build a proper URL given a coll of ocd-ids"
    (let [ocd-ids ["ocd-division/country:us/state:bar/place:foo"
                   "ocd-division/country:us/state:bar"
                   "ocd-division/country:us"]]
      (is (= "https://api.turbovote.org/elections/upcoming?district-divisions=ocd-division/country:us/state:bar/place:foo,ocd-division/country:us/state:bar,ocd-division/country:us"
             (turbovote-url ocd-ids))))))
