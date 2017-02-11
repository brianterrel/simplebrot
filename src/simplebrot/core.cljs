(ns simplebrot.core
  (:require [reagent.core :as reagent :refer [atom]]))

(enable-console-print!)



;; define your app data so that it doesn't get over-written on reload

(defonce app-state (atom {:text "Simplebrot"
                          :scale 125
                          :center [250 250]}))

(defn square-magnitude
  "Returns the square of Euclidean distance between two points"
  [x1 y1 x2 y2]
  ()
  (+ (* (- x1 x2) (- x1 x2))
     (* (- y1 y2) (- y1 y2))))

(defn draw-pixel! [point value context]
  (let [[x y] point]
    (set! (.-fillStyle context) (str "rgb(" value "," value "," value ")"))
    (. context (fillRect x y 5 5))))

(defn pixel-to-point [pixel center scale]
  (let [[x y] pixel
        [cx cy] center
        a (/ (- x cx) scale)
        b (/ (- cy y) scale)]
    [a b]))

(defn brot-val [pixel]
  (let [[c-real c-i] (pixel-to-point pixel (:center @app-state) (:scale @app-state))]
    (loop [count 50
           z-real 0
           z-i 0]
      (cond
        (identical? 0 count) 0
        (> (square-magnitude z-real z-i 0 0) 4) (* count 5)
        :else (recur
                (dec count)
                (+ c-real (- (* z-real z-real) (* z-i z-i)))
                (+ c-i (* 2 z-real z-i)))))))


(defn draw-from-list!
  "draw pixels from a list of pixels"
  [pixels context]
  (loop [loop-pixel (first pixels)
         further-pixels (rest pixels)]
    (if (nil? loop-pixel) nil
      (do
        (draw-pixel! loop-pixel (brot-val loop-pixel) context)
        (recur
          (first further-pixels)
          (rest further-pixels))))))

(defn lattice-point? [point size]
  (let [[x y] point]
    (if (and (= 0 (mod x size))
             (= 0 (mod y size)))
        true
        false)))

(defn coords-from-event [event]
  (let [x-offset (.-left (.getBoundingClientRect (. js/document (getElementById "main-canvas"))))
        x-position (.-clientX event)
        x (- x-position x-offset)
        y-offset (.-top (.getBoundingClientRect (. js/document (getElementById "main-canvas"))))
        y-position (.-clientY event)
        y (- y-position y-offset)]
    [x y]))

(defn center-from-coords [coords]
  (let [[x y] coords
        [cx cy] (:center @app-state)
        a (+ cx (- 250 x))
        b (- cx (- y 250))]
    [a b]))

(defn render-canvas [event state]
  (.profile js/console "rendering canvas")
  (let [context (. (. js/document (getElementById "main-canvas")) (getContext "2d"))
        x (:scale @state)]
    (draw-from-list! (for [x (range 500) y (range 500)] [x y]) context))
    ;(draw-from-list! (filter #(lattice-point? % 5)(for [x (range 500) y (range 500)] [x y])) context))
  (.profileEnd js/console))

(defn render-canvas-handler [event]
  (render-canvas event app-state))

(defn zoom-in-handler [event]
  (swap! app-state assoc-in [:scale] (* 2 (:scale @app-state)))
  (render-canvas-handler event))

(defn zoom-out-handler [event]
  (swap! app-state assoc-in [:scale] (quot (:scale @app-state) 2))
  (render-canvas-handler event))

(defn canvas-click-handler [event]
  (swap! app-state assoc-in [:center] (center-from-coords (coords-from-event event)))
  (render-canvas-handler event))

(defn simplebrot []
  [:center
    [:div
      [:h1 (:text @app-state)]
      [:canvas {:id "main-canvas"
                :height 500
                :width 500
                :on-click canvas-click-handler}]
      [:br]
      [:button {:on-click render-canvas-handler}
        "Draw"]
      [:button {:on-click zoom-in-handler}
        "Zoom in"]
      [:button {:on-click zoom-out-handler}
        "Zoom out"]]])
(reagent/render-component [simplebrot]
                          (. js/document (getElementById "app")))

(defn on-js-reload [])
  ;; optionally touch your app-state to force rerendering depending on
  ;; your application
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
